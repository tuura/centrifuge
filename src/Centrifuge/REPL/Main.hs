{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE UndecidableInstances #-}

module Main where

import System.IO
import System.IO.Unsafe
import System.Random
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.IntMap ((!))
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import Data.Monoid ((<>))

import Algebra.Graph.Class
import Algebra.Graph (edgeList)
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Language.Haskell.Interpreter as I
import Control.Monad.Catch
import Centrifuge.DSL.Generic

------------------------- Lib --------------------------------------------------
--------------------------------------------------------------------------------
data ModelState = ModelState { modules     :: IntMap.IntMap Module
                             , connections :: [Connection]
                             , dirty       :: Bool
                             } deriving (Show, Eq)

instance JSON.ToJSON ModelState where
  toJSON (ModelState modules connections _) =
    JSON.object [ "modules" .= JSON.toJSON (map snd . IntMap.toList $ modules)
                , "connections" .= JSON.toJSON connections
                ]

data Module = Module { moduleId :: Int
                     , x :: Int
                     , y :: Int
                     , description :: String
                     , cssClass :: String
                     , cssClasses :: [String]
                     , borderRadius :: Int
                     , width :: Int
                     , height :: Int
                     , ports :: [(Int, Int)]
                     }

instance JSON.ToJSON Module where
  toJSON m =
    JSON.object ["id" .= (("n" :: String) <> (show $ moduleId m :: String))
                , "x" .= x m
                , "y" .= y m
                , "description" .= ("Graph Node" :: Text.Text)
                , "class" .= ("node" :: Text.Text)
                , "classes" .= (["graph", "hide-label"] :: [Text.Text])
                , "border-radius" .= (10 :: Int)
                , "width" .= (20 :: Int)
                , "height" .= (20 :: Int)
                , "ports" .=
                  JSON.object [ "p" .= JSON.object [ "x" .= (10 :: Int)
                                                   , "y" .= (10 :: Int)
                                                   ]
                              ]
                ]

instance Eq Module where
  x == y = moduleId x == moduleId y

instance Show Module where
  show x = show . moduleId $ x

mkModule :: (Int, Int) -> Int -> Module
mkModule (x, y) i = Module { moduleId = i
                           , x = x -- unsafePerformIO $ randomInt
                           , y = y -- unsafePerformIO $ randomInt
                           , description = "Graph Module"
                           , cssClass = "Module"
                           , cssClasses = ["graph", "hide-label"]
                           , borderRadius = 10
                           , width = 20
                           , height = 20
                           , ports = [(10, 10)]
                           }

data Connection = Connection { moduleXId :: String
                             , moduleYId :: String
                             , portX     :: String
                             , portY     :: String
                             , connectionCssClasses :: [String]
                             }

instance JSON.ToJSON Connection where
  toJSON c = JSON.toJSON [moduleXId c, moduleYId c, "p" :: String, "p" :: String
                    , ("[\"graph-connection\"]" :: String)]

instance Eq Connection where
  p == q = moduleXId p == moduleXId q && moduleYId p == moduleYId q

instance Show Connection where
  show c = show (moduleXId c, moduleYId c)

mkConnection :: Int -> Int -> Connection
mkConnection x y = Connection { moduleXId = "n" <> (show $ x :: String)
                              , moduleYId = "n" <> (show $ y :: String)
                              , portX = "p"
                              , portY = "p"
                              , connectionCssClasses = ["graph-connection"]
                              }

mkConnections :: [Int] -> [Int] -> [Connection]
mkConnections vertices1 vertices2 =
  foldl (\acc v ->
    map (mkConnection v) vertices2 ++ acc) [] vertices1

instance Graph ModelState where
  type Vertex ModelState = Module
  empty = ModelState IntMap.empty [] True
  vertex v = ModelState (IntMap.singleton (moduleId v) v) [] True
  overlay (ModelState modulesX connectionsX _)
          (ModelState modulesY connectionsY _) =
    ModelState (IntMap.union modulesX modulesY)
               (L.union connectionsX connectionsY)
               True
  connect (ModelState modulesX connectionsX _)
          (ModelState modulesY connectionsY _) =
    ModelState (IntMap.union modulesX modulesY)
               (L.union (L.union connectionsX connectionsY)
                        (mkConnections (map snd . IntMap.toList $
                                          IntMap.map moduleId modulesX)
                                       (map snd . IntMap.toList $
                                          IntMap.map moduleId modulesY)))
               True

instance ToGraph ModelState where
    type ToVertex ModelState = Module
    toGraph (ModelState {modules = ms, connections = cs}) =
      edges . map connectionToEdge $ cs
      where
        connectionToEdge :: Connection -> (Module, Module)
        connectionToEdge (Connection {moduleXId = xId, moduleYId = yId}) =
          let x = read . drop 1 $ xId
              y = read . drop 1 $ yId
          in (ms ! x, ms ! y)

-- ex :: Char -> ModelState
-- ex 'a' = overlay (vertex (mkModule 1)) (vertex (mkModule 2))
-- ex 'b' = connect (vertex (mkModule 1)) (vertex (mkModule 2))
-- ex 'c' = connect (vertex (mkModule 1))
--                  (overlay (vertex (mkModule 2)) (vertex (mkModule 3)))
-- ex 'd' = connect (vertex (mkModule 1)) (vertex (mkModule 1))
-- ex 'e' = overlay (connect (vertex (mkModule 1)) (vertex (mkModule 2)))
--                  (connect (vertex (mkModule 2)) (vertex (mkModule 3)))
-- ex _   = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------

instance MonadState s m => MonadState s (I.InterpreterT m) where
  get = lift get
  put = lift . put
  state = lift . state

newtype App a = App {runApp :: I.InterpreterT (StateT ModelState IO) a}
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadState ModelState
           , MonadThrow, MonadCatch, MonadMask, I.MonadInterpreter
           )

--------------------------------------------------------------------------------

decodeMessage :: I.MonadInterpreter m => String -> m (Maybe (Map.Map String String))
decodeMessage = pure . JSON.decode . LBS.pack . L.map BS.c2w

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  r <- flip evalStateT (ModelState IntMap.empty [] True) . I.runInterpreter . runApp $ do
    loadDependencies
    I.eval "()"
    loop
  case r of
    Left err -> print err
    Right () -> return ()

loop :: App ()
loop = do
  (Just request) <- decodeMessage =<< liftIO getLine
  response <- dispatch request
  liftIO . LBSC8.putStrLn $ response
  loop

dispatch :: (I.MonadInterpreter m, MonadState ModelState m) =>
            Map.Map String String -> m LBSC8.ByteString
dispatch (Map.lookup "get" -> Just "engine_name") =
  pure . JSON.encode $ JSON.object [ "result" .= JSON.String "success"
                                   , "return" .= JSON.String "Graphs"
                                   ]
dispatch (Map.lookup "eval" -> Just "model = init()") =
  pure . JSON.encode $ JSON.object [ "result" .= JSON.String "success"
                                   , "return" .= JSON.String ""
                                   ]
dispatch (Map.lookup "eval" -> Just expr) =
  handleAll (\e -> pure $ encodeException e) $
    case classifyInput expr of
      Aux -> do
        let command = head $ words expr
            arg = dropWhile (/= ' ') expr
        ret <- if | command == ":t" -> I.typeOf arg
                  | otherwise -> pure "Invalid aux command"
        pure . JSON.encode $
          JSON.object [ "result" .= JSON.String "success"
                      , "return" .= JSON.String (Text.pack ret)
                      ]
      Statement  -> do
        I.runStmt expr
        pure . JSON.encode $ JSON.object [ "result" .= JSON.String "success"
                                         , "return" .= JSON.String "()"
                                         ]
      Expression -> do
        result <- I.eval expr
        pure . JSON.encode $
          JSON.object [ "result" .= JSON.String "success"
                      , "return" .= JSON.String (Text.pack result)
                      ]
      Render -> do
        state <- get
        let ident = head . reverse . words $ expr
        result <- I.interpret ident (I.as :: (Network BS.ByteString))
        let state' = updateModel state (renderNetwork result)
        put state'
        pure . JSON.encode $
          JSON.object [ "result" .= JSON.String "success"
                      , "return" .= JSON.String (Text.pack . show $ result)
                      , "state"  .= state'
                      ]

encodeException :: SomeException -> LBSC8.ByteString
encodeException e =
  JSON.encode $
    JSON.object [ "result" .= JSON.String "exception"
                , "return" .= JSON.String (Text.pack . show $ e)]

exec :: String -> IO String
exec statement = undefined

-- | Statements return unit and modify the environment, expressions don't
data InputType = Statement | Expression | Render | Aux
  deriving (Show)

-- | Check whether input is a statement or an expression
classifyInput :: String -> InputType
classifyInput statement =
  -- Input is a statement if it contains any kind of "assignment" action
  if | L.isInfixOf "<-" statement ||
       (L.isInfixOf "let" statement &&
        (not $ L.isInfixOf "in" statement)) -> Statement
     | L.isInfixOf "render" statement -> Render
     | head statement == ':' -> Aux
     | otherwise -> Expression

renderNetwork :: (Ord a, Show a) => Network a -> ModelState
renderNetwork n =
  let modules = renderModule <$> n
      connections =
        let es = edgeList . fromNetwork $ n
        in  map renderConnection es
  -- in undefined
  in ModelState (translate modules) connections True
  where renderModule :: Show a => a -> Module
        renderModule x = mkModule ( (unsafePerformIO randomInt)
                                  , (unsafePerformIO randomInt))
                                  (parseId $ x)

        renderConnection :: Show a => (a, a) -> Connection
        renderConnection (x, y) = mkConnection (parseId $ x)
                                               (parseId $ y)

        parseId :: Show a => a -> Int
        parseId = read . drop 1 . L.filter (/= '\"') . show

updateModel :: ModelState -> ModelState -> ModelState
updateModel old@ModelState { modules = oldModules
                           , connections = oldConnections
                           }
            new@ModelState { modules = newModules
                           , connections = newConnections
                           } =
  let -- commonModuleIDs = map moduleId $
        -- IntMap.intersection oldModules' newModules
      oldModules' = IntMap.intersection oldModules newModules
      newModules' = IntMap.difference newModules oldModules

      oldConnections' = L.intersect oldConnections newConnections
      newConnections' = (L.\\) newConnections oldConnections
  in ModelState
      (IntMap.union oldModules' newModules')
      (L.union oldConnections' newConnections') True

--------------------------------------------------------------------------------

randomInt :: IO Int
randomInt = fst . randomR (-500,500) <$> newStdGen

translate :: Network Module -> IntMap.IntMap Module
translate =
  foldl (\acc m -> IntMap.insert (moduleId m) m acc) IntMap.empty

--------------------------------------------------------------------------------

loadDependencies :: I.MonadInterpreter m => m ()
loadDependencies = do
  I.set [(I.:=) I.languageExtensions [ I.OverloadedStrings
                                     , I.ScopedTypeVariables
                                     ]
        ]
  I.setImportsQ [ ("Data.ByteString", Nothing)
                , ("Data.ByteString.Internal", Nothing)
                , ("Prelude", Nothing)
                , ("Centrifuge.DSL.Generic", Nothing)
                , ("Data.Monoid", Nothing)
                ]

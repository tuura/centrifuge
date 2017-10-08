{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import System.IO
import System.IO.Unsafe
import System.Random
import Control.Monad.IO.Class (liftIO)
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

getModule :: Int -> Module
getModule i = Module { moduleId = i
                 , x = unsafePerformIO $ randomInt
                 , y = unsafePerformIO $ randomInt
                 , description = "Graph Module"
                 , cssClass = "Module"
                 , cssClasses = ["graph", "hide-label"]
                 , borderRadius = 10
                 , width = 20
                 , height = 20
                 , ports = [(10, 10)]
                 }
  where
    randomInt :: IO Int
    randomInt = fst . randomR (-500,500) <$> newStdGen

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

ex :: Char -> ModelState
ex 'a' = overlay (vertex (getModule 1)) (vertex (getModule 2))
ex 'b' = connect (vertex (getModule 1)) (vertex (getModule 2))
ex 'c' = connect (vertex (getModule 1))
                 (overlay (vertex (getModule 2)) (vertex (getModule 3)))
ex 'd' = connect (vertex (getModule 1)) (vertex (getModule 1))
ex 'e' = overlay (connect (vertex (getModule 1)) (vertex (getModule 2)))
                 (connect (vertex (getModule 2)) (vertex (getModule 3)))
ex _   = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------

decodeMessage :: I.MonadInterpreter m => String -> m (Maybe (Map.Map String String))
decodeMessage = pure . JSON.decode . LBS.pack . L.map BS.c2w

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  r <- I.runInterpreter $ loadDependencies >> loop
  case r of
    Left err -> print err
    Right () -> return ()

dispatch :: (I.MonadInterpreter m) => Map.Map String String ->
                                      m LBSC8.ByteString
dispatch (Map.lookup "get" -> Just "engine_name") =
  pure . JSON.encode $ JSON.object [ "result" .= JSON.String "success"
                                   , "return" .= JSON.String "Graphs"
                                   ]
dispatch (Map.lookup "eval" -> Just "model = init()") =
  pure . JSON.encode $ JSON.object [ "result" .= JSON.String "success"
                                   , "return" .= JSON.String ""
                                   , "state" .= ex 'a']
dispatch (Map.lookup "eval" -> Just expr) =
  handleAll (\e -> pure $ encodeException e) $
    case classifyInput expr of
      Statement  -> do
        I.runStmt expr
        pure . JSON.encode $ JSON.object [ "result" .= JSON.String "success"
                                         , "return" .= JSON.String "()"
                                         ]
      Expression -> do
        result <- I.eval expr
        -- result <- I.interpret expr I.infer
        pure . JSON.encode $
          JSON.object [ "result" .= JSON.String "success"
                      , "return" .= JSON.String (Text.pack result)
                      ]
      Render -> do
        let ident = head . reverse . words $ expr
        result <- I.interpret ident (I.as :: (RawNetwork))
        let state = renderNetwork result
        -- result <- I.interpret "[1,2,3]" (I.as :: [Int])
        -- result <- I.interpret "[\"abc\"]" (I.as :: ([String]))
        -- result <- I.interpret "[\"cde\"]" (I.as :: ([LBS.ByteString]))
        pure . JSON.encode $
          JSON.object [ "result" .= JSON.String "success"
                      -- , "return" .= JSON.String (Text.pack . show . modules $ state)
                      , "return" .= JSON.String (Text.pack . show $ result)
                      , "state"  .= state
                      ]

encodeException :: SomeException -> LBSC8.ByteString -- JSON.Value
encodeException e =
  JSON.encode $
    JSON.object [ "result" .= JSON.String "exception"
                , "return" .= JSON.String (Text.pack . show $ e)]

loop :: I.MonadInterpreter m => m ()
loop = do
  (Just request) <- decodeMessage =<< liftIO getLine
  response <- dispatch request
  liftIO . LBSC8.putStrLn $ response
  loop

exec :: String -> IO String
exec statement = undefined

-- | Statements return unit and modify the environment, expressions don't
data InputType = Statement | Expression | Render
  deriving (Show)

-- | Check whether input is a statement or an expression
classifyInput :: String -> InputType
classifyInput statement =
  -- Input is a statement if it contains any kind of "assignment" action
  if | L.isInfixOf "<-" statement ||
       (L.isInfixOf "let" statement &&
        (not $ L.isInfixOf "in" statement)) -> Statement
  -- otherwise it's an expression
     | L.isInfixOf "render" statement -> Render
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
        renderModule x = getModule (parseId . show $ x)

        renderConnection :: Show a => (a, a) -> Connection
        renderConnection (x, y) = mkConnection (parseId $ show x)
                                               (parseId $ show y)

        parseId :: String -> Int
        parseId = read . drop 1 . L.filter (/= '\"')



translate :: Network Module -> IntMap.IntMap Module
translate =
  foldl (\acc m -> IntMap.insert (moduleId m) m acc) IntMap.empty
  -- IntMap.unions . traverse (\m -> IntMap.singleton (moduleId m) m)

-- | Check if a statement contains a readRawNetwork function
-- detectNetworkParsing :: String -> Bool
-- detectNetworkParsing str = L.isInfixOf "readRawNetwork" statement


loadDependencies :: I.MonadInterpreter m => m ()
loadDependencies = do
  I.set [(I.:=) I.languageExtensions [I.OverloadedStrings]]
  I.setImports ["Prelude", "Centrifuge.DSL.Generic", "Data.ByteString"]

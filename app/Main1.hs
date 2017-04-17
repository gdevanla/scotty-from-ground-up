--module Main3 where

import qualified Control.Monad.Trans.State.Strict as ST
import Data.List
import Control.Monad

-- State Monad
-- How to use a State Monad

type Route = String -> String

data AppState = AppState { routes::[Route]}

type AppStateT = ST.State AppState

constructResponse :: [String] -> String
constructResponse = unwords

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

routeHandler1 :: [Char] -> String
routeHandler1 request =
  constructResponse [
  "\nrequest in handler1: got " ++ request]


routeHandler2 request = constructResponse [
      "\n\trequest in handler2 got :" ++ request]

routeHandler3 request = constructResponse [
  "\n\t\trequest in handler3:" ++ request]

addRoute mf = ST.modify $ \s -> addRoute' mf s

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1
  addRoute routeHandler2
  addRoute routeHandler3

runMyApp initial_string my_app = do
  let s = ST.execState my_app AppState{ routes = []}
  let output = foldl (flip ($)) initial_string (routes s)
  return $ output

main = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp request myApp
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    main

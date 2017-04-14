--module Main3 where

import qualified Control.Monad.Trans.State.Strict as ST
import Data.List
import Control.Monad

-- State Monad
-- How to use a State Monad

type Route = String -> String

data AppState = AppState { routes::[Route]}

type AppStateT = ST.State AppState

construct_response args = intercalate " " args

add_route' mf s@(AppState {routes = mw}) = s {routes = mf:mw}

route_handler1 request =
  construct_response [
  "\nrequest in handler1: got " ++ request]

route_handler2 request = construct_response [
      "\n\trequest in handler2 got :" ++ request]

route_handler3 request = construct_response [
  "\n\t\trequest in handler3:" ++ request]

add_route mf = ST.modify $ \s -> add_route' mf s

myApp :: AppStateT ()
myApp = do
  add_route route_handler1
  add_route route_handler2
  add_route route_handler3

runMyApp initial_string my_app = do
  let s = ST.execState my_app $ AppState { routes = []}
  let output = foldl (flip ($)) initial_string (routes s)
  return $ output

main = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp request myApp
    case response of
      Just x -> putStrLn $ x
      Nothing -> putStrLn "Error"
    main

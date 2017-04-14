--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import Data.List
import Control.Monad

-- State Monad
-- How to use a State Monad

type Application = String -> String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}

type AppStateT = ST.State AppState

add_route' mf s@(AppState {routes = mw}) = s {routes = mf:mw}

construct_response args = intercalate " " args

route_handler1 request =
  construct_response [
  "request in handler1: got " ++ request]

route_handler2 request = construct_response [
      "request in handler2 got :" ++ request]

route_handler3 request = construct_response [
  "request in handler3:" ++ request]

default_route request = construct_response [
  request , "processed by default_route"]


route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    mw input_string
     --maybe tryNext
  else
    tryNext

add_route mf pat = ST.modify $ \s -> add_route' (route mf pat) s

myApp :: AppStateT ()
myApp = do
  add_route route_handler1 (\s -> s == "handler1")
  add_route route_handler2 (\s -> s == "handler2")
  add_route route_handler3 (\s -> s == "handler3")

runMyApp def my_app request = do
  let s = ST.execState my_app $ AppState { routes = []}
  let output = foldl (flip ($)) def (routes s) $ request
  return $ output

main = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp default_route myApp request
    case response of
      Just x -> putStrLn $ x
      Nothing -> putStrLn "Error"
    main
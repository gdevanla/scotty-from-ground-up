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

addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

constructResponse = unwords

routeHandler1 request =
  constructResponse [
  "request in handler1: got " ++ request]

routeHandler2 request = constructResponse [
      "request in handler2 got :" ++ request]

routeHandler3 request = constructResponse [
  "request in handler3:" ++ request]

default_route request = constructResponse [
  request , "processed by default_route"]


route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    mw input_string
     --maybe tryNext
  else
    tryNext

addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler1")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")

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

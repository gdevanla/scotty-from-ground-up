--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Data.List
import Control.Monad

-- State Monad
-- How to use a State Monad

type Application = String -> Maybe String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}

--type ActionError = String

type AppStateT = ST.State AppState

--type ActionT = Exc.ExceptT ActionError Maybe String

addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

constructResponse = unwords

route_handler1 request =
  Just $ constructResponse [
  "request in handler1: got " ++ request]

route_handler2 request = Nothing

route_handler3 request =
  Just $ constructResponse [
  "request in handler3:" ++ request]

default_route request =
  Just $ constructResponse [
  request , "processed by default_route"]

--handler s = Just "There was an error returned: " ++ s

route mw pat mw1 request =
  let tryNext = mw1 request in
  if pat request
  then
    let r = id (mw request) in
      case r of
        Just x -> r
        Nothing -> tryNext
  else
    tryNext

addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  addRoute route_handler1 (== "handler1")
  addRoute route_handler2 (== "handler2")
  addRoute route_handler3 (== "handler3")

runMyApp def my_app request = do
  let s = ST.execState my_app $ AppState { routes = []}
  let output = foldl (flip ($)) def (routes s) $ request
  output

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

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

add_route' mf s@(AppState {routes = mw}) = s {routes = mf:mw}

construct_response args = intercalate " " args

route_handler1 request =
  Just $ construct_response [
  "request in handler1: got " ++ request]

route_handler2 request = Nothing

route_handler3 request =
  Just $ construct_response [
  "request in handler3:" ++ request]

default_route request =
  Just $ construct_response [
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

add_route mf pat = ST.modify $ \s -> add_route' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  add_route route_handler1 (\s -> s == "handler1")
  add_route route_handler2 (\s -> s == "handler2")
  add_route route_handler3 (\s -> s == "handler3")

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
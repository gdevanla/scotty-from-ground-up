--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Error.Class
import Control.Monad
import Data.List
import Data.Maybe

-- State Monad
-- How to use a State Monad

type Application = String -> Exc.ExceptT ActionError Maybe String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}

type ActionError = String

type AppStateT = ST.State AppState

--type ActionT = Exc.ExceptT ActionError Maybe String

construct_response args = intercalate " " args

add_route' mf s@(AppState {routes = mw}) = s {routes = mf:mw}

--route_handler1 input = Exc.ExceptT $ Nothing --Just $ input ++ " middleware1 called\n"

route_handler2 input = Exc.ExceptT $ Just $ Right $ input ++ " middleware2 called\n"

route_handler_buggy :: String -> Exc.ExceptT ActionError Maybe String
route_handler_buggy input = throwError "test"

--route_handler3 input = Exc.ExceptT $ Just $ Right $ input ++ " middleware3 called\n"

--default_route request = Exc.ExceptT $ Just $ Right $ request ++ " default route called\n"

route_handler1 request =
  Exc.ExceptT $ Just $ Right $ construct_response [
  "request in handler1: got " ++ request]

route_handler3 request =
  Exc.ExceptT $ Just $ Right $ construct_response [
  "request in handler3:" ++ request]

default_route request =
  Exc.ExceptT $ Just $ Right $ construct_response [
  request , "processed by default_route"]

handler :: String -> Exc.ExceptT ActionError Maybe String
handler s = Exc.ExceptT $ Just $ Right $ "There was an error returned: " ++ s

route mw pat mw1 request =
  let tryNext = mw1 request in
  if pat request
  then
    --maybe tryNext mw $ return request
    let r = mw request `catchError` handler in
      --either (const Nothing) ((flip const) Nothing) $ r
      r
  else
    tryNext

add_route mf pat = ST.modify $ \s -> add_route' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  add_route route_handler1 (\s -> s == "handler1")
  add_route route_handler_buggy (\s -> s == "buggy")
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
    let value = Exc.runExceptT response
    putStrLn $ either id id $ fromJust $ value
    main


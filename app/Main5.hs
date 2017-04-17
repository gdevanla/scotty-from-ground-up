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

constructResponse = unwords

addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

--routeHandler1 input = Exc.ExceptT $ Nothing --Just $ input ++ " middleware1 called\n"

routeHandler2 input = Exc.ExceptT $ Just $ Right $ input ++ " middleware2 called\n"

routeHandlerBuggy :: String -> Exc.ExceptT ActionError Maybe String
routeHandlerBuggy input = throwError "test"

routeHandler1 request =
  Exc.ExceptT $ Just $ Right $ constructResponse [
  "request in handler1: got " ++ request]

routeHandler3 request =
  Exc.ExceptT $ Just $ Right $ constructResponse [
  "request in handler3:" ++ request]

defaultRoute request =
  Exc.ExceptT $ Just $ Right $ constructResponse [
  request , "processed by defaultRoute"]

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

addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler1")
  addRoute routeHandlerBuggy (== "buggy")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")

runMyApp def my_app request = do
  let s = ST.execState my_app AppState{ routes = []}
  let output = foldl (flip ($)) def (routes s) request
  output

main = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp defaultRoute myApp request
    let value = Exc.runExceptT response
    putStrLn $ either id id $ fromJust value
    main

--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import Data.List
import Control.Monad


type Application = String -> String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}
type AppStateT = ST.State AppState

--client functions

constructResponse :: [String] -> String
constructResponse = unwords

routeHandler1 :: String -> String
routeHandler1 request =
  constructResponse [
  "request in handler1: got " ++ request]

routeHandler2 :: String -> String
routeHandler2 request = constructResponse [
      "request in handler2 got :" ++ request]

routeHandler3 :: String -> String
routeHandler3 request = constructResponse [
  "request in handler3:" ++ request]

defaultRoute :: String -> String
defaultRoute request = constructResponse [
  request , "processed by defaultRoute"]

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler1")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")

main :: IO ()
main = myScotty myApp

-- framework functions

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: (String -> String) -> (String -> Bool)
  -> Route
route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    mw input_string
     --maybe tryNext
  else
    tryNext

addRoute ::
  Monad m =>
  (String -> String) -> (String -> Bool) -> ST.StateT AppState m ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s


runMyApp def app_state request = do
  let output = foldl (flip ($)) def (routes app_state) request
  return output


userInputLoop app_state = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp defaultRoute app_state request
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    userInputLoop app_state


myScotty my_app = do
    let app_state = ST.execState my_app AppState{ routes = []}
    userInputLoop app_state

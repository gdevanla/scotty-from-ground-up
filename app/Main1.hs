--module Main3 where

import qualified Control.Monad.Trans.State.Strict as ST
import Data.List
import Control.Monad

type Route = String -> String

data AppState = AppState { routes::[Route]}
type AppStateT = ST.State AppState


-- client functions
constructResponse :: [String] -> String
constructResponse = unwords


routeHandler1 :: String -> String
routeHandler1 request =
  constructResponse [
  "\nrequest in handler1: got " ++ request]

routeHandler2 :: String -> String
routeHandler2 request = constructResponse [
      "\n\trequest in handler2 got :" ++ request]

routeHandler3 :: String -> String
routeHandler3 request = constructResponse [
  "\n\t\trequest in handler3:" ++ request]

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1
  addRoute routeHandler2
  addRoute routeHandler3

main :: IO ()
main = myScotty myApp

--  framework methods

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

addRoute mf = ST.modify $ \s -> addRoute' mf s

runMyApp :: Monad m => String -> AppState -> m String
runMyApp initial_string my_app = do
  let output = foldl (flip ($)) initial_string (routes my_app)
  return $ output

userInputLoop app_state = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp request app_state
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    userInputLoop app_state

myScotty my_app = do
  let app_state = ST.execState my_app AppState{routes=[]}
  userInputLoop app_state
  
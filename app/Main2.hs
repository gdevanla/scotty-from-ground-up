--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import Data.List

-- State Monad
-- How to use a State Monad

type Application = String -> String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}

type AppStateT = ST.State AppState

addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

constructResponse = unwords

route_handler1 request =
  constructResponse [
  "\nrequest in handler1: got " ++ request]

route_handler2 request = constructResponse [
      "\n\trequest in handler2 got :" ++ request]

route_handler3 request = constructResponse [
  "\n\t\trequest in handler3:" ++ request]

route mw mw1 input_string =
  let tryNext = mw1 input_string in
  if input_string == "initial_string"
  then
    mw input_string
     --maybe tryNext
  else
    tryNext

addRoute mf = ST.modify $ \s -> addRoute' (route mf) s

myApp :: AppStateT ()
myApp = do
  addRoute route_handler1
  addRoute route_handler2
  addRoute route_handler3

runMyApp initial_string my_app = do
  let s = ST.execState my_app $ AppState { routes = []}
  let output = foldl (flip ($)) initial_string (routes s)
  return $ output

main = do
  print $ "Starting demonstration of routes"
  let x1 = runMyApp (\x-> "default") myApp
  case x1 of
    Just x -> print $ x "initial_string"
    Nothing -> print "error"

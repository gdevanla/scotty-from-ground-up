--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Data.List
import Data.Maybe
import Control.Monad

-- State Monad
-- How to use a State Monad

type Response = Maybe String
type Request = String

type Application = Request -> Response
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}
type AppStateT = ST.State AppState


-- client functions

constructResponse = unwords

routeHandler1 :: Request -> Response
routeHandler1 request =
  Just $ constructResponse [
  "request in handler1: got " ++ request]

routeHandler2 :: t -> Maybe a
routeHandler2 request = Nothing

routeHandler3 :: Request -> Response
routeHandler3 request =
  Just $ constructResponse [
  "request in handler3:" ++ request]

defaultRoute :: Request -> Response
defaultRoute request =
  Just $ constructResponse [
  request , "processed by defaultRoute"]

cond :: Eq t => t -> t -> Bool
cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler1")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")

main :: IO ()
main = myScotty myApp

-- framework functions
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route ::
  (Request -> Response) -> (Request -> Bool) -> Route
route mw pat mw1 request =
  let tryNext = mw1 request in
  if pat request
  then
    let r = mw request in
      if isJust r then r else tryNext
  else
    tryNext

addRoute ::
  Monad m =>
  (String -> Maybe String)
  -> (String -> Bool) -> ST.StateT AppState m ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

runMyApp ::
  (Request -> Response) -> AppState -> Request -> Response
runMyApp def app_state request = do
  let output = foldl (flip ($)) def (routes app_state) request
  output

userInputLoop :: AppState -> IO ()
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


myScotty :: ST.State AppState a -> IO ()
myScotty my_app = do
  let app_state = ST.execState my_app AppState{ routes = []}
  userInputLoop app_state
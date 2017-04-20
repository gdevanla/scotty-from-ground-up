--- Demonstrate handling routes only if previous one
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Error.Class
import Control.Monad
import Data.List
import Data.Maybe

type Response = String
type Request = String

type ActionError = String
type ActionT = Exc.Except ActionError Response
type Application = Request -> ActionT

type Route = Application -> Application

data AppState = AppState { routes:: [Route]}
type AppStateT = ST.State AppState

--client functions

constructResponse = unwords

routeHandler1 :: Request -> ActionT
routeHandler1 request =
  Exc.except $ Right $ constructResponse [
  "request in handler1: got " ++ request]

routeHandler2 :: Request -> ActionT
routeHandler2 input = Exc.except $ Right $ input ++ " middleware2 called\n"

routeHandlerBuggy :: Request -> ActionT
routeHandlerBuggy input = throwError "Error from routeHandlerBuggy"

routeHandler3 :: String -> ActionT
routeHandler3 request =
  Exc.except $ Right $ constructResponse [
  "request in handler3:" ++ request]

defaultRoute :: Request -> ActionT
defaultRoute request =
  Exc.except $ Right $ constructResponse [
  request , "processed by defaultRoute"]

cond :: Eq t => t -> t -> Bool
cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler1")
  addRoute routeHandlerBuggy (== "buggy")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")

main :: IO ()
main = myScotty myApp

--framework functions
errorHandler :: Request -> ActionT
errorHandler s = Exc.except $ Right $ "There was an error returned: " ++ s

route ::
  (Request -> ActionT)
  -> (Request -> Bool)
  -> (Request -> ActionT)
  -> Request
  -> ActionT
route mw pat mw1 request =
  let tryNext = mw1 request in
  if pat request
  then
    let r = mw request `catchError` errorHandler in
      r
  else
    tryNext

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

addRoute ::
  Monad m =>
  (Request -> ActionT)
  -> (Request -> Bool) -> ST.StateT AppState m ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

runMyApp :: (Request -> ActionT) -> AppState -> Request -> ActionT
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
    let value = Exc.runExcept response
    putStrLn $ either id id $ value
    main

myScotty :: ST.State AppState a -> IO ()
myScotty my_app = do
    let app_state = ST.execState my_app AppState{ routes = []}
    userInputLoop app_state

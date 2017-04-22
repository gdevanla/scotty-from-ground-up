--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Reader
import Control.Monad.Error.Class
import Data.Maybe

-- State Monad
-- How to use a State Monad

type Response = String
type Request =  String
type ActionError = String

type ActionT = Exc.ExceptT ActionError (Reader Request) Response

type Application = String -> String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}
type AppStateT = ST.State AppState

-- client functions
routeHandler1 :: ActionT
routeHandler1 = do
  input <- ask
  return $ "middlware_func1 got input = " ++ input

routeHandler2 :: ActionT
routeHandler2 = do
  input <- ask
  return $ "middlware_func2 got input = " ++ input

routeHandler3_buggy :: ActionT
routeHandler3_buggy = throwError "error from buggy handler"

routeHandler3 :: ActionT
routeHandler3 = do
  input <- ask
  return $ "routeHandler3 called = " ++ input

cond :: Eq t => t -> t -> Bool
cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler1")
  addRoute routeHandler3_buggy (== "buggy")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")

main :: IO ()
main = myScotty myApp

-- framework functions

errorHandler :: String -> ActionT
errorHandler error = do
  input <- ask
  return $ "There was an error returned for input=" ++ input ++ ", error=" ++ error 

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: ActionT -> (String -> Bool) -> Route
route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    let x = runAction mw input_string
        y = fromMaybe "" x
    in
      y
  else
    tryNext

runAction ::
  Exc.ExceptT ActionError (Reader Request) Response
  -> String -> Maybe String
runAction action request =
  let response = flip runReader request
                 $ Exc.runExceptT
                 $ action `catchError` errorHandler
      left =  (\x -> Just $ (++) "There was an error :" x)
      right = Just
  in
    either left right response

addRoute ::
  Monad m => ActionT -> (String -> Bool) -> ST.StateT AppState m ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

runMyApp :: Application -> AppState -> Application
runMyApp initial_string app_state = foldl (flip ($)) initial_string (routes app_state)

defRoute :: t -> [Char]
defRoute _ = "There was no route defined to process your request."

userInputLoop :: AppState -> IO ()
userInputLoop app_state = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'hanlder3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let x1 = runMyApp defRoute app_state request
    putStrLn x1
    putStrLn "\n\n\n"
    userInputLoop app_state

myScotty :: ST.State AppState a -> IO ()
myScotty my_app = do
  let app_state = ST.execState my_app AppState{routes = []}
  userInputLoop app_state

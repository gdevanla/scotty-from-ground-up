--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Reader
import Control.Monad.Error.Class
import Data.Maybe

-- State Monad
-- How to use a State Monad

type Response = String

type ActionT = Exc.ExceptT ActionError (Reader String) Response

type Application = String -> String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}

type ActionError = String

type AppStateT = ST.State AppState

--type ActionT = Exc.ExceptT ActionError Maybe String

addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

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

routeHandler3 = do
  input <- ask
  return $ "routeHandler3 called = " ++ input

handler :: String -> ActionT
handler error = do
  input <- ask
  return $ "There was an error returned " ++ input


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


runAction action request =
  let response = flip runReader request
                 $ Exc.runExceptT
                 $ action `catchError`  handler
      left = const $ Just "There was an error"
      right = Just
  in
    either left right response

addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler11")
  addRoute routeHandler3_buggy (== "buggy")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")

runMyApp initial_string my_app =
  let s = ST.execState my_app AppState{routes = []}
      output = foldl (flip ($)) initial_string (routes s) in
  output

defRoute _ = "There was no route defined to process your request."


main = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'hanlder3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let x1 = runMyApp defRoute myApp request
    putStrLn x1
    putStrLn "\n\n\n"
    main

--- Demonstrate handling routes only if previous one
import Data.List
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Reader
import Control.Monad.Error.Class

--
type Response = String
type Request = String

type ActionT a = Exc.ExceptT ActionError (ReaderT String (ST.State Response)) a

type Middleware = String -> String
type Route = Middleware -> Middleware

data AppState = AppState { middlewares:: [Route]}

type ActionError = String

type AppStateT = ST.State AppState


add_middleware' mf s@(AppState {middlewares = mw}) = s {middlewares = mf:mw}

construct_response args = intercalate "\n" args

route_handler1 :: ActionT ()
route_handler1 = do
  input_string <- ask
  let st = (
        ST.modify
          $ (\s -> construct_response [s, input_string, ": from route_handler1"]))
  lift . lift $ st

route_handler2 :: ActionT ()
route_handler2 = do
  input_string <- ask
  lift . lift $ (ST.modify $ (\s -> s ++ input_string ++ " inside middleware func 2"))


route_handler3_buggy :: ActionT ()
route_handler3_buggy = throwError "error from buggy handler"

route_handler3 :: ActionT ()
route_handler3 = do
  input_string <- ask
  lift . lift $ (ST.modify $ (\s -> s ++ input_string ++ " inside middleware func 3"))

add_middleware mf pat = ST.modify $ \s -> add_middleware' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

route :: ActionT () -> (Request -> Bool) -> Route
route mw pat mw1 request =
  let tryNext = mw1 request in
  if pat request
  then
    let x = runAction mw request
    in
      case x of
        Just x2 -> x2
        Nothing -> ""
  else
    tryNext

runAction :: ActionT () -> Request -> Maybe Response
runAction action request =
  let (a, s) = (flip ST.runState "")
               $ (flip runReaderT request)
               $ (Exc.runExceptT action)
      left = (const $ Just $ "There was an error")
      right = (const (Just $ s ++ "..state updated in run_action..")) in
    either  left right a

myApp :: AppStateT ()
myApp = do
  add_middleware route_handler1 (\s -> s == "middleware11")
  add_middleware route_handler3_buggy (\s -> s == "middleware1")
  add_middleware route_handler2 (\s -> s == "middleware2")
  add_middleware route_handler3 (\s -> s == "middleware3")

runMyApp initial_string my_app =
  let s = ST.execState my_app $ AppState { middlewares = []}
      output = foldl (flip ($)) initial_string (middlewares s) in
  output

defRoute _ = "There was no route defined to process your request."

main = do
  putStrLn "Please type in the request."
  request <- getLine
  putStrLn $ "Starting demonstration of middlewares"
  let x1 = runMyApp defRoute myApp request in
    putStrLn $ x1

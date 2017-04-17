--- Demonstrate handling routes only if previous one
import Data.List
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Reader
import Control.Monad.Error.Class
import Data.Maybe
--

type Response = String
type Request = String

type ActionT a = Exc.ExceptT ActionError (ReaderT String (ST.State Response)) a

type Application = String -> String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}

type ActionError = String

type AppStateT = ST.State AppState

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

constructResponse :: [[Char]] -> [Char]
constructResponse = unwords

routeHandler1 :: ActionT ()
routeHandler1 = do
  input_string <- ask
  let st =
        ST.modify
        (\_ -> constructResponse ["request:" ++ input_string, "processed by routeHandler1"])
  lift . lift $ st

routeHandler2 :: ActionT ()
routeHandler2 = do
  input_string <- ask
  lift . lift $ ST.modify  (\s -> s ++ input_string ++ " inside middleware func 2")

routeHandler3_buggy :: ActionT ()
routeHandler3_buggy = throwError "error from buggy handler"

routeHandler3 :: ActionT ()
routeHandler3 = do
  input_string <- ask
  lift . lift $ ST.modify (\s -> s ++ input_string ++ " inside middleware func 3")

handler :: String -> ActionT ()
handler error = lift . lift
  $ ST.modify (\s -> s ++ error ++ " inside middleware func 3")

addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

cond :: Eq t => t -> t -> Bool
cond condition_str = f where
  f i = i == condition_str

route :: ActionT () -> (Request -> Bool) -> Route
route mw pat mw1 request =
  let tryNext = mw1 request in
  if pat request
  then
    let x = runAction mw request
    in
      fromMaybe "" x
  else
    tryNext

runAction :: ActionT () -> Request -> Maybe Response
runAction action request =
  let (a, s) = flip ST.runState ""
               $ flip runReaderT request
               $ Exc.runExceptT
               $ action `catchError` handler
      left = const $ Just "There was an error"
      right = const $ Just s in
    either  left right a


myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler1")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")
  addRoute routeHandler3_buggy (== "buggy")


runMyApp ::
  (String -> String) -> ST.State AppState a -> String -> String
runMyApp initial_string my_app =
  let s = ST.execState my_app  AppState{routes = []}
      output = foldl (flip ($)) initial_string (routes s) in
  output


defRoute :: t -> [Char]
defRoute _ = "There was no route defined to process your request."

main :: IO ()
main = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'hanlder3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let x1 = runMyApp defRoute myApp request
    putStrLn x1
    putStrLn "\n\n\n"
    --main

--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Data.List
import Data.Maybe
import Control.Monad

-- State Monad
-- How to use a State Monad

type Application = String -> Maybe String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}

--type ActionError = String

type AppStateT = ST.State AppState

--type ActionT = Exc.ExceptT ActionError Maybe String

addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

constructResponse = unwords

routeHandler1 :: [Char] -> Maybe String
routeHandler1 request =
  Just $ constructResponse [
  "request in handler1: got " ++ request]

routeHandler2 :: t -> Maybe a
routeHandler2 request = Nothing

routeHandler3 :: [Char] -> Maybe String
routeHandler3 request =
  Just $ constructResponse [
  "request in handler3:" ++ request]

defaultRoute :: String -> Maybe String
defaultRoute request =
  Just $ constructResponse [
  request , "processed by defaultRoute"]

--handler s = Just "There was an error returned: " ++ s

route ::
  (t -> Maybe t1) -> (t -> Bool) -> (t -> Maybe t1) -> t -> Maybe t1
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

cond :: Eq t => t -> t -> Bool
cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  addRoute routeHandler1 (== "handler1")
  addRoute routeHandler2 (== "handler2")
  addRoute routeHandler3 (== "handler3")

runMyApp ::
  (String -> Maybe String)
  -> ST.State AppState a -> String -> Maybe String
runMyApp def my_app request = do
  let s = ST.execState my_app AppState{ routes = []}
  let output = foldl (flip ($)) def (routes s) request
  output

main :: IO ()
main = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp defaultRoute myApp request
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    main

--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Reader
import Control.Monad.Error.Class

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

add_route' mf s@(AppState {routes = mw}) = s {routes = mf:mw}

route_handler1 :: ActionT
route_handler1 = do
  input <- ask
  return $ "middlware_func1 got input = " ++ input

route_handler2 :: ActionT
route_handler2 = do
  input <- ask
  return $ "middlware_func2 got input = " ++ input

route_handler3_buggy :: ActionT
route_handler3_buggy = throwError "error from buggy handler"

route_handler3 = do
  input <- ask
  return $ "route_handler3 called = " ++ input

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
        y = case x of
              Just x2 -> x2
              Nothing -> "Nothing..."
    in
      y
  else
    tryNext


runAction action input_string =
  let response = do
        r <- ((flip runReader input_string) $ Exc.runExceptT action) `catchError`
             (\e -> ((flip runReader input_string) $ Exc.runExceptT (handler  e)))
        return r
      left = (const $ Just $ "There was an error")
      right = (Just) in
    either left right response

runAction2 action input_string =
  let response = do
        r <- (flip runReader input_string) $ Exc.runExceptT $ action `catchError`
             (\e -> (handler  e))
        return r
      left = (const $ Just $ "There was an error")
      right = (Just) in
    either left right response
  

add_route mf pat = ST.modify $ \s -> add_route' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  add_route route_handler1 (\s -> s == "handler11")
  add_route route_handler3_buggy (\s -> s == "buggy")
  add_route route_handler2 (\s -> s == "handler2")
  add_route route_handler3 (\s -> s == "handler3")

runMyApp initial_string my_app =
  let s = ST.execState my_app $ AppState { routes = []}
      output = foldl (flip ($)) initial_string (routes s) in
  output

defRoute _ = "There was no route defined to process your request."


main = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'hanlder3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let x1 = runMyApp defRoute myApp request
    putStrLn $ x1
    putStrLn "\n\n\n"
    main
    

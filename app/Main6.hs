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

handler :: ActionT
handler = do
  input <- ask
  return $ "There was an error returned " ++ input

route :: ActionT -> (String -> Bool) -> Route
route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    let x = do
          r <- runAction mw input_string
          return r
        x1 = either (\x1 -> Just $ "error =" ++ x1) (Just) x
        y = case x1 of
              Just x2 -> x2
              Nothing -> "Nothing..."
    in
      y
  else
    tryNext


runAction r input_string = do
  r <- (flip runReader input_string) $ Exc.runExceptT r
  return $ r
  --either (const Nothing) (Just) r

  -- let tryNext = mw1 input_string in
  -- if pat input_string
  -- then
  --   --maybe tryNext mw $ return input_string
  --   let r = mw input_string `catchError` handler in
  --     --either (const Nothing) ((flip const) Nothing) $ r
  --   r
  -- else
  --   tryNext

add_route mf pat = ST.modify $ \s -> add_route' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

myApp :: AppStateT ()
myApp = do
  add_route route_handler1 (\s -> s == "middleware11")
  add_route route_handler3_buggy (\s -> s == "middleware1")
  add_route route_handler2 (\s -> s == "middleware2")
  add_route route_handler3 (\s -> s == "middleware3")

runMyApp initial_string my_app =
  let s = ST.execState my_app $ AppState { routes = []}
      output = foldl (flip ($)) initial_string (routes s) in
  output

main = do
  print $ "Starting demonstration of routes"
  let x1 = runMyApp (\x-> "default middlware called") myApp "middleware1" in
    print $ show $ x1

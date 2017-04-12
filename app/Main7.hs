{-# LANGUAGE FlexibleContexts #-}
--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Reader
import Control.Monad.Error.Class

-- State Monad
-- How to use a State Monad

type Response = String

type ActionT a = Exc.ExceptT ActionError (ReaderT String (ST.State Response)) a

type Middleware = String -> String
type Route = Middleware -> Middleware

data AppState = AppState { middlewares:: [Route]}

type ActionError = String

type AppStateT = ST.State AppState

--type ActionT = Exc.ExceptT ActionError Maybe String

add_middleware' mf s@(AppState {middlewares = mw}) = s {middlewares = mf:mw}

middleware_func1 :: ActionT ()
middleware_func1 = do
  input_string <- ask
  lift . lift $ (ST.modify $ (\s -> s ++ input_string ++ "inside middleware func"))

  --ST.modify (\s -> s ++  "..and..input= " ++ input)
  --return $ ""

-- middleware_func2 :: ActionT String
-- middleware_func2 = do
--   input <- ask
--   return $ "middlware_func2 got input = " ++ input

middleware_func3_buggy :: ActionT ()
middleware_func3_buggy = throwError "error from buggy handler"

-- middleware_func3 :: ActionT String
-- middleware_func3 = do
--   input <- ask
--   return $ "middlware_func3 got_input = " ++ input

handler :: ActionT String
handler = do
  input <- ask
  return $ "handler with input = " ++ input

add_middleware mf pat = ST.modify $ \s -> add_middleware' (route mf pat) s

cond condition_str = f where
  f i = i == condition_str

route :: ActionT () -> (String -> Bool) -> Route
route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    let x = runAction mw input_string
    in
      case x of
        Just x2 -> x2
        Nothing -> "Nothing..."
  else
    tryNext

runAction :: ActionT () -> String -> Maybe Response
runAction action input_string =
  let (a, s) = (flip ST.runState "initial_state")
        $ (flip runReaderT input_string) $ (Exc.runExceptT action)
  in
  either (const $ Just $ "error:" ++ s) (const (Just $ s ++ "..state updated in run_action..")) a

myApp :: AppStateT ()
myApp = do
  add_middleware middleware_func1 (\s -> s == "middleware11")
  add_middleware middleware_func3_buggy (\s -> s == "middleware1")
  -- add_middleware middleware_func2 (\s -> s == "middleware2")
  -- add_middleware middleware_func3 (\s -> s == "middleware3")

runMyApp initial_string my_app =
  let s = ST.execState my_app $ AppState { middlewares = []}
      output = foldl (flip ($)) initial_string (middlewares s) in
  output

main = do
  print $ "Starting demonstration of middlewares"
  let x1 = runMyApp (\x-> "default middlware called") myApp "middleware11" in
    print $ show $ x1

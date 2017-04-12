--module Main3 where

import qualified Control.Monad.Trans.State.Strict as ST

-- State Monad
-- How to use a State Monad

data AppState = AppState { middlewares::[String->String]}

type AppStateT = ST.State AppState 

add_middleware' mf s@(AppState {middlewares = mw}) = s {middlewares = mf:mw}

middleware_func1 input = input ++ " middleware1 called\n"

middleware_func2 input = input ++ " middleware2 called\n"

middleware_func3 input = input ++ " middleware3 called\n"

add_middleware mf = ST.modify $ \s -> add_middleware' mf s

myApp :: AppStateT ()
myApp = do
  add_middleware middleware_func1
  add_middleware middleware_func2
  add_middleware middleware_func3

runMyApp initial_string my_app = do
  let s = ST.execState my_app $ AppState { middlewares = []}
  let output = foldl (flip ($)) initial_string (middlewares s)
  return $ Just output

main = do
  print $ "Starting demonstration of middlewares"
  let s = runMyApp "initial_string" myApp
  case s of
    Just x -> print $ x
    Nothing -> print "Error"


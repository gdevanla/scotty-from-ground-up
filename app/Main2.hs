--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST

-- State Monad
-- How to use a State Monad

type Middleware = String -> String
type Route = Middleware -> Middleware

data AppState = AppState { middlewares:: [Route]}

type AppStateT = ST.State AppState

add_middleware' mf s@(AppState {middlewares = mw}) = s {middlewares = mf:mw}

middleware_func1 input = input ++ " middleware1 called\n"

middleware_func2 input = input ++ " middleware2 called\n"

middleware_func3 input = input ++ " middleware3 called\n"

route :: Middleware -> Route
route mw mw1 input_string =
  let tryNext = mw1 input_string in
  if input_string == "initial_string"
  then
    mw input_string
     --maybe tryNext
  else
    tryNext

add_middleware mf = ST.modify $ \s -> add_middleware' (route mf) s

myApp :: AppStateT ()
myApp = do
  add_middleware middleware_func1
  add_middleware middleware_func2
  add_middleware middleware_func3

runMyApp initial_string my_app = do
  let s = ST.execState my_app $ AppState { middlewares = []}
  let output = foldl (flip ($)) initial_string (middlewares s)
  return $ output

main = do
  print $ "Starting demonstration of middlewares"
  let x1 = runMyApp (\x-> "default") myApp
  case x1 of
    Just x -> print $ x "initial_string"
    Nothing -> print "error"

    

    

--- Demonstrate handling routes only if previous one

import qualified Control.Monad.Trans.State.Strict as ST

-- State Monad
-- How to use a State Monad

type Application = String -> String
type Route = Application -> Application

data AppState = AppState { routes:: [Route]}

type AppStateT = ST.State AppState

add_route' mf s@(AppState {routes = mw}) = s {routes = mf:mw}

route_handler1 input = input ++ " middleware1 called\n"

route_handler2 input = input ++ " middleware2 called\n"

route_handler3 input = input ++ " middleware3 called\n"

route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    mw input_string
     --maybe tryNext
  else
    tryNext

add_route mf pat = ST.modify $ \s -> add_route' (route mf pat) s

myApp :: AppStateT ()
myApp = do
  add_route route_handler1 (\s -> s == "middleware1")
  add_route route_handler2 (\s -> s == "middleware2")
  add_route route_handler3 (\s -> s == "middleware3")

runMyApp initial_string my_app =
  let s = ST.execState my_app $ AppState { routes = []}
      output = foldl (flip ($)) initial_string (routes s) in
  output

main = do
  print $ "Starting demonstration of routes"
  let x1 = runMyApp (\x-> "default middlware called") myApp "middleware"
  print x1

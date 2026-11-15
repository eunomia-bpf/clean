import Longlocals_generated
open CC Longlocals

-- Check every identifier in order, plus every type: chunking must not drop,
-- duplicate, reorder or alter an entry while avoiding code-generator depth.
#eval do
  let names := "abcdefghijklmnop".toList.flatMap fun c =>
    (List.range 100).map fun n => identOfString
      (String.ofList ['x', c, Char.ofNat (48 + n / 10), Char.ofNat (48 + n % 10)])
  unless f_many_variables.fn_vars.map Prod.fst == names do
    throw (IO.userError "local variables changed order or contents")
  unless f_many_variables.fn_vars.all (fun v => tyBeq v.2 (tvolatile tuint)) do
    throw (IO.userError "local variable type changed")
  let temps := _x :: (List.range 2000).reverse.map (fun n => Positive.ofNat (128 + n))
  unless f_many_temporaries.fn_temps.map Prod.fst == temps do
    throw (IO.userError "temporaries changed order or contents")
  unless f_many_temporaries.fn_temps.all (fun v => tyBeq v.2 tuint) do
    throw (IO.userError "temporary type changed")
  IO.println "1600 local variables and 2001 temporaries retain order and types"

open Utils

let square = x => multiply(x, x)
let double = x => add(x, x)
let quadruple = x => U.quadruple(x)

module M = {
  let triple = x => multiply(x, 3)
  let divide = (x, y) => divide(x, y)
}

let is_whitelisted = module_name => {
  try {
    Belt.Array.some(whitelist, name => name == module_name)
  } catch {
  | _ => false
  }
}

external f0: {..} => int = "f0"

let v0 = f0({"a": U.triple(0)})

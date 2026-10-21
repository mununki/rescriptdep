let add = (a, b) => a + b
let multiply = (a, b) => a * b
let triple = x => multiply(x, 3)
let quadruple = x => multiply(x, 4)

module U = {
  let triple = x => multiply(x, 3)
  let quadruple = x => multiply(x, 4)
}

let divide = (a, b) => {
  let divider = (a, b) => a / b
  divider(a, b)
}

let whitelist = ["math"]

let is_whitelisted = module_name => {
  try {
    Belt.Array.some(whitelist, name => name == module_name)
  } catch {
  | _ => false
  }
}

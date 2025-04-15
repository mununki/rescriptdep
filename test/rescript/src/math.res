open Utils

let square = x => multiply(x, x)
let double = x => add(x, x)

module M = {
  let triple = x => multiply(x, 3)
}

let step = x => x + 1

let sumPositive = xs => {
  let total = ref(0)
  for x of xs {
    if x < 0 {
      continue
    }
    total := total.contents + step(x)
  }
  total.contents
}

let describe = (~offset=step(0), x) => `${Int.toString(step(x) + offset)}!`

let point = {"x": step(1)}
let readX = () => point["x"] + step(2)

module M = {
  @react.component
  let make = (~p as _) => <Comp0 />
}

let run = () => {
  let result = Math.square(5)
  Logger.log(`Result: ${result->Belt.Int.toString}`)

  <M p=10 />
}

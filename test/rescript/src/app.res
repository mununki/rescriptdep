module M = {
  @react.component
  let make = (~p as _) => <Comp0 />
}

let run = () => {
  let result = Math.square(5)
  Logger.log(`Result: ${result->Belt.Int.toString}`)
  Logger.log(`Users: ${Users.count()->Belt.Int.toString}`)

  <M p=10 />
}

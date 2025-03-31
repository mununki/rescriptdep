@react.component
let make = () => {
  let navigate = ReactRouter.Dom.useNavigate()

  <div>
    <h1> {"Seller"->React.string} </h1>
    <button onClick={_ => navigate("/about")}> {"About"->React.string} </button>
  </div>
}

// Generated by ReScript, PLEASE EDIT WITH CARE

import * as ReactRouterDom from "react-router-dom";
import * as JsxRuntime from "react/jsx-runtime";

function App(props) {
  let navigate = ReactRouterDom.useNavigate();
  return JsxRuntime.jsxs("div", {
    children: [
      JsxRuntime.jsx("h1", {
        children: "Seller"
      }),
      JsxRuntime.jsx("button", {
        children: "About",
        onClick: param => navigate("/about", undefined)
      })
    ]
  });
}

let make = App;

export {
  make,
}
/* react-router-dom Not a pure module */

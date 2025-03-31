module Dom = {
  type navigateOptions = {
    replace?: bool,
    preventScrollReset?: bool,
  }
  type navigate = (string, ~options: navigateOptions=?) => unit
  @module("react-router-dom")
  external useNavigate: unit => navigate = "useNavigate"

  module Link = {
    @module("react-router-dom") @react.component
    external make: (
      ~to: string,
      ~className: string=?,
      ~target: string=?,
      ~reloadDocument: bool=?,
      ~children: React.element,
    ) => React.element = "Link"
  }

  @module("react-router-dom")
  external useSearchParams: unit => (
    Webapi.Url.URLSearchParams.t,
    Webapi.Url.URLSearchParams.t => unit,
  ) = "useSearchParams"

  type location = {
    pathname: string,
    search: string,
    hash: string,
    state: unknown,
    key: string,
  }

  @module("react-router-dom")
  external useLocation: unit => location = "useLocation"

  type rec route = {
    index?: bool,
    path: string,
    element: React.element,
    children?: array<route>,
    errorElement?: React.element,
  }

  type router

  @module("react-router-dom")
  external createBrowserRouter: array<route> => router = "createBrowserRouter"

  module RouterProvider = {
    @module("react-router-dom") @react.component
    external make: (~router: router) => React.element = "RouterProvider"
  }

  module Outlet = {
    @module("react-router-dom") @react.component
    external make: unit => React.element = "Outlet"
  }

  type errorResponse = {
    status: int,
    statusText: string,
    data: unknown,
  }

  @module("react-router-dom")
  external useRouteError: unit => errorResponse = "useRouteError"

  @module("react-router-dom")
  external isRouteErrorResponse: errorResponse => bool = "isRouteErrorResponse"
}

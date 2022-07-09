@@jsxConfig({version: 3})

@react.component
let make = (~msg) => {
  <div> {msg->React.string} </div>
}

@@jsxConfig({version: 4, mode: "classic"})

@react.component
let make = (~msg) => {
  <div> {msg->React.string} </div>
}

@@jsxConfig({version: 4, mode: "automatic"})

@react.component
let make = (~msg) => {
  <div> {msg->React.string} </div>
}

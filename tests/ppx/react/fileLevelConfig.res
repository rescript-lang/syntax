@@jsxConfig({version: 4, mode: "automatic"})

@react.component
let make = (~msg) => {
  <div> {msg->React.string} </div>
}

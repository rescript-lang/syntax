// Component with type constraint
@react.component
let make = (~x: string, ~y: string) => React.string(x ++ y)

// Component with another name than "make"
@react.component
let withAnotherName = (~x) => React.string(x)

[@bs.module "react-dom/server"] [@bs.val] 
external renderToString : React.element => string =
  "renderToString";

[@bs.module "react-dom/server"] [@bs.val] 
external renderToStaticMarkup : React.element => string =
  "renderToStaticMarkup";

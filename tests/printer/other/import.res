// Import a single export from a module
import { dirname: string => string } from "path"

// old external
@module("path")
external dirname: string => string = "dirname"

// Import multiple exports from a module
import {
  delimiter: string,
  dirname: string => string,
  relative: (~from: string, ~\"to": string) => string
} from "path"

// old external
@module("path")
external delimiter: string = "delimiter"
@module("path")
external dirname: (string, string) => string = "dirname"
@module("path")
external relative: (~from: string, ~\"to": string) => string = "relative"

// Import an export with a more convenient alias
import {renameSync as rename: (string, string) => string} from "fs"

@module("fs")
external rename: (string, string) => string = "renameSync"

// import an export with an illegal ReScript identifier
import {"Fragment" as make: component<{"children": element}>} from "react"

// old external
@module("react")
external make: component<{"children": element}> = "Fragment"

// import an export with additional attributes
import {
  @splice
  join: array<string> => string
} from "path"

// old external
@module("path") @splice
external join: array<string> => string = "join"

// Rename multiple exports during import
import {
  renameSync as rename: (string, string) => string,
  unlinkSync as unlink: string => unit,
} from "fs" 

// old external
@module("path")
external rename: (string, string) => string = "renameSync"
@module("path")
external unlink: string => unit = "unlinkSync"

// import an entire module's contents (aka NameSpacedImport)
import * as leftPad: (string, int) => string from "leftPad"

// old external
@module
external leftPad: (string, int) => string = "leftPad"

// Importing default export
import schoolName: string from "/modules/school.js";
// syntactic sugar for
import {default as schoolName: string} from "/modules/school.js";

// old external
@module("/modules/school.js") external schoolName: string = "default"

// importing default export with named imports
import schoolName: string, {getGrade: student => int} from "/modules/school.js";

// old external
@module("/modules/school.js") external schoolName: string = "default"
@module("/modules/school.js") external getGrade: student => int = "getGrade"

// importing default export with multiple named imports
import
  schoolName: string,
  {
    getGrade: student => int,
    getTeachers: unit => array<teacher>
  }
from "/modules/school.js";

// old external
@module("/modules/school.js") external schoolName: string = "default"
@module("/modules/school.js") external getGrade: student => int = "getGrade"
@module("/modules/school.js") external getTeachers: unit => array<teacher> = "getTeachers"


// import values from JavaScript modules with @as and polyvariants
import {
  openSync: (
    path,
    @bs.string [
      | @bs.as("r") #Read
      | @bs.as("r+") #Read_write
      | @bs.as("rs+") #Read_write_sync
      | @bs.as("w") #Write
      | @bs.as("wx") #Write_fail_if_exists
      | @bs.as("w+") #Write_read
      | @bs.as("wx+") #Write_read_fail_if_exists
      | @bs.as("a") #Append
      | @bs.as("ax") #Append_fail_if_exists
      | @bs.as("a+") #Append_read
      | @bs.as("ax+") #Append_read_fail_if_exists
    ],
  ) => unit
} from "fs"

// old external
@module("fs")
external openSync: (
  path,
  @bs.string
  [
    | @bs.as("r") #Read
    | @bs.as("r+") #Read_write
    | @bs.as("rs+") #Read_write_sync
    | @bs.as("w") #Write
    | @bs.as("wx") #Write_fail_if_exists
    | @bs.as("w+") #Write_read
    | @bs.as("wx+") #Write_read_fail_if_exists
    | @bs.as("a") #Append
    | @bs.as("ax") #Append_fail_if_exists
    | @bs.as("a+") #Append_read
    | @bs.as("ax+") #Append_read_fail_if_exists
  ],
) => unit = "openSync"

// import a react component from a module
import {
  @react.component
  "Circle" as make: (
    ~center: Leaflet.point,
    ~radius: int,
    ~opacity: float,
    ~stroke: bool,
    ~color: string,
    ~children: React.element=?
  ) => React.element
} from "react-leaflet"

module JsComponent = {
  import
    @react.component
    make: (
      ~id: string=?,
      ~onChange: Js.Nullable.t<Js.Date.t> => unit,
      ~selected: Js.Date.t=?,
    ) => React.element from "./DatePicker"
}



import @attr * as leftPad: (string, int) => string from "leftPad"
import @attr * as leftPad: (string, int) => string from "leftPad"

import
  @react.component
  make: (
    ~id: string=?,
    ~onChange: Js.Nullable.t<Js.Date.t> => unit,
    ~selected: Js.Date.t=?,
  ) => React.element,
  @attr * as dateLib: 'a
from "./DatePicker"

import * as copyToClipboard: string => unit from "copy-to-clipboard"

// handle ""
@module("mat4") external create: unit => t = ""

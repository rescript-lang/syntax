type t = Dom.domTokenList

@get external length: t => int = ""

@bs.send.pipe(: t) @return(nullable) external item: int => option<string> = ""
@bs.send.pipe(: t) external add: string => unit = ""
@bs.send.pipe(: t) @variadic external addMany: array<string> => unit = "add"
@bs.send.pipe(: t) external contains: string => bool = "contains"
/* entries: iterator API, should have language support */
@bs.send.pipe(: t) external forEach: ((string, int) => unit) => unit = ""
/* keys: iterator API, should have language support */
@bs.send.pipe(: t) external remove: string => unit = ""
@bs.send.pipe(: t) @variadic external removeMany: array<string> => unit = "remove"
@bs.send.pipe(: t) external replace: (string, string) => unit = "" /* experimental */
@bs.send.pipe(: t)
external supports: string => bool = "" /* experimental, Content Management Level 1 */
@bs.send.pipe(: t) external toggle: string => bool = ""
@bs.send.pipe(: t) external toggleForced: (string, @as(json`true`) _) => bool = "toggle"
@bs.send.pipe(: t) external toString: string = ""
/* values: iterator API, should have language support */

@get
external value: t => string = "" /* experimental, from being merged with domSettableTokenList */
@set
external setValue: (t, string) => unit =
  "value" /* experimental, from being merged with domSettableTokenList */

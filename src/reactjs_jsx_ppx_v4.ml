(*
  This is the file that handles turning Reason JSX' agnostic function call into
  a ReasonReact-specific function call. Aka, this is a macro, using OCaml's ppx
  facilities; https://whitequark.org/blog/2014/04/16/a-guide-to-extension-
  points-in-ocaml/
  You wouldn't use this file directly; it's used by BuckleScript's
  bsconfig.json. Specifically, there's a field called `react-jsx` inside the
  field `reason`, which enables this ppx through some internal call in bsb
*)

open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

module PropsMap = Map.Make(String)

type domProp = {
  jsString : string option;
  typeString : Parsetree.core_type;
}

let nolabel = Nolabel
let labelled str = Labelled str
let optional str = Optional str


let mkPropConst ident = Typ.constr {loc = Location.none; txt = ident} []
let allowedProps = [
("children", {jsString = None; typeString = mkPropConst (Ldot (Lident "React", "element"))});
("key", {jsString = None; typeString = mkPropConst (Lident "string")});
("ref", {jsString = None; typeString = mkPropConst (Ldot (Lident "ReactDOMRe", "domRef"))});
("ariaCurrent", {jsString = Some("aria-current"); typeString = mkPropConst (Lident "page|step|location|date|time|true|false")});
("ariaDetails", {jsString = Some("aria-details"); typeString = mkPropConst (Lident "string")});
("ariaDisabled", {jsString = Some("aria-disabled"); typeString = mkPropConst (Lident "bool")});
("ariaHidden", {jsString = Some("aria-hidden"); typeString = mkPropConst (Lident "bool")});
("ariaInvalid", {jsString = Some("aria-invalid"); typeString = mkPropConst (Lident "grammar|false|spelling|true")});
("ariaKeyshortcuts", {jsString = Some("aria-keyshortcuts"); typeString = mkPropConst (Lident "string")});
("ariaLabel", {jsString = Some("aria-label"); typeString = mkPropConst (Lident "string")});
("ariaRoledescription", {jsString = Some("aria-roledescription"); typeString = mkPropConst (Lident "string")});
("ariaAutocomplete", {jsString = Some("aria-autocomplete"); typeString = mkPropConst (Lident "inline|list|both|none")});
("ariaChecked", {jsString = Some("aria-checked"); typeString = mkPropConst (Lident "true|false|mixed")});
("ariaExpanded", {jsString = Some("aria-expanded"); typeString = mkPropConst (Lident "bool")});
("ariaHaspopup", {jsString = Some("aria-haspopup"); typeString = mkPropConst (Lident "false|true|menu|listbox|tree|grid|dialog")});
("ariaLevel", {jsString = Some("aria-level"); typeString = mkPropConst (Lident "int")});
("ariaModal", {jsString = Some("aria-modal"); typeString = mkPropConst (Lident "bool")});
("ariaMultiline", {jsString = Some("aria-multiline"); typeString = mkPropConst (Lident "bool")});
("ariaMultiselectable", {jsString = Some("aria-multiselectable"); typeString = mkPropConst (Lident "bool")});
("ariaOrientation", {jsString = Some("aria-orientation"); typeString = mkPropConst (Lident "horizontal|vertical|undefined")});
("ariaPlaceholder", {jsString = Some("aria-placeholder"); typeString = mkPropConst (Lident "string")});
("ariaPressed", {jsString = Some("aria-pressed"); typeString = mkPropConst (Lident "true|false|mixed")});
("ariaReadonly", {jsString = Some("aria-readonly"); typeString = mkPropConst (Lident "bool")});
("ariaRequired", {jsString = Some("aria-required"); typeString = mkPropConst (Lident "bool")});
("ariaSelected", {jsString = Some("aria-selected"); typeString = mkPropConst (Lident "bool")});
("ariaSort", {jsString = Some("aria-sort"); typeString = mkPropConst (Lident "string")});
("ariaValuemax", {jsString = Some("aria-valuemax"); typeString = mkPropConst (Lident "float")});
("ariaValuemin", {jsString = Some("aria-valuemin"); typeString = mkPropConst (Lident "float")});
("ariaValuenow", {jsString = Some("aria-valuenow"); typeString = mkPropConst (Lident "float")});
("ariaValuetext", {jsString = Some("aria-valuetext"); typeString = mkPropConst (Lident "string")});
("ariaAtomic", {jsString = Some("aria-atomic"); typeString = mkPropConst (Lident "bool")});
("ariaBusy", {jsString = Some("aria-busy"); typeString = mkPropConst (Lident "bool")});
("ariaLive", {jsString = Some("aria-live"); typeString = mkPropConst (Lident "off|polite|assertive|rude")});
("ariaRelevant", {jsString = Some("aria-relevant"); typeString = mkPropConst (Lident "string")});
("ariaDropeffect", {jsString = Some("aria-dropeffect"); typeString = mkPropConst (Lident "copy|move|link|execute|popup|none")});
("ariaGrabbed", {jsString = Some("aria-grabbed"); typeString = mkPropConst (Lident "bool")});
("ariaActivedescendant", {jsString = Some("aria-activedescendant"); typeString = mkPropConst (Lident "string")});
("ariaColcount", {jsString = Some("aria-colcount"); typeString = mkPropConst (Lident "int")});
("ariaColindex", {jsString = Some("aria-colindex"); typeString = mkPropConst (Lident "int")});
("ariaColspan", {jsString = Some("aria-colspan"); typeString = mkPropConst (Lident "int")});
("ariaControls", {jsString = Some("aria-controls"); typeString = mkPropConst (Lident "string")});
("ariaDescribedby", {jsString = Some("aria-describedby"); typeString = mkPropConst (Lident "string")});
("ariaErrormessage", {jsString = Some("aria-errormessage"); typeString = mkPropConst (Lident "string")});
("ariaFlowto", {jsString = Some("aria-flowto"); typeString = mkPropConst (Lident "string")});
("ariaLabelledby", {jsString = Some("aria-labelledby"); typeString = mkPropConst (Lident "string")});
("ariaOwns", {jsString = Some("aria-owns"); typeString = mkPropConst (Lident "string")});
("ariaPosinset", {jsString = Some("aria-posinset"); typeString = mkPropConst (Lident "int")});
("ariaRowcount", {jsString = Some("aria-rowcount"); typeString = mkPropConst (Lident "int")});
("ariaRowindex", {jsString = Some("aria-rowindex"); typeString = mkPropConst (Lident "int")});
("ariaRowspan", {jsString = Some("aria-rowspan"); typeString = mkPropConst (Lident "int")});
("ariaSetsize", {jsString = Some("aria-setsize"); typeString = mkPropConst (Lident "int")});
("defaultChecked", {jsString = None; typeString = mkPropConst (Lident "bool")});
("defaultValue", {jsString = None; typeString = mkPropConst (Lident "string")});
("accessKey", {jsString = None; typeString = mkPropConst (Lident "string")});
("className", {jsString = None; typeString = mkPropConst (Lident "string")});
("contentEditable", {jsString = None; typeString = mkPropConst (Lident "bool")});
("contextMenu", {jsString = None; typeString = mkPropConst (Lident "string")});
("dir", {jsString = None; typeString = mkPropConst (Lident "string")});
("draggable", {jsString = None; typeString = mkPropConst (Lident "bool")});
("hidden", {jsString = None; typeString = mkPropConst (Lident "bool")});
("id", {jsString = None; typeString = mkPropConst (Lident "string")});
("lang", {jsString = None; typeString = mkPropConst (Lident "string")});
("role", {jsString = None; typeString = mkPropConst (Lident "string")});
("style", {jsString = None; typeString = mkPropConst (Ldot (Lident "ReactDOMRe", "style"))});
("spellCheck", {jsString = None; typeString = mkPropConst (Lident "bool")});
("tabIndex", {jsString = None; typeString = mkPropConst (Lident "int")});
("title", {jsString = None; typeString = mkPropConst (Lident "string")});
("itemID", {jsString = None; typeString = mkPropConst (Lident "string")});
("itemProp", {jsString = None; typeString = mkPropConst (Lident "string")});
("itemRef", {jsString = None; typeString = mkPropConst (Lident "string")});
("itemScope", {jsString = None; typeString = mkPropConst (Lident "bool")});
("itemType", {jsString = None; typeString = mkPropConst (Lident "string")});
("accept", {jsString = None; typeString = mkPropConst (Lident "string")});
("acceptCharset", {jsString = None; typeString = mkPropConst (Lident "string")});
("action", {jsString = None; typeString = mkPropConst (Lident "string")});
("allowFullScreen", {jsString = None; typeString = mkPropConst (Lident "bool")});
("alt", {jsString = None; typeString = mkPropConst (Lident "string")});
("async", {jsString = None; typeString = mkPropConst (Lident "bool")});
("autoComplete", {jsString = None; typeString = mkPropConst (Lident "string")});
("autoCapitalize", {jsString = None; typeString = mkPropConst (Lident "string")});
("autoFocus", {jsString = None; typeString = mkPropConst (Lident "bool")});
("autoPlay", {jsString = None; typeString = mkPropConst (Lident "bool")});
("challenge", {jsString = None; typeString = mkPropConst (Lident "string")});
("charSet", {jsString = None; typeString = mkPropConst (Lident "string")});
("checked", {jsString = None; typeString = mkPropConst (Lident "bool")});
("cite", {jsString = None; typeString = mkPropConst (Lident "string")});
("crossOrigin", {jsString = None; typeString = mkPropConst (Lident "string")});
("cols", {jsString = None; typeString = mkPropConst (Lident "int")});
("colSpan", {jsString = None; typeString = mkPropConst (Lident "int")});
("content", {jsString = None; typeString = mkPropConst (Lident "string")});
("controls", {jsString = None; typeString = mkPropConst (Lident "bool")});
("coords", {jsString = None; typeString = mkPropConst (Lident "string")});
("data", {jsString = None; typeString = mkPropConst (Lident "string")});
("dateTime", {jsString = None; typeString = mkPropConst (Lident "string")});
("default", {jsString = None; typeString = mkPropConst (Lident "bool")});
("defer", {jsString = None; typeString = mkPropConst (Lident "bool")});
("disabled", {jsString = None; typeString = mkPropConst (Lident "bool")});
("download", {jsString = None; typeString = mkPropConst (Lident "string")});
("encType", {jsString = None; typeString = mkPropConst (Lident "string")});
("form", {jsString = None; typeString = mkPropConst (Lident "string")});
("formAction", {jsString = None; typeString = mkPropConst (Lident "string")});
("formTarget", {jsString = None; typeString = mkPropConst (Lident "string")});
("formMethod", {jsString = None; typeString = mkPropConst (Lident "string")});
("headers", {jsString = None; typeString = mkPropConst (Lident "string")});
("height", {jsString = None; typeString = mkPropConst (Lident "string")});
("high", {jsString = None; typeString = mkPropConst (Lident "int")});
("href", {jsString = None; typeString = mkPropConst (Lident "string")});
("hrefLang", {jsString = None; typeString = mkPropConst (Lident "string")});
("htmlFor", {jsString = None; typeString = mkPropConst (Lident "string")});
("httpEquiv", {jsString = None; typeString = mkPropConst (Lident "string")});
("icon", {jsString = None; typeString = mkPropConst (Lident "string")});
("inputMode", {jsString = None; typeString = mkPropConst (Lident "string")});
("integrity", {jsString = None; typeString = mkPropConst (Lident "string")});
("keyType", {jsString = None; typeString = mkPropConst (Lident "string")});
("kind", {jsString = None; typeString = mkPropConst (Lident "string")});
("label", {jsString = None; typeString = mkPropConst (Lident "string")});
("list", {jsString = None; typeString = mkPropConst (Lident "string")});
("loop", {jsString = None; typeString = mkPropConst (Lident "bool")});
("low", {jsString = None; typeString = mkPropConst (Lident "int")});
("manifest", {jsString = None; typeString = mkPropConst (Lident "string")});
("max", {jsString = None; typeString = mkPropConst (Lident "string")});
("maxLength", {jsString = None; typeString = mkPropConst (Lident "int")});
("media", {jsString = None; typeString = mkPropConst (Lident "string")});
("mediaGroup", {jsString = None; typeString = mkPropConst (Lident "string")});
("method", {jsString = None; typeString = mkPropConst (Lident "string")});
("min", {jsString = None; typeString = mkPropConst (Lident "string")});
("minLength", {jsString = None; typeString = mkPropConst (Lident "int")});
("multiple", {jsString = None; typeString = mkPropConst (Lident "bool")});
("muted", {jsString = None; typeString = mkPropConst (Lident "bool")});
("name", {jsString = None; typeString = mkPropConst (Lident "string")});
("nonce", {jsString = None; typeString = mkPropConst (Lident "string")});
("noValidate", {jsString = None; typeString = mkPropConst (Lident "bool")});
("open_", {jsString = Some("open"); typeString = mkPropConst (Lident "bool")});
("optimum", {jsString = None; typeString = mkPropConst (Lident "int")});
("pattern", {jsString = None; typeString = mkPropConst (Lident "string")});
("placeholder", {jsString = None; typeString = mkPropConst (Lident "string")});
("poster", {jsString = None; typeString = mkPropConst (Lident "string")});
("preload", {jsString = None; typeString = mkPropConst (Lident "string")});
("radioGroup", {jsString = None; typeString = mkPropConst (Lident "string")});
("readOnly", {jsString = None; typeString = mkPropConst (Lident "bool")});
("rel", {jsString = None; typeString = mkPropConst (Lident "string")});
("required", {jsString = None; typeString = mkPropConst (Lident "bool")});
("reversed", {jsString = None; typeString = mkPropConst (Lident "bool")});
("rows", {jsString = None; typeString = mkPropConst (Lident "int")});
("rowSpan", {jsString = None; typeString = mkPropConst (Lident "int")});
("sandbox", {jsString = None; typeString = mkPropConst (Lident "string")});
("scope", {jsString = None; typeString = mkPropConst (Lident "string")});
("scoped", {jsString = None; typeString = mkPropConst (Lident "bool")});
("scrolling", {jsString = None; typeString = mkPropConst (Lident "string")});
("selected", {jsString = None; typeString = mkPropConst (Lident "bool")});
("shape", {jsString = None; typeString = mkPropConst (Lident "string")});
("size", {jsString = None; typeString = mkPropConst (Lident "int")});
("sizes", {jsString = None; typeString = mkPropConst (Lident "string")});
("span", {jsString = None; typeString = mkPropConst (Lident "int")});
("src", {jsString = None; typeString = mkPropConst (Lident "string")});
("srcDoc", {jsString = None; typeString = mkPropConst (Lident "string")});
("srcLang", {jsString = None; typeString = mkPropConst (Lident "string")});
("srcSet", {jsString = None; typeString = mkPropConst (Lident "string")});
("start", {jsString = None; typeString = mkPropConst (Lident "int")});
("step", {jsString = None; typeString = mkPropConst (Lident "float")});
("summary", {jsString = None; typeString = mkPropConst (Lident "string")});
("target", {jsString = None; typeString = mkPropConst (Lident "string")});
("type_", {jsString = Some("type"); typeString = mkPropConst (Lident "string")});
("useMap", {jsString = None; typeString = mkPropConst (Lident "string")});
("value", {jsString = None; typeString = mkPropConst (Lident "string")});
("width", {jsString = None; typeString = mkPropConst (Lident "string")});
("wrap", {jsString = None; typeString = mkPropConst (Lident "string")});
("onCopy", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Clipboard"), "t"))) (mkPropConst (Lident "unit"))});
("onCut", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Clipboard"), "t"))) (mkPropConst (Lident "unit"))});
("onPaste", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Clipboard"), "t"))) (mkPropConst (Lident "unit"))});
("onCompositionEnd", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Composition"), "t"))) (mkPropConst (Lident "unit"))});
("onCompositionStart", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Composition"), "t"))) (mkPropConst (Lident "unit"))});
("onCompositionUpdate", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Composition"), "t"))) (mkPropConst (Lident "unit"))});
("onKeyDown", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Keyboard"), "t"))) (mkPropConst (Lident "unit"))});
("onKeyPress", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Keyboard"), "t"))) (mkPropConst (Lident "unit"))});
("onKeyUp", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Keyboard"), "t"))) (mkPropConst (Lident "unit"))});
("onFocus", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Focus"), "t"))) (mkPropConst (Lident "unit"))});
("onBlur", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Focus"), "t"))) (mkPropConst (Lident "unit"))});
("onChange", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Form"), "t"))) (mkPropConst (Lident "unit"))});
("onInput", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Form"), "t"))) (mkPropConst (Lident "unit"))});
("onSubmit", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Form"), "t"))) (mkPropConst (Lident "unit"))});
("onInvalid", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Form"), "t"))) (mkPropConst (Lident "unit"))});
("onClick", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onContextMenu", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDoubleClick", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDrag", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDragEnd", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDragEnter", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDragExit", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDragLeave", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDragOver", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDragStart", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onDrop", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onMouseDown", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onMouseEnter", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onMouseLeave", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onMouseMove", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onMouseOut", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onMouseOver", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onMouseUp", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Mouse"), "t"))) (mkPropConst (Lident "unit"))});
("onSelect", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Selection"), "t"))) (mkPropConst (Lident "unit"))});
("onTouchCancel", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Touch"), "t"))) (mkPropConst (Lident "unit"))});
("onTouchEnd", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Touch"), "t"))) (mkPropConst (Lident "unit"))});
("onTouchMove", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Touch"), "t"))) (mkPropConst (Lident "unit"))});
("onTouchStart", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Touch"), "t"))) (mkPropConst (Lident "unit"))});
("onScroll", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "UI"), "t"))) (mkPropConst (Lident "unit"))});
("onWheel", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Wheel"), "t"))) (mkPropConst (Lident "unit"))});
("onAbort", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onCanPlay", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onCanPlayThrough", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onDurationChange", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onEmptied", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onEncrypetd", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onEnded", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onError", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onLoad", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Image"), "t"))) (mkPropConst (Lident "unit"))});
("onLoadedData", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onLoadedMetadata", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onLoadStart", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onPause", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onPlay", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onPlaying", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onProgress", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onRateChange", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onSeeked", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onSeeking", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onStalled", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onSuspend", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onTimeUpdate", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onVolumeChange", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onWaiting", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Media"), "t"))) (mkPropConst (Lident "unit"))});
("onAnimationStart", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Animation"), "t"))) (mkPropConst (Lident "unit"))});
("onAnimationEnd", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Animation"), "t"))) (mkPropConst (Lident "unit"))});
("onAnimationIteration", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Animation"), "t"))) (mkPropConst (Lident "unit"))});
("onTransitionEnd", {jsString = None; typeString = Typ.arrow nolabel (mkPropConst (Ldot (Ldot (Lident "ReactEvent", "Transition"), "t"))) (mkPropConst (Lident "unit"))});
("accentHeight", {jsString = None; typeString = mkPropConst (Lident "string")});
("accumulate", {jsString = None; typeString = mkPropConst (Lident "string")});
("additive", {jsString = None; typeString = mkPropConst (Lident "string")});
("alignmentBaseline", {jsString = None; typeString = mkPropConst (Lident "string")});
("allowReorder", {jsString = None; typeString = mkPropConst (Lident "string")});
("alphabetic", {jsString = None; typeString = mkPropConst (Lident "string")});
("amplitude", {jsString = None; typeString = mkPropConst (Lident "string")});
("arabicForm", {jsString = None; typeString = mkPropConst (Lident "string")});
("ascent", {jsString = None; typeString = mkPropConst (Lident "string")});
("attributeName", {jsString = None; typeString = mkPropConst (Lident "string")});
("attributeType", {jsString = None; typeString = mkPropConst (Lident "string")});
("autoReverse", {jsString = None; typeString = mkPropConst (Lident "string")});
("azimuth", {jsString = None; typeString = mkPropConst (Lident "string")});
("baseFrequency", {jsString = None; typeString = mkPropConst (Lident "string")});
("baseProfile", {jsString = None; typeString = mkPropConst (Lident "string")});
("baselineShift", {jsString = None; typeString = mkPropConst (Lident "string")});
("bbox", {jsString = None; typeString = mkPropConst (Lident "string")});
("begin_", {jsString = Some("begin"); typeString = mkPropConst (Lident "string")});
("bias", {jsString = None; typeString = mkPropConst (Lident "string")});
("by", {jsString = None; typeString = mkPropConst (Lident "string")});
("calcMode", {jsString = None; typeString = mkPropConst (Lident "string")});
("capHeight", {jsString = None; typeString = mkPropConst (Lident "string")});
("clip", {jsString = None; typeString = mkPropConst (Lident "string")});
("clipPath", {jsString = None; typeString = mkPropConst (Lident "string")});
("clipPathUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("clipRule", {jsString = None; typeString = mkPropConst (Lident "string")});
("colorInterpolation", {jsString = None; typeString = mkPropConst (Lident "string")});
("colorInterpolationFilters", {jsString = None; typeString = mkPropConst (Lident "string")});
("colorProfile", {jsString = None; typeString = mkPropConst (Lident "string")});
("colorRendering", {jsString = None; typeString = mkPropConst (Lident "string")});
("contentScriptType", {jsString = None; typeString = mkPropConst (Lident "string")});
("contentStyleType", {jsString = None; typeString = mkPropConst (Lident "string")});
("cursor", {jsString = None; typeString = mkPropConst (Lident "string")});
("cx", {jsString = None; typeString = mkPropConst (Lident "string")});
("cy", {jsString = None; typeString = mkPropConst (Lident "string")});
("d", {jsString = None; typeString = mkPropConst (Lident "string")});
("decelerate", {jsString = None; typeString = mkPropConst (Lident "string")});
("descent", {jsString = None; typeString = mkPropConst (Lident "string")});
("diffuseConstant", {jsString = None; typeString = mkPropConst (Lident "string")});
("direction", {jsString = None; typeString = mkPropConst (Lident "string")});
("display", {jsString = None; typeString = mkPropConst (Lident "string")});
("divisor", {jsString = None; typeString = mkPropConst (Lident "string")});
("dominantBaseline", {jsString = None; typeString = mkPropConst (Lident "string")});
("dur", {jsString = None; typeString = mkPropConst (Lident "string")});
("dx", {jsString = None; typeString = mkPropConst (Lident "string")});
("dy", {jsString = None; typeString = mkPropConst (Lident "string")});
("edgeMode", {jsString = None; typeString = mkPropConst (Lident "string")});
("elevation", {jsString = None; typeString = mkPropConst (Lident "string")});
("enableBackground", {jsString = None; typeString = mkPropConst (Lident "string")});
("end_", {jsString = Some("end"); typeString = mkPropConst (Lident "string")});
("exponent", {jsString = None; typeString = mkPropConst (Lident "string")});
("externalResourcesRequired", {jsString = None; typeString = mkPropConst (Lident "string")});
("fill", {jsString = None; typeString = mkPropConst (Lident "string")});
("fillOpacity", {jsString = None; typeString = mkPropConst (Lident "string")});
("fillRule", {jsString = None; typeString = mkPropConst (Lident "string")});
("filter", {jsString = None; typeString = mkPropConst (Lident "string")});
("filterRes", {jsString = None; typeString = mkPropConst (Lident "string")});
("filterUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("floodColor", {jsString = None; typeString = mkPropConst (Lident "string")});
("floodOpacity", {jsString = None; typeString = mkPropConst (Lident "string")});
("focusable", {jsString = None; typeString = mkPropConst (Lident "string")});
("fontFamily", {jsString = None; typeString = mkPropConst (Lident "string")});
("fontSize", {jsString = None; typeString = mkPropConst (Lident "string")});
("fontSizeAdjust", {jsString = None; typeString = mkPropConst (Lident "string")});
("fontStretch", {jsString = None; typeString = mkPropConst (Lident "string")});
("fontStyle", {jsString = None; typeString = mkPropConst (Lident "string")});
("fontVariant", {jsString = None; typeString = mkPropConst (Lident "string")});
("fontWeight", {jsString = None; typeString = mkPropConst (Lident "string")});
("fomat", {jsString = None; typeString = mkPropConst (Lident "string")});
("from", {jsString = None; typeString = mkPropConst (Lident "string")});
("fx", {jsString = None; typeString = mkPropConst (Lident "string")});
("fy", {jsString = None; typeString = mkPropConst (Lident "string")});
("g1", {jsString = None; typeString = mkPropConst (Lident "string")});
("g2", {jsString = None; typeString = mkPropConst (Lident "string")});
("glyphName", {jsString = None; typeString = mkPropConst (Lident "string")});
("glyphOrientationHorizontal", {jsString = None; typeString = mkPropConst (Lident "string")});
("glyphOrientationVertical", {jsString = None; typeString = mkPropConst (Lident "string")});
("glyphRef", {jsString = None; typeString = mkPropConst (Lident "string")});
("gradientTransform", {jsString = None; typeString = mkPropConst (Lident "string")});
("gradientUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("hanging", {jsString = None; typeString = mkPropConst (Lident "string")});
("horizAdvX", {jsString = None; typeString = mkPropConst (Lident "string")});
("horizOriginX", {jsString = None; typeString = mkPropConst (Lident "string")});
("ideographic", {jsString = None; typeString = mkPropConst (Lident "string")});
("imageRendering", {jsString = None; typeString = mkPropConst (Lident "string")});
("in_", {jsString = Some("in"); typeString = mkPropConst (Lident "string")});
("in2", {jsString = None; typeString = mkPropConst (Lident "string")});
("intercept", {jsString = None; typeString = mkPropConst (Lident "string")});
("k", {jsString = None; typeString = mkPropConst (Lident "string")});
("k1", {jsString = None; typeString = mkPropConst (Lident "string")});
("k2", {jsString = None; typeString = mkPropConst (Lident "string")});
("k3", {jsString = None; typeString = mkPropConst (Lident "string")});
("k4", {jsString = None; typeString = mkPropConst (Lident "string")});
("kernelMatrix", {jsString = None; typeString = mkPropConst (Lident "string")});
("kernelUnitLength", {jsString = None; typeString = mkPropConst (Lident "string")});
("kerning", {jsString = None; typeString = mkPropConst (Lident "string")});
("keyPoints", {jsString = None; typeString = mkPropConst (Lident "string")});
("keySplines", {jsString = None; typeString = mkPropConst (Lident "string")});
("keyTimes", {jsString = None; typeString = mkPropConst (Lident "string")});
("lengthAdjust", {jsString = None; typeString = mkPropConst (Lident "string")});
("letterSpacing", {jsString = None; typeString = mkPropConst (Lident "string")});
("lightingColor", {jsString = None; typeString = mkPropConst (Lident "string")});
("limitingConeAngle", {jsString = None; typeString = mkPropConst (Lident "string")});
("local", {jsString = None; typeString = mkPropConst (Lident "string")});
("markerEnd", {jsString = None; typeString = mkPropConst (Lident "string")});
("markerHeight", {jsString = None; typeString = mkPropConst (Lident "string")});
("markerMid", {jsString = None; typeString = mkPropConst (Lident "string")});
("markerStart", {jsString = None; typeString = mkPropConst (Lident "string")});
("markerUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("markerWidth", {jsString = None; typeString = mkPropConst (Lident "string")});
("mask", {jsString = None; typeString = mkPropConst (Lident "string")});
("maskContentUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("maskUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("mathematical", {jsString = None; typeString = mkPropConst (Lident "string")});
("mode", {jsString = None; typeString = mkPropConst (Lident "string")});
("numOctaves", {jsString = None; typeString = mkPropConst (Lident "string")});
("offset", {jsString = None; typeString = mkPropConst (Lident "string")});
("opacity", {jsString = None; typeString = mkPropConst (Lident "string")});
("operator", {jsString = None; typeString = mkPropConst (Lident "string")});
("order", {jsString = None; typeString = mkPropConst (Lident "string")});
("orient", {jsString = None; typeString = mkPropConst (Lident "string")});
("orientation", {jsString = None; typeString = mkPropConst (Lident "string")});
("origin", {jsString = None; typeString = mkPropConst (Lident "string")});
("overflow", {jsString = None; typeString = mkPropConst (Lident "string")});
("overflowX", {jsString = None; typeString = mkPropConst (Lident "string")});
("overflowY", {jsString = None; typeString = mkPropConst (Lident "string")});
("overlinePosition", {jsString = None; typeString = mkPropConst (Lident "string")});
("overlineThickness", {jsString = None; typeString = mkPropConst (Lident "string")});
("paintOrder", {jsString = None; typeString = mkPropConst (Lident "string")});
("panose1", {jsString = None; typeString = mkPropConst (Lident "string")});
("pathLength", {jsString = None; typeString = mkPropConst (Lident "string")});
("patternContentUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("patternTransform", {jsString = None; typeString = mkPropConst (Lident "string")});
("patternUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("pointerEvents", {jsString = None; typeString = mkPropConst (Lident "string")});
("points", {jsString = None; typeString = mkPropConst (Lident "string")});
("pointsAtX", {jsString = None; typeString = mkPropConst (Lident "string")});
("pointsAtY", {jsString = None; typeString = mkPropConst (Lident "string")});
("pointsAtZ", {jsString = None; typeString = mkPropConst (Lident "string")});
("preserveAlpha", {jsString = None; typeString = mkPropConst (Lident "string")});
("preserveAspectRatio", {jsString = None; typeString = mkPropConst (Lident "string")});
("primitiveUnits", {jsString = None; typeString = mkPropConst (Lident "string")});
("r", {jsString = None; typeString = mkPropConst (Lident "string")});
("radius", {jsString = None; typeString = mkPropConst (Lident "string")});
("refX", {jsString = None; typeString = mkPropConst (Lident "string")});
("refY", {jsString = None; typeString = mkPropConst (Lident "string")});
("renderingIntent", {jsString = None; typeString = mkPropConst (Lident "string")});
("repeatCount", {jsString = None; typeString = mkPropConst (Lident "string")});
("repeatDur", {jsString = None; typeString = mkPropConst (Lident "string")});
("requiredExtensions", {jsString = None; typeString = mkPropConst (Lident "string")});
("requiredFeatures", {jsString = None; typeString = mkPropConst (Lident "string")});
("restart", {jsString = None; typeString = mkPropConst (Lident "string")});
("result", {jsString = None; typeString = mkPropConst (Lident "string")});
("rotate", {jsString = None; typeString = mkPropConst (Lident "string")});
("rx", {jsString = None; typeString = mkPropConst (Lident "string")});
("ry", {jsString = None; typeString = mkPropConst (Lident "string")});
("scale", {jsString = None; typeString = mkPropConst (Lident "string")});
("seed", {jsString = None; typeString = mkPropConst (Lident "string")});
("shapeRendering", {jsString = None; typeString = mkPropConst (Lident "string")});
("slope", {jsString = None; typeString = mkPropConst (Lident "string")});
("spacing", {jsString = None; typeString = mkPropConst (Lident "string")});
("specularConstant", {jsString = None; typeString = mkPropConst (Lident "string")});
("specularExponent", {jsString = None; typeString = mkPropConst (Lident "string")});
("speed", {jsString = None; typeString = mkPropConst (Lident "string")});
("spreadMethod", {jsString = None; typeString = mkPropConst (Lident "string")});
("startOffset", {jsString = None; typeString = mkPropConst (Lident "string")});
("stdDeviation", {jsString = None; typeString = mkPropConst (Lident "string")});
("stemh", {jsString = None; typeString = mkPropConst (Lident "string")});
("stemv", {jsString = None; typeString = mkPropConst (Lident "string")});
("stitchTiles", {jsString = None; typeString = mkPropConst (Lident "string")});
("stopColor", {jsString = None; typeString = mkPropConst (Lident "string")});
("stopOpacity", {jsString = None; typeString = mkPropConst (Lident "string")});
("strikethroughPosition", {jsString = None; typeString = mkPropConst (Lident "string")});
("strikethroughThickness", {jsString = None; typeString = mkPropConst (Lident "string")});
("string", {jsString = None; typeString = mkPropConst (Lident "string")});
("stroke", {jsString = None; typeString = mkPropConst (Lident "string")});
("strokeDasharray", {jsString = None; typeString = mkPropConst (Lident "string")});
("strokeDashoffset", {jsString = None; typeString = mkPropConst (Lident "string")});
("strokeLinecap", {jsString = None; typeString = mkPropConst (Lident "string")});
("strokeLinejoin", {jsString = None; typeString = mkPropConst (Lident "string")});
("strokeMiterlimit", {jsString = None; typeString = mkPropConst (Lident "string")});
("strokeOpacity", {jsString = None; typeString = mkPropConst (Lident "string")});
("strokeWidth", {jsString = None; typeString = mkPropConst (Lident "string")});
("surfaceScale", {jsString = None; typeString = mkPropConst (Lident "string")});
("systemLanguage", {jsString = None; typeString = mkPropConst (Lident "string")});
("tableValues", {jsString = None; typeString = mkPropConst (Lident "string")});
("targetX", {jsString = None; typeString = mkPropConst (Lident "string")});
("targetY", {jsString = None; typeString = mkPropConst (Lident "string")});
("textAnchor", {jsString = None; typeString = mkPropConst (Lident "string")});
("textDecoration", {jsString = None; typeString = mkPropConst (Lident "string")});
("textLength", {jsString = None; typeString = mkPropConst (Lident "string")});
("textRendering", {jsString = None; typeString = mkPropConst (Lident "string")});
("to_", {jsString = Some("to"); typeString = mkPropConst (Lident "string")});
("transform", {jsString = None; typeString = mkPropConst (Lident "string")});
("u1", {jsString = None; typeString = mkPropConst (Lident "string")});
("u2", {jsString = None; typeString = mkPropConst (Lident "string")});
("underlinePosition", {jsString = None; typeString = mkPropConst (Lident "string")});
("underlineThickness", {jsString = None; typeString = mkPropConst (Lident "string")});
("unicode", {jsString = None; typeString = mkPropConst (Lident "string")});
("unicodeBidi", {jsString = None; typeString = mkPropConst (Lident "string")});
("unicodeRange", {jsString = None; typeString = mkPropConst (Lident "string")});
("unitsPerEm", {jsString = None; typeString = mkPropConst (Lident "string")});
("vAlphabetic", {jsString = None; typeString = mkPropConst (Lident "string")});
("vHanging", {jsString = None; typeString = mkPropConst (Lident "string")});
("vIdeographic", {jsString = None; typeString = mkPropConst (Lident "string")});
("vMathematical", {jsString = None; typeString = mkPropConst (Lident "string")});
("values", {jsString = None; typeString = mkPropConst (Lident "string")});
("vectorEffect", {jsString = None; typeString = mkPropConst (Lident "string")});
("version", {jsString = None; typeString = mkPropConst (Lident "string")});
("vertAdvX", {jsString = None; typeString = mkPropConst (Lident "string")});
("vertAdvY", {jsString = None; typeString = mkPropConst (Lident "string")});
("vertOriginX", {jsString = None; typeString = mkPropConst (Lident "string")});
("vertOriginY", {jsString = None; typeString = mkPropConst (Lident "string")});
("viewBox", {jsString = None; typeString = mkPropConst (Lident "string")});
("viewTarget", {jsString = None; typeString = mkPropConst (Lident "string")});
("visibility", {jsString = None; typeString = mkPropConst (Lident "string")});
("widths", {jsString = None; typeString = mkPropConst (Lident "string")});
("wordSpacing", {jsString = None; typeString = mkPropConst (Lident "string")});
("writingMode", {jsString = None; typeString = mkPropConst (Lident "string")});
("x", {jsString = None; typeString = mkPropConst (Lident "string")});
("x1", {jsString = None; typeString = mkPropConst (Lident "string")});
("x2", {jsString = None; typeString = mkPropConst (Lident "string")});
("xChannelSelector", {jsString = None; typeString = mkPropConst (Lident "string")});
("xHeight", {jsString = None; typeString = mkPropConst (Lident "string")});
("xlinkActuate", {jsString = None; typeString = mkPropConst (Lident "string")});
("xlinkArcrole", {jsString = None; typeString = mkPropConst (Lident "string")});
("xlinkHref", {jsString = None; typeString = mkPropConst (Lident "string")});
("xlinkRole", {jsString = None; typeString = mkPropConst (Lident "string")});
("xlinkShow", {jsString = None; typeString = mkPropConst (Lident "string")});
("xlinkTitle", {jsString = None; typeString = mkPropConst (Lident "string")});
("xlinkType", {jsString = None; typeString = mkPropConst (Lident "string")});
("xmlns", {jsString = None; typeString = mkPropConst (Lident "string")});
("xmlnsXlink", {jsString = None; typeString = mkPropConst (Lident "string")});
("xmlBase", {jsString = None; typeString = mkPropConst (Lident "string")});
("xmlLang", {jsString = None; typeString = mkPropConst (Lident "string")});
("xmlSpace", {jsString = None; typeString = mkPropConst (Lident "string")});
("y", {jsString = None; typeString = mkPropConst (Lident "string")});
("y1", {jsString = None; typeString = mkPropConst (Lident "string")});
("y2", {jsString = None; typeString = mkPropConst (Lident "string")});
("yChannelSelector", {jsString = None; typeString = mkPropConst (Lident "string")});
("z", {jsString = None; typeString = mkPropConst (Lident "string")});
("zoomAndPan", {jsString = None; typeString = mkPropConst (Lident "string")});
("about", {jsString = None; typeString = mkPropConst (Lident "string")});
("datatype", {jsString = None; typeString = mkPropConst (Lident "string")});
("inlist", {jsString = None; typeString = mkPropConst (Lident "string")});
("prefix", {jsString = None; typeString = mkPropConst (Lident "string")});
("property", {jsString = None; typeString = mkPropConst (Lident "string")});
("resource", {jsString = None; typeString = mkPropConst (Lident "string")});
("typeof", {jsString = None; typeString = mkPropConst (Lident "string")});
("vocab", {jsString = None; typeString = mkPropConst (Lident "string")});
("dangerouslySetInnerHTML", {jsString = None; typeString = mkPropConst (Lident "{. \"__html\": string}")});
("suppressContentEditableWarning", {jsString = None; typeString = mkPropConst (Lident "bool")});
]

let propsMap = PropsMap.empty

let addProp (key, value) map = PropsMap.add key value map

let propsMap =
    List.fold_right addProp allowedProps propsMap

let rec find_opt p = function
  | [] -> None
  | x :: l -> if p x then Some x else find_opt p l
let isOptional str = match str with
| Optional _ -> true
| _ -> false
let isLabelled str = match str with
| Labelled _ -> true
| _ -> false
let getLabel str = match str with
| Optional str | Labelled str -> str
| Nolabel -> ""
let optionIdent = Lident "option"

let constantString ~loc str = Ast_helper.Exp.constant ~loc (Pconst_string (str, None))
let safeTypeFromValue valueStr =
let valueStr = getLabel valueStr in
match String.sub valueStr 0 1 with
| "_" -> "T" ^ valueStr
| _ -> valueStr

type 'a children = | ListLiteral of 'a | Exact of 'a
type componentConfig = {
  propsName: string;
  propsType: string option;
}


(* if children is a list, convert it to an array while mapping each element. If not, just map over it, as usual *)
let transformChildrenIfListUpper ~loc ~mapper theList =
  let rec transformChildren_ theList accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match theList with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> begin
      match accum with
      | [singleElement] -> Exact singleElement
      | accum -> ListLiteral (List.rev accum |> Exp.array ~loc)
      end
    | {pexp_desc = Pexp_construct (
        {txt = Lident "::"},
        Some {pexp_desc = Pexp_tuple (v::acc::[])}
      )} ->
      transformChildren_ acc ((mapper.expr mapper v)::accum)
    | notAList -> Exact (mapper.expr mapper notAList)
  in
  transformChildren_ theList []

let extractChildren ?(removeLastPositionUnit=false) ~loc propsAndChildren =
  let rec allButLast_ lst acc = match lst with
    | [] -> []
    | (Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})::[] -> acc
    | (Nolabel, _)::_rest -> raise (Invalid_argument "JSX: found non-labelled argument before the last position")
    | arg::rest -> allButLast_ rest (arg::acc)
  in
  let allButLast lst = allButLast_ lst [] |> List.rev in
  match (List.partition (fun (label, _) -> label = labelled "children") propsAndChildren) with
  | ([], props) ->
    (* no children provided? Place a placeholder list *)
    (Exp.construct ~loc {loc; txt = Lident "[]"} None, if removeLastPositionUnit then allButLast props else props)
  | ([(_, childrenExpr)], props) ->
    (childrenExpr, if removeLastPositionUnit then allButLast props else props)
  | _ -> raise (Invalid_argument "JSX: somehow there's more than one `children` label")

let extractKey ~loc:_loc propsAndChildren =
  match (List.partition (fun (label, _) -> label = labelled "key") propsAndChildren) with
  | ([], props) ->
    (* no key provided? Place a placeholder list *)
    (None, props)
  | ([(_, keyExpr)], props) ->
    (Some keyExpr, props)
  | _ -> raise (Invalid_argument "JSX: somehow there's more than one `key` label")

(* Helper method to look up the [@react.component] attribute *)
let hasAttr (loc, _) =
  loc.txt = "react.component"

(* Helper method to filter out any attribute that isn't [@react.component] *)
let otherAttrsPure (loc, _) =
  loc.txt <> "react.component"

(* Iterate over the attributes and try to find the [@react.component] attribute *)
let hasAttrOnBinding {pvb_attributes} = find_opt hasAttr pvb_attributes <> None

(* Finds the name of the variable the binding is assigned to, otherwise raises Invalid_argument *)
let getFnName binding =
  match binding with
  | {pvb_pat = {
      ppat_desc = Ppat_var {txt}
    }} -> txt
  | _ -> raise (Invalid_argument "react.component calls cannot be destructured.")

(* Lookup the value of `props` otherwise raise Invalid_argument error *)
let getPropsNameValue acc (loc, exp) =
    match (loc, exp) with
    | ({ txt = Lident "props" }, { pexp_desc = Pexp_ident {txt = Lident str} }) -> { acc with propsName = str }
    | ({ txt = Lident "propsType" }, { pexp_desc = Pexp_ident {txt = Lident str} }) -> { acc with propsType = Some(str) }
    | ({ txt }, _) -> raise (Invalid_argument ("react.component only accepts props and propsType as an options, given: " ^ Longident.last txt))

(* Lookup the `props` record or string as part of [@react.component] and store the name for use when rewriting *)
let getPropsAttr payload =
  let defaultProps = {propsName = "Props"; propsType = None} in
  match payload with
  | Some(PStr(
    {pstr_desc = Pstr_eval ({
      pexp_desc = Pexp_record (recordFields, None)
      }, _)}::_rest
      )) ->
      List.fold_left getPropsNameValue defaultProps recordFields
  | Some(PStr({pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident {txt = Lident "props"}}, _)}::_rest)) -> {defaultProps with propsName = "props"}
  | Some(PStr({pstr_desc = Pstr_eval (_, _)}::_rest)) -> raise (Invalid_argument ("react.component accepts a record config with props as an options."))
  | _ -> defaultProps

(* Plucks the label, loc, and type_ from an AST node *)
let pluckLabelDefaultLocType (label, default, _, _, loc, type_) = (label, default, loc, type_)

(* Lookup the filename from the location information on the AST node and turn it into a valid module identifier *)
let filenameFromLoc (pstr_loc: Location.t) =
  let fileName = match pstr_loc.loc_start.pos_fname with
  | "" -> !Location.input_name
  | fileName -> fileName
  in
  let fileName = try
      Filename.chop_extension (Filename.basename fileName)
    with | Invalid_argument _-> fileName in
  let fileName = String.capitalize_ascii fileName in
  fileName

(* Build a string representation of a module name with segments separated by $ *)
let makeModuleName fileName nestedModules fnName =
  let fullModuleName = match (fileName, nestedModules, fnName) with
  (* TODO: is this even reachable? It seems like the fileName always exists *)
  | ("", nestedModules, "make") -> nestedModules
  | ("", nestedModules, fnName) -> List.rev (fnName :: nestedModules)
  | (fileName, nestedModules, "make") -> fileName :: (List.rev nestedModules)
  | (fileName, nestedModules, fnName) -> fileName :: (List.rev (fnName :: nestedModules))
  in
  let fullModuleName = String.concat "$" fullModuleName in
  fullModuleName

(*
  AST node builders
  These functions help us build AST nodes that are needed when transforming a [@react.component] into a
  constructor and a props external
*)

(* Build an AST node representing all named args for the `external` definition for a component's props *)
let rec recursivelyMakeNamedArgsForExternal list args =
  match list with
  | (label, default, loc, interiorType)::tl ->
    recursivelyMakeNamedArgsForExternal tl (Typ.arrow
    ~loc
    label
    (match (label, interiorType, default) with
    (* ~foo=1 *)
    | (label, None, Some _) ->
    {
      ptyp_desc = Ptyp_var (safeTypeFromValue label);
      ptyp_loc = loc;
      ptyp_attributes = [];
    }
    (* ~foo: int=1 *)
    | (_label, Some type_, Some _) ->
    type_
    (* ~foo: option(int)=? *)
    | (label, Some ({ptyp_desc = Ptyp_constr ({txt=(Lident "option")}, [type_])}), _)
    | (label, Some ({ptyp_desc = Ptyp_constr ({txt=(Ldot (Lident "*predef*", "option"))}, [type_])}), _)
    (* ~foo: int=? - note this isnt valid. but we want to get a type error *)
    | (label, Some type_, _) when isOptional label ->
    type_
    (* ~foo=? *)
    | (label, None, _) when isOptional label ->
    {
      ptyp_desc = Ptyp_var (safeTypeFromValue label);
      ptyp_loc = loc;
      ptyp_attributes = [];
    }
    (* ~foo *)
    | (label, None, _) ->
    {
      ptyp_desc = Ptyp_var (safeTypeFromValue label);
      ptyp_loc = loc;
      ptyp_attributes = [];
    }
    | (_label, Some type_, _) ->
    type_
    )
    args)
  | [] -> args

(* Build an AST node for the [@bs.obj] representing props for a component *)
let makePropsValue fnName loc namedArgListWithKeyAndRef propsType =
  let propsName = fnName ^ "Props" in {
  pval_name = {txt = propsName; loc};
  pval_type =
      recursivelyMakeNamedArgsForExternal
        namedArgListWithKeyAndRef
        (Typ.arrow
          nolabel
          {
            ptyp_desc = Ptyp_constr ({txt= Lident("unit"); loc}, []);
            ptyp_loc = loc;
            ptyp_attributes = [];
          }
          propsType
        );
  pval_prim = [""];
  pval_attributes = [({txt = "bs.obj"; loc = loc}, PStr [])];
  pval_loc = loc;
}

(* Build an AST node representing an `external` with the definition of the [@bs.obj] *)
let makePropsExternal fnName loc namedArgListWithKeyAndRef propsType =
  {
    pstr_loc = loc;
    pstr_desc = Pstr_primitive (makePropsValue fnName loc namedArgListWithKeyAndRef propsType)
  }

(* Build an AST node for the signature of the `external` definition *)
let makePropsExternalSig fnName loc namedArgListWithKeyAndRef propsType =
  {
    psig_loc = loc;
    psig_desc = Psig_value (makePropsValue fnName loc namedArgListWithKeyAndRef propsType)
  }

(* Build an AST node for the props name when converted to a Js.t inside the function signature  *)
let makePropsName ~loc name =
  {
    ppat_desc = Ppat_var {txt = name; loc};
    ppat_loc = loc;
    ppat_attributes = [];
  }

let makeObjectField loc (str, attrs, type_) =
  (* intentionally not using attrs - they probably don't work on object fields. use on *Props instead *)
  Otag ({ loc; txt = str }, attrs, type_)

(* Build an AST node representing a "closed" Js.t object representing a component's props *)
let makePropsType ~loc namedTypeList =
  Typ.mk ~loc (
    Ptyp_constr({txt= Ldot (Lident("Js"), "t"); loc}, [{
        ptyp_desc = Ptyp_object(
          List.map (makeObjectField loc) namedTypeList,
          Closed
        );
        ptyp_loc = loc;
        ptyp_attributes = [];
      }])
    )

let makeTypeExp ~fnName ~loc manifest namedTypeList =
  let params = ref [] in
  let grabType (_, _, core_type) = core_type in
  let typ mapper core_type = match core_type with
  | {ptyp_desc = Ptyp_var str} when String.sub str 0 1 <> "_" -> let () = params := ((core_type, Invariant) :: !params) in core_type
  | _ -> default_mapper.typ mapper core_type
  in
  let mapper = typ { default_mapper with typ } in
  let _ = List.map mapper (List.map grabType namedTypeList) in
  let params = !params in
    Str.mk @@ Pstr_type (Nonrecursive, [Type.mk ~params ~manifest { loc; txt = fnName}])

(* Builds an AST node for the entire `external` definition of props *)
let makeExternalDecl fnName loc namedArgListWithKeyAndRef namedTypeList =
  makePropsExternal
    fnName
    loc
    (List.map pluckLabelDefaultLocType namedArgListWithKeyAndRef)
    (makePropsType ~loc namedTypeList)

let depIgnore loc = [({loc; txt = "warning"}, (PStr [Str.eval (Exp.constant (Pconst_string ("-3", None)))]))]
let unerasableIgnore loc = ({loc; txt = "warning"}, (PStr [Str.eval (Exp.constant (Pconst_string ("-16", None)))]))

(* TODO: some line number might still be wrong *)
let jsxMapper () =

  let jsxVersion = ref None in

  let transformUppercaseCall3 modulePath mapper loc attrs _ callArguments =
    let (children, argsWithLabels) = extractChildren ~loc ~removeLastPositionUnit:true callArguments in
    let (key, argsWithLabels) = extractKey ~loc argsWithLabels in
    let childrenExpr = transformChildrenIfListUpper ~loc ~mapper children in
    let recursivelyTransformedArgsForMake = argsWithLabels |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)) in
    let (jsxCall, args) = (match childrenExpr with
        | Exact children -> ("jsx", [(labelled "children", children)])
        | ListLiteral ({ pexp_desc = Pexp_array list }) when list = [] -> ("jsx", [])
        | ListLiteral expression ->
        ("jsxs", [(labelled "children", (Exp.apply
          ~loc
          ~attrs
          (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "array")})
          ([
            (nolabel, expression)
          ])))])) in
    let args = recursivelyTransformedArgsForMake
      @ args @ [(nolabel, Exp.construct ~loc {loc; txt = Lident "()"} None)] in
    let isCap str = let first = String.sub str 0 1 in
    let capped = String.uppercase_ascii first in first = capped in
    let ident = match modulePath with
    | Lident _ -> Ldot (modulePath, "make")
    | (Ldot (_modulePath, value) as fullPath) when isCap value -> Ldot (fullPath, "make")
    | modulePath -> modulePath in
    let propsIdent = match ident with
    | Lident path -> Lident (path ^ "Props")
    | Ldot(ident, path) -> Ldot (ident, path ^ "Props")
    | _ -> raise (Invalid_argument "JSX name can't be the result of function applications") in
    let props =
      Exp.apply ~attrs ~loc (Exp.ident ~loc {loc; txt = propsIdent}) args in
    match key with
    | Some key ->
    (Exp.apply
      ~loc
      ~attrs
      (Exp.ident ~loc ~attrs:(depIgnore loc) {loc; txt = Ldot (Lident "React", jsxCall ^ "Keyed")})
      ([
        (nolabel, Exp.ident ~loc {txt = ident; loc});
        (nolabel, props);
        (nolabel, key)
      ]))
     | None ->
     (Exp.apply
       ~loc
       ~attrs
       (Exp.ident ~loc ~attrs:(depIgnore loc) {loc; txt = Ldot (Lident "React", jsxCall)})
       ([
         (nolabel, Exp.ident ~loc {txt = ident; loc});
         (nolabel, props)
       ]))
     in

    let getDataName maybeDataString =
      let length = String.length maybeDataString in
      if length > 4 then
        match String.sub maybeDataString 0 4 with
        | "data" -> Some {jsString = Some("data-" ^ String.sub maybeDataString 4 (length - 4)); typeString = mkPropConst (Lident "string")}
        | _ -> None
      else None
    in

    let transformLowercaseCall3 mapper loc attrs callArguments id =
      let argsToBlankRecordArgs (name, optional, _argExp) =
        let name = Longident.last name.txt in
        let propType = match PropsMap.find_opt name propsMap with
        | None -> getDataName name
        | propType -> propType
        in
        match propType with
        | None -> Location.raise_errorf ~loc:loc "ReasonReact: %s not recognized as a prop" name
        | Some propType ->
        let jsName = match propType.jsString with
        | Some name -> name
        | None -> name
        in
        let typeConstraint = (propType).typeString in
        let typeConstraint = if optional then Typ.constr {loc = Location.none; txt = Lident "option"} [typeConstraint] else typeConstraint in
        ({loc; txt = Lident jsName}, Exp.constraint_ (Exp.ident {loc; txt = Lident name}) typeConstraint)
      in

      let argsToExp exp (name, _optional, argExp) =
        Exp.let_ Nonrecursive [Vb.mk
                (Pat.var {loc = name.loc; txt = Longident.last name.txt})
                argExp
              ] exp
      in
      let (children, argsWithLabels) = extractChildren ~loc ~removeLastPositionUnit:true callArguments in
      let (key, argsWithLabels) = extractKey ~loc argsWithLabels in
      let componentNameExpr = constantString ~loc id in
      let componentExpr = Exp.apply ~attrs ~loc
        (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOM", "stringToComponent")})
        [(nolabel, componentNameExpr)] in
      let childrenExpr = transformChildrenIfListUpper ~loc ~mapper children in
      let recursivelyTransformedArgsForMake = argsWithLabels |> List.map (fun (label, expression) -> ({loc; txt = Lident (getLabel label)}, isOptional label, mapper.expr mapper expression)) in
      let (jsxCall, args) = (match childrenExpr with
          | Exact children -> ("jsx", [({loc; txt = Lident "children"}, false, children)])
          | ListLiteral ({ pexp_desc = Pexp_array list }) when list = [] -> ("jsx", [])
          | ListLiteral expression ->
          ("jsxs", [({loc; txt = Lident "children"}, false, (Exp.apply
            ~loc
            ~attrs
            (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "array")})
            ([
              (nolabel, expression)
            ])))])) in
      let args = recursivelyTransformedArgsForMake
        @ args in
        (* Pexp_extension ({txt = "bs.obj"}, PStr [{
           pstr_desc = Pstr_eval ({
                pexp_desc = Pexp_record (fields, (None as _noExt));
               pexp_loc = obj_loc;
               pexp_attributes = []
              },
              ([] as _empty_attrs)
           );
           pstr_loc
          }]   *)
      let blankRecordArgs = List.map argsToBlankRecordArgs args in
      let props = match blankRecordArgs with
      | [] -> Exp.construct {loc; txt = Lident "()"} None
      | blankRecordArgs ->
      List.fold_left argsToExp (
        Exp.extension ({loc; txt = "bs.obj"}, PStr [Str.eval (Exp.record ~attrs ~loc blankRecordArgs None)])
      ) args in
        (* Exp.apply ~attrs ~loc (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "domProps")}) args in *)
      match key with
      | Some key ->
      (Exp.apply
        ~loc
        ~attrs
        (Exp.ident ~loc ~attrs:(depIgnore loc) {loc; txt = Ldot (Lident "React", jsxCall ^ "Keyed")})
        ([
          (nolabel, componentExpr);
          (nolabel, props);
          (nolabel, key)
        ]))
      | None ->
      (Exp.apply
        ~loc
        ~attrs
        (Exp.ident ~loc ~attrs:(depIgnore loc) {loc; txt = Ldot (Lident "React", jsxCall)})
        ([
          (nolabel, componentExpr);
          (nolabel, props)
        ]))
    in

  let rec recursivelyTransformNamedArgsForMake mapper expr list =
    let expr = mapper.expr mapper expr in
    match expr.pexp_desc with
    (* TODO: make this show up with a loc. *)
    | Pexp_fun (Labelled "key", _, _, _)
    | Pexp_fun (Optional "key", _, _, _) -> raise (Invalid_argument "Key cannot be accessed inside of a component. Don't worry - you can always key a component from its parent!")
    | Pexp_fun (Labelled "ref", _, _, _)
    | Pexp_fun (Optional "ref", _, _, _) -> raise (Invalid_argument "Ref cannot be passed as a normal prop. Please use `forwardRef` API instead.")
    | Pexp_fun (arg, default, pattern, expression) when isOptional arg || isLabelled arg ->
      let () =
      (match (isOptional arg, pattern, default) with
      | (true, { ppat_desc = Ppat_constraint (_, { ptyp_desc })}, None) ->
        (match ptyp_desc with
         | Ptyp_constr({txt=(Lident "option")}, [_]) -> ()
         | _ ->
             let currentType = (match ptyp_desc with
             | Ptyp_constr({txt}, []) -> String.concat "." (Longident.flatten txt)
             | Ptyp_constr({txt}, _innerTypeArgs) -> String.concat "." (Longident.flatten txt) ^ "(...)"
             | _ -> "...")
             in
             Location.raise_errorf ~loc:pattern.ppat_loc "ReasonReact: optional argument annotations must have explicit `option`. Did you mean `option(%s)=?`?" currentType)
      | _ -> ()) in
      let alias = (match pattern with
      | {ppat_desc = Ppat_alias (_, {txt}) | Ppat_var {txt}} -> txt
      | {ppat_desc = Ppat_any} -> "_"
      | _ -> getLabel arg) in
      let type_ = (match pattern with
      | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
      | _ -> None) in

      recursivelyTransformNamedArgsForMake mapper expression ((arg, default, pattern, alias, pattern.ppat_loc, type_) :: list)
    | Pexp_fun (Nolabel, _, { ppat_desc = (Ppat_construct ({txt = Lident "()"}, _) | Ppat_any)}, _expression) ->
        (list, None)
    | Pexp_fun (Nolabel, _, { ppat_desc = Ppat_var ({txt})}, _expression) ->
        (list, Some txt)

    | _ -> (list, None)
  in


  let argToType types (name, default, _noLabelName, _alias, loc, type_) = match (type_, name, default) with
    | (Some ({ptyp_desc = Ptyp_constr ({txt=(Lident "option")}, [type_])}), name, _) when isOptional name ->
      (getLabel name, [], {
        type_ with
        ptyp_desc = Ptyp_constr ({loc=type_.ptyp_loc; txt=optionIdent}, [type_]);
      }) :: types
    | (Some type_, name, Some _default) ->
      (getLabel name, [], {
      ptyp_desc = Ptyp_constr ({loc; txt=optionIdent}, [type_]);
      ptyp_loc = loc;
      ptyp_attributes = [];
      }) :: types
    | (Some type_, name, _) ->
      (getLabel name, [], type_) :: types
    | (None, name, _) when isOptional name ->
      (getLabel name, [], {
        ptyp_desc = Ptyp_constr ({loc; txt=optionIdent}, [{
          ptyp_desc = Ptyp_var (safeTypeFromValue name);
          ptyp_loc = loc;
          ptyp_attributes = [];
        }]);
        ptyp_loc = loc;
        ptyp_attributes = [];
        }) :: types
    | (None, name, _) when isLabelled name ->
      (getLabel name, [], {
        ptyp_desc = Ptyp_var (safeTypeFromValue name);
        ptyp_loc = loc;
        ptyp_attributes = [];
        }) :: types
    | _ -> types
  in

  let argToConcreteType types (name, loc, type_) = match name with
    | name when isLabelled name ->
    (getLabel name, [], type_) :: types
    | name when isOptional name ->
  (getLabel name, [], Typ.constr ~loc {loc; txt=optionIdent} [type_]) :: types
    | _ -> types
  in

  let nestedModules = ref([]) in
  let transformComponentDefinition mapper structure returnStructures = match structure with
  (* external *)
  | ({
      pstr_loc;
      pstr_desc = Pstr_primitive ({
        pval_name = { txt = fnName };
        pval_attributes;
        pval_type;
      } as value_description)
    } as pstr) ->
    (match List.filter hasAttr pval_attributes with
    | [] -> structure :: returnStructures
    | [_] ->
    let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
      (match ptyp_desc with
      | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest)) when isLabelled name || isOptional name ->
        getPropTypes ((name, ptyp_loc, type_)::types) rest
      | Ptyp_arrow (Nolabel, _type, rest) ->
        getPropTypes types rest
      | Ptyp_arrow (name, type_, returnValue) when isLabelled name || isOptional name ->
        (returnValue, (name, returnValue.ptyp_loc, type_)::types)
      | _ -> (fullType, types))
    in
    let (innerType, propTypes) = getPropTypes [] pval_type in
    let namedTypeList = List.fold_left argToConcreteType [] propTypes in
    let pluckLabelAndLoc (label, loc, type_) = (label, None (* default *), loc, Some type_) in
    let retPropsType = makePropsType ~loc:pstr_loc namedTypeList in
    let externalPropsDecl = makePropsExternal fnName pstr_loc (List.map pluckLabelAndLoc propTypes) retPropsType in
    (* can't be an arrow because it will defensively uncurry *)
    let newExternalType = Ptyp_constr (
      {loc = pstr_loc; txt = Ldot ((Lident "React"), "componentLike")},
      [retPropsType; innerType]
    ) in
    let newStructure = {
      pstr with pstr_desc = Pstr_primitive {
        value_description with pval_type = {
          pval_type with ptyp_desc = newExternalType;
        };
        pval_attributes = List.filter otherAttrsPure pval_attributes;
      }
    } in
    externalPropsDecl :: newStructure :: returnStructures
    | _ -> raise (Invalid_argument "Only one react.component call can exist on a component at one time"))
  (* let component = ... *)
  | {
      pstr_loc;
      pstr_desc = Pstr_value (
        recFlag,
        valueBindings
      )
    } ->
      let fileName = filenameFromLoc pstr_loc in
      let emptyLoc = Location.in_file fileName in
      let mapBinding binding = if (hasAttrOnBinding binding) then
        let bindingLoc = binding.pvb_loc in
        let bindingPatLoc = binding.pvb_pat.ppat_loc in
        let binding = { binding with pvb_pat = { binding.pvb_pat with ppat_loc = emptyLoc}; pvb_loc = emptyLoc} in
        let fnName = getFnName binding in
        let fullModuleName = makeModuleName fileName !nestedModules fnName in
        let modifiedBindingOld binding =
          let expression = binding.pvb_expr in
          (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
          let rec spelunkForFunExpression expression = (match expression with
          (* let make = (~prop) => ... *)
          | {
            pexp_desc = Pexp_fun _
          } -> expression
          (* let make = {let foo = bar in (~prop) => ...} *)
          | {
              pexp_desc = Pexp_let (_recursive, _vbs, returnExpression)
            } ->
            (* here's where we spelunk! *)
            spelunkForFunExpression returnExpression
          (* let make = React.forwardRef((~prop) => ...) *)
          | { pexp_desc = Pexp_apply (_wrapperExpression, [(Nolabel, innerFunctionExpression)]) } ->
              spelunkForFunExpression innerFunctionExpression
          | {
              pexp_desc = Pexp_sequence (_wrapperExpression, innerFunctionExpression)
            } ->
              spelunkForFunExpression innerFunctionExpression
          | _ -> raise (Invalid_argument "react.component calls can only be on function definitions or component wrappers (forwardRef, memo).")
          ) in
          spelunkForFunExpression expression
        in
        let modifiedBinding binding =
          let wrapExpressionWithBinding expressionFn expression =
            Vb.mk
              ~loc:bindingLoc
              ~attrs:(List.filter otherAttrsPure binding.pvb_attributes)
              (Pat.var ~loc:bindingPatLoc {loc = bindingPatLoc; txt = fnName}) (expressionFn expression) in
          let expression = binding.pvb_expr in
          let unerasableIgnoreExp exp = { exp with pexp_attributes = (unerasableIgnore emptyLoc) :: exp.pexp_attributes } in
          (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
          let rec spelunkForFunExpression expression = (match expression with
          (* let make = (~prop) => ... with no final unit *)
          | {
            pexp_desc = Pexp_fun ((Labelled(_) | Optional(_) as label), default, pattern, ({pexp_desc = Pexp_fun _} as internalExpression))
          } ->
            let (wrap, hasUnit, exp) = spelunkForFunExpression internalExpression in
            (wrap, hasUnit, unerasableIgnoreExp {expression with pexp_desc = Pexp_fun (label, default, pattern, exp)})
          (* let make = (()) => ... *)
          (* let make = (_) => ... *)
          | {
            pexp_desc = Pexp_fun (Nolabel, _default, { ppat_desc = Ppat_construct ({txt = Lident "()"}, _) | Ppat_any}, _internalExpression)
            } -> ((fun a -> a), true, expression)
          (* let make = (~prop) => ... *)
          | {
            pexp_desc = Pexp_fun (_label, _default, _pattern, _internalExpression)
          } -> ((fun a -> a), false, unerasableIgnoreExp  expression)
          (* let make = {let foo = bar in (~prop) => ...} *)
          | {
              pexp_desc = Pexp_let (recursive, vbs, internalExpression)
            } ->
            (* here's where we spelunk! *)
            let (wrap, hasUnit, exp) = spelunkForFunExpression internalExpression in
            (wrap, hasUnit, {expression with pexp_desc = Pexp_let (recursive, vbs, exp)})
          (* let make = React.forwardRef((~prop) => ...) *)
          | { pexp_desc = Pexp_apply (wrapperExpression, [(Nolabel, internalExpression)]) } ->
            let (_, hasUnit, exp) = spelunkForFunExpression internalExpression in
            ((fun exp -> Exp.apply wrapperExpression [(nolabel, exp)]), hasUnit, exp)
          | {
              pexp_desc = Pexp_sequence (wrapperExpression, internalExpression)
            } ->
            let (wrap, hasUnit, exp) = spelunkForFunExpression internalExpression in
            (wrap, hasUnit, {expression with pexp_desc = Pexp_sequence (wrapperExpression, exp)})
          | e -> ((fun a -> a), false, e)
          ) in
          let (wrapExpression, hasUnit, expression) = spelunkForFunExpression expression in
          (wrapExpressionWithBinding wrapExpression, hasUnit, expression)
        in
        let (bindingWrapper, hasUnit, expression) = modifiedBinding binding in
        let reactComponentAttribute = try
          Some(List.find hasAttr binding.pvb_attributes)
        with | Not_found -> None in
        let (_attr_loc, payload) = match reactComponentAttribute with
        | Some (loc, payload) -> (loc.loc, Some payload)
        | None -> (emptyLoc, None) in
        let props = getPropsAttr payload in
        (* do stuff here! *)
        let (namedArgList, forwardRef) = recursivelyTransformNamedArgsForMake mapper (modifiedBindingOld binding) [] in
        let binding = { binding with pvb_expr = expression; pvb_attributes = [] } in
        let namedArgListWithKeyAndRef = match forwardRef with
        | Some(_) ->  (optional("ref"), None, Pat.var {txt = "ref"; loc = emptyLoc}, "ref", emptyLoc, None) :: namedArgList
        | None -> namedArgList
        in
        let namedArgListWithKeyAndRefForNew = match forwardRef with
        | Some(txt) -> namedArgList @ [(nolabel, None, Pat.var {txt; loc = emptyLoc}, txt, emptyLoc, None)]
        | None -> namedArgList
        in
        let pluckArg (label, _, _, alias, loc, _) =
          let labelString = (match label with | label when isOptional label || isLabelled label -> getLabel label | _ -> "") in
          (label,
            (match labelString with
              | "" ->  (Exp.ident ~loc {
                txt = (Lident alias);
                loc
              })
              | labelString -> (Exp.apply ~loc
                (Exp.ident ~loc {txt = (Lident "##"); loc })
                [
                  (nolabel, Exp.ident ~loc {txt = (Lident props.propsName); loc });
                  (nolabel, Exp.ident ~loc {
                    txt = (Lident labelString);
                    loc
                  })
                ]
              )
            )
          ) in
        let namedTypeList = List.fold_left argToType [] namedArgList in
        let loc = emptyLoc in
        let externalDecl = makeExternalDecl fnName loc namedArgListWithKeyAndRef namedTypeList in
        let propsType = match props.propsType with
        | Some(fnName) -> Some(makeTypeExp ~fnName ~loc (makePropsType ~loc namedTypeList) namedTypeList)
        | None -> None
        in
        let innerExpressionArgs = (List.map pluckArg namedArgListWithKeyAndRefForNew) @
          if hasUnit then [(Nolabel, Exp.construct {loc; txt = Lident "()"} None)] else [] in
        let innerExpression = Exp.apply (Exp.ident {loc; txt = Lident(fnName)}) innerExpressionArgs in
        let innerExpressionWithRef = match (forwardRef) with
        | Some txt ->
          {innerExpression with pexp_desc = Pexp_fun (nolabel, None, {
            ppat_desc = Ppat_var { txt; loc = emptyLoc };
            ppat_loc = emptyLoc;
            ppat_attributes = [];
          }, innerExpression)}
        | None -> innerExpression
        in
        let fullExpression = Exp.fun_
          nolabel
          None
          {
            ppat_desc = Ppat_constraint (
              makePropsName ~loc:emptyLoc props.propsName,
              makePropsType ~loc:emptyLoc namedTypeList
            );
            ppat_loc = emptyLoc;
            ppat_attributes = [];
          }
          innerExpressionWithRef in
        let fullExpression = match (fullModuleName) with
        | ("") -> fullExpression
        | (txt) -> Exp.let_
          Nonrecursive
          [Vb.mk
            ~loc:emptyLoc
            (Pat.var ~loc:emptyLoc {loc = emptyLoc; txt})
            fullExpression
          ]
          (Exp.ident ~loc:emptyLoc {loc = emptyLoc; txt = Lident txt}) in
        let newBinding = bindingWrapper fullExpression in
        (* let newBinding = Vb.mk (Pat.var {loc = emptyLoc; txt = fnName}) (Exp.mk fullExpression) in *)
        (Some externalDecl, propsType, binding, Some newBinding)
        (* (Some externalDecl, propsType, newBinding, None) *)
      else
        (None, None, binding, None)
      in
      let structuresAndBinding = List.map mapBinding valueBindings in
      let otherStructures (extern, propType, binding, newBinding) (externs, propTypes, bindings, newBindings) =
        let externs = match extern with
        | Some extern -> extern :: externs
        | None -> externs in
        let propTypes = match propType with
        | Some propType -> propType :: propTypes
        | None -> propTypes in
        let newBindings = match newBinding with
        | Some newBinding -> newBinding :: newBindings
        | None -> newBindings in
        (externs, propTypes, binding :: bindings, newBindings)
      in
      let (externs, propTypes, bindings, newBindings) = List.fold_right otherStructures structuresAndBinding ([], [], [], []) in
      List.concat([
        propTypes;
        externs;
        [{
          pstr_loc;
          pstr_desc = Pstr_value (
            recFlag,
            bindings
          )
        }] @ (match newBindings with
        | [] -> []
        | newBindings -> [{
          pstr_loc = emptyLoc;
          pstr_desc = Pstr_value (
            recFlag,
            newBindings
          )
        }]);
        returnStructures
      ])
    | structure -> structure :: returnStructures in

  let reactComponentTransform mapper structures =
  List.fold_right (transformComponentDefinition mapper) structures [] in

  let transformComponentSignature _mapper signature returnSignatures = match signature with
  | ({
      psig_loc;
      psig_desc = Psig_value ({
        pval_name = { txt = fnName };
        pval_attributes;
        pval_type;
      } as psig_desc)
    } as psig) ->
    (match List.filter hasAttr pval_attributes with
    | [] -> signature :: returnSignatures
    | [_] ->
    let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
      (match ptyp_desc with
      | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest)) when isOptional name || isLabelled name ->
        getPropTypes ((name, ptyp_loc, type_)::types) rest
      | Ptyp_arrow (Nolabel, _type, rest) ->
        getPropTypes types rest
      | Ptyp_arrow (name, type_, returnValue) when isOptional name || isLabelled name ->
        (returnValue, (name, returnValue.ptyp_loc, type_)::types)
      | _ -> (fullType, types))
    in
    let (innerType, propTypes) = getPropTypes [] pval_type in
    let namedTypeList = List.fold_left argToConcreteType [] propTypes in
    let pluckLabelAndLoc (label, loc, type_) = (label, None, loc, Some type_) in
    let retPropsType = makePropsType ~loc:psig_loc namedTypeList in
    let externalPropsDecl = makePropsExternalSig fnName psig_loc (List.map pluckLabelAndLoc propTypes) retPropsType in
        (* can't be an arrow because it will defensively uncurry *)
    let newExternalType = Ptyp_constr (
      {loc = psig_loc; txt = Ldot ((Lident "React"), "componentLike")},
      [retPropsType; innerType]
    ) in
    let newStructure = {
      psig with psig_desc = Psig_value {
        psig_desc with pval_type = {
          pval_type with ptyp_desc = newExternalType;
        };
        pval_attributes = List.filter otherAttrsPure pval_attributes;
      }
    } in
    externalPropsDecl :: newStructure :: returnSignatures
    | _ -> raise (Invalid_argument "Only one react.component call can exist on a component at one time"))
  | signature -> signature :: returnSignatures in

  let reactComponentSignatureTransform mapper signatures =
  List.fold_right (transformComponentSignature mapper) signatures [] in


  let transformJsxCall mapper callExpression callArguments attrs =
    (match callExpression.pexp_desc with
     | Pexp_ident caller ->
       (match caller with
        | {txt = Lident "createElement"} ->
          raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")

        (* Foo.createElement(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
        | {loc; txt = Ldot (modulePath, ("createElement" | "make"))} ->
          (match !jsxVersion with
          | None
          | Some "3" -> transformUppercaseCall3 modulePath mapper loc attrs callExpression callArguments
          | Some _ -> raise (Invalid_argument "JSX: the JSX version must be 2 or 3"))

        (* div(~prop1=foo, ~prop2=bar, ~children=[bla], ()) *)
        (* turn that into
          ReactDOMRe.createElement(~props=ReactDOMRe.props(~props1=foo, ~props2=bar, ()), [|bla|]) *)
        | {loc; txt = Lident id} ->
          (match !jsxVersion with
          | None
          | Some "3" -> transformLowercaseCall3 mapper loc attrs callArguments id
          | Some _ -> raise (Invalid_argument "JSX: the JSX version must be 2 or 3"))

        | {txt = Ldot (_, anythingNotCreateElementOrMake)} ->
          raise (
            Invalid_argument
              ("JSX: the JSX attribute should be attached to a `YourModuleName.createElement` or `YourModuleName.make` call. We saw `"
               ^ anythingNotCreateElementOrMake
               ^ "` instead"
              )
          )

        | {txt = Lapply _} ->
          (* don't think there's ever a case where this is reached *)
          raise (
            Invalid_argument "JSX: encountered a weird case while processing the code. Please report this!"
          )
       )
     | _ ->
       raise (
         Invalid_argument "JSX: `createElement` should be preceeded by a simple, direct module name."
       )
    ) in

  let signature =
    (fun mapper signature -> default_mapper.signature mapper @@ reactComponentSignatureTransform mapper signature) in

  let structure =
    (fun mapper structure ->
      match structure with
      | structures -> begin
        default_mapper.structure mapper @@ reactComponentTransform mapper structures
      end
    ) in

  let expr =
    (fun mapper expression -> match !jsxVersion, expression with
       (* Does the function application have the @JSX attribute? *)
       | _, {
           pexp_desc = Pexp_apply (callExpression, callArguments);
           pexp_attributes
         } ->
         let (jsxAttribute, nonJSXAttributes) = List.partition (fun (attribute, _) -> attribute.txt = "JSX") pexp_attributes in
         (match (jsxAttribute, nonJSXAttributes) with
         (* no JSX attribute *)
         | ([], _) -> default_mapper.expr mapper expression
         | (_, nonJSXAttributes) -> transformJsxCall mapper callExpression callArguments nonJSXAttributes)

       (* is it a list with jsx attribute? Reason <>foo</> desugars to [@JSX][foo]*)
       | _, ({
           pexp_desc =
            Pexp_construct ({txt = Lident "::"; loc}, Some {pexp_desc = Pexp_tuple _})
            | Pexp_construct ({txt = Lident "[]"; loc}, None);
           pexp_attributes
         } as listItems) ->
          let (jsxAttribute, nonJSXAttributes) = List.partition (fun (attribute, _) -> attribute.txt = "JSX") pexp_attributes in
          (match (jsxAttribute, nonJSXAttributes) with
          (* no JSX attribute *)
          | ([], _) -> default_mapper.expr mapper expression
          | (_, nonJSXAttributes) ->
            let fragment = Exp.ident ~loc {loc; txt = Ldot (Ldot (Lident "React", "Fragment"), "make")} in
            let fragmentProps = Exp.ident ~loc {loc; txt = Ldot (Ldot (Lident "React", "Fragment"), "makeProps")} in
            let childrenExpr = transformChildrenIfListUpper ~loc ~mapper listItems in
            let (jsxCall, args) = (match childrenExpr with
            | Exact children -> ("jsx", [(labelled "children", children)])
            | ListLiteral ({ pexp_desc = Pexp_array list }) when list = [] -> ("jsx", [])
            | ListLiteral expression ->
            ("jsxs", [(labelled "children", (Exp.apply
              ~loc
              (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "array")})
              ([
                (nolabel, expression)
              ])))])) in
            let args = args @ [(nolabel, Exp.construct ~loc {loc; txt = Lident "()"} None)] in
            Exp.apply
              ~loc
              (* throw away the [@JSX] attribute and keep the others, if any *)
              ~attrs:(nonJSXAttributes @ (depIgnore loc))
              (* ReactDOMRe.createElement *)
              (Exp.ident ~loc {loc; txt = Ldot (Lident "React", jsxCall)})
              ([
                (nolabel, fragment);
                (nolabel, Exp.apply ~loc fragmentProps args)
              ])
         )
       (* Delegate to the default mapper, a deep identity traversal *)
       | _, e -> default_mapper.expr mapper e) in

  let module_binding =
    (fun mapper module_binding ->
      let _ = nestedModules := module_binding.pmb_name.txt :: !nestedModules in
      let mapped = default_mapper.module_binding mapper module_binding in
      let _ = nestedModules := List.tl !nestedModules in
      mapped
    ) in

  { default_mapper with structure; expr; signature; module_binding; }

let rewrite_implementation (code: Parsetree.structure) : Parsetree.structure =
  let mapper = jsxMapper () in
  mapper.structure mapper code

let rewrite_signature (code : Parsetree.signature) : Parsetree.signature =
  let mapper = jsxMapper () in
  mapper.signature mapper code

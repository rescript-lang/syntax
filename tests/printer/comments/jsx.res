module Cite = {
  @react.component
  let make = (~author: option<string>, ~children) => {
    // For semantics, check out
    // https://css-tricks.com/quoting-in-html-quotations-citations-and-blockquotes/
    <div>
      foo
    </div>
  }
}

<A
  value=""
  // Comment
/>

<A /* comment */ />

<A>
  // Comment
</A>

<A
// comment1
foo="1"
// comment2
>
  // comment3
  <B
  // comment5
  foo="1"
  // comment6
  >
  // comment7
  <C />
  // comment8
  </B>
  // comment4
</A>

<div>
  // Must not jump inside braces
  {React.string("Hello, World!")}
</div>

<div>
  // Must not jump inside braces
  {// But this one is inside
    React.string("Hello, World!")}
</div>

let x = <>
  // before a
  {a} // after a
  // before b
  {b} // after b
</>

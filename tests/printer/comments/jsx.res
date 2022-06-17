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

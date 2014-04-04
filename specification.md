CLIP Specification
==================

Proposed methods of markup:
 * Separate file similar to css ( selector{ foo } )
     This is dumb and convoluted. The only advantage would be a strict
     separation, but it's not worth the price.
 * Blocks in a clip namespace ( <clip:foo> )
     This could be an addition, but definitely not the only method
     since attributes are more suited for filling specific tags than
     having to wrap everything in its own block.
 * Special clip attribute on valid html blocks ( <div clip="foo"> )
     Probably in combination with the previous method. The problem with
     attributes is that they're a bother to write and read.

Parsing phases:
 0 Parse HTML into DOM (plump).
 1 Recurse into tags depth-first and expand macros where encountered.
 2 While unwinding the recursion, execute functions.


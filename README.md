About Clip
----------
Clip is an attempt at a templating library that allows you to write templates in a way that is both accessible to direct webdesign and flexible. The main idea is to incorporate transformation commands into an HTML file through tags and attributes. Clip is heavily dependant on [Plump](https://shinmera.github.io/plump) and [lQuery](https://shinmera.github.io/lquery). 

How To
------
Load Clip through ASDF or Quicklisp.

```(ql:quickload :clip)```

To process a template, simply call ```process```:

```(clip:process #p"my-template.html")```

You may also pass in pure strings or plump nodes. Most likely you will want to include some kind of data in your template. Data in Clip is managed through a central `*CLIPBOARD*`. The additional arguments you pass to `PROCESS` are entered into the initial clipboard like a plist (key and value alternating).

Depending on the current tag environment and how the template is processed at the time these values can come into play. Most of the time, entering the name as passed into `PROCESS` in the template as an attribute value will then evaluate to the according value using `RESOLVE-VALUE`. In the case of a symbol this then delegates to `CLIP`, which returns the value stored in the clipboard.

The value returned by `PROCESS` is the node you passed into it. You can parse this back into a string using `PLUMP:SERIALIZE` or lQuery's `WRITE-TO-FILE`.

Standard Tags
-------------
* NOOP <br />
  This tag only processes its attributes, but none of its children.

* LET <br />
  Creates a new clipboard environment with all the tag attributes bound in the following manner: The attribute key is put into the clipboard as if by READ and associated with the value of `RESOLVE-VALUE` of the attribute value.

* ITERATE <br />
  Looks for one attribute called 'OVER' and then works like the ITERATE attribute processor using the value of the OVER attribute.

* EXPAND <br />
  This tag expands its attributes and then calls `PROCESS-NODE` on itself again. This is useful to generate attributes to be expanded.

* SPLICE <br />
  Splices all nodes within it into the parent's child list at the position of itself (essentially replacing it with its children).

* WHEN <br />
  Looks for a 'TEST' attribute and if the value of it as by `RESOLVE-VALUE` is non-NIL, acts like SPLICE. Otherwise it removes itself including its children from the DOM.

* UNLESS <br />
  Same as WHEN, but inverted.

* IF <br />
  Looks for either a 'TEST' attribute or a 'TEST' tag as one of its direct children. If the test as by `RESOLVE-VALUE` is non-NIL, all children of the 'THEN' tag are spliced in place of the IF block. Otherwise the same is done for the 'ELSE' block. If neither 'THEN' or 'ELSE' blocks are found as direct children of the 'IF', the 'IF' is simply removed from the DOM.

* USING <br />
  Binds the clipboard to the resolved-value of its 'FIELD' attribute. This binding is only active for blocks within the 'USING' block.

Standard Attributes
-------------------
* LQUERY <br />
  Calls lQuery functions on the node as if by `($ node ..)`. Note that only lQuery functions are available, not its macros.

* EVAL <br />
  Simply calls `EVAL` on the value of `READ-FROM-STRING` of the attribute value.

* ITERATE <br />
  The value (as by `RESOLVE-VALUE`) is used as an iteration list or vector. The first node within the node this attribute belongs to is copied once for each item in the iteration list and processed with that item used as the clipboard.

* AS <br />
  Simply changes that node's tag-name to the value of this attribute.

* COUNT <br />
  Inserts the value of `*TARGET-COUNT*` as the attribute value. This is useful to follow processing order in debugging.

Extending Clip
--------------
You can define new tag and attribute processors with the macros `DEFINE-TAG-PROCESSOR` and `DEFINE-ATTRIBUTE-PROCESSOR`. For tag processors you will usually want to make sure to call `PROCESS-ATTRIBUTES` and `PROCESS-CHILDREN` to ensure that tags and attributes within are properly processed. To retrieve values most of the time you need to use `RESOLVE-VALUE` (or its shorthand `RESOLVE-ATTRIBUTE`) unless you want to whip up your own system for one reason or another.

Further Reading
---------------
* [Plump](https://shinmera.github.io/plump)
* [lQuery](https://shinmera.github.io/lquery)

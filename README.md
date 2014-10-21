About Clip
----------
Clip is an attempt at a templating library that allows you to write templates in a way that is both accessible to direct webdesign and flexible. The main idea is to incorporate transformation commands into an HTML file through tags and attributes. Clip is heavily dependant on [Plump](https://shinmera.github.io/plump) and [lQuery](https://shinmera.github.io/lquery). 

How To
------
Load Clip through ASDF or Quicklisp.

```
(ql:quickload :clip)
```

To process a template, simply call `PROCESS`:

```
(clip:process #p"my-template.ctml")
```

You may also pass in pure strings or plump nodes. Most likely you will want to include some kind of data in your template. Data in Clip is managed through a central `CLIPBOARD`. The additional arguments you pass to `PROCESS` are entered into the initial clipboard like a plist (key and value alternating).

Depending on the current tag environment and how the template is processed at the time these values can come into play. Most of the time, entering the name as passed into `PROCESS` in the template as an attribute value will then evaluate to the according value using `RESOLVE-VALUE`. In the case of a symbol this then delegates to `CLIP`, which returns the value stored in the clipboard.

The value returned by `PROCESS` is the node you passed into it. You can parse this back into a string using `PLUMP:SERIALIZE` or lQuery's `WRITE-TO-FILE`.

Standard Tags
-------------
* `C:EXPAND` <br />
  This tag expands its attributes and then calls `PROCESS-NODE` on itself again. This is useful to generate attributes to be expanded.

* `C:IF` <br />
  Looks for either a `TEST` attribute or a `C:TEST` tag as one of its direct children. If the test as by `RESOLVE-VALUE` is non-NIL, all children of the `C:THEN` tag are spliced in place of the IF block. Otherwise the same is done for the `C:ELSE` block. If neither `C:THEN` or `C:ELSE` blocks are found as direct children of the `C:IF`, the `C:IF` is simply removed from the DOM.

* `C:ITERATE` <br />
  Looks for one attribute called `OVER` and then works like the `ITERATE` attribute processor using the value of the `OVER` attribute.

* `C:LET` <br />
  Creates a new clipboard environment with all the tag attributes bound in the following manner: The attribute key is put into the clipboard directly and associated with the value of `RESOLVE-VALUE` of the attribute value. Acts like `SPLICE`.

* `C:NOOP` <br />
  This tag only processes its attributes, but none of its children.

* `C:SPLICE` <br />
  Splices all nodes within it into the parent's child list at the position of itself (essentially replacing it with its children).

* `C:UNLESS` <br />
  Same as WHEN, but inverted.

* `C:USING` <br />
  Binds the clipboard to the resolved-value of its `VALUE` attribute. Acts like `SPLICE`.

* `C:WHEN` <br />
  Looks for a `TEST` attribute and if the value of it as by `RESOLVE-VALUE` is non-NIL, acts like `SPLICE`. Otherwise it removes itself including its children from the DOM.

Standard Attributes
-------------------
* `AS` <br />
  Simply changes that node's tag-name to the value of this attribute.

* `COUNT` <br />
  Inserts the value of `*TARGET-COUNT*` as the attribute value. This is useful to follow processing order during debugging.

* `EVAL` <br />
  Simply calls `EVAL` on the value of `READ-FROM-STRING` of the attribute value.

* `ITERATE` <br />
  The value (as by `RESOLVE-VALUE`) is used as an iteration list or vector. The first node within the node this attribute belongs to is copied once for each item in the iteration list and processed with that item used as the clipboard.

* `LQUERY` <br />
  Calls lQuery functions on the node as if by `($ node ..)`. Note that only lQuery functions are available, not its macros.

* `FILL` <br />
  The attribute value is read as a plist, the keys of which designate other attribute names and the values are resolved to the objects to use. For each named attribute, its value is modified by replacing `{thing}` by the result of `clip` on the respective object's field `thing`.

Extending Clip
--------------
You can define new tag and attribute processors with the macros `DEFINE-TAG-PROCESSOR` and `DEFINE-ATTRIBUTE-PROCESSOR`. For tag processors you will usually want to make sure to call `PROCESS-ATTRIBUTES` and `PROCESS-CHILDREN` to ensure that tags and attributes within are properly processed. To retrieve values most of the time you need to use `RESOLVE-VALUE` (or its shorthand `RESOLVE-ATTRIBUTE`) unless you want to whip up your own system for one reason or another. All tags that you define will automatically be prefixed with `C:` in order to help highlighting template tags and ensure that there are no collisions with existing tags.

Editor Support
--------------
The Emacs extension [Web-Mode](http://web-mode.org/)(version 9.0.77+) provides syntax highlighting for Clip templates. In order for it to recognise the templates, use the `.ctml` file extension. A huge thanks goes to [Bois Francois-Xavier](https://github.com/fxbois) for adding the support.

Tutorials
---------
These are short tutorials to help explaining the effects of each tag and to illustrate some basic templating techniques. The examples will only work with Clip>=0.5.1 and lQuery>=3.1.1 .

###Updating a node with values

    (clip:process-to-string
     "<span lquery=\"(text text) (add-class class)\" />"
     :text "Hi!" :class "clip-text")

Explanation: The `LQUERY` attribute allows you to perform lQuery operations on the node it is an attribute of. In this case, the `TEXT` function sets the text of the node to the value of `TEXT`, which we told Clip to be `"Hi!"`. Similarly for `ADD-CLASS`. Any non-keyword symbol within the template is automatically resolved to a field on the current clipboard. You may think of the clipboard as a form of lexical environment for the template, which we currently set to have the variables `TEXT` and `CLASS` bound. The default `CLIPBOARD` object is special in the sense that it does not differentiate between accessing it with keywords, symbols or strings and is case-insensitive. This makes it easier to access in templates.

Please see the [lQuery](https://shinmera.github.io/lquery) documentation for all possible node manipulation functions.

###Populating from a list

    (clip:process-to-string
     "<ol iterate=\"todo-list\"><li lquery=\"(text *)\"></li></ol>"
     :todo-list '("Write tutorials" "Make tiramisu" "Visit grandma"))

The `ITERATE` attribute goes over the list or vector of elements its attribute-value resolves to and uses each item as the current clipboard for the iteration element. Since in this case these values themselves are direct strings we cannot retrieve further values from them and instead need to use `*` to refer to the entire clipboard.

###Conditionals

    (clip:process-to-string
     "<ul iterate=\"users\">
      <li><c:if test=\"anonymous\"><c:then>Username Hidden</c:then><c:else lquery=\"(text username)\"/></c:if></li>
    </ul>"
     :users '((:username "Some Guy" :anonymous T) (:username "Some Other Guy" :anonymous NIL) (:username "You" :anonymous NIL)))

Clip offers a couple of constructs to perform conditionals. These constructs are `C:WHEN` `C:UNLESS` and `C:IF`, after their CL equivalents. Each take an attribute called `TEST` that has to resolve to a non-NIL value to be taken as true. In the case of `C:IF`, three special local child tags are used: `C:THEN`, `C:ELSE` and `C:TEST`. The `C:TEST` tag can be used as an alternative to the test attribute. The other two should be self-explanatory. Note that none of the child-tags or attributes of an unchosen branch are processed.

###Bindings

    (clip:process-to-string
     "<c:using value=\"num\">
      <c:let orig=\"*\" double=\"(* * 2)\" square=\"(expt * 2)\" root=\"(sqrt *)\">
        <span lquery=\"(text (list orig double square root))\" />
      </c:let>
    </c:using>"
     :num 2)

In order to manipulate the clipboard bindings you can use the `C:USING` and `C:LET` special tags. `C:USING` replaces the clipboard environment with what the value of its `VALUE` attribute resolves to. `C:LET` on the other hand creates a new `CLIPBOARD` object, setting the specified symbol/value pairs from its attributes.

###Clipboard Stack

    (clip:process-to-string
     "<ul iterate=\"articles\">
      <li><article>
        <header><div class=\"author\" lquery=\"(text (** :author))\">AUTHOR</div></header>
        <section class=\"content\" lquery=\"(text *)\">CONTENT</section>
      </article></li>
    </ul>"
     :author "Max Mastermind" :articles '("Whoa I am blogging!!" "I don't know what to write, sadface."))

Sometimes you need to refer to values in clipboards outside of the current binding. No worries, this is easy to do as the clipboards are organised using a stack. You can reach clipboards higher up in the stack using the asterisk symbols. Each asterisk more is one clipboard higher. Using the asterisk symbol as a variable returns the clipboard directly, using it as a function call is akin to doing `(CLIP ** 'thing)`. In order to avoid clashing with the `*` multiplication function, the asterisk function shorthand is only active for two or more asterisks.

###Function References

    (defun seconds () (decode-universal-time (get-universal-time)))
    (clip:process-to-string
     "<time lquery=\"(text (seconds))\">TIME</time>")

Whenever you require to use functions within clip documents, you need to be aware of the current value of `*package*`. As values that are resolved are first parsed using `read`, they are influenced by `*package*`. You can of course use fully qualified symbol names, but often times it is useful to bind the variable to the package you need to reduce verbosity.

You must also be aware of the special resolving for symbols used as function calls within standard resolvings. As mentioned in the previous section, symbols only consisting of asterisks are specially handled. Additionally, the symbols `cl:quote`, `cl:function`, `cl:or`, `cl:and`, `cl:if`, `cl:when`, and `cl:unless` are handled to work like their usual macro/special equivalents. Any other symbol is treated as follows: If a function's symbol with the same symbol-name is externalised from  the `clip` package, the `clip` function is used. If not, the function named by the symbol in the symbol's package is used. This is done so that, no matter your package, you will always have access to functions like `clip` and `clipboard`. As an unfortunate side-effect of a symbol not knowing whether it was fully qualified or not, this means that even if you use the full symbol name with package in your template, as long as the name is external in `clip`, the `clip` function is used instead. You will have to use a combination of `funcall` and `#'` to circumvent this limitation..

Further Reading
---------------
* [Plump](https://shinmera.github.io/plump)
* [lQuery](https://shinmera.github.io/lquery)

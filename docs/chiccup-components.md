# Building UI Components with Chiccup

Chiccup's killer feature isn't its syntax — it's that **HTML is just data**, and data is what Scheme is best at. Once you internalize that, building UIs stops looking like templating and starts looking like ordinary functional programming: components are functions, composition is function calls, and your design system is just a module of small, well-named procedures.

This guide walks through the patterns that make Chiccup codebases scale: how to factor out reusable pieces, how to compose them, how to handle layouts and conditional content, and how to organize a growing UI without reaching for a template engine.

## Table of Contents

- [The Core Idea: Components are Functions](#the-core-idea-components-are-functions)
- [Composing Components](#composing-components)
- [Parameterizing Components](#parameterizing-components)
- [Rendering Lists](#rendering-lists)
- [Layouts and the "Slot" Pattern](#layouts-and-the-slot-pattern)
- [Conditional Rendering](#conditional-rendering)
- [Building a Small Design System](#building-a-small-design-system)
- [Organizing Components](#organizing-components)
- [Common Pitfalls](#common-pitfalls)

## The Core Idea: Components are Functions

A Chiccup component is any function that returns a Chiccup form (a list). There is no `defcomponent` macro, no special registration, no lifecycle. If it returns a quasiquoted list, it's a component.

```scheme
(define (greeting name)
  `[h1.text-2xl.font-bold ,(string-append "Hello, " name "!")])

(ccup->html (greeting "Ada"))
;; => <h1 class="text-2xl font-bold">Hello, Ada!</h1>
```

That's the whole abstraction. Components compose with normal function calls, parameterize with normal arguments, and test with normal `equal?` assertions on the returned list — no DOM, no renderer, no virtual tree.

## Composing Components

Because components return lists, you can drop one into another with unquote (`,`):

```scheme
(define (avatar url)
  `[img.w-10.h-10.rounded-full (@ (src ,url))])

(define (user-badge name avatar-url)
  `[.flex.items-center.gap-3
    ,(avatar avatar-url)
    [span.font-medium ,name]])

(define (comment author body)
  `[.p-4.bg-white.rounded-lg.shadow-sm
    ,(user-badge (alist-ref 'name author) (alist-ref 'avatar author))
    [p.mt-2.text-gray-700 ,body]])
```

Each layer only knows about the one below it. `comment` doesn't care how `user-badge` lays itself out, and `user-badge` doesn't care that `avatar` happens to be an `img` rather than a `div` with a background image. Swap one out and the rest keeps working.

## Parameterizing Components

CHICKEN gives you optional arguments and keyword arguments out of the box. Use them — they're the cleanest way to express "this component has sensible defaults but you can override them."

### Optional arguments

```scheme
(define (button label #!optional (variant 'primary))
  (let ((classes (case variant
                   ((primary)   "bg-teal-600 text-white hover:bg-teal-700")
                   ((secondary) "bg-white text-teal-700 border border-teal-300")
                   ((danger)    "bg-red-600 text-white hover:bg-red-700"))))
    `[button (@ (class ,(string-append "px-4 py-2 rounded font-semibold " classes)))
      ,label]))

(button "Save")              ; primary by default
(button "Cancel" 'secondary)
(button "Delete" 'danger)
```

### Keyword arguments

When a component has several knobs, keyword arguments read better than positional ones:

```scheme
(define (card #!key title body (footer #f) (highlighted #f))
  `[.bg-white.rounded-lg.shadow-sm.p-6
    (@ ,@(if highlighted '((class "ring-2 ring-teal-500")) '()))
    [h3.text-lg.font-semibold ,title]
    [.mt-2.text-gray-700 ,body]
    ,(if footer `[.mt-4.pt-4.border-t ,footer] '())])

(card title: "Welcome"
      body:  "Get started with Schematra in minutes."
      footer: (button "Learn more"))
```

The caller doesn't have to remember argument order, and adding a new option later doesn't break existing call sites.

## Rendering Lists

Mapping over data is where Chiccup feels most natural — it's just `map` and unquote-splicing (`,@`).

```scheme
(define (tag-list tags)
  `[.flex.flex-wrap.gap-2
    ,@(map (lambda (tag)
             `[span.text-xs.bg-teal-100.text-teal-700.px-2.py-1.rounded ,tag])
           tags)])

(tag-list '("scheme" "web" "functional"))
```

Note the distinction:

- `,(expr)` inserts a single value.
- `,@(expr)` splices a list of values into the surrounding list.

If you forget the `@`, the whole list of children is inserted as a *single* child instead of being spliced as siblings. Chiccup then tries to parse that inner list as another element — usually leading to a broken render or an error — rather than producing the flat sequence of `<span>`s you wanted.

### Empty states

When the list might be empty, branch in Scheme — not in the template:

```scheme
(define (post-list posts)
  (if (null? posts)
      `[.text-center.text-gray-500.py-8 "No posts yet."]
      `[.space-y-4 ,@(map post-card posts)]))
```

This is the kind of thing template engines make awkward and Scheme makes obvious.

## Layouts and the "Slot" Pattern

Most apps have a shared chrome — header, footer, meta tags — that wraps every page. Express that as a function that takes the page content as an argument:

```scheme
(define (layout page #!key (title "My App") (description ""))
  `[html (@ (lang "en"))
    [head
     [meta (@ (charset "utf-8"))]
     [title ,title]
     [meta (@ (name "description") (content ,description))]
     [link (@ (rel "stylesheet") (href "/static/app.css"))]]
    [body.bg-gray-50
     ,(navbar)
     [main.max-w-4xl.mx-auto.py-8.px-4 ,page]
     ,(footer)]])
```

Route handlers then become trivial:

```scheme
(get ("/")
     `(ccup ,(layout (home-page) title: "Welcome")))

(get ("/about")
     `(ccup ,(layout (about-page)
                     title: "About"
                     description: "Who we are and what we do.")))
```

### Multiple slots

If a layout needs more than one hole — say, a sidebar and a main column — just add more parameters:

```scheme
(define (two-column-layout #!key sidebar main)
  `[.grid.grid-cols-1.md:grid-cols-4.gap-6
    [aside.md:col-span-1 ,sidebar]
    [.md:col-span-3 ,main]])

(two-column-layout
 sidebar: (filter-panel current-filters)
 main:    (results-grid current-results))
```

This is the same idea as React's `children` prop or Vue's slots, except it's just function arguments — nothing to learn.

## Conditional Rendering

Use Scheme's conditionals. They're already there and they already work.

### Conditional elements

```scheme
(define (nav-link href label #!key (active #f))
  `[a (@ (href ,href)
         (class ,(if active
                     "text-teal-600 font-semibold"
                     "text-gray-700 hover:text-teal-600")))
    ,label])
```

### Conditional attributes

Use unquote-splicing with `if` to add or omit attributes entirely:

```scheme
(define (text-input name #!key (value "") (required #f) (placeholder ""))
  `[input.border.rounded.px-3.py-2
    (@ (type "text")
       (name ,name)
       (value ,value)
       (placeholder ,placeholder)
       ,@(if required '((required)) '()))])
```

When `required` is `#f`, the attribute is left out completely — not rendered as `required="false"`.

### Conditional sub-trees

Return `'()` (the empty list) to render nothing. Chiccup treats it as an empty body, not as a `<()>` tag:

```scheme
(define (alert message #!key (dismissible #f))
  `[.bg-yellow-50.border.border-yellow-200.rounded.p-4
    [p.text-yellow-800 ,message]
    ,(if dismissible
         `[button.text-yellow-600.text-sm "Dismiss"]
         '())])
```

## Building a Small Design System

The same way functions compose into bigger functions, atomic components compose into molecules and then into pages. Here's a tiny but realistic slice:

### Atoms

```scheme
(define (badge label #!key (color 'teal))
  (let ((bg (case color ((teal) "bg-teal-100 text-teal-700")
                        ((red)  "bg-red-100 text-red-700")
                        ((gray) "bg-gray-100 text-gray-700"))))
    `[span (@ (class ,(string-append "text-xs px-2 py-1 rounded " bg))) ,label]))

(define (icon name)
  `[svg.w-4.h-4.inline-block (@ (data-icon ,name))])
```

### Molecules

```scheme
(define (post-meta date tags)
  `[.flex.items-center.gap-3.text-sm.text-gray-500
    [time ,date]
    [.flex.gap-1 ,@(map (lambda (t) (badge t color: 'teal)) tags)]])

(define (post-card post)
  (let ((title (alist-ref 'title post))
        (date  (alist-ref 'date post))
        (tags  (alist-ref 'tags post))
        (slug  (alist-ref 'slug post)))
    `[.bg-white.rounded-lg.shadow-sm.p-6.hover:shadow-md.transition-shadow
      ,(post-meta date tags)
      [h3.text-lg.font-semibold.mt-2
       [a (@ (href ,(string-append "/blog/" slug))) ,title]]]))
```

### Page

```scheme
(define (blog-index posts)
  `[.max-w-3xl.mx-auto
    [h1.text-3xl.font-bold.mb-6 "Blog"]
    ,(if (null? posts)
         `[.text-gray-500 "No posts yet."]
         `[.space-y-4 ,@(map post-card posts)])])
```

Each layer pulls from the one below it. When the designer asks for a new badge color, you change `badge` and every place that uses it updates. When a new card variant is needed, you add it next to `post-card` without touching the page.

## Organizing Components

There's no framework-mandated structure. A convention that fits Schematra well — and matches the framework's own file naming (`schematra.body-parser.scm`, `schematra.ws.scm`, `schematra.utils.scm`) — is to keep things flat and let dotted suffixes signal a file's role:

- **`*.components.scm`** — atomic, reusable pieces (buttons, badges, cards, layouts). One file per feature area, e.g. `nav.components.scm`, `forms.components.scm`.
- **`*.views.scm`** — page-level functions that assemble components into something a route can return, e.g. `blog.views.scm`, `dashboard.views.scm`.
- **`*.emails.scm`** (or similar) — components for non-HTTP HTML like email templates.

A small app might end up with just `app.components.scm` + `app.views.scm`; a larger one grows more files but stays in a single directory. The suffix tells you a file's role at a glance without forcing you to navigate a `components/` vs `pages/` tree — and it sidesteps directory-based ambiguity (is `card.scm` a component or a page?).

For larger apps, put components in their own CHICKEN module so you get explicit imports and exports:

```scheme
(module ui-components
  (button badge card layout)

  (import scheme chicken.base chiccup)

  (define (button label #!optional (variant 'primary)) ...)
  (define (badge label #!key (color 'teal)) ...)
  ;; ...
  )
```

Then in your route file:

```scheme
(import schematra chiccup ui-components)
```

This is the same hygiene you'd apply to any growing Scheme codebase — components don't need special treatment.

## Common Pitfalls

A few things trip up newcomers:

**Forgetting unquote-splicing for lists.** If you `,(map ...)` instead of `,@(map ...)`, you get a single nested list child instead of multiple siblings. The HTML often still renders, but the structure is wrong.

**Returning a string from a "component" function.** Components should return Chiccup forms (lists), not pre-rendered HTML strings. If you call `ccup->html` inside a component, you lose composability — the parent can no longer wrap or modify the structure. From a route handler, return the Chiccup form wrapped in a `(ccup ...)` response tuple (e.g. `` `(ccup ,(my-page)) ``) and Schematra will render it for you — no manual `ccup->html` call needed.

**Trying to add behavior inside Chiccup forms.** Chiccup is for *structure*. Interactivity belongs in the browser — either via [htmx](https://htmx.org/) attributes (which work great with Chiccup's `@` syntax), [WebSockets](docs.md#websockets), or a regular JavaScript file. Don't try to embed event handlers as Scheme code; embed them as HTML attributes or hook them up client-side.

**Quasiquote scoping.** Inside `` `[...] ``, everything is quoted by default. Use `,` to drop back into evaluation. A common mistake is writing `[h1 (string-append "Hi " name)]` instead of `` [h1 ,(string-append "Hi " name)] `` — the first emits the literal SXML, not the result of the call.

---

That's the whole story: components are functions, functions compose, and a list is a perfectly good UI. Lean on the language you already know, and the framework gets out of your way.

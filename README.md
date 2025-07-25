# Schematra

![Schematra Logo](logo.png)

A minimal web framework for [CHICKEN Scheme](https://call-cc.org/), inspired by [Sinatra](https://sinatrarb.com/). Schematra is currently an early exploration project created for learning purposes, but hopefully it will grow into something more useful over time.

## Why Schematra?

I created Schematra because I wanted to:

- **Improve my knowledge of scheme**: Building a web framework is a great way to explore a language's capabilities and idioms
- **Create something simple**: Most web frameworks are complex beasts. Schematra aims to be minimal and understandable
- **Enable modern web development**: The framework is designed to work well with modern tools like [Tailwind CSS](https://tailwindcss.com/) and [htmx](https://htmx.org/), making it easy to build interactive web applications without heavy JavaScript frameworks. Although tbh this is completely agnostic to how the framework works, it's what most of my examples will use.

## Features

- Simple route definition with `get` and `post` functions
- URL parameter extraction (e.g., `/users/:id`) & body parsing
- Middleware support
- Included naive session middleware (cookie storage)
- Development mode with REPL integration (leveraging emacs `run-scheme`)
- Very simple [hiccup](https://github.com/weavejester/hiccup) inspired template system
- Built on top of the solid [Spiffy](http://wiki.call-cc.org/eggref/5/spiffy) web server

## Installation

First, make sure you have [CHICKEN Scheme](https://call-cc.org/) installed. Then install the required dependencies:

```bash
chicken-install spiffy intarweb uri-common srfi-1 srfi-13 srfi-18 format
```

For development mode, you'll also need:

```bash
chicken-install nrepl
```

Next step would be to build schematra & chiccup:

```bash
csc -s schematra.scm -j schematra
csc -s chiccup.scm -j chiccup
```

## Quick Start

Here's a simple "Hello World" application:

```scheme
(import schematra)

;; Define routes
(get "/" (lambda (req params) "Hello, World!"))

(get "/users/:id" 
     (lambda (req params)
       (let ((user-id (alist-ref "id" params equal?)))
         (format "User ID: ~A" user-id))))

(post "/submit"
      (lambda (req params)
        (let ((body (request-body-string req)))
          (format "Received: ~A" body))))

;; Start the server
(schematra-install)
(schematra-start port: 8080)
```

Save this as `app.scm` and run:

```bash
csi app.scm
```

Visit `http://localhost:8080` to see your application running.

## Development Mode

For interactive development, start the server in development mode:

```scheme
(schematra-start development?: #t port: 8080 repl-port: 1234)
```

This starts the web server in a background thread and opens an NREPL on port 1234. You can connect with your favorite Scheme editor or use `nc localhost 1234` for a simple REPL session.

For a more elegant environment, you can use emacs `run-scheme` by running `C-u M-x run-scheme RET nc localhost 1234`.

## Route Parameters

Schematra supports URL parameters using the `:parameter` syntax, as well as query params:

```scheme
(define (lookup key alist)
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        #f)))

(get "/users/:user-id/posts/:post-id"
     (lambda (req params)
       (let ((user-id (lookup "user-id" params))
             (post-id (lookup "post-id" params))
			 (q       (lookup 'q params))) ;; query params use symbol params
         (format "User: ~A, Post: ~A" user-id post-id))))
```

The `params` argument contains both URL parameters (with string keys) and query parameters (with symbol keys).

## Working with Modern Web Tools

Schematra plays nicely with modern web development tools:

### Tailwind CSS

Include Tailwind via CDN in your HTML responses:

```scheme
(get "/tw-demo"
     (lambda (req params)
       (ccup/html
	`[html
	  [head [script (("src" . "https://cdn.tailwindcss.com"))]]
	  [body.bg-gray-100.p-8 [h1.text-3xl.font-bold.text-blue-600 "Hello, Tailwind!"]]])))
```

### htmx

Add htmx for dynamic interactions:

```scheme
(get "/htmx-demo"
     (lambda (req params)
       (ccup/html
	`[html
	  [head [script (("src" . "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js"))]]
	  [body
	   [button (("hx-get" . "/clicked") ("hx-target" . "#result")) "Click me!"]
	   [\#result]]])))

(get "/clicked"
     (lambda (req params)
       (ccup/html `[p "Button was clicked!"])))
```

## Current Status

**This is a toy project!** Schematra is still in early development and should not be used for production applications. It's missing many features you'd expect from a mature web framework:

- No session management
- No middleware system
- Limited error handling
- No template engine (but you can use the included chiccup, a [hiccup](https://github.com/weavejester/hiccup)-inspired template system)
- No database integration (but you can use any [database egg](https://eggs.call-cc.org/5/#db))
- No background job system
- No security features

However, it's perfect for:
- Learning Scheme
- Prototyping simple web applications
- Experimenting with htmx and Tailwind CSS
- Understanding how web frameworks work under the hood

## Contributing

If you find Schematra interesting and want to help it grow beyond a toy project, contributions are welcome! Feel free to:

- Report bugs or suggest features via GitHub issues
- Submit pull requests with improvements
- Share your experience using Schematra
- Help improve the documentation

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See the source code for the complete license text.

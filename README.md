# Schematra

A minimal web framework for CHICKEN Scheme, inspired by Sinatra. Schematra is currently a toy project created for learning purposes, but hopefully it will grow into something more useful over time.

## Why Schematra?

I created Schematra because I wanted to:

- **Learn Scheme better**: Building a web framework is a great way to explore a language's capabilities and idioms
- **Create something simple**: Most web frameworks are complex beasts. Schematra aims to be minimal and understandable
- **Enable modern web development**: The framework is designed to work well with modern tools like [Tailwind CSS](https://tailwindcss.com/) and [htmx](https://htmx.org/), making it easy to build interactive web applications without heavy JavaScript frameworks

## Features

- Simple route definition with `get` and `post` macros
- URL parameter extraction (e.g., `/users/:id`)
- Request body parsing
- Development mode with REPL integration
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
csi -s app.scm
```

Visit `http://localhost:8080` to see your application running.

## Development Mode

For interactive development, start the server in development mode:

```scheme
(schematra-start development?: #t port: 8080 repl-port: 1234)
```

This starts the web server in a background thread and opens an nREPL on port 1234. You can connect with your favorite Scheme editor or use `telnet localhost 1234` for a simple REPL session.

## Route Parameters

Schematra supports URL parameters using the `:parameter` syntax:

```scheme
(get "/users/:user-id/posts/:post-id"
     (lambda (req params)
       (let ((user-id (alist-ref "user-id" params equal?))
             (post-id (alist-ref "post-id" params equal?)))
         (format "User: ~A, Post: ~A" user-id post-id))))
```

The `params` argument contains both URL parameters (with string keys) and query parameters (with symbol keys).

## Working with Modern Web Tools

Schematra plays nicely with modern web development tools:

### Tailwind CSS

Include Tailwind via CDN in your HTML responses:

```scheme
(get "/" 
     (lambda (req params)
       "<html>
         <head>
           <script src=\"https://cdn.tailwindcss.com\"></script>
         </head>
         <body class=\"bg-gray-100 p-8\">
           <h1 class=\"text-3xl font-bold text-blue-600\">Hello, Tailwind!</h1>
         </body>
       </html>"))
```

### htmx

Add htmx for dynamic interactions:

```scheme
(get "/" 
     (lambda (req params)
       "<html>
         <head>
           <script src=\"https://unpkg.com/htmx.org@1.9.10\"></script>
         </head>
         <body>
           <button hx-get=\"/clicked\" hx-target=\"#result\">Click me!</button>
           <div id=\"result\"></div>
         </body>
       </html>"))

(get "/clicked"
     (lambda (req params)
       "<p>Button was clicked!</p>"))
```

## Current Status

⚠️ **This is a toy project!** Schematra is still in early development and should not be used for production applications. It's missing many features you'd expect from a mature web framework:

- No session management
- No middleware system
- Limited error handling
- No template engine (but you can use the included chiccup, a hiccup-like template system)
- No database integration
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

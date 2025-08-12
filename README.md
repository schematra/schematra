# Schematra

<img src="public/logo.png" alt="Schematra Logo" width="600px"/>

*A Sinatra love letter in Scheme*

A minimal web framework for [CHICKEN Scheme](https://call-cc.org/), inspired by [Sinatra](https://sinatrarb.com/). Schematra combines the elegance of Scheme with modern web development patterns.

## Why Schematra?

- **Functional by Design**: Built for developers who appreciate functional programming principles
- **Minimal & Understandable**: Simple enough to understand completely, powerful enough to build real applications
- **Chiccup Templates**: Write HTML with Lisp syntax - components are just functions
- **Modern Web Ready**: Works seamlessly with [Tailwind CSS](https://tailwindcss.com/) and [htmx](https://htmx.org/)

## Features

- Simple route definition with `get`, `post`, etc. functions
- URL parameter extraction and query parameter handling
- Middleware system with built-in session management
- Development mode with REPL integration
- [Hiccup](https://github.com/weavejester/hiccup)-inspired template system (Chiccup)
- Built on the solid [Spiffy](http://wiki.call-cc.org/eggref/5/spiffy) web server

## Quick Start

### Installation

```bash
git clone https://github.com/rolandoam/schematra
cd schematra
chicken-install
```

### Hello World

```scheme
(import schematra chiccup)

(get ("/" params) 
     (ccup/html `[html [body [h1 "Hello, Schematra!"]]]))

(schematra-install)
(schematra-start)
```

Save as `app.scm`, run with `csi app.scm`, and visit `http://localhost:8080`.

## Documentation

For comprehensive documentation including:

- **[Getting Started Guide](docs/docs.md#getting-started)** - Installation, system requirements, and your first app
- **[Core Concepts](docs/docs.md#core-concepts)** - Routing, parameters, and request handling
- **[Chiccup Templating](docs/docs.md#chiccup-templating)** - HTML generation with Lisp syntax
- **[API Reference](docs/docs.md#api-reference)** - Complete function documentation
- **[Advanced Topics](docs/docs.md#advanced-topics)** - Middleware, sessions, static files, and deployment
- **[Examples & Recipes](docs/docs.md#examples--recipes)** - Common patterns and use cases

Visit the **[full documentation](docs/docs.md)**.

## Current Status

**This is an exploration project!** Schematra is in early development and not recommended for production use. It's perfect for:

- Learning Scheme and web development concepts
- Prototyping web applications
- Experimenting with functional web programming
- Understanding how web frameworks work

### What's Working
- ✅ Routing and parameter handling
- ✅ Middleware system
- ✅ Session management
- ✅ Chiccup HTML templating
- ✅ Development mode with REPL
- ✅ Static file serving

### What's Coming
- [ ] Enhanced error handling
- [ ] Database integration helpers
- [ ] WebSocket support
- [ ] Background job system
- [ ] Production deployment guides

## Contributing

Contributions welcome! See our [contributing guidelines](docs/docs.md#contributing) for:

- Reporting bugs and requesting features
- Code style and development setup
- Roadmap and planned features

## License

GNU General Public License v3.0 - see [LICENSE.md](LICENSE.md) for details.

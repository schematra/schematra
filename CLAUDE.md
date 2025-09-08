# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Schematra is a modern web framework for CHICKEN Scheme inspired by Sinatra. It's a mono-repo containing multiple eggs (CHICKEN Scheme packages) that work together to provide a complete web development experience.

### Architecture Components

- **Core Framework** (`eggs/schematra/`): Main web framework with routing, middleware, request handling
- **HTML Generation** (`eggs/chiccup/`): S-expression-based HTML templating with CSS selector syntax
- **Session Management** (`eggs/schematra-session/`): Cookie-based session middleware
- **CSRF Protection** (`eggs/schematra-csrf/`): Cross-site request forgery protection
- **OAuth2 Support** (`eggs/oauthtoothy/`): OAuth2 authentication integration
- **Body Parser** (`eggs/schematra/schematra-body-parser.scm`): Request body parsing middleware

## Development Commands

### Building and Installation
```bash
# Install dependencies (external packages not in this repo)
chicken-install $(cat deps.txt)

# Install individual eggs locally
cd eggs/chiccup && chicken-install && cd ../..
cd eggs/schematra && chicken-install && cd ../..
cd eggs/schematra-session && chicken-install && cd ../..
cd eggs/schematra-csrf && chicken-install && cd ../..
cd eggs/oauthtoothy && chicken-install && cd ../..

# Compile the main web application
csc -O2 -d0 schematra-web.scm

# Run the compiled application
./schematra-web
```

### Testing
```bash
# Run chiccup tests
csi -s tests/chiccup_tests.scm
```

### Release Management
```bash
# Release specific eggs with versions
./release.scm "schematra:1.2.3,chiccup:0.5.0"

# Release single egg
./release.scm "schematra:1.2.3"
```

### Docker
```bash
# Build Docker image
docker build -t schematra .

# Run in container
docker run -p 8080:8080 schematra
```

## Key File Structure

- `schematra-web.scm`: Main web application demonstrating the framework
- `eggs/*/`: Individual CHICKEN Scheme packages (eggs)
- `examples/`: Sample applications (2048 game, task board, OAuth demo, SSE demo)
- `docs/docs.md`: Comprehensive framework documentation
- `release.scm`: Multi-egg release automation script
- `parse-deps.scm`: Dependency parsing utility
- `deps.txt`: External dependencies list

## Development Patterns

### Route Definition
Routes are defined using verb functions with Sinatra-style paths:
```scheme
(get ("/") "Hello World")
(post ("/users/:id") (lambda () (handle-user-update)))
```

### HTML Generation with Chiccup
Use S-expressions with CSS selector syntax:
```scheme
(ccup->html `[div.container 
              [h1#title "Hello"]
              [p.text "World"]])
```

### Middleware Usage
Middleware is composable and function-based:
```scheme
(use-middleware! (session-middleware "secret-key"))
(use-middleware! (csrf-middleware))
```

## Testing Strategy

- Unit tests exist for chiccup HTML generation
- Test files use custom lightweight test framework (not external dependency)
- Tests validate HTML output, CSS selectors, escaping, and boolean attributes

## Release Process

This is a mono-repo with versioned eggs. The release script handles:
- Git tagging for each egg version
- Updating release-info files
- Creating release commits
- Multi-egg releases in single command

## Dependencies

External dependencies are managed in `deps.txt` and include:
- Core CHICKEN modules (srfi-*, chicken.*)
- Web-related: spiffy, intarweb, uri-common
- Utilities: medea (JSON), openssl, http-client
- Cryptography: message-digest, hmac, sha2, base64
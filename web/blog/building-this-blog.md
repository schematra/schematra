<img class="mx-auto" data-ai-model="imagefx" data-ai-generated="2025-10-08" data-ai-prompt="open storybook on wooden desk, left page shows colorful fairy tale illustration of web server castle, right page displays actual Scheme programming code with parentheses and define statements, visible text reading (define web-framework) and lambda expressions, lisp syntax with nested parentheses clearly visible, book title on cover says Scheme Web Frameworks, programming textbook meets children's book aesthetic, soft warm lighting, educational illustration style, code snippets legible and prominent" style="width: 80%" src="https://s3-us-west-1.amazonaws.com/assets.schematra.com/public/images/story-book-1.jpg" alt="Storybook castle" title="Storybook castle - AI Generated, look at the source for details">

<p class="text-xs text-center">(An imaginary castle where all scheme dreams come true - generated with ImageFX<sub><a href="#fn-1">1</a></sub> on Oct 8 2025)</p>

# Building This Blog: A Schematra Story

Welcome to the Schematra blog! It seems that the hello world of web
frameworks is to build a blog "engine". So, this first post is about
that. I'm going to talk about how I built this blog using the very
framework it's about. All-in-all I think it took me about 15 minutes,
(maybe a bit more because I had to figure out a way to get lowdown to
work with chiccup). It probably took me longer to write this post.

In the future I expect to discuss why some decisions were made and try
to come up with more real-world examples, like the ORM we're building
and the async job system. My vision for Schematra is to become a
lightweight "(some) batteries included" web app framework.

## The Requirements

I wanted a simple blog for release announcements and tutorials. The
constraints were clear:

- **No database**: Just markdown files that can be committed to git. I
  want to keep it easy to deploy. Plus: super easy for other folks to
  contribute, just send me a PR.
- **Fast**: (ish). Cache everything, read files only once. Naive cache
  for now, should be good to ~100 entries. I can improve later.
- **Simple**: The entire blog should not add much to the current
  landing page (ideally just a couple of extra functions).
- **Easy to create content**: Markdown seems to be a good option. I'm
  a big fan of org-mode, but in this case I think it might be an
  overkill. Plus, markdown is way more popular than org-mode.

## The Stack

The blog uses three key pieces:

- **Schematra**: For routing, obviously (`/blog` and `/blog/:slug`
  were added to the existing app)
- **Chiccup**: For HTML generation, because it's based on sxml, it
  works great with:
- **[Lowdown](https://wiki.call-cc.org/eggref/5/lowdown)**: For
  parsing markdown to SXML.

## The Architecture

### Post Index as Data

One of the benefits of using scheme is that data is part of the
language in a very natural way. So instead of deciding what format to
use to store the post directory/index, we can use a simple list of
[association
lists](https://en.wikipedia.org/wiki/Association_list). Having the
content stored like that, parsing is trivial: just use `read`.

Blog posts are defined in `blog/posts.scm`:

<pre><code class="language-scheme">;; Blog posts index
;; Each post is an alist with metadata
;; The 'file' field should point to a markdown file in the web/blog/ directory
(
 ((slug . "welcome-to-schematra")
  (title . "Building This Blog: A Schematra Story")
  (date . "2025-10-08")
  (author . "Rolando Abarca")
  (excerpt . "How I built this blog using...")
  (tags . ("tutorial" "meta"))
  (file . "welcome-to-schematra.md"))
)
</code></pre>

### Caching Strategy

The blog uses two caches to avoid filesystem I/O on every request:

<pre><code class="language-scheme">;; Cache blog posts list
(define *blog-posts-cache* #f)

;; Cache parsed markdown (filepath -> SXML)
(define *markdown-cache* '())
</code></pre>

Posts are loaded once at startup. Markdown files are parsed on first access and cached as normalized SXML.

### Getting markdown to work

Lowdown's SXML output wasn't directly compatible with Chiccup - it
returns some nodes with text children embedded in a list. This is a
bit problematic but it might actually need a change in chiccup in the
future. For now, we can quickly normalize it:

<pre><code class="language-scheme">(define (normalize-sxml node)
  (cond
   ((and (list? node) (every string? node))
    (string-concatenate node))
   ((and (list? node) (symbol? (car node)))
    (cons (car node) (map normalize-sxml (cdr node))))
   (else node)))
</code></pre>

### Code Blocks

[GFM](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)
triple-backticks aren't supported by lowdown, so we use inline HTML:

    <pre><code class="language-scheme">
    (your code here)
    </code></pre>

Highlight.js picks up the `language-*` class and does the syntax highlighting.

## The Routes

Two routes, clean and simple:

<pre><code class="language-scheme">(get "/blog"
  (ccup->html (layout (blog-list-page))))

(get "/blog/:slug"
  (let ((slug (alist-ref "slug" (current-params) equal?)))
    (ccup->html (layout (blog-post-page slug)))))
</code></pre>

## What I Learned

Building this blog reinforced what I love about Schematra:

- **Data is code, code is data**: The post index is just Scheme data you can manipulate
- **Composition & leveraging the ecosystem**: SXML from lowdown → Chiccup → HTML
- **No magic**: The entire implementation is readable in minutes

## Coming Soon

I'll be posting updates about new features, release notes, and
tutorials on building real applications with Schematra.

---

*This blog was built in an afternoon. The code is in the [schematra
repo](https://github.com/schematra/schematra) under
`/web`.*

<p id="fn-1" class="text-xs"><sup>1</sup>: I'm a believer in AI tools and use them frequently, but I hate seeing AI-generated slop trying to trick people. When I use AI-generated images in this blog, I'll always mention it and include metadata in the img tag (prompt, model, generation date). You won't get the exact same image with the same prompt, but at least you'll know the intention behind it.</p>

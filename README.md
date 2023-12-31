# WARNING
The docs are currently out of date, but things are changing quickly. Once development settles down, 
the docs will be updated.

# What is Kobayashi?

Kobayashi is a tool for generating static websites! It takes `.kby` files and transforms them into `HTML`.

# What is a .kby file?

A `.kby` file is a custom markup file format designed to design websites with Kobayashi. The spec is as follows:
* `@text` --> header containing "text"
* `@@text` --> subheader containing "text"
* `*text*` --> bold text
* `/text/` --> italic text
* `[text|url]` --> hyperlink "text" that redirects to `url`
* `<./path/to/image.jpeg>` --> image
* `-` --> begins a bulleted list item.

Feel free to nest bold and italic text, as well as include them in other elements which have text content (e.g. headers, links).

Paragraphs are made up of lines of text. The "@" and "-" character
can appear in paragraphs no problem, but "\*", "/" and "\\" will need to be escaped by prefixing them with a "\\".

To end a header, subheader, image, list, or paragraph use two newlines. Single newlines will be
treated as spaces.

For images, you can specify a path to a local image relative to the directory (i.e. starting with `./` that the html page will live in.
Remote images can be specified using a url. The current file types that Kobayashi can handle are: `JPEG` (or `JPG`) and `PNG`.

Bulleted lists are made up of lines beginning with a `-` and ended by a single newline for each item.

### Example
Here is a simple example of a `.kby` file.

```
@Hello, world!

@@This is a /subheader/.

Mai *best girl*.
/Kuroneko is *brilliant*/.

I do not take criticism.

- Check out
- my cool
- bulleted list!
```

# Okay, but how do I use Kobayashi?
Kobayashi is still in the early stages of development, and is unstable. If you want to try her out, you can build the
source code using [Cabal](https://www.haskell.org/cabal/).

```
cabal install
```

You can then build individual `.kby` files with. 

```
kobayashi build /path/to/file.kby
```

The `HTML` file will be output in the directory where the command was run. The file will will have the same name 
as the `.kby` file that was input. You can specify a custom output directory using the `-odir` option.

There is currently no support for batch builds, but there will be by the first release.

If you are ever stuck, list out all possible commands and options by using 

```
kobayashi help
```

# What's next?

Here is my current list of to-dos before the first release:

Definite includes:
* Config file
* Better docs (GitHub wiki?? Page on my site??).

Nice to have:
* Individual file builds.
* Macros that expand to preformatted HTML.
* Embedded `HTML` (kinda goes with the macros).
* Comments.
* Math.
* BNF grammar for kby spec.
* Templates (w/ params too)
* Check if files/images linked to exist.
* clean command

Some other ideas for after a stable release:
* Test suite
* Look into RSS feeds for site.
* WYSIWYG editor that kobayashi can also build from (gui makes things in IR)

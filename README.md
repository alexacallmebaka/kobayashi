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

Feel free to nest bold and italic text, as well as include them in other elements which have text content (e.g. headers, links).

Paragraphs are made up of lines of text. The "@" character
can appear in paragraphs no problem, but "\*", "/" and "\\" will need to be escaped by prefixing them with a "\\".

To end a header, subheader, image, or paragraph use two newlines. Single newlines will be
treated as spaces.

For images, you can specify a path to a local image relative to the directory (i.e. starting with `./` that the html page will live in.
Remote images can be specified using a url. The current file types that Kobayashi can handle are: `JPEG` (or `JPG`) and `PNG`.

### Example
Here is a simple example of a `.kby` file.

```
@Hello, world!

@@This is a /subheader/.

Mai *best girl*.
/Kuroneko is *brilliant*/.

I do not take criticism.
```

# Okay, but how do I use Kobayashi?
Kobayashi is still in the early stages of development, and is unstable. If you want to try her out, you can build the
source code using [Cabal](https://www.haskell.org/cabal/).

```
cabal build
```


You can then build individual `.kby` files with. 

```
cabal run -- kobayashi build /path/to/file.kby
```

The `HTML` file will be output in the directory where the command was run. The file will will have the same name 
as the `.kby` file that was input. You can specify a custom output directory using the `-odir` option.

There is currently no support for batch builds, but there will be by the first release.

If you are ever stuck, list out all possible commands and options by using 

```
cabal run -- kobayashi help
```

# What's next?

Here is my current list of to-dos before the first release:

* Batch builds.
* Code blocks/verbatim.
* Underline.
* Strikethrough.
* Lists.
* Embedded `HTML` (kinda goes with the macros).
* Navbar/macros.
* Config file?
* Add CSS.
* Pretty `HTML` formatting.
* Comments.
* Math.
* BNF grammar for kby spec.
* templates
* Check if files/images linked to exist.

Some other ideas for after a stable release:
* Look into RSS feeds for site.
* WYSIWYG editor that kobayashi can also build from. 

# What is Kobayashi?

Kobayashi is a tool for generating static websites! She takes `.kby` files and transforms them into `HTML`.

# What is a .kby file?

A `.kby` file is a custom markup file format designed to design websites with Kobayashi. The spec is as follows:
* @text --> header containing "text"
* @@text --> subheader containing "text"
* \*text\* --> bold text
* /text/ --> italic text

Feel free to nest bold and italic text, as well as include them in other elements which have text content (e.g. headers).

Paragraphs are made up of lines of text. To end a paragraph, use two newlines after the final sentence. The "@" character
can appear in paragraphs no problem, but "\*", "/" and "\\" will need to be escaped by prefixing them with a "\\".

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
source code using the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/).

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
* Images.
* Links.
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
* CFG for kby spec.
* templates

Some other ideas for after a stable release:
* Integration with docker (i.e. auto-builds).
* Look into RSS feeds for site.
* WYSIWYG editor that she can also build from. 

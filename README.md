# CamlCrawler

CamlCrawler is an interactive video game engine made in OCaml.

It is a side-project I did to train myself to make a small application in pure functional style.  
No OOP has been used and all libraries used also follow a "functional"/"pure OCaml" style.

## Modules

The story is written in a YAML file, parsed by the `Story_parser` module and passed to the `Game` module which itself manages the game using the `Terminal` module: chapter to display, choices availables...

The `Terminal` module is a small, standalone library that displays a `Notty` image ("content") in the top part of the terminal, and displays choices in the lower part.  
Choices can be selected using arrows and enter keys.  
The content may be bigger than the terminal height. In this case, the `Terminal` will allow the user to scroll using the mouse wheel, or display one page after another using the page up/down keys.

To facilitate the creation of a `Notty` image, a `Notty_markdown` library is also included. It is a really simple one which converts a Markdown text to a `Notty` terminal-compatible image, with properly styled text.  
What styling is supported:
- bold text
- italic text
- line breaks (soft and hard)
- paragraphs (with automatic line wrapping)
- headings (bold text with spacing before and after but same size as the rest)
Any other Markdown styling (links, code blocks...) will raise a failure.

## Libraries

- [`Notty`](https://github.com/pqwy/notty/) for interactive terminal features
- [`omd`](https://github.com/ocaml/omd) for Markdown parsing
- [`ocaml-yaml`](https://github.com/avsm/ocaml-yaml) for YAML parsing
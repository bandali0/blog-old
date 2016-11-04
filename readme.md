Blog
====

This is the source code for [my personal site][aminb]. It is based on
a static site generator written by [Ruud van Asseldonk][ruudva] in Haskell.

[![Build Status][ci-img]][ci]

> "The generator includes a tiny templating engine, an html and css
> minifier, and an aggressive font subsetter. One of my objectives was
> to cut all the crap (which almost by definition includes javascript)
> without compromising on design. An average page of my site weighs
> less than jQuery alone (which jokingly describes itself as
> “lightweight footprint”). That includes webfonts."
> -- <cite>Ruud van Asseldonk</cite>

Previously I used [Jekyll][jekyll] but have since migrated to this
setup, a small homemade static site generator.

[aminb]: https://aminb.org
[ruudva]: https://ruudvanasseldonk.com
[ci-img]: https://travis-ci.org/aminb/blog.svg
[ci]:     https://travis-ci.org/aminb/blog
[jekyll]: http://jekyllrb.com/

License
-------
The source code for this site is licensed under version 3 of the the
[GNU General Public Licence][gplv3]. See the `licence` file. The content of the
posts is licensed under the [Creative Commons BY SA][cc] licence. For the font
license details, see the readme in the fonts directory.

[gplv3]: https://gnu.org/licenses/gpl.html
[cc]:    https://creativecommons.org/licenses/by-sa/4.0/

Compiling
---------
Build the generator, then build the site (requires fonts to be present):

    $ stack build
    $ stack exec blog

Or compile with good old Cabal:

    $ cabal update
    $ cabal sandbox init
    $ cabal install -j
    $ cabal run

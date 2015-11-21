## The Programming Language Zoo in Idris

My idea here is to take the really neat set of languages by Andrej Bauer
@andrejbauer found on [The Programming Language Zoo](http://andrej.com/plzoo/),
which are implemented in OCaml, and try reimplementing them in Idris.

I am using [Lightyear](https://github.com/ziman/lightyear) as a parser
combinator library. Since Idris does not yet have a package management system,
you will need to do a local install to use it. To do this, download the source
from GitHub. Then, if you are on Linux and have make, run:

    make install

Otherwise, you can just do

    idris –-install lightyear.ipkg

which is just what the Makefile is doing anyway.

Also I have found David Christiansen's [Idris Code
Highlighter](https://github.com/david-christiansen/idris-code-highlighter/blob/master/src/Highlight/Parser.idr)
to be a very instructive example of using Lightyear.

Also Scott Wlaschin @swlaschin, as always a brilliant pedagogue, has a series
on [parser
combinators](http://fsharpforfunandprofit.com/posts/understanding-parser-combinators/#series-toc)
that has been enormously helpful to me. And thanks to Craig Stuntz @craigstuntz
for pointing me to the original Programming Language zoo.

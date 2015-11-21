## The Programming Language Zoo in Idris

My idea here is to take the really neat set of languages found on [The
Programming Language Zoo](http://andrej.com/plzoo/), which are implemented in
OCaml, and try reimplementing them in Idris.

I am using [Lightyear](https://github.com/ziman/lightyear) as a parser
combinator library. Since Idris does not yet have a package management system,
you will need to do a local install to use it. To do this, download the source
from GitHub. Then, if you are on Linux and have make, run:

    make install

Otherwise, you can just do

    idris –-install lightyear.ipkg

which is just what the Makefile is doing anyway.

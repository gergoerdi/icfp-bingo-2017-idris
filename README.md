ICFP 2017 Bingo (Idris edition)
===============================

I wrote the original implementation of the [ICFP 2017 Bingo][bingo] in
Haskell using GHCJS and Reflex-DOM; however, the generated JavaScript
weighed in at 2 MB, which is pretty bad considering how trivial this
single-page webapp is.

My friend and ex-coworker [Encsé][encse] mentioned I should give Idris a
try, so I did. The resulting JavaScript is 150 kB, much better.

Dependencies:
-------------

* [`idrisjs`][idrisjs] for interfacing with the DOM
* `effects` for random number generation
* `contrib` for ST as used by `idrisjs`

[Live demo][demo]
=================

[bingo]:   https://gergo.erdi.hu/projects/icfp-bingo-2017
[encse]:   https://csokavar.hu
[idrisjs]: https://github.com/rbarreiro/idrisjs
[demo]:    https://gergo.erdi.hu/projects/icfp-bingo-2017/idr/

-- | Western twelve-tone scale.
module Temporal.Music.Western.P12(
    module Temporal.Music.Western,
    tone,
    -- * Steps
    -- | Step defines a value of type score (hence @Track@), so we can 
    -- transform them on the fly:
    --
    -- > qn $ line [forte c, d, e, low b, forte $ bn d]
    c, d, e, f, g, a, b, 
    cs, ds, es, fs, gs, as, bs,
    cf, df, ef, ff, gf, af, bf,

    -- * Scales
    eqt, pyth, 
    hind, hindFs, hindGb

    -- * Examples
    -- | Little example of usage:
    --
    -- > import Temporal.Music.Western.P12
    -- >
    -- > res :: Score ()
    -- > res = stretch 0.2 $ chord [
    -- >            qn $ line [c, c, bn g, e, dbn f],
    -- >            low $ line [c, c, d, low a]]
)
where

import Temporal.Music(Score, temp, note, Step)
import Temporal.Music.Scales(eqt, pyth, hind, hindFs, hindGb)

import Temporal.Music.Western

-- | Constructs 'Score' with one note. 'Scale' is set to equal
-- tempered scale (from 'c1').
tone :: Step -> Score (Note a)
tone = note 

c, d, e, f, g, a, b, 
    cs, ds, es, fs, gs, as, bs,
    cf, df, ef, ff, gf, af, bf :: Score (Note a)

c    = tone 0
cs   = tone 1
d    = tone 2
ds   = tone 3
e    = tone 4
f    = tone 5
fs   = tone 6
g    = tone 7
gs   = tone 8
a    = tone 9
as   = tone 10
b    = tone 11

bs   = c
es   = f

cf   = b
df   = cs
ef   = ds
ff   = e
gf   = fs
af   = gs
bf   = as


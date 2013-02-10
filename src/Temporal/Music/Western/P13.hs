-- | Bohlen-Pierce tones
module Temporal.Music.Western.P13(
        module Temporal.Music.Western,
        tone,
        -- * Tones
        c, cs, cf, d, ds, df, e, es, ef,
        f, fs, ff, g, gs, gf, h, hs, hf, 
        j, js, jf, a, as, af, b, bs, bf,

        -- * Scales
        eqBP, justBP
)    
where

import Data.Default

import Temporal.Music(Score, temp, setScale, c1, note)
import Temporal.Music.Western
import Temporal.Music.Scales(eqBP, justBP)

c, cs, cf, d, ds, df, e, es, ef,
    f, fs, ff, g, gs, gf, h, hs, hf, 
    j, js, jf, a, as, af, b, bs, bf :: Default a => Score (Note a)

-- | Constructs 'Score' with one note. 'Scale' is set to equal
-- tempered Bohlen-Pierce scale (from 'c1').
tone :: Default a => Step -> Score (Note a)
tone = setScale (eqBP c1) . note 

c  = tone 0
cs = tone 1 
d  = tone 2 
e  = tone 3
f  = tone 4
fs = tone 5
g  = tone 6
h  = tone 7
hs = tone 8
j  = tone 9
a  = tone 10
as = tone 11
b  = tone 12

cf = b
ds = e
df = cs
es = f
ef = d
ff = e
gs = h
gf = fs
hf = g
js = a
jf = hs
af = j

bf = as
bs = c 

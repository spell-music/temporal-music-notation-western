-- | Western twelve-tone scale.
module Temporal.Music.Western.P12(
    module Temporal.Music,
    module Temporal.Music.Western.Dynamics,
    Chromatic(..), P12, Score12, Note12,
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
    -- > res :: Score12 ()
    -- > res = stretch 0.2 $ chord [
    -- >            qn $ line [c, c, bn g, e, dbn f],
    -- >            low $ line [c, c, d, low a]]
)
where

import Data.Finite
import Temporal.Music
import Temporal.Music.Scales(eqt, pyth, hind, hindFs, hindGb)

import Temporal.Music.Western.Dynamics

-- twelve tone
type P12 = Chromatic 

-- | Chromatic twelve tone scale. Default scale is equal temperament.
data Chromatic = 
      C  | Cs | D  | Ds | E  | F 
    | Fs | G  | Gs | A  | As | B
    deriving (Enum, Bounded, Eq, Show)

instance Finite Chromatic

instance HasScale Chromatic where
    defScale = const $ eqt c1

type Score12 a = Score (Note12 a)

type Note12 a = Note Dynamics Chromatic a


chrom :: P12 -> Score12 a
chrom = temp . note MPiano 

c, d, e, f, g, a, b, 
    cs, ds, es, fs, gs, as, bs,
    cf, df, ef, ff, gf, af, bf :: Score12 a

c    = chrom C
cs   = chrom Cs
d    = chrom D
ds   = chrom Ds
e    = chrom E
f    = chrom F
fs   = chrom Fs
g    = chrom G
gs   = chrom Gs
a    = chrom A
as   = chrom As
b    = chrom B

bs   = c
es   = f

cf   = b
df   = cs
ef   = ds
ff   = e
gf   = fs
af   = gs
bf   = as


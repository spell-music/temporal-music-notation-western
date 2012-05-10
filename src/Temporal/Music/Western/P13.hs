-- | Bohlen-Pierce tones
module Temporal.Music.Western.P13(
        module Temporal.Music,
        module Temporal.Music.Western.Dynamics,
        -- * Tones
        BohlenPierce(..), P13, Note13, Score13,

        c, cs, cf, d, ds, df, e, es, ef,
        f, fs, ff, g, gs, gf, h, hs, hf, 
        j, js, jf, a, as, af, b, bs, bf,

        -- * Scales
        eqBP, justBP
)    
where

import Data.Finite
import Temporal.Music
import Temporal.Music.Western.Dynamics
import Temporal.Music.Scales(eqBP, justBP)

type P13 = BohlenPierce

-- | Tones for Bohlen-Pierce 13-tone scale. 
-- Default scale defined in 'Pch' class is equal 
-- temperament Bohlen-Pierce scale
data BohlenPierce = 
      C  | Cs | D  | E  | F  | Fs | G 
    | H  | Hs | J  | A  | As | B
    deriving (Enum, Bounded, Eq, Show)

instance Finite BohlenPierce

instance HasScale BohlenPierce where
    defScale = const $ eqBP c1

c, cs, cf, d, ds, df, e, es, ef,
    f, fs, ff, g, gs, gf, h, hs, hf, 
    j, js, jf, a, as, af, b, bs, bf :: Score13 a


type Note13 a   = Note Dynamics BohlenPierce a
type Score13 a  = Score (Note13 a)

tone :: P13 -> Score13 a
tone = temp . note MPiano 

c  = tone C  
cs = tone Cs 
d  = tone D 
e  = tone E
f  = tone F
fs = tone Fs
g  = tone G
h  = tone H
hs = tone Hs
j  = tone J
a  = tone A
as = tone As
b  = tone B

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

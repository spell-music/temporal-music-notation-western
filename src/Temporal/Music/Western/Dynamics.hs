module Temporal.Music.Western.Dynamics (
    Dynamics(..), 
    -- * Levels
    pppiano, ppiano, piano, mpiano, mforte, forte, fforte, ffforte,
    -- * Shortcuts to set dynamics level.
    ppp', pp', p', mp', mf', f', ff', fff')
where

import Data.Finite
import Temporal.Music

-- | Traditional volume levels
data Dynamics =   PPPiano | PPiano | Piano 
                | MPiano  | MForte 
                | Forte   | FForte | FFForte
    deriving (Show, Eq, Enum, Bounded)


instance Finite Dynamics

instance HasDiap Dynamics where
    defDiap = const $ (1e-5, 1)


pppiano, ppiano, piano, mpiano, mforte, forte, fforte, ffforte :: 
    Score (Note Dynamics pch a) -> Score (Note Dynamics pch a)


pppiano = setLevel PPPiano
ppiano = setLevel PPiano
piano = setLevel Piano

mpiano = setLevel MPiano
mforte = setLevel MForte

forte = setLevel Forte
fforte = setLevel FForte
ffforte = setLevel FFForte

ppp', pp', p', mp', mf', f', ff', fff' :: 
    Score (Note Dynamics pch a) -> Score (Note Dynamics pch a)

ppp' = pppiano
pp' = ppiano
p' = piano
mp' = mpiano
mf' = mforte
f' = forte
ff' = fforte
fff' = ffforte


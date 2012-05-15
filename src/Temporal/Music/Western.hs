{-# Language FlexibleContexts #-}

-- | Names specific to western music tradition
module Temporal.Music.Western(
    -- * Basic functions
    -- | All basic functions (composition, time stretching, 
    -- dynamic changing of volume and pitch etc, etc) live in this module. 
    module Temporal.Music,
    -- * Volume
    -- | Dynamics values form 9-level equally spaced grid. 
    -- They are from quietest
    -- to loudest: piano pianissimo (ppp), pianissimo (pp), piano (p),
    -- mezzo piano (mp), mezzo forte (mf), forte (f), fortissimo (ff),
    -- forte fortissimo (fff).
    --  These modifiers change level relative to the current level.
    -- It means that
    --
    -- > piano . forte = id
    pppiano, ppiano, piano, mpiano, mforte, forte, fforte, ffforte,
    -- ** Shortcuts to set dynamics level.
    ppp', pp', p', mp', mf', f', ff', fff',

    -- ** Envelops
    dim, cresc, 

    -- * Score    
    rondo, reprise, 
    
    -- * Tempo
    -- | Tempo terms specify not a rigid value but tempo range. So all
    -- terms are functions from relative power of term (it's value
    -- of type Double from 0 to 1) to some tempo value. Zero means
    -- the lowest value from tempo range and one means the highest value.
    -- To be used with 'bpm' function.
    Tempo, 
	lento, largo, larghetto, grave, adagio, adagietto,
    andante, andantino, moderato, allegretto,
    allegro, vivace, presto, prestissimo 
) where

import Temporal.Music

pppiano, ppiano, piano, mpiano, mforte, forte, fforte, ffforte :: 
    (VolumeLike a) => Score a -> Score a

pppiano = quieter 4;    ppiano  = quieter 3;    piano   = quieter 2
mpiano  = quieter 1;    mforte  = louder 1;     forte   = louder 2;
fforte  = louder 3;     ffforte = louder 4;

ppp', pp', p', mp', mf', f', ff', fff' ::
    (VolumeLike a) => Score a -> Score a

ppp'    = pppiano;  pp'     = ppiano;   p'      = piano;
mp'     = mpiano;   mf'     = mforte;   f'      = forte;
ff'     = fforte;   fff'    = ffforte;

-- | diminuendo
dim :: VolumeLike a => Accent -> Score a -> Score a
dim = envelope . (*) . negate

-- | crescendo
cresc :: VolumeLike a => Accent -> Score a -> Score a
cresc = envelope . (*)

---------------------------------------
-- forms

-- | rondo form
--
-- >rondo a b c = line [a, b, a, c, a]
rondo :: Score a -> Score a -> Score a -> Score a
rondo a b c = line [a, b, a, c, a]

-- | reprise form
--
-- >reprise a b1 b2 = line [a, b1, a, b2]
reprise :: Score a -> Score a -> Score a -> Score a
reprise a b c = line [a, b, a, c]

---------------------------------------
-- tempo

type Tempo = Double

largoRange, larghettoRange, 
	adagioRange, adagiettoRange, 
    andanteRange, andantinoRange,
	moderatoRange, allegroRange,
    allegrettoRange, vivaceRange,
	prestoRange, prestissimoRange :: (Double, Double)

largoRange       = ( 40, 60) 
larghettoRange   = ( 60, 66) 
adagioRange      = ( 66, 76)
adagiettoRange   = ( 70, 80)
andanteRange     = ( 76, 80)
andantinoRange   = ( 80,100)
moderatoRange    = (101,110)
allegrettoRange  = (115,125)
allegroRange     = (120,139)
vivaceRange      = (135,160)
prestoRange      = (168,200)
prestissimoRange = (200,230)

getTempo :: (Tempo, Tempo) -> Double -> Tempo
getTempo (a, b) x = a + (b - a) * x

-- | very slow (40-60 bpm), like largo
lento :: Double -> Tempo
lento       = largo

-- | very slow (40-60 bpm)
largo :: Double -> Tempo
largo       = getTempo largoRange

-- | rather broadly (60-66 bpm)
larghetto :: Double -> Tempo
larghetto   = getTempo larghettoRange

-- | slow and sloemn (60 - 66 bpm)
grave :: Double -> Tempo
grave = larghetto

-- | slow and stately (literally "at ease") (66-76 bpm)
adagio :: Double -> Tempo
adagio      = getTempo adagioRange

-- | rather slow (70-80 bpm)
adagietto :: Double -> Tempo 
adagietto = getTempo adagiettoRange

-- | at awalking pace (76-80 bpm)
andante :: Double -> Tempo 
andante     = getTempo andanteRange

-- | slightly faster then andante (80-100 bpm)
andantino :: Double -> Tempo 
andantino     = getTempo andantinoRange

-- | moderately (101-110 bpm)
moderato :: Double -> Tempo
moderato    = getTempo moderatoRange

-- | moderately fast (115-125 bpm)
allegretto :: Double -> Tempo
allegretto     = getTempo allegrettoRange

-- | fast, at 'march tempo' (120-139 bpm)
allegro :: Double -> Tempo
allegro     = getTempo allegroRange

-- | lively and fast (135-160 bpm)
vivace :: Double -> Tempo
vivace = getTempo vivaceRange

-- | very fast (168-200 bpm)
presto :: Double -> Tempo
presto      = getTempo prestoRange

-- | extremely fast (200 - 230 bpm)
prestissimo :: Double -> Tempo
prestissimo = getTempo prestissimoRange



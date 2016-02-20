{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands = take 5 (rands(mkSeed 1))

rands s = fst(rand(s)) : rands(snd(rand(s)))

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
import Prelude
import Sound.Tidal.Context
import qualified Foreign.Store as ForeignStore
import Data.Word (Word32)
import Data.String (fromString)
import Control.Arrow ((>>>))

-- Runs computation once. Persists across ghci sessions.
runOnce :: Word32 -> IO a -> IO a
runOnce index computation = do
  result <- ForeignStore.lookupStore index
  case result of
    Nothing -> do
      x <- computation
      ForeignStore.writeStore (ForeignStore.Store index) x
      return x
    Just store -> ForeignStore.readStore (ForeignStore.Store index)

main :: IO ()
main = do
  (cps, d1,t1) <- runOnce 0 $ do
    (cps, getNow) <- bpsUtils
    (d1, t1) <- superDirtSetters getNow
    return (cps, d1, t1)
  cps (80/60)
  d1 $ prog silence 

add x y = overlay x y
addMod f x = add (f x) x
appendMod f x = append' (f x) x
catMod mods x = slowcat (fmap ($x) mods)
patMod mods x = tt (recip . fromIntegral $ length mods) (catMod mods) x
tt x f = (slow x) . f. (slow (1/x))

prog :: Pattern ParamMap -> Pattern ParamMap
prog = let (>>) = flip (.) in do
  addMod $ do
    const $ sound $ samples "dr55 ~ [dr55 dr55] dr55" "2"
    (# gain "0.8")
  jux $ ((2/4) ~>)
  ((2/4) ~>)
  -- catMod [ id , trunc 0.25 ]
  -- catMod [ trunc 0.25, id ]
  -- every 2 $ trunc 0.25 >>> (0.25 ~>)
  -- tt 1 $ catMod [ id, ((4/8) ~>) ] >>> catMod [ ((4/8) ~>), id ] -- cool
  addMod $ do
    const $ sound $ samples "808bd" "1"
    tt 2 $ catMod [slow (2/8), slow (3/8)]
    (# gain "0.9")
  whenmod 16 12 $ do
    (slow 2)
  catMod [id,  tt 1 (|*| speed "1 0 1 1")]
  -- tt 0.5 $ catMod [ id, trunc 0.5 ] >>> catMod [ trunc 0.5, id ] -- cool
  -- jux $ do
  --   catMod [ id, (0.5 ~>) ] >>> catMod [ id, (0.5 <~) ]
  --   catMod [ id, (0.5 ~>) ] >>> catMod [ id, (0.5 <~) ]

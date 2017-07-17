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

newtype Func a = Func {appFunc :: a -> a }
instance Monoid (Func a) where
  mempty = Func id
  mappend (Func f) (Func g) = Func (g . f)

main :: IO ()
main = do
  (cps, d1,t1) <- runOnce 0 $ do
    (cps, getNow) <- bpsUtils
    (d1, t1) <- superDirtSetters getNow
    return (cps, d1, t1)
  cps (65/60)
  d1 $ prog silence

add x y = stack [x, y]
addMod f x = add (f x) x
appendMod f x = append' (f x) x
catMod mods x = slowcat (fmap ($x) mods)
fastcatMod mods x = cat (fmap ($x) mods)

prog :: Pattern ParamMap -> Pattern ParamMap
prog = let (>>) = (>>>) in do
  add $ ($ silence) $ do
    const $ sound $ samples "tech*8" (run 8)
    (# cut "8")
    catMod
      [ (|*| speed "1")
      , (|*| speed "2")
      ]
    catMod
      [ striate' 1 (1/4)
      , striate' 1 1
      ]
    catMod
      [ (|*| speed "1 1")
      , (|*| speed "2 1")
      , do
        catMod
          [ (|*| speed "4 2")
          , (|*| speed "4 1")
          ]
      , (|*| speed "4 1")
      ]
    catMod
      [ (# vowel "a e")
      , (# vowel "e e e")
      ]
    (|*| speed "1 0.5")
    catMod [id, id]


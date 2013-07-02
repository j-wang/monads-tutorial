{--| Exercises from Monads Tutorial

http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
Exercises done by James Wang, 25 Jun 2013 (no guarantees they're right!)

--}

module Monads (
  f,
  g,
  bind',
  unit,
  lift
  ) where         

import Control.Exception


{--|

bind' :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))

bind' must serve two purposes: it must (1) apply f' to the correct part of g' x 
and (2) concatenate the string returned by g' with the string returned by f'.

Exercise One
Write the function bind. --}

f :: Float -> (Float, String)
f x = (x + 3.0, "f was called; ")

g :: Float -> (Float, String)
g x = (x / 2.0, "g was called; ")

bind' :: (Float -> (Float, String)) -> ((Float, String) -> (Float, String))
bind' f' (gx, gs) = let (fx, fs) = f' gx in (fx, gs ++ fs)

{--|

Given a pair of debuggable functions, f' and g', we can now compose them 
together to make a new debuggable function bind f' . g'. Write this 
composition as f'*g'. Even though the output of g' is incompatible with the 
input of f' we still have a nice easy way to concatenate their operations. And 
this suggests another question: is there an 'identity' debuggable function. 
The ordinary identity has these properties: f . id = f and id . f = f. So 
we're looking for a debuggable function, call it unit, such that unit * f = f 
* unit = f. Obviously we'd expect it to produce the empty debugging string and 
otherwise act a bit like the identity.

Exercise Two
Define unit.

--}

unit :: Float -> (Float, String)
unit x = (x, "")

-- f is a normal function. Basically, monadization of the normal function
lift :: (Float -> Float) -> (Float -> (Float, String))
lift f' x = (f' x, "")
-- lift f' = unit . f'

{--|

Exercise Three

Show that lift f * lift g = lift (f.g). Basically should be in the testing 
framework.

--}

-- See test framework

{--|

Exercise Four

bind :: (Complex Double -> [Complex Double]) -> ([Complex Double] -> [Complex 
Double])

--}

bind :: (Monad m) => m a -> (a -> m b) -> m b
bind x f = f x

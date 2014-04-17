module DataHand.Layouts.Games.DoTA2 where
import DataHand.Layout
import DataHand.Keys
import Prelude hiding (Left, Right)

ø = NoEvent

ab1 = Q
ab2 = W
ab3 = E
abU = R
ab4 = ø
ab5 = ø

it1 = Z
it2 = X
it3 = C
it4 = V
it5 = ø
it6 = ø

camU = Up
camD = Down
camL = Left
camR = Right

selH = F1
selO = ø
sel1 = One
sel2 = Two
sel3 = Three
sel4 = Four
sel5 = Five
sel6 = Six
sel7 = Seven
sel8 = Eight
sel9 = Nine
sel0 = Zero
selC = ø

shop = F4
dlvr = ø
buys = ø -- buy sticky item

evnt = Space

attk = A
move = M
stop = S
hold = H

-- notes: 
--   sel1 can be used in place of selH to control e.g. beastmaster + boar
--   shop mode uses qwer + 1-0, so add a layer just for that mode which includes shift + alt for queueing and announcing the items
--   is there a way to shop with keys without that "always" option on?
--   have layer for setting groups and selecting 3-0
-- no means to control camera here, but using headtracking may be the ideal solution to that, or using a thumbstick on the mouse.
-- consider inverting abil,item order at risk of making this extra confusing
-- maybe have right hand for quickshop?
-- shop mode could be capslock or norm lever while held up

dota2_layout = Layout {
    normal = Layer
--       +----+           +----+           +----+           +----+
           ab1              ab2              ab3              ab4
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      ø   sel2  ab4    ø   sel1  ab5    ø   hold  it6    ø   attk  it5    ø    ø    ø
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
           it1              it2              it3              it4         ø    NAS  ø  
--       +----+           +----+           +----+           +----+      +----+----+----+
    ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø ø
  , nas = EmptyLayer
  , function = EmptyLayer
  , tenk = EmptyLayer
  }


dota2_experimental_layout = Layout {
    normal = Layer
--       +----+           +----+           +----+           +----+
          camU              ab3              ab2              ab1
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     camL sel1 camR    it1 sel2  ab5    it2 hold  ab4   it3  attk  abU    Tab  ø    ø
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          camD              it4              it5              it6         ø    NAS  ø  
--       +----+           +----+           +----+           +----+      +----+----+----+

--                        +----+           +----+           +----+           +----+
                            ø                ø                ø                ø  
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      ø    ø    ø      ø    ø    ø      ø    ø    ø      ø    ø    ø      ø    ø    ø              
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      ø    ø    ø           ø                ø                ø                ø      
--  +----+----+----+      +----+           +----+           +----+           +----+     

  , nas = EmptyLayer
  , function = EmptyLayer
  , tenk = EmptyLayer
  }

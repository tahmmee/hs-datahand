module DataHand.Layouts.Games.AgeOfWonders3 where
import DataHand.Layout
import DataHand.Keys
import Prelude hiding (Left, Right)

--endt = Return -- FIXME +ctrl
help = F1
saveMenu = F2
loadMenu = F3
chatWindow = Tab
toggleHexGrid = F4 -- or Ctrl-g

camForward = W
camBackward = S
camLeft = A
camRight = D
camRotateUp = V
camRotateDown = R
camRotateLeft = E
camRotateRight = Q
camCenter = C
camCenterOnLeader = Home
camCenterOnPlayerActions = O
camModeToggle = F

zoomIn = Z -- or Minus
zoomOut = X -- or Equal
zoomReset = BackSpace
nextHero  = H
toggleDomainView = B
mapLayerUp = PageUp
mapLayerDown = PageDown
--quickSave = -- ctrl-s
nextEvent = Space

move = M
switchGuardMode = G
selectNextArmy = N
removeArmyFromEventList = J

aow3_layout = Layout {
    normal = Layer
--       +----+           +----+           +----+           +----+
                                                                     
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
                                                                          ret caps  Tab   
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
                                                                         Norm shft lctl
--       +----+           +----+           +----+           +----+      +----+----+----+

--                        +----+           +----+           +----+           +----+
                                                                                
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     bksp nasl  sp                                                                     
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      Alt  NAS  fn                                                                
--  +----+----+----+      +----+           +----+           +----+           +----+     

  , nas = EmptyLayer
  , function = EmptyLayer
  , tenk = EmptyLayer
  }

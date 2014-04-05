module DataHand.Layouts.ProgrammerDvorak where
import DataHand.Layout
import DataHand.Keys
import DataHand.KeyAliases

strict_prog_dvorak = Layout {
    normal = Layer
--       +----+           +----+           +----+           +----+
          scol              com              per              P
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      del  A   fslh    esc  O    X     bktk  E    Y      dqt  U    I      ret caps  Tab   
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
           sqt              Q                J                K          Norm shft lctl
--       +----+           +----+           +----+           +----+      +----+----+----+

--                        +----+           +----+           +----+           +----+
                            G                C                R                L
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     bksp nasl  sp     D    H    sqt    F    T    col    B    N   pent    At  dash bslh
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      Alt  NAS  fn          M                W                V                Z  
--  +----+----+----+      +----+           +----+           +----+           +----+     

  , nas = Layer
--      +----+           +----+           +----+           +----+
         perc              svn             Five              thr
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     del  amp  dol    esc lb   numl   Null lcb  Null   Null rcb   lp     ret caps  Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
         Null             Null             Null             Null        Norm shft lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                          Zero             Two              Four             Six
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
    bksp nasl  sp     eq  ast  Bang   Nine  rp  Null   Null Null Null   Null Null Null
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     Alt  NAS  fn         Null             Null             Null             Null
-- +----+----+----+      +----+           +----+           +----+           +----+     
  , function = EmptyLayer
  , tenk = EmptyLayer
    }

my_prog_dvorak = Layout {
    normal = Layer
--       +----+           +----+           +----+           +----+
          scol              com              per              P
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      del  A   fslh    esc  O    X     bktk  E    Y      dqt  U    I      ret caps  Tab   
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
           sqt              Q                J                K          Norm shft lctl
--       +----+           +----+           +----+           +----+      +----+----+----+

--                        +----+           +----+           +----+           +----+
                            G                C                R                L
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     bksp nasl  sp     D    H    sqt    F    T    col    B    N   pent    At  dash bslh
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      Alt  NAS  fn          M                W                V                Z  
--  +----+----+----+      +----+           +----+           +----+           +----+     

  , nas = Layer
--      +----+           +----+           +----+           +----+
         amp               lb              lcb               lp
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
    Null svn  Null   Null Five Null   Null thr  Null   Null One  Nine   ret caps  Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
         til              perc             dol              eq          Norm shft lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           rp              lcb               rb              Bang
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
    bksp nasl  sp    Zero Two  Null   Null Four Null   Null Six  Null   Null Eight Null
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     Alt  NAS  fn         ast              Hash             Plus             bktk
-- +----+----+----+      +----+           +----+           +----+           +----+     
  , function = EmptyLayer
  , tenk = EmptyLayer
    }
    -- (why [] dont match is explained on his webpage. it took me awhile to realize
    -- he shifts his fingers not along the columns! (F->5,J->8) still dont get his rationale though.)
    -- $ &[{}( =  * )+]! #
    -- ~ %7531 9  0 2468 `
    --       becomes
    --    &[{(      )}]!
    --    7531 9  0 2468
    --    ~%$=      *#+`
    --
    -- TODO use some 2d rendering to make an image of the map
    --       TODO does it make sense to leave /? @^ #` -_ out of NAS?
    --            maybe add them into side-keys in NAS like orig.datahand did?
    --
    -- ~% $ &   [{}( =*)+] !# `
    --    considerations: on orig, capslock allowed easy hex entry... solve using 10k layer for hex entry

-- TODO: dvorak in cyrillic

module DataHand.Layouts.ProgrammerDvorak where
import DataHand.Layout
import DataHand.Keys
import DataHand.KeyAliases
import Prelude hiding (Left, Right)

-- NOTEs on my adaptation:
-- Why keys '[', ']' dont match is explained on his webpage. it took me awhile to realize
-- he shifts his fingers not along the normal columns! (instead he moves index fingers F->5,J->8)
-- His rationale is that typing "]-" is awkward (for right placement) and
-- that breaking up {} is not an option (for left placement; this one makes
-- a lot less sense to me, since they shouldnt be on the same side to begin with.
--
-- My thinking is that on the datahand, you must use a metakey to access BOTH numbers and symbols,
-- so having numbers vs. symbols on the home row is largely the same. I
-- preserve the datahand mapping idiom for the classic dvorak number sequence,
-- however.
--
-- $ &[{}( =  * )+]! #
-- ~ %7531 9  0 2468 `
--       becomes
--    &[{(      )}]!
--    7531 9  0 2468
--    ~%$=      *#+`
--
-- TODO use some 2d rendering to make an image of the map
-- TODO does it make sense to leave ;: '" <> /? @^ #` -_ out of NAS? These keys are
--      there because of original prog-dvorak placement. I get the utility of having
--      their unshifted halves one keystroke away, but having some symbols accessed
--      by shift+ and others by NAS+ may not be as conceptually clean as other
--      solutions.
--      Maybe add the shifter versions into side-keys in NAS like orig.datahand did?
--      The orig layout added <> :; ,. /? (no \|)which were all already accessible in Normal mode
-- TODO considerations: on orig, capslock allowed easy hex entry... solve using 10k layer for hex entry
-- TODO support exotic shift states to dhteensy like @^ key in prog.dv, maybe by adding a separate Shift key layer
my_prog_dvorak = Layout {
-- high prio symbols: ,. :; '" - /? | @ # `
--                    + \
-- Rules, rationale:
--   * QuestionMark and other punctuanion on left hand because most words end with
--     consonants (i think that's the original dvorak reasoning).
--   * forwardslash + nas = backslash
--   * TODO: NAS col, dqt, etc locations are same as Normal?
--    have pipe here shiftable to \ (not a big deal since nas+/ makes more sense anyway
--    copy, paste, etc macros that are portable
    normal = Layer
--       +----+           +----+           +----+           +----+
           sqt              com              per              P
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      At   A   dash   Pipe  O    X      qm   E    Y     fslh  U    I      ret caps  Tab
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          scol              Q                J                K          Norm shft lctl
--       +----+           +----+           +----+           +----+      +----+----+----+

--                        +----+           +----+           +----+           +----+
                            G                C                R                L
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     bksp nasl  sp     D    H    esc    F    T   lwin    B    N   lapp    del  S    ø
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      Alt  NAS  fn          M                W                V                Z  
--  +----+----+----+      +----+           +----+           +----+           +----+     

  , nas = Layer
--      +----+           +----+           +----+           +----+
         amp               lb              lcb               lp
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ø    svn  ø      ø   Five  ø      ø   thr   ø     bslh One  Nine   ret caps  Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
         til              perc             dol              eq          Norm shft lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           rp              rcb               rb              Bang
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
    bksp nasl  sp    eigt Zero  Caret  ø    Two  ø      ø   Four  ø      ø    Six   ø  
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     Alt  NAS  fn         ast              Hash             Plus             bktk
-- +----+----+----+      +----+           +----+           +----+           +----+     
  , function = Layer
--      +----+           +----+           +----+           +----+
          F9               F5               F1               Up
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ø    ø    ø      ø    ø    ø      ø    ø    ø     Left Home rite    ret caps  Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          F11              F7               F3              Down        Norm shft lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           Up               F2               F6               F10 
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
    bksp nasl  sp    Left Home rite    End  ø    ø      ø    ø    ø      ø    ø    ø
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     Alt  NAS  fn         Down              F4               F8               F12 
-- +----+----+----+      +----+           +----+           +----+           +----+     
  , tenk = EmptyLayer
  }

-- TODO: dvorak in cyrillic
dvorak_cyrillic = Layout {
    normal = EmptyLayer
  , nas = EmptyLayer
  , function = EmptyLayer
  , tenk = EmptyLayer
  }

-- XXX incomplete, probably should be abandoned
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
     del  amp  dol    esc lb   numl    ø   lcb   ø      ø   rcb   lp     ret caps  Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          ø                ø                ø                ø          Norm shft lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                          Zero             Two              Four             Six
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
    bksp nasl  sp     eq  ast  Bang   Nine  rp   ø      ø    ø    ø      ø    ø    ø  
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     Alt  NAS  fn          ø                ø                ø                ø  
-- +----+----+----+      +----+           +----+           +----+           +----+     
  , function = EmptyLayer
  , tenk = EmptyLayer
    }


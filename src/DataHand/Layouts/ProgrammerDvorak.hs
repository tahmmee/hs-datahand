module DataHand.Layouts.ProgrammerDvorak (experimental) where
import DataHand.Layout
import DataHand.Keys
import DataHand.KeyAliases

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
--    7531 9  8 0246
--    ~%$=      *#+`
--  The higher frequency of lower digits is real. Classic dvorak was invented
--  before the digital age, but since then, the frequency of 0 has far exceeded
--  all other even numerals, hence my shifting of them rightwards. I was quite
--  suprised at how much better this arrangement served me.
--
-- TODO use some 2d rendering to make an image of the map, ideally one that looks similar to the original stickers.
-- TODO does it make sense to leave ;: '" <> /? @^ #` -_ out of NAS? These keys are
--      there because of original prog-dvorak placement. I get the utility of having
--      their unshifted halves one keystroke away, but having some symbols accessed
--      by shift+ and others by NAS+ may not be as conceptually clean as other
--      solutions.
--      Maybe add the shifter versions into side-keys in NAS like orig.datahand did?
--      The orig layout added <> :; ,. /? (no \|)which were all already accessible in Normal mode
-- TODO DO 10k LAYER and support HEX ENTRY easily (have a-f, h as suffix, 0x prefix macro, ", 0x" macro maybe even
-- TODO MACRO SUPPORT for situations like the above.
-- TODO support exotic shift states to dhteensy like @^ key in prog.dv, maybe by adding a separate Shift key layer
-- TODO maybe switch out @ for something used more frequently like #+*
-- TODO BETTER FNMOUSE/ARROWS: have fn lever go to last fn state, then cycle fn
--      types [arrows, mouse, more if desired]. Thereafter, use Norm to return, and
--      then Fn to switch back to last Fn mode in that list. This frees up the two
--      middle finger buttons for more useful binds.
-- TODO Add FnArrow+NAS mode that turns arrow keys into pageup,down,home,end
-- TODO naslock and capslock are pretty much wasted. i can hold the damn key
--      down just fine, thanks. instead, make naslock go to 10kmode and capslock
--      cycle whole layouts, maybe. i may want to put capslock somewhere buried in
--      NAS, just in case i ever need it.
-- TODO Profile switching: hold both locks and press a home row key to select
--      that slot. or release a lock key last to cycle the opposite direction.
-- TODO USB connection to other hand, attach as separate devices?
-- TODO: NAS col, dqt, etc locations are same as Normal?
--   * copy, paste, etc macros that are portable; alt-tab on single key on left-hand side
--   * gnu screen: ctrl-a, ctrl-a n/p, " etc.
--   * xmonad bindings
-- TODO make / shift to \ rather than ? , consider other possibilites
-- TODO missing fn keys like printscreen,scrolllock, numlock
-- TODO have winkey be both levers simulaneously pressed
-- TODO C-a a,n,p,[,] macros for screen
-- TODO give some thought to the difficulty i have typing ("1");
-- TODO FEATURE consider chording support for games. this is probably better/faster than hard to reach directional buttons. research what timing thresholds and behavior are most usable with chording. There is an inherent, unavoidable collision when mashing 2 buttons. Do I send both original keypresses anyway?(requires keymap accomodations to not send superfluous commands) or do i wait for a chord 50ms or so? (then all regular keys are unresponsive!) a middle ground would be to have some keys "chordable" meaning they would always exhibit the delay, and have others remain fully responsive.
-- times when superflous keys would be okay:
--   * modal menus in rtses, so long as underlying keys dont share binds
--   * not many others, lol. this method seems pretty undesirable.

experimental = Layout {
-- high prio symbols: ,. :; '" - /? | @ # `
--                    + \
-- Rules, rationale:
--   * QuestionMark and other punctuanion on left hand because most words end with
--     consonants (i think that's the original dvorak reasoning).
--
--   * forwardslash + nas = backslash
    normal = Layer
--       +----+           +----+           +----+           +----+
          scol              com              per              P
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      At   A   dash    dol  O    X     Pipe  E    Y     fslh  U    I      ret caps  Tab
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
           sqt              Q                J                K          Norm shft lctl
--       +----+           +----+           +----+           +----+      +----+----+----+

--                        +----+           +----+           +----+           +----+
                            G                C                R                L
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     bksp nasl  sp     D    H    esc    F    T   lwin    B    N   lapp    del  S    ø
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      Alt  NAS  fn          M                W                V                Z  
--  +----+----+----+      +----+           +----+           +----+           +----+     

  , normalS = Layer
--       +----+           +----+           +----+           +----+
           ø                qm              Bang              ø
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      til  ø    ø     Hash  ø    ø      amp  ø    ø     bslh  ø    ø      ø    ø    ø
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
           ø                ø                ø                ø           ø    ø    ø   
--       +----+           +----+           +----+           +----+      +----+----+----+

--                        +----+           +----+           +----+           +----+
                            ø                ø                ø                ø
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      ø    ø    ø      ø    ø    ø      ø    ø    ø      ø    ø    ø      ø    ø    ø
--  +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
      ø    ø    ø           ø                ø                ø                ø     
--  +----+----+----+      +----+           +----+           +----+           +----+     

  , nas = Layer
--      +----+           +----+           +----+           +----+
          ø                lb               lcb              lp
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ø    svn Plus    ø   Five  ø      lab  thr  rab   bslh  One Nine    ret caps  Tab
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
         bktk             perc             dol              eq          Norm shft lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           rp              rcb               rb               ø
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
    bksp nasl  sp    eigt Zero Caret   ø    Two  ø      ø   Four  ø      ins  Six  ø
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     Alt  NAS  fn          ast             Hash              ø                til
-- +----+----+----+      +----+           +----+           +----+           +----+     
  , nasS = EmptyLayer
  , function = Layer
--      +----+           +----+           +----+           +----+
          ø                ø               pgup              up
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     F11  ø    F9     F7   ø    F5     F3  End   F1   left Home rite    ret caps  Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          ø                ø               pgdn             down        Norm shft lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           up              pgup              ps               ø
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
    bksp nasl  sp    left Home rite    F2   End  F4     F6   ø    F8     F10  ø    F12
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     Alt  NAS  fn         down             pgdn              ø                ø
-- +----+----+----+      +----+           +----+           +----+           +----+     
  , functionS = EmptyLayer
  , tenk = EmptyLayer
  , tenkS = EmptyLayer
  }

-- TODO: dvorak in cyrillic
dvorak_cyrillic = Layout {
    normal = EmptyLayer
  , normalS = EmptyLayer
  , nas = EmptyLayer
  , nasS = EmptyLayer
  , function = EmptyLayer
  , functionS = EmptyLayer
  , tenk = EmptyLayer
  , tenkS = EmptyLayer
  }

-- XXX incomplete, probably should be abandoned
strict = Layout {
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

  , normalS = EmptyLayer
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
  , nasS = EmptyLayer
  , function = EmptyLayer
  , functionS = EmptyLayer
  , tenk = EmptyLayer
  , tenkS = EmptyLayer
    }


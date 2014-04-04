{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module Main where

import Control.Applicative
import Data.Traversable
import Data.Foldable hiding (concat)
import GHC.Enum
import Data.List

join delim l = concat (intersperse delim l)

data Key = Null
         | Dummy01
         | Dummy02
         | Dummy03
         | A
         | B
         | C
         | D
         | E
         | F
         | G
         | H
         | I
         | J
         | K
         | L
         | M
         | N
         | O
         | P
         | Q
         | R
         | S
         | T
         | U
         | V
         | W
         | X
         | Y
         | Z
         | One
         | Two
         | Three
         | Four
         | Five
         | Six
         | Seven
         | Eight
         | Nine
         | Zero
         | Return
         | Escape
         | Backspace
         | Tab
         | Space
         | Minus
         | Equal
         | LeftBracket
         | RightBracket
         | BackSlash
         | Number -- XXX is this also shift+3?
         | Semicolon
         | Quote
         | BackTick
         | Comma
         | Period
         | Slash
         | CapsLock
         | F1
         | F2
         | F3
         | F4
         | F5
         | F6
         | F7
         | F8
         | F9
         | F10
         | F11
         | F12
         | PrintScreen
         | ScrollLock
         | Pause
         | Insert
         | Home
         | PageUp
         | Delete
         | End
         | PageDown
         | Right
         | Left
         | Down
         | Up
         | NumLock
         | PadSlash
         | PadAsterix
         | PadMinus
         | PadPlus
         | PadEnter
         | Pad1
         | Pad2
         | Pad3
         | Pad4
         | Pad5
         | Pad6
         | Pad7
         | Pad8
         | Pad9
         | Pad0
         | PadPeriod
         | Dummy64 | Dummy65 | Dummy66 | Dummy67 | Dummy68 | Dummy69
         | Dummy6A | Dummy6B | Dummy6C | Dummy6D | Dummy6E | Dummy6F | Dummy70
         | Dummy71 | Dummy72 | Dummy73 | Dummy74 | Dummy75 | Dummy76 | Dummy77 | Dummy78
         | Dummy79 | Dummy7A | Dummy7B | Dummy7C | Dummy7D | Dummy7E | Dummy7F
         | RegularShift
         | Dummy81 | Dummy82 | Dummy83
         | CapA
         | CapB
         | CapC
         | CapD
         | CapE
         | CapF
         | CapG
         | CapH
         | CapI
         | CapJ
         | CapK
         | CapL
         | CapM
         | CapN
         | CapO
         | CapP
         | CapQ
         | CapR
         | CapS
         | CapT
         | CapU
         | CapV
         | CapW
         | CapX
         | CapY
         | CapZ
         | Bang
         | At
         | Hash
         | Dollar
         | Percent
         | Caret
         | Ampersand
         | Asterisk
         | LeftParenthesis
         | RightParenthesis
         | DummyA8 | DummyA9 | DummyAA | DummyAB | DummyAC | DummyAD
         | Plus
         | LeftCurlyBracket
         | RightCurlyBracket
         | Pipe
         | DummyB2 -- what is shift+"number"?
         | Colon
         | DoubleQuote
         | Tilde
         | DummyB6 | DummyB7 | DummyB8
         | DummyB9 | DummyBA | DummyBB | DummyBC | DummyBD | DummyBE | DummyBF | DummyC0
         | DummyC1 | DummyC2 | DummyC3 | DummyC4 | DummyC5 | DummyC6 | DummyC7 | DummyC8
         | DummyC9 | DummyCA | DummyCB | DummyCC | DummyCD | DummyCE | DummyCF
         | DummyD0 | DummyD1 | DummyD2 | DummyD3 | DummyD4 | DummyD5 | DummyD6 | DummyD7
         | DummyD8 | DummyD9 | DummyDA | DummyDB | DummyDC | DummyDD | DummyDE | DummyDF
         | DummyE0 | DummyE1 | DummyE2 | DummyE3 | DummyE4 | DummyE5 | DummyE6 | DummyE7
         | DummyE8 | DummyE9 | DummyEA | DummyEB | DummyEC | DummyED | DummyEE | DummyEF
         -- special datahand keycodes follow
         | Norm
         | NAS
         | NASLock
         | Function
         -- these are different codes from 'regular' keyboard metakeys
         | Shift
         | Control
         | Alt
    deriving (Show, Eq, Ord, Bounded, Enum)

-- TODO: organize each into separate .hs, create converter for .xml LGS files

-- Aliases for prettier layouts:
scol = Semicolon
col = Colon
com = Comma
per = Period
del = Delete
esc = Escape
bktk = BackTick
sqt = Quote
ret = Return
caps = CapsLock
shft = Shift
lctl = Control -- XXX LeftControl
dqt = DoubleQuote
bslh = BackSlash
fslh = Slash
nasl = NASLock
lb = LeftBracket
rb = RightBracket
sp = Space
bksp = Backspace
fn = Function
pent = PadEnter
dash = Minus
thr = Three
svn = Seven
eigt = Eight
amp = Ampersand
perc = Percent
dol = Dollar
numl = NumLock
lcb = LeftCurlyBracket
rcb = RightCurlyBracket
lp = LeftParenthesis
rp = RightParenthesis
ast = Asterisk
eq = Equal
til = Tilde

data Layer t = EmptyLayer | Layer -- TODO should EmptyLayer be all nulls?
--      +----+           +----+           +----+           +----+
          t                t                t                t  
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     t    t    t      t    t    t      t    t    t      t    t    t      t    t    t     
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          t                t                t                t           t    t    t  
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           t                t                t                t  
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     t    t    t      t    t    t      t    t    t      t    t    t      t    t    t  
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     t    t    t           t                t                t                t  
-- +----+----+----+      +----+           +----+           +----+           +----+     
 deriving (Show, Eq, Functor, Traversable, Foldable)


data RawLayer t = EmptyRawLayer | RawLayer t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t deriving (Show, Eq, Functor, Traversable, Foldable)

-- TODO this is almost certainly not the best way to represent+effect this mapping?
toRaw EmptyLayer = EmptyRawLayer
toRaw (Layer
--      +----+           +----+           +----+           +----+
          a                b                c                d
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     e    f    g     h     i    j      k    l    m      n    o    p      q    r    s
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          t                u                v                w           x    y    z
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           aa               bb               cc               dd
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ee   ff   gg     hh   ii   jj     kk   ll   mm     nn   oo   pp     qq   rr   ss
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     tt   uu   vv          ww               xx               yy               zz
-- +----+----+----+      +----+           +----+           +----+           +----+     
    ) = (RawLayer
--      +----+           +----+           +----+           +----+
          hh               aa               e                a 
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ii   jj   f      g    ww   xx     t    u    kk     bb   h    b      ll   mm   i 
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          j                nn               cc               k           c    oo   pp
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           l                m                yy               zz
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     v    w    qq     dd   n    d      rr   ss   o      p    tt   ee     z    s    uu
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ff   y    r           gg               vv               q                x 
-- +----+----+----+      +----+           +----+           +----+           +----+     

    )



data Layout = Layout { normal :: Layer Key
                     , nas :: Layer Key
                     , function :: Layer Key
                     , tenk :: Layer Key
    } deriving (Show, Eq)

default_qwerty_layout = [
--      +----+           +----+           +----+           +----+
         Q               , W              , E              , R
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   , del, A  ,lb    , esc, S  , B    ,bktk, D  , T    , dqt, F  , G    , ret,caps, Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
        , Z              , X              , C              , V         ,Norm,shft,lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                         , U              , I              , O              , P
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   ,bksp,nasl, sp   , H  , J  , sqt  , Y  , K  , col  , N  , L  ,pent  ,rb  ,scol,bslh
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   , Alt, NAS, fn        , M              , com            , per            ,fslh
-- +----+----+----+      +----+           +----+           +----+           +----+     
   ]
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

main = do
    let layerToRawMap EmptyLayer = []
        layerToRawMap l = map fromEnum $ (toList . toRaw) l

    let dumpRawHeader Layout{..} = putStrLn $ join "\n" [
            "#define KEY_DH_NORM 0xf0",
            "#define KEY_DH_NAS 0xf1",
            "#define KEY_DH_NASLK 0xf2",
            "#define KEY_DH_FN 0xf3",
            "#define KEY_DH_SHIFT 0xf4",
            "#define KEY_DH_CTRL 0xf5",
            "#define KEY_DH_ALT 0xf6",
            "const char PROGMEM normal_keys [] = {" ++ join ", " (map show $ layerToRawMap normal  ) ++ "};",
            "const char PROGMEM    nas_keys [] = {" ++ join ", " (map show $ layerToRawMap nas     ) ++ "};",
            "const char PROGMEM     fn_keys [] = {" ++ join ", " (map show $ layerToRawMap function) ++ "};"
            ]
    dumpRawHeader my_prog_dvorak


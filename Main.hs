--{-# FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module Main where

import Control.Applicative
import Data.Traversable
import Data.Foldable hiding (concat)
import Data.Maybe
import Data.Tuple
import GHC.Enum
import Data.Flags
import Data.List

data MetaKey = LeftControl
             | LeftShift
             | LeftAlt
             | LeftGui
             | RightControl
             | RightShift
             | RightAlt
             | RightGui
             deriving (Show, Eq, Ord, Bounded)

instance Enum MetaKey where
    fromEnum = fromJust . flip lookup metakey_map
    toEnum = fromJust . flip lookup (map swap metakey_map)

metakey_map = [-- (LeftControl, 0x01)
              --, (LeftShift, 0x02)
              --, (LeftAlt, 0x04)
              --, (LeftGui, 0x08)
              --, (RightControl, 0x10)
              --, (RightShift, 0x20)
              --, (RightAlt, 0x40)
              --, (RightGui, 0x80)
              (LeftShift, 0x80)
    ]

--newtype KeyFlags = KeyFlags Key deriving (Eq, Flags)
--instance Flags Key where
--    noflags = Null
--    andflags a b = toEnum (fromEnum a | fromEnum b)
--    butflags a b = toEnum (fromEnum a | fromEnum b)
--    butflags a b = toEnum (fromEnum a | fromEnum b)
--test = KeyFlags Norm .+. KeyFlags NAS

--{enum MyFlags, MyFlags
--  , myFlag1 = C_FLAG1
--  , myFlag2 = C_FLAG2
--  , myFlag3 = C_FLAG3
--  }

data KeyCode = KeyCode [MetaKey] Key

join delim l = concat (intersperse delim l)

instance Show KeyCode where
    show (KeyCode metas k) = concat [join "+" (map show metas), "-", show k]
 
--instance Enum Key where
--    --fromEnum = fromJust . flip lookup dhkey_map
--    --toEnum = fromJust . flip lookup (map swap dhkey_map)
--    fromEnum Norm = 0xf0
--    fromEnum NAS = 0xf1
--    fromEnum NASLock = 0xf2
--    fromEnum Function = 0xf3
--    fromEnum Shift = 0xf4
--    fromEnum Control = 0xf5
--    fromEnum Alt = 0xf6

--dhkey_map = [
--    (Norm, 0xf0),
--    (NAS, 0xf1),
--    (NASLock, 0xf2),
--    (Function, 0xf3),
--    (Shift, 0xf4),
--    (Control, 0xf5),
--    (Alt, 0xf6)
--    ]

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
         | DummyA8 | DummyA9 | DummyAA | DummyAB | DummyAC | DummyAD | DummyAE
         | LeftCurlyBracket
         | RightCurlyBracket
         | Pipe
         | DummyB2 -- what is shift+"number"?
         | Colon
         | DoubleQuote
         | DummyB5 | DummyB6 | DummyB7 | DummyB8
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

normal_keys = [ 
          H, U, Delete, Q,
          J, Quote, A, LeftBracket,
          M, Comma, Z, X,
          Y, I, Escape, W,
          K, Colon, S, B,
          N, O, BackTick, E,
          L, PadEnter, D, T,
          Period, Slash, C, V,
          RightBracket, P, DoubleQuote, R,
          Semicolon, BackSlash, F, G,
          Alt, Backspace, Control, Tab,
          NAS, NASLock, Shift, CapsLock,
          Space, Function, Return, Norm
 ]

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
lbrk = LeftBracket
rbrk = RightBracket
sp = Space
bksp = Backspace
fn = Function
pent = PadEnter
dash = Minus

sequence_map = [33,26,4,0,34,35,5,6,48,49,19,20,36,27,7,1,37,38,8,9,39,28,10,2,40,41,11,12,50,51,21,22,42,29,13,3,43,44,14,15,45,30,25,18,46,31,24,17,32,47,16,23]

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


-- XXX TODO RawLayer and use of it to convert to raw mapping is a WIP.
-- TODO need to actually translate the ints to positions in the data constructor
data RawLayer t = EmptyRawLayer | RawLayer t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t 

-- TODO this is almost certainly not the best way to represent+effect this mapping
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
   , del, A  ,lbrk  , esc, S  , B    ,bktk, D  , T    , dqt, F  , G    , ret,caps, Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
        , Z              , X              , C              , V         ,Norm,shft,lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                         , U              , I              , O              , P
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   ,bksp,nasl, sp   , H  , J  , sqt  , Y  , K  , col  , N  , L  ,pent  ,rbrk,scol,bslh
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   , Alt, NAS, fn        , M              , com            , per            ,fslh
-- +----+----+----+      +----+           +----+           +----+           +----+     
   ]

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
  , function = EmptyLayer
  , tenk = EmptyLayer
    }

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
--programmer_dvorak_layout_nas = [
----      +----+           +----+           +----+           +----+
--         perc            , svn            ,Five            , thr
---- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
--   , del, amp, dol  , esc,lbkt,numl  , X  ,lcb , X    , X  ,rcb , lp   , ret,caps, Tab   
---- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
--        , X            , X              , X              , X         ,Norm,shft,lctl
----      +----+           +----+           +----+           +----+      +----+----+----+
--
----                       +----+           +----+           +----+           +----+
--                         , 
---- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
--   ,bksp,nasl, sp   ,
---- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
--   , Alt, NAS, fn        ,
---- +----+----+----+      +----+           +----+           +----+           +----+     
--   ]

main = do
    let dump key = putStrLn $ (show key) ++ " = " ++ (show $ fromEnum key)

    --print $ KeyCode [LeftShift, LeftControl] Quote --TODO
    --print $ sort default_qwerty_layout
    --print $ sort normal_keys

    --print $ map (fromJust . (flip elemIndex $ default_qwerty_layout)) normal_keys -- compute the sequence map
    --print $ normal_keys
    --print $ map (default_qwerty_layout !!) sequence_map
    --
    -- from when i was using lists rather than traversable functors
    --let dumpRawMap Layout{normal = Layer normal} = print $ map (fromEnum . (!!) normal) sequence_map
    --let dumpRawMap Layout{..} = print $ map (fromEnum . (!!) normal) sequence_map

    print my_prog_dvorak

    let fromEnumLayers Layout{..} = fmapDefault (fromEnum :: Key -> Int) normal
    print $ fromEnumLayers my_prog_dvorak

    let layerToRawMap EmptyLayer = []
        layerToRawMap l = map fromEnum $ map (toList l !!) sequence_map

    let dumpRawHeader Layout{..} = putStrLn $ join "\n" [
            "static char PROGMEM normal_keys [] = {" ++ join ", " (map show $ layerToRawMap normal  ) ++ "};",
            "static char PROGMEM    nas_keys [] = {" ++ join ", " (map show $ layerToRawMap nas     ) ++ "};",
            "static char PROGMEM     fn_keys [] = {" ++ join ", " (map show $ layerToRawMap function) ++ "};"
            ]
    dumpRawHeader my_prog_dvorak

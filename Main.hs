--{-# FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE OverloadedStrings #-}
module Main where

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
--    noflags = Dummy00
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

data Key = Dummy00
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
         | Number
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
         -- (gap here)
         | Dummy6A | Dummy6B | Dummy6C | Dummy6D | Dummy6E | Dummy6F | Dummy70
         | Dummy71 | Dummy72 | Dummy73 | Dummy74 | Dummy75 | Dummy76 | Dummy77 | Dummy78
         | Dummy79 | Dummy7A | Dummy7B | Dummy7C | Dummy7D | Dummy7E | Dummy7F | Shift
         | Dummy81 | Dummy82 | Dummy83 | Dummy84 | Dummy85 | Dummy86 | Dummy87 | Dummy88
         | Dummy89 | Dummy8A | Dummy8B | Dummy8C | Dummy8D | Dummy8E | Dummy8F | Dummy90
         | Dummy91 | Dummy92 | Dummy93 | Dummy94 | Dummy95 | Dummy96 | Dummy97 | Dummy98
         | Dummy99 | Dummy9A | Dummy9B | Dummy9C | Dummy9D | Dummy9E | Dummy9F | DummyA0
         | DummyA1 | DummyA2 | DummyA3 | DummyA4 | DummyA5 | DummyA6 | DummyA7 | DummyA8
         | DummyA9 | DummyAA | DummyAB | DummyAC | DummyAD | DummyAE | DummyAF | DummyB0
         | DummyB1 | DummyB2
         | Colon
         | DoubleQuote
         | DummyB5 | DummyB6 | DummyB7 | DummyB8
         | DummyB9 | DummyBA | DummyBB | DummyBC | DummyBD | DummyBE | DummyBF | DummyC0
         | DummyC1 | DummyC2 | DummyC3 | DummyC4 | DummyC5 | DummyC6 | DummyC7 | DummyC8
         | DummyC9 | DummyCA | DummyCB | DummyCC | DummyCD | DummyCE | DummyCF | DummyD0
         | DummyD1 | DummyD2 | DummyD3 | DummyD4 | DummyD5 | DummyD6 | DummyD7 | DummyD8
         | DummyD9 | DummyDA | DummyDB | DummyDC | DummyDD | DummyDE | DummyDF
         -- special datahand keycodes follow
         | Norm
         | NAS
         | NASLock
         | Function
         -- these are different codes from 'regular' keyboard metakeys
         | DHShift
         | Control
         | Alt
    deriving (Show, Eq, Ord, Bounded, Enum)


-- maybe change this to DH_SHIFT(key) form
--define DH_SHIFT 0x80
--normal_keys = map fromEnum [ H, U, Escape, Delete
normal_keys = [ 
          --  H, U, Escape, Delete
          --, J, Quote, A, LeftBracket
          --, M, Comma, Z, X
          --, Y, I, Q, W
          --, K, Semicolon, S, B
          --, N, O, BackTick, E
          --, L, P, D, T
          --, Period, Slash, C, V
          --, RightBracket, P, DoubleQuote, R
          --, Semicolon, BackSlash, F, G
          --, Alt, Backspace, Control, Tab
          --, NAS, NASLock, Shift, CapsLock
          --, Space, Function, Enter, Norm
          -- the above was the custom layout in keymaps.h
          -- below from keymaps-original.h
          H, U, Delete, Q,
          J, Quote, A, LeftBracket,
          M, Comma, Z, X,
          Y, I, Escape, W,
          K, Colon, S, B, -- actually semicolon
          N, O, BackTick, E,
          L, PadEnter, D, T,
          Period, Slash, C, V,
          RightBracket, P, DoubleQuote, R,
          Semicolon, BackSlash, F, G,
          Alt, Backspace, Control, Tab,
          NAS, NASLock, Shift, CapsLock,
          Space, Function, Return, Norm
 ]

-- TODO: represent this nicely with comment annotations, something like this:

-- Aliases for prettier layouts:
scol = Semicolon
col = toEnum (fromEnum Shift + fromEnum Semicolon)
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
dqt = toEnum (fromEnum Shift + fromEnum Quote)
bslh = BackSlash
fslh = Slash
nasl = NASLock
lbrk = LeftBracket
rbrk = RightBracket
sp = Space
bksp = Backspace
fn = Function
pent = PadEnter
at = toEnum (fromEnum Shift + fromEnum Two)
dash = Minus

--sequence_map = [3,15,23,35,2,6,7,14,18,19,22,26,27,34,38,39,50,47,43,10,11,30,31,51,46,42,1,13,21,33,41,45,48,0,4,5,12,16,17,20,24,25,32,36,37,40,44,49,8,9,28,29]
sequence_map = [33,26,4,0,34,35,5,6,48,49,19,20,36,27,7,1,37,38,8,9,39,28,10,2,40,41,11,12,50,51,21,22,42,29,13,3,43,44,14,15,45,30,25,18,46,31,24,17,32,47,16,23]

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

programmer_dvorak_layout = [
--      +----+           +----+           +----+           +----+
         scol            , com            , per            , P
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   , del, A  ,fslh  , esc, O  , X    ,bktk, E  , Y    , dqt, U  , I    , ret,caps, Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
        , sqt            , Q              , J              , K         ,Norm,shft,lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                         , G              , C              , R              , L
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   ,bksp,nasl, sp   , D  , H  , sqt  , F  , T  , col  , B  , N  ,pent  , at ,dash,bslh
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   , Alt, NAS, fn        , M              , W              , V              , Z  
-- +----+----+----+      +----+           +----+           +----+           +----+     
   ]


--
--const char PROGMEM nas_keys[]=
--        { 
--          // 6 & del !
--          KEY_6, KEY_7 + DH_SHIFT, KEY_DELETE, KEY_1 + DH_SHIFT,
--          // 7 _ 1 ~
--          KEY_7, KEY_MINUS + DH_SHIFT, KEY_1, KEY_TILDE + DH_SHIFT,
--          // + , = x
--          KEY_EQUAL + DH_SHIFT, KEY_COMMA, KEY_EQUAL, KEY_X,
--          // ( * esc @
--          KEY_6 + DH_SHIFT, KEY_8 + DH_SHIFT, KEY_ESC, KEY_2 + DH_SHIFT,
--          // 8 : 3 numlk
--          KEY_8, KEY_SEMICOLON + DH_SHIFT, KEY_2, KEY_B,
--          // ; ( < #
--          KEY_SEMICOLON, KEY_9 + DH_SHIFT, KEY_COMMA + DH_SHIFT, KEY_3 + DH_SHIFT,
--          // 9 ent 3 >
--          KEY_9, KEY_P, KEY_3, KEY_PERIOD + DH_SHIFT,
--          // . ? % -
--          KEY_PERIOD, KEY_SLASH + DH_SHIFT, KEY_5 + DH_SHIFT, KEY_MINUS,
--          // 10off ) / $
--          KEY_RIGHT_BRACE, KEY_0 + DH_SHIFT, KEY_SLASH, KEY_4 + DH_SHIFT,
--          // 0 10kon 4 5
--          KEY_0, KEY_BACKSLASH, KEY_4, KEY_5,
--
--          KEY_DH_ALT, KEY_BACKSPACE, KEY_DH_CTRL, KEY_TAB,
--          KEY_DH_NAS, KEY_DH_NASLK, KEY_DH_SHIFT, KEY_CAPS_LOCK,
--          KEY_SPACE, KEY_DH_FN, KEY_ENTER, KEY_DH_NORM,
--};
--
--const char PROGMEM fn_keys[]=
--        { 
--          // <- uparrow del f2
--          KEY_LEFT, KEY_UP, KEY_DELETE, KEY_F2,
--          // home -> l/r scrllk
--          KEY_HOME, KEY_RIGHT, KEY_A, KEY_SCROLL_LOCK,
--          // downar f7 f1 f3
--          KEY_DOWN, KEY_F7, KEY_F1, KEY_F3,
--          // end f8 esc f4
--          KEY_END, KEY_F8, KEY_ESC, KEY_F4,
--          // arrowon shift mb3 numlk
--          KEY_K, KEY_SEMICOLON, KEY_S, KEY_NUM_LOCK,
--          // ins f10 = f6
--          KEY_INSERT, KEY_F10, KEY_EQUAL, KEY_F6,
--          // print ent mouseon ent
--          KEY_PRINTSCREEN, KEY_P, KEY_D, KEY_ENTER,
--          // f9 pgdn f5 downarr
--          KEY_F9, KEY_PAGE_DOWN, KEY_F5, KEY_DOWN,
--          // f11 pgup arrleft arrup
--          KEY_F11, KEY_PAGE_UP, KEY_LEFT, KEY_UP,
--          //  pause f12 home arrright
--          KEY_PAUSE, KEY_F12, KEY_HOME, KEY_RIGHT,
--
--          KEY_DH_ALT, KEY_BACKSPACE, KEY_DH_CTRL, KEY_TAB,
--          KEY_DH_NAS, KEY_DH_NASLK, KEY_DH_SHIFT, KEY_CAPS_LOCK,
--          KEY_SPACE, KEY_DH_FN, KEY_ENTER, KEY_DH_NORM,
--};
--
main = do
   let dump key = putStrLn $ (show key) ++ " = " ++ (show $ fromEnum key)
   --dump Norm
   --dump A
   --dump PadPeriod
   --print $ KeyCode [LeftShift, LeftControl] Quote --TODO
   --print $ sort default_qwerty_layout
   --print $ sort normal_keys

   --print $ map (fromJust . (flip elemIndex $ default_qwerty_layout)) normal_keys -- compute the sequence map
   --print $ normal_keys
   --print $ map (default_qwerty_layout !!) sequence_map

   print $ map (fromEnum . (!!) programmer_dvorak_layout) sequence_map

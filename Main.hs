module Main where

import Data.Maybe
import Data.Tuple
import GHC.Enum

--data KeyCode
data DHKey = Norm
           | NAS
           | NASLock
           | Function
           -- these are different codes from 'regular' keyboard metakeys
           | Shift
           | Ctrl
           | Alt
           deriving (Show, Eq, Ord, Bounded)

instance Enum DHKey where
    fromEnum = fromJust . flip lookup dhkey_map
    toEnum = fromJust . flip lookup (map swap dhkey_map)

dhkey_map = [
    (Norm, 0xf0),
    (NAS, 0xf1),
    (NASLock, 0xf2),
    (Function, 0xf3),
    (Shift, 0xf4),
    (Ctrl, 0xf5),
    (Alt, 0xf6)
    ]

data MetaKey = LeftCtrl
             | LeftShift
             | LeftAlt
             | LeftGui
             | RightCtrl
             | RightShift
             | RightAlt
             | RightGui
             deriving (Show, Eq, Ord, Bounded)

instance Enum MetaKey where
    fromEnum = fromJust . flip lookup metakey_map
    toEnum = fromJust . flip lookup (map swap metakey_map)

metakey_map = [
    (LeftCtrl, 0x01),
    (LeftShift, 0x02),
    (LeftAlt, 0x04),
    (LeftGui, 0x08),
    (RightCtrl, 0x10),
    (RightShift, 0x20),
    (RightAlt, 0x40),
    (RightGui, 0x80)
    ]

data Key = Dummy0
         | Dummy1
         | Dummy2
         | Dummy3
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
         | Enter
         | Escape
         | BackSpace
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
    deriving (Show, Eq, Ord, Bounded, Enum)


-- maybe change this to DH_SHIFT(key) form
--define DH_SHIFT 0x80
--normal_keys = map fromEnum [ H, U, Escape, Delete
normal_keys = [ H, U, Escape, Delete
                           , J, Quote, A, LeftBracket
          , M, Comma, Z, X
          , Y, I, Q, W
          , K, Semicolon, S, B
          , N, O, BackTick, E
          , L, P, D, T
          , Period, Slash, C, V
          , RightBracket, P
--          --Quote + Shift
          , Quote
          , R
          , Semicolon, BackSlash, F, G
          --, Alt
--          Alt, Backspace, Ctrl, Tab
--          NAS, NASLk, Shift, CapsLock
--          Space, Function, Enter, Norm
     ]

-- TODO: represent this nicely with comment annotations, something like this:

-- Synonyms for better-annotated layouts:
scol = Semicolon
col = Dummy0 --TODO Shift + Colon
com = Comma
per = Period
del = Delete
esc = Escape
bktk = BackTick
qt = Quote
ret = Enter
caps = CapsLock
shft = Shift
lctl = LeftCtrl
dqt = Dummy0 --TODO: Shift + Quote
bslh = BackSlash
fslh = Slash
nasl = NASLock
lbrk = LeftBracket
rbrk = RightBracket
sp = Space
bksp = BackSpace
fn = Function

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
   ,bksp,nasl, sp   , H  , J  , dqt  , Y  , K  , col  , N  , L  , T    ,rbrk,scol,bslh
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   , Alt, NAS, fn        , M              , com            , per            ,fslh
-- +----+----+----+      +----+           +----+           +----+           +----+     
   ]

programmer_dvorak_layout = [
--      +----+           +----+           +----+           +----+
         scol            , com            , per            , P
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   , del, A  , X    , esc, O  , X    ,bktk, E  , Y    , qt , U  , I    , ret,caps, Tab   
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
        , X              , X              , X              , X         ,Norm,shft,lctl
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                         , U              , I              , O              , P
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   ,bksp,nasl, sp   , H  , J  , dqt  , Y  , K  , col  , N  , L  , T    ,rbrk,scol,bslh
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
   , Alt, NAS, fn        , M              , com            , per            ,fslh
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
   dump Norm
   dump A
   dump PadPeriod
   print normal_keys

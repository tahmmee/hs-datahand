module DataHand.Keys where

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
         | Number -- XXX what is this? is this also shift+3?
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
         | ShiftReturn
         | ShiftEscape
         | ShiftBackspace
         | ShiftTab
         | ShiftSpace
         | ShiftMinus
         | Plus
         | LeftCurlyBracket
         | RightCurlyBracket
         | Pipe
         | ShiftNumber -- what is shift+"number"?
         | Colon
         | DoubleQuote
         | Tilde
         | LeftAngleBracket
         | RightAngleBracket
         | QuestionMark
         | ShiftCapsLock
         | F13
         | F14
         | F15
         | F16
         | F17
         | F18
         | F19
         | F20
         | F21
         | F22
         | F23
         | F24
         | DummyC6 | DummyC7 | DummyC8
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

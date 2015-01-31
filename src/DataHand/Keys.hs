module DataHand.Keys where

data Key = NoEvent
         | ErrorRollOver
         | POSTFail
         | ErrorUndefined
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
         -- 0x10
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
         -- 0x20
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
         -- 0x30
         | RightBracket
         | BackSlash
         | Number -- XXX I think this is Ellipsis or INT2
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
         -- 0x40
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
         | RightArrow
         -- 0x50
         | LeftArrow
         | DownArrow
         | UpArrow
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
         -- 0x60
         | Pad8
         | Pad9
         | Pad0
         | PadPeriod
         | Int1
         | Application
         | Power
         | PadEqual
         | F13
         | F14
         | F15
         | F16
         | F17
         | F18
         | F19
         | F20
         -- 0x70
         | F21
         | F22
         | F23
         | F24
         | Execute
         | Help
         | Menu
         | Select
         | Stop
         | Again
         | Undo
         | Cut
         | Copy
         | Paste
         | Find
         | Mute
         -- 0x80
         | VolumeUp
         | VolumeDown
         | LockingCapsLock
         | LockingNumLock
         | LockingScrollLock
         | PadComma
         | PadEqualAgain
         | Internat1
         | Internat2
         | Internat3
         | Internat4
         | Internat5
         | Internat6
         | Internat7
         | Internat8
         | Internat9
         -- 0x90
         | Lang1
         | Lang2
         | Lang3
         | Lang4
         | Lang5
         | Lang6
         | Lang7
         | Lang8
         | Lang9
         | AltErase
         | SysRq
         | Cancel
         | Clear
         | Prior
         | Return2 -- XXX are both valid?
         | Separator -- this doesn't print anything on US layout, dunno bout others; use Pipe (shifted BackSlash)
         -- 0xA0
         | Out
         | Oper
         | ClearAgain
         | CrSelProps
         | ExSel
         | DummyA5 | DummyA6 | DummyA7 | DummyA8
         | DummyA9 | DummyAA | DummyAB | DummyAC | DummyAD | DummyAE | DummyAF
         | DummyB0 | DummyB1 | DummyB2 | DummyB3 | DummyB4 | DummyB5 | DummyB6 | DummyB7
         | DummyB8 | DummyB9 | DummyBA | DummyBB | DummyBC | DummyBD | DummyBE | DummyBF
         | DummyC0 | DummyC1 | DummyC2 | DummyC3 | DummyC4 | DummyC5 | DummyC6 | DummyC7
         | DummyC8 | DummyC9 | DummyCA | DummyCB | DummyCC | DummyCD | DummyCE | DummyCF
         | DummyD0 | DummyD1 | DummyD2 | DummyD3 | DummyD4 | DummyD5 | DummyD6 | DummyD7
         | DummyD8 | DummyD9 | DummyDA | DummyDB | DummyDC | DummyDD | DummyDE | DummyDF
         -- 0xE0
         | LeftControl
         | LeftShift
         | LeftAlt
         | LeftGUI
         | RightControl
         | RightShift
         | RightAlt
         | RightGUI
         | DummyE8 | DummyE9 | DummyEA | DummyEB | DummyEC | DummyED | DummyEE | DummyEF
         -- 0xf0 : special datahand keycodes follow
         | Norm --FIXME this is like windows key???
         | NAS
         | NASLock
         | Function
         -- these are different codes from 'regular' keyboard metakeys
         | Shift
         | Control
         | Alt
         | DummyF7
         | DummyF8 | DummyF9 | DummyFA | DummyFB | DummyFC | DummyFD | DummyFE | DummyFF
         | Dummy100 | Dummy101 | Dummy102 | Dummy103 | Dummy104 | Dummy105 | Dummy106 | Dummy107
         | Dummy108 | Dummy109 | Dummy10A | Dummy10B | Dummy10C | Dummy10D | Dummy10E | Dummy10F
         | Dummy110 | Dummy111 | Dummy112 | Dummy113 | Dummy114 | Dummy115 | Dummy116 | Dummy117
         | Dummy118 | Dummy119 | Dummy11A | Dummy11B | Dummy11C | Dummy11D
         | Bang
         | At
         -- 0x120
         | Hash
         | Dollar
         | Percent
         | Caret
         | Ampersand
         | Asterisk
         | LeftParenthesis
         | RightParenthesis
         | ShiftReturn -- FIXME: i dont think these keys have inherent shift states like alphanum
         | ShiftEscape
         | ShiftBackspace
         | ShiftTab
         | ShiftSpace
         | ShiftMinus
         | Plus
         | LeftCurlyBracket
         -- 0x130
         | RightCurlyBracket
         | Pipe
         | ShiftNumber -- what is shift+"number"???
         | Colon
         | DoubleQuote
         | Tilde
         | LeftAngleBracket
         | RightAngleBracket
         | QuestionMark
         | Dummy139 | Dummy13A | Dummy13B | Dummy13C | Dummy13D | Dummy13E | Dummy13F
         | Dummy140 | Dummy141 | Dummy142 | Dummy143 | Dummy144 | Dummy145 | Dummy146 | Dummy147
         | Dummy148 | Dummy149 | Dummy14A | Dummy14B | Dummy14C | Dummy14D | Dummy14E | Dummy14F
         | Dummy150 | Dummy151 | Dummy152 | Dummy153 | Dummy154 | Dummy155 | Dummy156 | Dummy157
         | Dummy158 | Dummy159 | Dummy15A | Dummy15B | Dummy15C | Dummy15D | Dummy15E | Dummy15F
         deriving (Show, Eq, Ord, Bounded, Enum)

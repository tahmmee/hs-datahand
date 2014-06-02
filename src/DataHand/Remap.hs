module DataHand.Remap where

data Pin = Pin {
    pinNumber :: Integer
}

data Port = Port {
    name :: String
  , bindings :: CBindings
  , pins :: [Pin]
}

data CBindings = CBindings {
    mode :: String
  , read :: String
  , write :: String
}

data Controller = Controller {
    model :: ControllerModel
  , ports :: [Port]
}

data ControllerModel = Atmel | Teensy

data Remap = Remap [(Pin, Pin)]

teensy = Controller {
    model = Teensy
  , ports = [
        Port {
            name = "D"
          , bindings = CBindings {
                mode = "DDRD"
              , read = "PIND"
              , write = "PORTD"
            }
          , pins = [
                Pin 3
              , Pin 4
              , Pin 5
              , Pin 6
              , Pin 7
              , Pin 8
              , Pin 9
              , Pin 10
            ]
        }
      , Port {
            name = "E"
          , bindings = CBindings {
                mode = "DDRE"
              , read = "PINE"
              , write = "PORTE"
            }
          , pins = [
                Pin 11
              , Pin 12
              , Pin 31
              , Pin 32
            ]
        }
      , Port {
            name = "C"
          , bindings = CBindings {
                mode = "DDRC"
              , read = "PINC"
              , write = "PORTC"
            }
          , pins = [
                Pin 13
              , Pin 14
              , Pin 15
              , Pin 16
              , Pin 17
              , Pin 18
              , Pin 19
              , Pin 20
            ]
        }
      , Port {
            name = "F"
          , bindings = CBindings {
                mode = "DDRF"
              , read = "PINF"
              , write = "PORTF"
            }
          , pins = [
                Pin 28
              , Pin 27
              , Pin 26
              , Pin 25
              , Pin 24
              , Pin 23
              , Pin 22
              , Pin 21
            ]
        }
      , Port {
            name = "PORTB"
          , bindings = CBindings {
                mode = "DDRB"
              , read = "PINB"
              , write = "PORTB"
            }
          , pins = [
                Pin 33
              , Pin 34
              , Pin 35
              , Pin 36
              , Pin 37
              , Pin 38
              , Pin 39
              , Pin 2
            ]
        }
  ]
}

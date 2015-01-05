#!/bin/bash
dhteensy="../dhteensy"
cabal configure && cabal build && dist/build/hs-datahand/hs-datahand | tee "${dhteensy}/keymaps-elitak.h"
cd "${dhteensy}" ; make ; cd -
teensy_loader_cli -mmcu=at90usb1286 -w "${dhteensy}/dhteensy.hex"

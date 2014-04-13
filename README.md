###Usage
1. Run something like hs-datahand | tee ~/dhteensy/keymaps.h to produce a header for dhteensy (https://github.com/imarko/dhteensy).
2. Use it to compile with your keymap embedded.
3. Run teensy_loader_cli -mmcu=at90usb1286 -w ~/dhteensy/dhteensy.hex


###TODO
* Norm is really windows key
* Fn mode doesnt stick
* figure out actual keycodes and which is actual winkey
* add mouse support

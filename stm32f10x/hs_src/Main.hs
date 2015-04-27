{-# LINE 1 "hs_src/Main.hsc" #-}
module Main where
{-# LINE 2 "hs_src/Main.hsc" #-}
import Foreign.Ptr
import Foreign.Storable

import Control.Monad hiding (join)
import Data.Foldable (toList)
import Data.List (intersperse)
import DataHand.Layout
import DataHand.Layouts.ProgrammerDvorak
import DataHand.USB.Descriptors
import Data.Bits

foreign import ccall "c_extern.h Delay" c_delay :: Word32 -> IO ()

{-# LINE 15 "hs_src/Main.hsc" #-}
gPIOA_BASE = nullPtr `plusPtr` 1073809408
{-# LINE 16 "hs_src/Main.hsc" #-}
gPIOB_BASE = nullPtr `plusPtr` 1073810432
{-# LINE 17 "hs_src/Main.hsc" #-}
gPIOC_BASE = nullPtr `plusPtr` 1073811456
{-# LINE 18 "hs_src/Main.hsc" #-}


-- see table 20 in manual for the 4 bits that control each pin on every port
-- (CNF1,CNF0,MODE1,MODE0) are the 4 bits, msb->lsb.
-- (0,0,0,1) is push-pull output, max speed 10Mhz
crl :: Ptr Word32 -> Ptr Word32
crl = (`plusPtr` 0x00) -- port control register (1st uint32 in the struct)
odr :: Ptr Word32 -> Ptr Word32
odr = (`plusPtr` 0x0c) -- Output Data Register pins (4th uint32 in the struct)

gpioOut :: Word32 -> IO ()
gpioOut regval = do
  forM_ [ gPIOA_BASE
        , gPIOB_BASE
        , gPIOC_BASE ] $ \port -> do
    poke (odr port) regval
  c_delay 500000 --micro or milli? i dont know

join :: String -> [String] -> String
join = (concat .) . intersperse

main :: IO ()
main = do
    --let layerToRawMap EmptyLayer = []
    --    layerToRawMap l = map fromEnum $ (toList . toRaw) l
    --return $ layerToRawMap (normal experimental)

    -- configure the pins?
    poke (crl gPIOA_BASE) 0x11111111 -- all output
    poke (crl gPIOB_BASE) 0x11111111 -- all output
    poke (crl gPIOC_BASE) 0x11111111 -- all output
    c_delay 500000
    forever $ do
        gpioOut 0xff
        gpioOut 0x00

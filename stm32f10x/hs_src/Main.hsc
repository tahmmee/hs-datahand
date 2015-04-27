{-# LANGUAGE CPP #-}
module Main where
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
#include "stm32f10x.h"

gpioPtr :: Ptr Word32
-- TODO: import these as defines via ffi
-- these are all #defined in lib/CMSIS/Core/CM3/stm32f10x.h
gpioPtr = odr
  where periphBase     = nullPtr        `plusPtr` 0x40000000
        apb2periphBase = periphBase     `plusPtr` 0x10000
        gpioaBase      = apb2periphBase `plusPtr` 0x0800 -- each gpio port addr points to a struct GPIO_TypeDef
        odr            = gpioaBase      `plusPtr` 12 -- Output Data Register pins (4th uint32 in the struct)

gpioOut :: Word32 -> IO ()
gpioOut v = do
  poke gpioPtr v
  c_delay 500000

join delim l = concat (intersperse delim l)

main :: IO ()
main = do
    let layerToRawMap EmptyLayer = []
        layerToRawMap l = map fromEnum $ (toList . toRaw) l
    return $ layerToRawMap (normal experimental)
    forever $ gpioOut 0xff
    --forever $ mapM_ gpioOut pat
    --where
    --  o13 = (1 `shiftL` 0)
    --  o15 = (1 `shiftL` 1)
    --  pat = [o13, o13 .|. o15, o15, 0]

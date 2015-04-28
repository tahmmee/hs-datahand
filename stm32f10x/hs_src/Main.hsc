{-# LANGUAGE RecordWildCards #-}
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
gPIOA_BASE = nullPtr `plusPtr` #const GPIOA_BASE
gPIOB_BASE = nullPtr `plusPtr` #const GPIOB_BASE
gPIOC_BASE = nullPtr `plusPtr` #const GPIOC_BASE
-- TODO: import the struct and use names as offsets via ffi (sounds like this isnt supported)

data GPIO = GPIO { crl :: Word32
                 , crh :: Word32
                 , idr :: Word32
                 , odr :: Word32
                 , bsrr :: Word32
                 , brr :: Word32
                 , lckr :: Word32
                 }
instance Storable GPIO where
    sizeOf _                     = (#size GPIO_TypeDef)
    alignment                    = sizeOf
    peek ptr                     = do crl   <- (#peek GPIO_TypeDef, CRL)      ptr
                                      crh   <- (#peek GPIO_TypeDef, CRH)      ptr
                                      idr   <- (#peek GPIO_TypeDef, IDR)      ptr
                                      odr   <- (#peek GPIO_TypeDef, ODR)      ptr
                                      bsrr  <- (#peek GPIO_TypeDef, BSRR)     ptr
                                      brr   <- (#peek GPIO_TypeDef, BRR)      ptr
                                      lckr  <- (#peek GPIO_TypeDef, LCKR)     ptr
                                      return $ GPIO crl crh idr odr bsrr brr lckr
    poke ptr (GPIO crl crh idr odr bsrr brr lckr) = do
                           (#poke GPIO_TypeDef, CRL)   ptr crl
                           (#poke GPIO_TypeDef, CRH)   ptr crh
                           (#poke GPIO_TypeDef, IDR)   ptr idr
                           (#poke GPIO_TypeDef, ODR)   ptr odr
                           (#poke GPIO_TypeDef, BSRR)  ptr bsrr
                           (#poke GPIO_TypeDef, BRR)   ptr brr
                           (#poke GPIO_TypeDef, LCKR)  ptr lckr


-- see table 20 in manual for the 4 bits that control each pin on every port
-- (CNF1,CNF0,MODE1,MODE0) are the 4 bits, msb->lsb.
-- (0,0,0,1) is push-pull output, max speed 10Mhz
--crl :: Ptr Word32 -> Ptr Word32
--crl = (`plusPtr` 0x00) -- port control register (1st uint32 in the struct)
--crl = (#poke GPIO_TypeDef, CRL)
--odr :: Ptr Word32 -> Ptr Word32
--odr = (`plusPtr` 0x0c) -- Output Data Register pins (4th uint32 in the struct)
--odr = (#poke GPIO_TypeDef, ODR)

gpioOut :: Word32 -> IO ()
gpioOut regval = do
  forM_ [ gPIOA_BASE
        , gPIOB_BASE
        , gPIOC_BASE ] $ \port -> do
    gpio :: GPIO <- peek port
    poke port gpio{odr=regval}
  c_delay 500000 --micro or milli? i dont know

join :: String -> [String] -> String
join = (concat .) . intersperse

main :: IO ()
main = do
    --let layerToRawMap EmptyLayer = []
    --    layerToRawMap l = map fromEnum $ (toList . toRaw) l
    --return $ layerToRawMap (normal experimental)

    -- configure the pins. needed? manual says they default to input...
    forM_ [ gPIOA_BASE
          , gPIOB_BASE
          , gPIOC_BASE ] $ \port -> do
    gpio :: GPIO <- peek port
    poke port gpio{crl=0x11111111}
    c_delay 500000

    mapM_ gpioOut pat
    where
      o13 = (1 `shiftL` 13)
      o15 = (1 `shiftL` 15)
      pat = [o13, o13 .|. o15, o15, 0]

   -- forever $ do
   --     gpioOut 0xff
   --     gpioOut 0x00

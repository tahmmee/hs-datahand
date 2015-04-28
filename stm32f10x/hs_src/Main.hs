{-# LINE 1 "hs_src/Main.hsc" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LINE 2 "hs_src/Main.hsc" #-}
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

{-# LINE 16 "hs_src/Main.hsc" #-}
gPIOA_BASE = nullPtr `plusPtr` 1073809408
{-# LINE 17 "hs_src/Main.hsc" #-}
gPIOB_BASE = nullPtr `plusPtr` 1073810432
{-# LINE 18 "hs_src/Main.hsc" #-}
gPIOC_BASE = nullPtr `plusPtr` 1073811456
{-# LINE 19 "hs_src/Main.hsc" #-}
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
    sizeOf _                     = ((28))
{-# LINE 31 "hs_src/Main.hsc" #-}
    alignment                    = sizeOf
    peek ptr                     = do crl   <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))      ptr
{-# LINE 33 "hs_src/Main.hsc" #-}
                                      crh   <- ((\hsc_ptr -> peekByteOff hsc_ptr 4))      ptr
{-# LINE 34 "hs_src/Main.hsc" #-}
                                      idr   <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))      ptr
{-# LINE 35 "hs_src/Main.hsc" #-}
                                      odr   <- ((\hsc_ptr -> peekByteOff hsc_ptr 12))      ptr
{-# LINE 36 "hs_src/Main.hsc" #-}
                                      bsrr  <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))     ptr
{-# LINE 37 "hs_src/Main.hsc" #-}
                                      brr   <- ((\hsc_ptr -> peekByteOff hsc_ptr 20))      ptr
{-# LINE 38 "hs_src/Main.hsc" #-}
                                      lckr  <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))     ptr
{-# LINE 39 "hs_src/Main.hsc" #-}
                                      return $ GPIO crl crh idr odr bsrr brr lckr
    poke ptr (GPIO crl crh idr odr bsrr brr lckr) = do
                           ((\hsc_ptr -> pokeByteOff hsc_ptr 0))   ptr crl
{-# LINE 42 "hs_src/Main.hsc" #-}
                           ((\hsc_ptr -> pokeByteOff hsc_ptr 4))   ptr crh
{-# LINE 43 "hs_src/Main.hsc" #-}
                           ((\hsc_ptr -> pokeByteOff hsc_ptr 8))   ptr idr
{-# LINE 44 "hs_src/Main.hsc" #-}
                           ((\hsc_ptr -> pokeByteOff hsc_ptr 12))   ptr odr
{-# LINE 45 "hs_src/Main.hsc" #-}
                           ((\hsc_ptr -> pokeByteOff hsc_ptr 16))  ptr bsrr
{-# LINE 46 "hs_src/Main.hsc" #-}
                           ((\hsc_ptr -> pokeByteOff hsc_ptr 20))   ptr brr
{-# LINE 47 "hs_src/Main.hsc" #-}
                           ((\hsc_ptr -> pokeByteOff hsc_ptr 24))  ptr lckr
{-# LINE 48 "hs_src/Main.hsc" #-}


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

module Main where
import Foreign.Ptr
import Foreign.Storable

import Control.Monad hiding (join)
import Data.Foldable (toList)
import Data.List (intersperse)
import DataHand.Layout
import DataHand.Layouts.ProgrammerDvorak
import DataHand.USB.Descriptors

foreign import ccall "c_extern.h Delay" c_delay :: Word32 -> IO ()

gpioPtr :: Ptr Word32
gpioPtr = odr
  where periphBase     = nullPtr        `plusPtr` 0x40000000
        arb2periphBase = periphBase     `plusPtr` 0x10000
        gpioaBase      = arb2periphBase `plusPtr` 0x0800
        odr            = gpioaBase      `plusPtr` 12

gpioOut :: Word32 -> IO ()
gpioOut v = do
  poke gpioPtr v
  c_delay 500000

join delim l = concat (intersperse delim l)
main :: IO ()
main = do
    let layerToRawMap EmptyLayer = []
        layerToRawMap l = map fromEnum $ (toList . toRaw) l
--    let dumpRawHeader Layout{normal=normal, nas=nas, function=function} = join "\n" [
--            "// This header was generated using https://github.com/elitak/hs-datahand"
--          , "const char PROGMEM normal_keys [] = {" ++ join ", " (map show $ layerToRawMap normal  ) ++ "};"
--          , "const char PROGMEM    nas_keys [] = {" ++ join ", " (map show $ layerToRawMap nas     ) ++ "};"
--          , "const char PROGMEM     fn_keys [] = {" ++ join ", " (map show $ layerToRawMap function) ++ "};"
--            ]
    --hdr <- return $ dumpRawHeader experimental
    return $ layerToRawMap (normal experimental)
    return ()
--main = forever $ mapM_ gpioOut pat
--  where
--    o13 = (1 `shiftL` 13)
--    o15 = (1 `shiftL` 15)
--    pat = [o13, o13 .|. o15, o15, 0]

{--
lib/CMSIS/Core/CM3/stm32f10x.h
define PERIPH_BASE           ((uint32_t)0x40000000) /*!< SRAM base address in
define APB2PERIPH_BASE       (PERIPH_BASE + 0x10000)
define GPIOA_BASE            (APB2PERIPH_BASE + 0x0800)
typedef struct
{
  __IO uint32_t CRL;
  __IO uint32_t CRH;
  __IO uint32_t IDR;
  __IO uint32_t ODR;
  __IO uint32_t BSRR;
  __IO uint32_t BRR;
  __IO uint32_t LCKR;
} GPIO_TypeDef;
--}

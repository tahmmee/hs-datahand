import Data.Word
import Data.Bits
import Control.Monad hiding (join)
import Foreign.Ptr
import Foreign.Storable

import Data.Foldable (toList)
import Data.List (intersperse)
import DataHand.Layout
import DataHand.Layouts.ProgrammerDvorak
import System.USB.Descriptors
import Data.EnumSet as ES hiding (empty)
import Jhc.Enum as E

import Data.ByteString (empty)

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
        layerToRawMap l = map E.fromEnum $ (toList . toRaw) l
--    let dumpRawHeader Layout{normal=normal, nas=nas, function=function} = join "\n" [
--            "// This header was generated using https://github.com/elitak/hs-datahand"
--          , "const char PROGMEM normal_keys [] = {" ++ join ", " (map show $ layerToRawMap normal  ) ++ "};"
--          , "const char PROGMEM    nas_keys [] = {" ++ join ", " (map show $ layerToRawMap nas     ) ++ "};"
--          , "const char PROGMEM     fn_keys [] = {" ++ join ", " (map show $ layerToRawMap function) ++ "};"
--            ]
    --hdr <- return $ dumpRawHeader experimental
    return $ layerToRawMap (normal experimental)
    --forever $ gpioOut 1
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

-- TODO move descriptor reporting related stuff to DataHand.USB.Descriptors or so

device_descriptor = DeviceDesc {
    deviceUSBSpecReleaseNumber = (0, 0, 0, 2)
  , deviceReleaseNumber  = (0, 0, 0, 1)
  , deviceClass = 0
  , deviceSubClass = 0
  , deviceProtocol = 0
  , deviceMaxPacketSize0 = 32
  , deviceVendorId = 0x16C0 -- XXX endswap?
  , deviceProductId = 0x047D
  , deviceNumConfigs = 1
  , deviceSerialNumberStrIx = Just 0
  , deviceManufacturerStrIx = Just 1
  , deviceProductStrIx = Just 2
-- FIXME unaccounted for:
--        18,                                     // bLength
--        1,                                      // bDescriptorType
}

config_descriptor = ConfigDesc
    {
      configValue = 1 -- TODO i shouldnt have to manually enum these. constructors should resolve these for me. or atleast not by index?

    , configStrIx = Just 0
    , configAttribs = fromEnums [SelfPowered, USB1BusPowered] -- XXX check this matches 0xC0
    , configMaxPower = 50
    , configInterfaces = []
    , configExtra = empty
    }
-- TODO make this all compilable by ghc as a cabal library, using ifdefs to differentiate ghc/jhc if necessary, then build a test harness around it, first as basic exe then quickcheck etc.


--const uint8_t PROGMEM config1_descriptor[CONFIG1_DESC_SIZE] = {
--        // configuration descriptor, USB spec 9.6.3, page 264-266, Table 9-10
--        9,                                      // bLength;
--        2,                                      // bDescriptorType;
--        LSB(CONFIG1_DESC_SIZE),                 // wTotalLength
--        MSB(CONFIG1_DESC_SIZE),
--
--        2,                                      // bNumInterfaces
--        1,                                      // bConfigurationValue
--        0,                                      // iConfiguration
--        0xC0,                                   // bmAttributes
--        50,                                     // bMaxPower
--        
--
--
--        // interface descriptor, USB spec 9.6.5, page 267-269, Table 9-12
--        9,                                      // bLength
--        4,                                      // bDescriptorType
--        KEYBOARD_INTERFACE,                     // bInterfaceNumber
--        0,                                      // bAlternateSetting
--        1,                                      // bNumEndpoints
--        0x03,                                   // bInterfaceClass (0x03 = HID)
--        0x01,                                   // bInterfaceSubClass (0x01 = Boot)
--        0x01,                                   // bInterfaceProtocol (0x01 = Keyboard)
--        0,                                      // iInterface
--
--
--
--
--        // HID interface descriptor, HID 1.11 spec, section 6.2.1
--        9,                                      // bLength
--        0x21,                                   // bDescriptorType
--        0x11, 0x01,                             // bcdHID
--        0,                                      // bCountryCode
--        1,                                      // bNumDescriptors
--        0x22,                                   // bDescriptorType
--        sizeof(keyboard_hid_report_desc),       // wDescriptorLength
--        0,
--
--
--
--        // endpoint descriptor, USB spec 9.6.6, page 269-271, Table 9-13
--        7,                                      // bLength
--        5,                                      // bDescriptorType
--        KEYBOARD_ENDPOINT | 0x80,               // bEndpointAddress
--        0x03,                                   // bmAttributes (0x03=intr)
--        KEYBOARD_SIZE, 0,                       // wMaxPacketSize
--        1,                                      // bInterval
--
--
--
--        // interface descriptor, USB spec 9.6.5, page 267-269, Table 9-12
--        9,                                      // bLength
--        4,                                      // bDescriptorType
--          DEBUG_INTERFACE,                        // bInterfaceNumber
--        0,                                      // bAlternateSetting
--        1,                                      // bNumEndpoints
--        0x03,                                   // bInterfaceClass (0x03 = HID)
--        0x00,                                   // bInterfaceSubClass
--        0x00,                                   // bInterfaceProtocol
--        0,                                      // iInterface
--
--
--
--
--        // HID interface descriptor, HID 1.11 spec, section 6.2.1
--        9,                                      // bLength
--        0x21,                                   // bDescriptorType
--        0x11, 0x01,                             // bcdHID
--        0,                                      // bCountryCode
--        1,                                      // bNumDescriptors
--        0x22,                                   // bDescriptorType
--        sizeof(debug_hid_report_desc),          // wDescriptorLength
--        0,
--
--
--
--
--        // endpoint descriptor, USB spec 9.6.6, page 269-271, Table 9-13
--        7,                                      // bLength
--        5,                                      // bDescriptorType
--        DEBUG_TX_ENDPOINT | 0x80,               // bEndpointAddress
--        0x03,                                   // bmAttributes (0x03=intr)
--        DEBUG_TX_SIZE, 0,                       // wMaxPacketSize
--        1                                       // bInterval
--};


module DataHand.USB.Descriptors where
import System.USB.Descriptors
import Data.EnumSet as ES hiding (empty)
import Jhc.Enum as E

import Data.ByteString (empty)

deviceDescriptor = DeviceDesc {
    deviceUSBSpecReleaseNumber = (0, 0, 0, 2)
  , deviceReleaseNumber  = (0, 0, 0, 1)
  , deviceClass = 0
  , deviceSubClass = 0
  , deviceProtocol = 0
  , deviceMaxPacketSize0 = 32
  , deviceVendorId = 0x16C0 -- XXX endswap?
  , deviceProductId = 0x047D
  , deviceNumConfigs = 1 -- XXX should be calculated based on length of [Config]
  , deviceSerialNumberStrIx = Just 0
  , deviceManufacturerStrIx = Just 1
  , deviceProductStrIx = Just 2
-- FIXME unaccounted for:
--        18,                                     // bLength
--        1,                                      // bDescriptorType
}

configDescriptor = ConfigDesc
    {
      configValue = 1 -- TODO i shouldnt have to manually enum these. constructors should resolve these for me. or atleast not by index?

    , configStrIx = Just 0
    , configAttribs = fromEnums [SelfPowered, USB1BusPowered] -- XXX check this matches 0xC0 XXX i dont like that i have to remember to put USB1BusPowered everywhere. As I understand it, in usb2.0, this legacy 1.0 flag must always beset.
    , configMaxPower = 50 -- TODO is this mA? will giving more power help longer cable + uncooperative port combinations i was encountering with the teensy? stm32 prly takes a good deal more power to begin with...
    , configInterfaces = []
    , configExtra = empty
    }
-- TODO make this all compilable by ghc as a cabal library, using ifdefs to differentiate ghc/jhc if necessary, then build a test harness around it, first as basic exe, then quickcheck etc.


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


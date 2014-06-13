module System.USB.Descriptors
    (
      DeviceDesc(..)
    , ReleaseNumber
    , VendorId, ProductId
    , ConfigDesc(..)
    , ConfigAttribs
    , DeviceStatus(..)
    , Interface
    , InterfaceDesc(..)
    , EndpointDesc(..)
    , EndpointAddress(..)
    , TransferDirection(..)
    , EndpointAttribs
    , TransferType(..)
    , Synchronization(..)
    , Usage(..)
    , MaxPacketSize(..)
    , TransactionOpportunities(..)
    --, maxIsoPacketSize
    --, LangId, PrimaryLangId, SubLangId
    , StrIx
    ) where

import Data.ByteString (ByteString)
import Data.EnumSet

data DeviceDesc = DeviceDesc
 { -- | USB specification release number.
      deviceUSBSpecReleaseNumber :: !ReleaseNumber

      -- | USB-IF class code for the device.
    , deviceClass :: !Word8

      -- | USB-IF subclass code for the device, qualified by the 'deviceClass'
      -- value.
    , deviceSubClass :: !Word8

      -- | USB-IF protocol code for the device, qualified by the 'deviceClass'
      -- and 'deviceSubClass' values.
    , deviceProtocol :: !Word8

      -- | Maximum packet size for endpoint 0.
    , deviceMaxPacketSize0 :: !Word8

      -- | USB-IF vendor ID.
    , deviceVendorId :: !VendorId

      -- | USB-IF product ID.
    , deviceProductId :: !ProductId

      -- | Device release number.
    , deviceReleaseNumber :: !ReleaseNumber

      -- | Optional index of string descriptor describing manufacturer.
    , deviceManufacturerStrIx :: !(Maybe StrIx)

      -- | Optional index of string descriptor describing product.
    , deviceProductStrIx :: !(Maybe StrIx)

      -- | Optional index of string descriptor containing device serial number.
    , deviceSerialNumberStrIx :: !(Maybe StrIx)

      -- | Number of possible configurations.
    , deviceNumConfigs :: !Word8
}

type ReleaseNumber = (Int, Int, Int, Int) -- this is a BinaryCodedDecimal (4bits per digit)
type VendorId  = Word16
type ProductId = Word16
type StrIx = Word8

-- XXX NOTE to self. If i get stuck or tired in typing all these structs, just
-- send raw ones copied from C source as a binary blob.

type ConfigValue = Word8
data ConfigDesc = ConfigDesc
    { -- | Identifier value for the configuration.
      configValue :: !ConfigValue

      -- | Optional index of string descriptor describing the configuration.
    , configStrIx :: !(Maybe StrIx)

      -- | Configuration characteristics.
    , configAttribs :: !ConfigAttribs

      -- | Maximum power consumption of the USB device from the bus in the
      -- configuration when the device is fully operational.  Expressed in 2 mA
      -- units (i.e., 50 = 100 mA).
    , configMaxPower :: !Word8

      -- | Vector of interfaces supported by the configuration.
    , configInterfaces :: ![Interface]

      -- | Extra descriptors. If @libusb@ encounters unknown configuration
      -- descriptors, it will store them here, should you wish to parse them.
    , configExtra :: !ByteString

    } --deriving (Show, Read, Eq)

-- | The USB 2.0 specification specifies that the configuration attributes only
-- describe the device status.
type ConfigAttribs = DeviceStatus

deviceStatus = fromEnum DeviceCapability

data DeviceCapability = 
    RemoteWakeup          -- ^ The Remote Wakeup field indicates whether the
                           --   device is currently enabled to request remote
                           --   wakeup. The default mode for devices that
                           --   support remote wakeup is disabled.
  | SelfPowered           -- ^ The Self Powered field indicates whether the
                           --   device is currently self-powered
--instance Eq DeviceStatus where
--    (a :: DeviceStatus) == (b :: DeviceStatus) = unpackWord8LE 

--instance Num DeviceStatus where
--    a + b = a .|. b
--    a * b = a .&. b
--
--instance Bits DeviceStatus where
--    b1 .&. b2 = 1 `shiftL` (fromEnum b1)
--    b1 .|. b2 = b1

data InterfaceDesc = InterfaceDesc
    { -- | Number of the interface.
      interfaceNumber :: !InterfaceNumber
      -- | Value used to select the alternate setting for the interface.
    , interfaceAltSetting :: !InterfaceAltSetting
      -- | USB-IF class code for the interface.
    , interfaceClass :: !Word8
      -- | USB-IF subclass code for the interface, qualified by the
      -- 'interfaceClass' value.
    , interfaceSubClass :: !Word8
      -- | USB-IF protocol code for the interface, qualified by the
      -- 'interfaceClass' and 'interfaceSubClass' values.
    , interfaceProtocol :: !Word8
      -- | Optional index of string descriptor describing the interface.
    , interfaceStrIx :: !(Maybe StrIx)
      -- | Vector of endpoints supported by the interface.
    , interfaceEndpoints :: ![EndpointDesc]

      -- | Extra descriptors. If @libusb@ encounters unknown interface
      -- descriptors, it will store them here, should you wish to parse them.
    , interfaceExtra :: !ByteString
    } --deriving (COMMON_INSTANCES)

-- | An interface is represented as a vector of alternate interface settings.
type Interface = [InterfaceDesc]


type InterfaceNumber = Word8
type InterfaceAltSetting = Word8

data EndpointDesc = EndpointDesc
    { -- | The address of the endpoint described by the descriptor.
      endpointAddress :: !EndpointAddress
    -- | Attributes which apply to the endpoint when it is configured using the
    -- 'configValue'.
    , endpointAttribs :: !EndpointAttribs
    -- | Maximum packet size the endpoint is capable of sending/receiving.
    , endpointMaxPacketSize :: !MaxPacketSize
    -- | Interval for polling endpoint for data transfers. Expressed in frames
    -- or microframes depending on the device operating speed (i.e., either 1
    -- millisecond or 125 &#956;s units).
    , endpointInterval :: !Word8
    -- | /For audio devices only:/ the rate at which synchronization feedback
    -- is provided.
    , endpointRefresh :: !Word8
    -- | /For audio devices only:/ the address of the synch endpoint.
    , endpointSynchAddress :: !Word8
    -- | Extra descriptors. If @libusb@ encounters unknown endpoint descriptors,
    -- it will store them here, should you wish to parse them.
    , endpointExtra :: !ByteString
    } --deriving (COMMON_INSTANCES)

type EndpointAttribs = TransferType

-- | Describes what types of transfers are allowed on the endpoint.
data TransferType =
          -- | Control transfers are typically used for command and status
          -- operations.
          Control

          -- | Isochronous transfers occur continuously and periodically.
        | Isochronous !Synchronization !Usage

          -- | Bulk transfers can be used for large bursty data.
        | Bulk

          -- | Interrupt transfers are typically non-periodic, small device
          -- \"initiated\" communication requiring bounded latency.
        | Interrupt
          --deriving (COMMON_INSTANCES)

-- | See section 5.12.4.1 of the USB 2.0 specification.
data Synchronization =
          NoSynchronization
        | Asynchronous -- ^ Unsynchronized,
                       --   although sinks provide data rate feedback.
        | Adaptive     -- ^ Synchronized using feedback or feedforward
                       --   data rate information
        | Synchronous  -- ^ Synchronized to the USB’s SOF (/Start Of Frame/)
          --deriving (Enum, COMMON_INSTANCES)

-- | See section 5.12.4.2 of the USB 2.0 specification.
data Usage = Data
           | Feedback
           | Implicit
             --deriving (Enum, COMMON_INSTANCES)


---------------





-- | The address of an endpoint.
data EndpointAddress = EndpointAddress
    { endpointNumber    :: !Int -- ^ Must be >= 0 and <= 15
    , transferDirection :: !TransferDirection
    } --deriving (COMMON_INSTANCES)

-- | The direction of data transfer relative to the host.
data TransferDirection = Out -- ^ Out transfer direction (host -> device) used
                             --   for writing.
                       | In  -- ^ In transfer direction (device -> host) used
                             --   for reading.
                 --deriving (COMMON_INSTANCES)
data MaxPacketSize = MaxPacketSize
    { maxPacketSize            :: !Size
    , transactionOpportunities :: !TransactionOpportunities
    } --deriving (COMMON_INSTANCES)

data TransactionOpportunities = Zero -- ^ None (1 transaction per microframe)
                              | One  -- ^ 1 additional (2 per microframe)
                              | Two  -- ^ 2 additional (3 per microframe)
         --deriving (Enum, Ord, COMMON_INSTANCES)

type Size = Int


-- TODO: cant use ffi to make use of libusb... so will have to marshall manually, ordering bytes correctly. maybe lookup an example of how to do this properly.

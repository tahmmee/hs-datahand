{- |
Extract and inject an Enum value into an EnumSet.
-}
module Data.EnumSet.PackedEnum
   {-# DEPRECATED "use Data.FlagSet instead" #-}
   (T(Cons), unpack, pack, clear, put, )
   where

import qualified Data.EnumSet as ES

import qualified Data.Bits as B
import Data.Bits (Bits, (.&.), )


{- |
@ T w a b@ describes a contiguous set of bit indices into the word type @w@
where the indices are of type @a@ and the set of indices represent a value of type @b@.
-}
data T w a b = Cons w Int

{- |
Extract an enumeration value from the specified index set.
-}
unpack ::
   (Integral w, Bits w, Enum a, Enum b) =>
   T w a b -> ES.T w a -> b
unpack (Cons mask pos) =
   toEnum . fromIntegral . (mask .&.) .
   flip B.shiftR pos . ES.decons

{- |
Create an enumeration set, where an value of type @b@
is placed at the specified indices.
-}
pack ::
   (Num w, Bits w, Enum a, Enum b) =>
   T w a b -> b -> ES.T w a
pack (Cons mask pos) =
   ES.Cons . flip B.shiftL pos .
   (mask .&.) . fromIntegral . fromEnum

{- |
Clear all bits at the specified indices.
-}
clear ::
   (Bits w, Enum a, Enum b) =>
   T w a b -> ES.T w a -> ES.T w a
clear (Cons mask pos) =
   ES.Cons . (B.complement (B.shiftL mask pos) .&.) . ES.decons

{- |
Overwrite an enumset at the specified indices with the value of type @b@.
-}
put ::
   (Num w, Bits w, Enum a, Enum b) =>
   T w a b -> b -> ES.T w a -> ES.T w a
put set x =
   (pack set x ES..|.) . clear set

{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Foldable hiding (concat)
import Data.List
import DataHand.Layout
import DataHand.Layouts.ProgrammerDvorak

-- small exercise here of converting to point-free notation: composing the compose function hurts my brain o_O
--join delim l = concat (intersperse delim l)
--join delim = concat . (intersperse delim)
join = (concat .) . intersperse
--join = (.) ((.) concat) intersperse

-- TODO:  create converter for .xml LGS files

main = do
    -- TODO make tuple list and map across rather than copypasta here
    let dumpRawHeader Layout{..} = putStrLn $ join "\n" [
            "// This header was generated using https://github.com/elitak/hs-datahand"
          , "const uint16_t PROGMEM  normal_keys [] = {" ++ join ", " (dump normal  ) ++ "};"
          , "const uint16_t PROGMEM normalS_keys [] = {" ++ join ", " (dump normalS ) ++ "};"
          , "const uint16_t PROGMEM     nas_keys [] = {" ++ join ", " (dump nas     ) ++ "};"
          , "const uint16_t PROGMEM    nasS_keys [] = {" ++ join ", " (dump nasS    ) ++ "};"
          , "const uint16_t PROGMEM      fn_keys [] = {" ++ join ", " (dump function) ++ "};"
          , "const uint16_t PROGMEM     fnS_keys [] = {" ++ join ", " (dump functionS) ++ "};"
            ]

    dumpRawHeader experimental
    where 
          --TODO learning to be had here: add the EmptyLayer definition and fulfill the monomorphism restriction
          --layerToRawMap :: Show a => b -> [Int]
          --layerToRawMap EmptyLayer = []
          layerToRawMap = map fromEnum . toList . toRaw
          --dump :: Show a => Layer a -> [String]
          dump = map show . layerToRawMap


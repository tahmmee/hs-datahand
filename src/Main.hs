{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Foldable hiding (concat)
import Data.List
import DataHand.Layout
import DataHand.Layouts.ProgrammerDvorak

join delim l = concat (intersperse delim l)

-- TODO:  create converter for .xml LGS files

main = do
    let layerToRawMap EmptyLayer = []
        layerToRawMap l = map fromEnum $ (toList . toRaw) l

    let dumpRawHeader Layout{..} = putStrLn $ join "\n" [
            "const char PROGMEM normal_keys [] = {" ++ join ", " (map show $ layerToRawMap normal  ) ++ "};",
            "const char PROGMEM    nas_keys [] = {" ++ join ", " (map show $ layerToRawMap nas     ) ++ "};",
            "const char PROGMEM     fn_keys [] = {" ++ join ", " (map show $ layerToRawMap function) ++ "};"
            ]

    dumpRawHeader experimental

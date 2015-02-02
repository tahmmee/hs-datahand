module Main where

import Data.Foldable hiding (concat)
import Data.List
import DataHand.Layout
import DataHand.Layouts.ProgrammerDvorak

-- TODO:  create converter for .xml LGS files

main = do

    dumpRawHeader experimental
    where 
          join = (concat .) . intersperse
          layerToRawMap EmptyLayer = []
          layerToRawMap layer = map fromEnum . toList . toRaw $ layer
          dump = map show . layerToRawMap
          dumpRawHeader layout = putStrLn $ join "\n" $ [
              "// This header was generated using https://github.com/elitak/hs-datahand" ] ++
             ["const uint16_t PROGMEM  " ++ name ++ "_keys [] = {" ++ join ", " (dump $ tbl layout) ++ "};"
              | (name, tbl) <- tables ]
          tables = [ ("normal", normal)
                   , ("normalS", normalS)
                   , ("nas", nas)
                   , ("nasS", nasS)
                   , ("fn", function)
                   , ("fnS", functionS) ]

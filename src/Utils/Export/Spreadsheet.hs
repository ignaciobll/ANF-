module Utils.Export.Spreadsheet where


import           Data.List                      ( intersperse )
import           Utils.VarsTable                ( VarsTable(..)
                                                , getInt2VList
                                                )


exportVarsTable :: Show a => VarsTable a -> String
exportVarsTable = unlines . fmap (\(n, v) -> show n ++ "," ++ show v) . getInt2VList

exportStringVarsTable :: VarsTable String -> String
exportStringVarsTable = unlines . fmap (\(n, v) -> show n ++ "," ++ v) . getInt2VList

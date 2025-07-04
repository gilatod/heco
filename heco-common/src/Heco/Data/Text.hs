module Heco.Data.Text where

import Data.Text.Lazy qualified as TL
import Data.List (intersperse)
import Data.Function ((&))

formatList :: [TL.Text] -> TL.Text
formatList l =
    let inner = mconcat $ intersperse ", " l
    in "[" <> inner <> "]"

formatXML :: TL.Text -> [(TL.Text, TL.Text)] -> Maybe TL.Text -> TL.Text
formatXML tag attrs content =
    case content of
        Just c ->
            "<" <> tag <> attrsText <> ">" <> c <>
            "</" <> tag <> ">"
        Nothing ->
            "<" <> tag <> attrsText <> "/>"
    where
        attrsText =
            if null attrs
                then ""
                else attrs &
                    (" " <>) . TL.concat . intersperse " "
                    . map \(k, v) -> k <> "=\"" <> v <> "\""

formatQuoteXML :: TL.Text -> TL.Text -> TL.Text
formatQuoteXML tag content = formatXML tag [] (Just content)
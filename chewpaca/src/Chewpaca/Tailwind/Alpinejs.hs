{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Chewpaca.Tailwind.Alpinejs where

import           Text.Blaze.Html5               ( AttributeValue )
import           Text.Blaze.Internal            ( Attribute
                                                , attribute
                                                )

alpineClick :: AttributeValue -> Attribute
alpineClick = attribute "@click" " @click=\""

alpineXData :: AttributeValue -> Attribute
alpineXData = attribute "x-data" " x-data=\""

alpineXShow :: AttributeValue -> Attribute
alpineXShow = attribute "x-show" " x-show=\""

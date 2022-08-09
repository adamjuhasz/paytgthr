{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Chewpaca.Web.Users.Search where

import           Chewpaca.Tailwind.Classes      ( mediumButton
                                                , indigoColorButton
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderSearchBar :: Html
renderSearchBar = do
  H.form ! A.action "/users/search/text" ! A.method "get" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.label ! A.for "fulltext" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
          "Text to search for (name, email, phone, apto ids, address, bank name, etc)"
        H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
          H.input ! A.name "q" ! A.id "fulltext" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
        H.div ! A.class_ "mt-2" $ do
          H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
            H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
              "Search"

  H.form ! A.action "/users/search/uuid" ! A.method "get" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.label ! A.for "uuid" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
          "User id to go to"
        H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
          H.input ! A.name "q" ! A.id "uuid" ! A.placeholder "00000000-0000-0000-0000-000000000000" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
        H.div ! A.class_ "mt-2" $ do
          H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
            H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
              "Goto user"

  H.form ! A.action "/users/search/address" ! A.method "get" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.label ! A.for "address" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
          "Address only search (street, city, state (2 letter abbrv), zip"
        H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
          H.input ! A.name "q" ! A.id "address" ! A.placeholder "San Francisco" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
        H.div ! A.class_ "mt-2" $ do
          H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
            H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
              "Search address"

  H.form ! A.action "/users/search/banking" ! A.method "get" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.label ! A.for "banking" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
          "Banking search (account name, bank name, routing #, account #)"
        H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
          H.input ! A.name "q" ! A.id "banking" ! A.placeholder "Chime" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
        H.div ! A.class_ "mt-2" $ do
          H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
            H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
              "Search banking info"

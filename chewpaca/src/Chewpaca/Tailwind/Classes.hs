{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Chewpaca.Tailwind.Classes where

import           Text.Blaze.Html5               ( (!)
                                                , AttributeValue
                                                , Html
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

tableHeaderCellClasses :: AttributeValue
tableHeaderCellClasses =
  " px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider "

tableFirstCellClasses :: AttributeValue
tableFirstCellClasses =
  " px-6 py-4 whitespace-no-wrap text-sm leading-5 font-medium text-gray-900 "

tableCellClasses :: AttributeValue
tableCellClasses =
  " px-6 py-4 whitespace-no-wrap text-sm leading-5 text-gray-500 "

tableLinkClasses :: AttributeValue
tableLinkClasses = " text-indigo-600 hover:text-indigo-900 "

badgeClasses :: AttributeValue
badgeClasses =
  " px-2 inline-flex text-xs leading-5 font-semibold rounded-full "

-- inline brittany config for width
-- brittany-next-binding --columns 1000
dataTable :: Html -> Html
dataTable contents = do
  H.div ! A.class_ "flex flex-col" $ do
    H.div ! A.class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" $ do
      H.div ! A.class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" $ do
        H.div ! A.class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" $ do
          H.table ! A.class_ "min-w-full divide-y divide-gray-200" $ do
            contents

mediumButton :: AttributeValue
mediumButton =
  " inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150 "

indigoColorButton :: AttributeValue
indigoColorButton =
  " text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "

redColorButton :: AttributeValue
redColorButton =
  " text-white bg-red-600 hover:bg-red-500 focus:border-red-700 focus:shadow-outline-red active:bg-red-700 "

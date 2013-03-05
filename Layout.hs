{-# LANGUAGE OverloadedStrings #-}

module Layout where
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


front = template "test" $ "Hello, welcome to our page"

shortBody t b = template t b

template :: Html -> Html -> Html
template title body =
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
        H.body $ body 

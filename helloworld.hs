{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [parseRoutes|
/ RootR GET
|]
instance Yesod HelloWorld 
getRootR :: Handler RepHtml
getRootR = defaultLayout [whamlet| <h1> Hello world!|]
main :: IO ()
main = warpDebug 3000 HelloWorld

module KBYDoc
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    , URL (..)
    , UnorderedListItem (..)
    ) where

import qualified Data.Text as T

import HTML

--types {{{1
type Document = [BlockElem]

--want to add an "Other" fallback type that just raw links to file that we use with normal links

--would like to be able to check if local URL points to an image... isImg function??
--also would like to store type of thing that a link is pointing to for image purposes... if no extension can just be "other"
--doing the above is really hard by parsing the link... 
--(eg this is an image https://static.wikia.nocookie.net/p__/images/c/ca/Kuroneko_ruri_gokou.png/revision/latest?cb=20120928143400&path-prefix=protagonist) 
--in theory i just need ".png" to show up somewhere optionally followed by a /, but more research is required. would it be
--possible to have an image url with no indication of it being an image? maybe something wacky like from a cdn? the real question
--is at that point do you even allow that? because file extensions are technically optional too...
--maybe i could do an http request and check the mimetype???????
--there also may be some advantage to cloneing the image and keeping it locally...
--looks like ill be needing a semantic analysis pass... maybe just do this in the build pass when im building out links... maybe or maybe parse.
--whereever i do it it needs access to to the map with the asset location

--replace ./ with root dir (maybe switch to %/??), 
--replace $/ with asset dir
-- ~ as shorcut for current dir?

data URL = RemoteRef { refSrc :: T.Text } 
         | PageRef { refSrc :: T.Text }
         | AssetRef { refSrc :: T.Text }
         deriving (Eq, Show, Ord)

data UnorderedListItem = UnorderedListItem [InlineElem] deriving (Eq, Show, Ord)

data BlockElem = Paragraph [InlineElem]
               | Header [InlineElem]
               | Subheader [InlineElem] 
               | UnorderedList [UnorderedListItem]
               | Image URL deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | Link [InlineElem] URL
                | PlainText T.Text deriving (Eq, Show, Ord)
--1}}}

--how to turn doc to html.
instance HTML BlockElem where
    htmlify (Header inner) = wrap inner H1
    htmlify (Subheader inner) = wrap inner H2
    htmlify (Paragraph inner) = wrap inner P
    htmlify (UnorderedList inner) = wrap inner UL
    --need to somehow ctahc and throw errors when i dont have an assetref here.
    htmlify (Image (AssetRef src)) = "<img src=\"/assets" ++ ( T.unpack src ) ++ "\">"
    htmlify (Image (RemoteRef src)) = "<img src=\"" ++ ( T.unpack src ) ++ "\">"

instance HTML InlineElem where
    htmlify (Bold inner) = wrap inner Strong
    htmlify (Italic inner) = wrap inner Em
    htmlify (Link title (PageRef url)) = wrap title (A $ T.unpack url)
    htmlify (Link title (AssetRef url)) = wrap title (A $ "/assets" ++ (T.unpack url) )
    htmlify (Link title (RemoteRef url)) = wrap title (A $ T.unpack url)
    htmlify (PlainText inner) = T.unpack inner 

instance HTML UnorderedListItem where
  htmlify (UnorderedListItem inner)  = wrap inner LI

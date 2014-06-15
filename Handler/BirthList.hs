{-# LANGUAGE OverloadedStrings #-}
module Handler.BirthLists (
      getBirthListsR, getBirthListR
    , listBirthLists, subCatPic
    ) where

import Import
import Control.Monad

import Handler.Home (productPic)

getBirthListsR :: Handler Html
getBirthListsR = undefined

getBirthListR :: BirthListId -> Handler Html
getBirthListR = undefined

listBirthLists :: YesodDB App [(Entity BirthList, Maybe (Entity Picture))]
listBirthLists = do
    bls <- selectList [] [Asc BirthListName]
    forM bls $ \blEntity@(Entity blId bl) -> do
        case birthListMainProduct bl of
            Just prodId -> do
                prod <- getJust prodId

            Nothing     -> return (blEntity, Nothing)

birthListPic :: Entity BirthList -> YesodDB App (Maybe (Entity Picture))
birthListPic bl
    | Just prodId <- birthListMainProduct bl = productPic prodId
    | otherwise                              = return Nothing

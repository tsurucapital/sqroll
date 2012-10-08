module Database.Sqroll.Table.Naming
    ( makeFieldName
    , unCamelCase
    ) where

import Data.Char (isUpper, toLower)
import Data.List (intercalate, isPrefixOf)

makeFieldName :: String -> String -> String
makeFieldName dtn fn = unCamelCase $
    if map toLower dtn `isPrefixOf` map toLower fn
        then drop (length dtn) fn
        else fn

unCamelCase :: String -> String
unCamelCase = intercalate "_" . map (map toLower) . caseGroup

-- | Group a name based on caps in a more or less intuitive way
--
-- > caseGroup "Person"      == ["Person"]
-- > caseGroup "IORef"       == ["IO", "Ref"]
-- > caseGroup "FooBar"      == ["Foo", "Bar"]
-- > caseGroup "RequestHTTP" == ["Request", "HTTP"]
--
caseGroup :: String -> [String]
caseGroup = mergeSingles . caseGroup'
  where
    mergeSingles xs = case rest of
        (y : ys) -> merged ++ [y] ++ mergeSingles ys
        []       -> merged
      where
        (ss, rest) = break (not . isSingle) xs
        merged     = if null ss then [] else [concat ss]

    isSingle [_] = True
    isSingle _   = False

    caseGroup' []    = []
    caseGroup' (h : str)
        | null xs   = [h : x]
        | null x    = [h] : caseGroup' xs
        | otherwise = (h : x) : caseGroup' xs
      where
        (x, xs) = break isUpper str

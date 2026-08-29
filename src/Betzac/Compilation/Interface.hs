{- | A file's compiled interface: what a dependent needs in order to compile against it,
and nothing else. Bodies stay behind the boundary, so an unchanged dependency can be
read rather than recompiled.
-}
module Betzac.Compilation.Interface (
    InterfaceHash,
    hashText,
    renderHash,
    InterfaceEntry,
    interfaceEntry,
    ieLabel,
    ieOrder,
    ieOrigin,
    ieSpan,
    DependencyStamp (..),
    Interface (..),
    interfaceHash,
    formatVersion,
    renderInterface,
    parseInterface,
) where

import Data.Bits (shiftR, xor, (.&.))
import Data.Char (ord)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Numeric (readHex, showHex)
import Text.Megaparsec (SourcePos (..), mkPos, unPos)

import Betzac.AST.Types (Labelling)
import Betzac.Span (HasSpan (..), Span (..))

{- | A 64-bit FNV-1a digest. Change detection only: a collision costs a stale build, not
a crash, which is the trade until something depends on it being cryptographic.
-}
newtype InterfaceHash = InterfaceHash Word64
    deriving (Eq, Ord, Show)

hashText :: Text -> InterfaceHash
hashText = InterfaceHash . T.foldl' char 14695981039346656037
  where
    -- Each code point as four bytes, so the digest does not depend on an encoder.
    char h c = foldl' byte h [(ord c `shiftR` s) .&. 0xff | s <- [0, 8, 16, 24]]
    byte h b = (h `xor` fromIntegral b) * 1099511628211

renderHash :: InterfaceHash -> String
renderHash (InterfaceHash w) = let s = showHex w "" in replicate (16 - length s) '0' ++ s

parseHash :: String -> Either String InterfaceHash
parseHash s = case readHex s of
    [(w, "")] -> Right $ InterfaceHash w
    _ -> Left $ "malformed hash: " ++ s

{- | One exported label, as a dependent sees it. 'ieOrigin' is the file that really
defines it, which is not the file re-exporting it.
-}
data InterfaceEntry = InterfaceEntry
    { ieLabel :: Labelling
    , ieOrder :: Int
    -- ^ Lexical position within 'ieOrigin', for tie-breaking.
    , ieOrigin :: FilePath
    , ieSpan :: Span
    -- ^ Where in 'ieOrigin', for go-to-definition.
    }
    deriving (Eq, Show)

instance HasSpan InterfaceEntry where
    getSpan = ieSpan

{- | The only way to build an entry: a span carries its own file name, and letting it
disagree with 'ieOrigin' is the one way this record can lie.
-}
interfaceEntry :: Labelling -> Int -> FilePath -> Span -> InterfaceEntry
interfaceEntry label order origin sp = InterfaceEntry label order origin (anchor sp)
  where
    anchor Generated = Generated
    anchor (RealSpan s e) = RealSpan s{sourceName = origin} e{sourceName = origin}

-- | A dependency as it stood when an interface was produced.
data DependencyStamp = DependencyStamp
    { dsPath :: FilePath
    , dsHash :: InterfaceHash
    }
    deriving (Eq, Show)

data Interface = Interface
    { ifSource :: InterfaceHash
    -- ^ Of the source text this was produced from.
    , ifDeps :: [DependencyStamp]
    , ifExports :: [InterfaceEntry]
    }
    deriving (Eq, Show)

{- | What dependents are entitled to notice. Deliberately blind to 'ifSource' and
'ifDeps': an edit that leaves the exports alone must leave this alone too, or nothing
downstream is spared.
-}
interfaceHash :: Interface -> InterfaceHash
interfaceHash = hashText . T.unlines . map renderExport . sortedExports

-- | Bumping this invalidates every artifact on disk.
formatVersion :: Int
formatVersion = 1

magic :: Text
magic = T.pack "betzac-bi"

sortedExports :: Interface -> [InterfaceEntry]
sortedExports = sortOn ieLabel . ifExports

renderInterface :: Interface -> Text
renderInterface i =
    T.unlines $
        T.unwords [magic, tshow formatVersion]
            : T.unwords [T.pack "source", T.pack $ renderHash (ifSource i)]
            : map renderDep (ifDeps i)
            ++ map renderExport (sortedExports i)

-- | The path goes last: it may contain spaces, and nothing else on a line may.
renderDep :: DependencyStamp -> Text
renderDep d = T.unwords [T.pack "dep", T.pack $ renderHash (dsHash d), T.pack $ dsPath d]

renderExport :: InterfaceEntry -> Text
renderExport e =
    T.unwords $
        [T.pack "export", T.pack (ieLabel e), tshow (ieOrder e)]
            ++ map tshow (renderSpan $ ieSpan e)
            ++ [T.pack $ ieOrigin e]

-- | Start and end as line/column pairs. Megaparsec counts from 1, so zeroes mean 'Generated'.
renderSpan :: Span -> [Int]
renderSpan Generated = [0, 0, 0, 0]
renderSpan (RealSpan s e) = [unPos (sourceLine s), unPos (sourceColumn s), unPos (sourceLine e), unPos (sourceColumn e)]

readSpan :: FilePath -> [Int] -> Either String Span
readSpan _ [0, 0, 0, 0] = Right Generated
readSpan origin [sl, sc, el, ec]
    | all (> 0) [sl, sc, el, ec] = Right $ RealSpan (pos sl sc) (pos el ec)
  where
    pos l c = SourcePos origin (mkPos l) (mkPos c)
readSpan _ ns = Left $ "malformed span: " ++ unwords (map show ns)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

{- | A malformed or stale-versioned artifact is a 'Left' for the caller to treat as a
cache miss, never as a compilation error: nothing a user wrote is wrong.
-}
parseInterface :: Text -> Either String Interface
parseInterface src = case map T.words (T.lines src) of
    [] -> Left "empty interface"
    (header : rest) -> checkHeader header >> body rest
  where
    checkHeader [m, v]
        | m /= magic = Left $ "not an interface file: " ++ T.unpack m
        | v /= tshow formatVersion = Left $ "interface format " ++ T.unpack v ++ ", expected " ++ show formatVersion
        | otherwise = Right ()
    checkHeader ws = Left $ "malformed header: " ++ T.unpack (T.unwords ws)

    body ls = do
        srcHash <- one [h | (k : h : _) <- ls, k == T.pack "source"]
        Interface <$> parseHash (T.unpack srcHash) <*> traverse dep deps <*> traverse export exports
      where
        deps = [ws | (k : ws) <- ls, k == T.pack "dep"]
        exports = [ws | (k : ws) <- ls, k == T.pack "export"]

    one [x] = Right x
    one _ = Left "expected exactly one source line"

    dep (h : rest) = flip DependencyStamp <$> parseHash (T.unpack h) <*> pure (T.unpack $ T.unwords rest)
    dep ws = Left $ "malformed dep line: " ++ T.unpack (T.unwords ws)

    export (label : order : sl : sc : el : ec : rest) = do
        n <- readInt order
        ns <- traverse readInt [sl, sc, el, ec]
        let origin = T.unpack $ T.unwords rest
        interfaceEntry (T.unpack label) n origin <$> readSpan origin ns
    export ws = Left $ "malformed export line: " ++ T.unpack (T.unwords ws)

    readInt t = case reads (T.unpack t) of
        [(n, "")] -> Right n
        _ -> Left $ "malformed integer: " ++ T.unpack t

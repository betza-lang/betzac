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
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
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
downstream is spared. Blind to the file table too, so a change in layout is not a
change in meaning.
-}
interfaceHash :: Interface -> InterfaceHash
interfaceHash = hashText . T.unlines . map digest . sortedExports

-- | One export, canonically, with the path last because it may hold spaces.
digest :: InterfaceEntry -> Text
digest e =
    T.unwords $
        [T.pack (ieLabel e), tshow (ieOrder e)]
            ++ map tshow (renderSpan $ ieSpan e)
            ++ [T.pack $ ieOrigin e]

-- | Bumping this invalidates every artifact on disk.
formatVersion :: Int
formatVersion = 1

magic :: Text
magic = T.pack "betzac-bi"

sortedExports :: Interface -> [InterfaceEntry]
sortedExports = sortOn ieLabel . ifExports

{- | Origins interned into a table. A label and a path may each hold spaces, and only
one free-form field can end a line, so the export line names its origin by index and
keeps its label last.
-}
originTable :: Interface -> [FilePath]
originTable = Set.toAscList . Set.fromList . map ieOrigin . ifExports

renderInterface :: Interface -> Text
renderInterface i =
    T.unlines $
        T.unwords [magic, tshow formatVersion]
            : T.unwords [T.pack "source", T.pack $ renderHash (ifSource i)]
            : map renderDep (ifDeps i)
            ++ zipWith renderOrigin [0 :: Int ..] origins
            ++ map renderExport (sortedExports i)
  where
    origins = originTable i
    indexOf o = Map.findWithDefault 0 o $ Map.fromList (zip origins [0 :: Int ..])
    renderExport e =
        T.unwords $
            [T.pack "export", tshow (ieOrder e)]
                ++ map tshow (renderSpan $ ieSpan e)
                ++ [tshow (indexOf $ ieOrigin e), T.pack (ieLabel e)]

-- | The path goes last: it may contain spaces, and nothing else on a line may.
renderDep :: DependencyStamp -> Text
renderDep d = T.unwords [T.pack "dep", T.pack $ renderHash (dsHash d), T.pack $ dsPath d]

renderOrigin :: Int -> FilePath -> Text
renderOrigin ix path = T.unwords [T.pack "file", tshow ix, T.pack path]

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

{- | Split off @n@ space-separated fields, keeping the rest of the line verbatim. Every
line ends in a path, and a path may hold spaces; nothing before it may.
-}
splitFields :: Int -> Text -> Maybe ([Text], Text)
splitFields n t
    | n <= 0 = Just ([], t)
    | otherwise = case T.breakOn space t of
        (f, rest) | not (T.null rest) -> consing f <$> splitFields (n - 1) (T.drop 1 rest)
        _ -> Nothing
  where
    space = T.pack " "
    consing f (fs, rest) = (f : fs, rest)

{- | A malformed or stale-versioned artifact is a 'Left' for the caller to treat as a
cache miss, never as a compilation error: nothing a user wrote is wrong.
-}
parseInterface :: Text -> Either String Interface
parseInterface src = case T.lines src of
    [] -> Left "empty interface"
    (header : rest) -> checkHeader header >> body rest
  where
    checkHeader line = case splitFields 1 line of
        Just ([m], v)
            | m /= magic -> Left $ "not an interface file: " ++ T.unpack m
            | v /= tshow formatVersion -> Left $ "interface format " ++ T.unpack v ++ ", expected " ++ show formatVersion
            | otherwise -> Right ()
        _ -> Left $ "malformed header: " ++ T.unpack line

    body ls = do
        let tagged = mapMaybe (splitFields 1) ls
            linesFor key = [rest | ([kw], rest) <- tagged, kw == T.pack key]
        srcHash <- one $ linesFor "source"
        origins <- Map.fromList <$> traverse origin (linesFor "file")
        Interface
            <$> parseHash (T.unpack srcHash)
            <*> traverse dep (linesFor "dep")
            <*> traverse (export origins) (linesFor "export")

    one [x] = Right x
    one _ = Left "expected exactly one source line"

    dep line = case splitFields 1 line of
        Just ([h], path) -> flip DependencyStamp <$> parseHash (T.unpack h) <*> pure (T.unpack path)
        _ -> Left $ "malformed dep line: " ++ T.unpack line

    origin :: Text -> Either String (Int, FilePath)
    origin line = case splitFields 1 line of
        Just ([ix], path) -> flip (,) (T.unpack path) <$> readInt ix
        _ -> Left $ "malformed file line: " ++ T.unpack line

    export origins line = case splitFields 6 line of
        Just ([order, sl, sc, el, ec, ix], label) -> do
            n <- readInt order
            ns <- traverse readInt [sl, sc, el, ec]
            path <- lookupOrigin origins =<< readInt ix
            interfaceEntry (T.unpack label) n path <$> readSpan path ns
        _ -> Left $ "malformed export line: " ++ T.unpack line

    lookupOrigin origins ix = maybe (Left $ "no file " ++ show ix ++ " in the table") Right $ Map.lookup ix origins

    readInt :: Text -> Either String Int
    readInt t = case reads (T.unpack t) of
        [(n, "")] -> Right n
        _ -> Left $ "malformed integer: " ++ T.unpack t

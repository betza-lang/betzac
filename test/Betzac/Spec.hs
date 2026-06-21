import qualified Lexer.CoreSpec as CoreSpec
import qualified Lexer.LexerQC as LexerQC
import qualified Lexer.LexerSpec as LexerSpec
import qualified Lexer.ScanSpec as ScanSpec
import qualified Parser.ParserHedgehog as ParserHedgehog
import Test.Hspec

main :: IO ()
main = do
    hspec CoreSpec.spec
    hspec ScanSpec.spec
    hspec LexerSpec.spec
    putStrLn "" >> putStrLn "========= Properties ========="
    hspec LexerQC.spec
    hspec ParserHedgehog.spec

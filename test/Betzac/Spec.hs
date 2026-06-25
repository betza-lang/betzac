import qualified Lexer.LexerQC as LexerQC
import qualified Lexer.LexerSpec as LexerSpec
import qualified Parser.ParserHedgehog as ParserHedgehog
import Test.Hspec

main :: IO ()
main = do
    hspec LexerSpec.spec
    putStrLn "" >> putStrLn "========= Properties ========="
    hspec LexerQC.spec
    hspec ParserHedgehog.spec

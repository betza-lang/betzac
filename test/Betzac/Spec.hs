import qualified Lexer.CoreSpec as CoreSpec
import qualified Lexer.LexerQC as LexerQC
import Test.Hspec

main :: IO ()
main = do
    hspec CoreSpec.spec
    putStrLn "" >> putStrLn "========= QuickCheck ========="
    hspec LexerQC.spec

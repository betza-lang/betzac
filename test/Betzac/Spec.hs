import qualified Compilation.DriverSpec as DriverSpec
import qualified Compilation.FlagSpec as FlagSpec
import qualified Compilation.ScopeSpec as ScopeSpec
import qualified Lexer.LexerQC as LexerQC
import qualified Lexer.LexerSpec as LexerSpec
import qualified Parser.ParserHedgehog as ParserHedgehog
import qualified Pipeline.RecoverySpec as RecoverySpec
import Test.Hspec

main :: IO ()
main = do
    hspec LexerSpec.spec
    hspec FlagSpec.spec
    hspec ScopeSpec.spec
    hspec DriverSpec.spec
    hspec RecoverySpec.spec
    putStrLn "" >> putStrLn "========= Properties ========="
    hspec LexerQC.spec
    hspec ParserHedgehog.spec

import qualified AST.GenericSpec as GenericSpec
import qualified Compilation.DriverSpec as DriverSpec
import qualified Compilation.FlagSpec as FlagSpec
import qualified Compilation.PreludeSpec as PreludeSpec
import qualified Compilation.ScopeSpec as ScopeSpec
import qualified Lexer.LexerQC as LexerQC
import qualified Lexer.LexerSpec as LexerSpec
import qualified Parser.ParserHedgehog as ParserHedgehog
import qualified Pipeline.RecoverySpec as RecoverySpec
import qualified Semantic.LiteralAssignmentSpec as LiteralAssignmentSpec
import qualified Semantic.RedundantModifierSpec as RedundantModifierSpec
import Test.Hspec

main :: IO ()
main = do
    hspec LexerSpec.spec
    hspec FlagSpec.spec
    hspec ScopeSpec.spec
    hspec DriverSpec.spec
    hspec PreludeSpec.spec
    hspec RecoverySpec.spec
    hspec LiteralAssignmentSpec.spec
    hspec RedundantModifierSpec.spec
    putStrLn "" >> putStrLn "========= Properties ========="
    hspec GenericSpec.spec
    hspec LexerQC.spec
    hspec ParserHedgehog.spec

module Compilation.FlagSpec (spec) where

import Betzac.Compilation.Flag (
    CompilerFlag (..),
    Wspecifier (..),
    applyOptions,
    optionsFromFlags,
 )
import Betzac.Diagnostic (
    SemanticProblem (..),
    SemanticProblemKind (DuplicateDirective, DuplicateLabel, RedundantOverride, UnresolvedLabel, UnusedUsing),
    Severity (Error, Warning),
    mkProblem,
 )
import Betzac.Span (Span (Generated))

import Test.Hspec (Spec, describe, it, shouldBe)

problem :: SemanticProblemKind -> SemanticProblem
problem k = mkProblem Warning k Generated

spec :: Spec
spec = describe "Compilation.Flag" $ do
    describe "applyOptions" $ do
        it "silences a Wunused-governed diagnostic by default (no flags)" $ do
            let opts = optionsFromFlags []
            length (applyOptions opts [problem DuplicateLabel]) `shouldBe` 0

        it "reveals a Wunused-governed diagnostic as a warning under -Wunused" $ do
            let opts = optionsFromFlags [GenerateWarnings Wunused]
            map semSev (applyOptions opts [problem DuplicateLabel]) `shouldBe` [Warning]

        it "promotes a Wunused-governed diagnostic to an error under -Werror=unused, even without -Wunused" $ do
            let opts = optionsFromFlags [PromoteWarningsToError Wunused]
            map semSev (applyOptions opts [problem DuplicateLabel]) `shouldBe` [Error]

        it "-w suppresses a diagnostic even when its own -W flag is also passed" $ do
            let opts = optionsFromFlags [SuppressWarnings, GenerateWarnings Wunused]
            length (applyOptions opts [problem DuplicateLabel]) `shouldBe` 0

        it "-Werror wins over -w" $ do
            let opts = optionsFromFlags [SuppressWarnings, PromoteWarningsToError Wunused]
            map semSev (applyOptions opts [problem DuplicateLabel]) `shouldBe` [Error]

        it "reveals a Wdirective-governed diagnostic under -Wdirective, independent of -Wunused" $ do
            let opts = optionsFromFlags [GenerateWarnings Wdirective]
            map semSev (applyOptions opts [problem DuplicateDirective]) `shouldBe` [Warning]
            length (applyOptions opts [problem DuplicateLabel]) `shouldBe` 0

        it "silences an unnecessary override by default, and reveals it under -Wdirective" $ do
            length (applyOptions (optionsFromFlags []) [problem RedundantOverride]) `shouldBe` 0
            map semSev (applyOptions (optionsFromFlags [GenerateWarnings Wdirective]) [problem RedundantOverride])
                `shouldBe` [Warning]

        it "promotes an unnecessary override under -Werror=directive" $ do
            let opts = optionsFromFlags [PromoteWarningsToError Wdirective]
            map semSev (applyOptions opts [problem RedundantOverride]) `shouldBe` [Error]

        it "does not reveal an unnecessary override under -Wunused, since it is not unused-governed" $ do
            let opts = optionsFromFlags [GenerateWarnings Wunused]
            length (applyOptions opts [problem RedundantOverride]) `shouldBe` 0

        it "governs an unused using by -Wunused, not -Wdirective" $ do
            length (applyOptions (optionsFromFlags [GenerateWarnings Wdirective]) [problem UnusedUsing])
                `shouldBe` 0
            map semSev (applyOptions (optionsFromFlags [GenerateWarnings Wunused]) [problem UnusedUsing])
                `shouldBe` [Warning]

        it "leaves an unconditional error untouched regardless of flags" $ do
            let opts = optionsFromFlags [SuppressWarnings]
                unconditional = mkProblem Error UnresolvedLabel Generated
            map semSev (applyOptions opts [unconditional]) `shouldBe` [Error]

        it "-w silences a warning that no -W flag governs" $ do
            let opts = optionsFromFlags [SuppressWarnings]
            length (applyOptions opts [problem UnresolvedLabel]) `shouldBe` 0

        it "keeps a warning that no -W flag governs when -w is absent" $ do
            let opts = optionsFromFlags []
            map semSev (applyOptions opts [problem UnresolvedLabel]) `shouldBe` [Warning]

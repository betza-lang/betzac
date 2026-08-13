module Compilation.FlagSpec (spec) where

import Betzac.Compilation.Flag (
    CompilerFlag (..),
    Wspecifier (..),
    applyOptions,
    optionsFromFlags,
 )
import Betzac.Diagnostic (
    SemanticProblem (..),
    SemanticProblemKind (DuplicateDirective, DuplicateLabel, UnresolvedLabel),
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

        it "leaves an unconditional diagnostic untouched regardless of flags" $ do
            let opts = optionsFromFlags [SuppressWarnings]
                unconditional = mkProblem Error UnresolvedLabel Generated
            map semSev (applyOptions opts [unconditional]) `shouldBe` [Error]

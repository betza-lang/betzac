module Betzac.Debug.PrettyPrint (PrettyPrint (..), Summarizable (..)) where

class PrettyPrint x where
    prettyPrint :: x -> String

class Summarizable x where
    summarize :: x -> String

{-# LANGUAGE DataKinds #-}
module Agda2Lambox.Compile.TypeScheme where

import Control.Monad.IO.Class ( liftIO )

import Agda.Compiler.Backend
import Agda.TypeChecking.Telescope (telView)
import Agda.TypeChecking.Substitute (TelV(TelV))

import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Type
import LambdaBox.Env

compileTypeScheme :: Definition -> CompileM (GlobalTypeDecl Typed)
compileTypeScheme Defn{..} = do
  TelV tyargs _ <- telView defType
  liftIO $ putStrLn "compiling type scheme"
  pure $ TypeAliasDecl $ Nothing

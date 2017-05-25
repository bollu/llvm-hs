{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses,
  OverloadedStrings
  #-}
module LLVM.Internal.Intrinsic where

import LLVM.Prelude

import Foreign.Ptr

import Foreign.Ptr
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as ByteString

import Foreign.C
import Foreign.Ptr

import qualified LLVM.Internal.FFI.InlineAssembly as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.Module as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

import qualified LLVM.AST as A (Definition(..))
import qualified LLVM.AST.InlineAssembly as A
import qualified LLVM.AST.Type as A

import LLVM.Internal.Coding 
import LLVM.Internal.EncodeAST
import LLVM.Internal.DecodeAST
import LLVM.Internal.Value

import qualified LLVM.AST as A (Definition(..))
import qualified LLVM.AST.InlineAssembly as A
import qualified LLVM.AST.Intrinsic as A
import LLVM.Internal.Coding
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST

instance DecodeM DecodeAST A.Intrinsic (Ptr FFI.Value) where
  decodeM v = undefined

instance EncodeM EncodeAST A.Intrinsic (Ptr FFI.Value) where
  encodeM v = undefined
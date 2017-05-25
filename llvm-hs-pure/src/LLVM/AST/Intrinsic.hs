module LLVM.AST.Intrinsic where
import LLVM.Prelude

data Intrinsic = CoroResume | CoroCreate | CoroDestroy deriving(Eq, Ord, Read, Show, Typeable, Data, Generic)

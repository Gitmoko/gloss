{-# LANGUAGE BangPatterns #-}
module Stage.Diffusion
        ( DiffSolver    (..)
        , diffusion)
where
import Model
import FieldElt
import Stage.Linear
import Data.Array.Repa          as R
import Data.Array.Repa.Eval     as R
import Data.Vector.Unboxed

-- ^陰解法か陽解法のどちらを使うかのデータ
data DiffSolver
        = DiffStable Int -- ^ IntはSolver.hsのlineraSolverに渡す、陰解法の中で使う反復法のステップ回数
        | DiffUnstable


-- | Diffuse a field at a certain rate.
diffusion 
        :: (FieldElt a, Num a, Elt a, Unbox a) 
        => DiffSolver -- ^陰解法か陽解法か
        -> Delta  -- ^1ステップで流れるシミュレーション空間での時間
        -> Rate -- ^拡散係数 ConfigのconfigDiff
        -> Field a -- ^計算前の場 
        -> IO (Field a) -- ^ 計算後の場
diffusion !solver !delta !rate field 
 = {-# SCC diffusion #-}
   field `deepSeqArray`  
   let  _ :. _ :. width' = R.extent field
        !width           = fromIntegral width'
   in   case solver of
         DiffUnstable
          -> let !a     = delta * rate * width * width
             in   unstableSolver field field a

         DiffStable iters
          -> let !a     = delta * rate * width * width
                 !c     = 1 + 4 * a
             in  linearSolver field field a c iters

{-# SPECIALIZE diffusion 
        :: DiffSolver -> Delta -> Rate
        -> Field Float 
        -> IO (Field Float) #-}

{-# SPECIALIZE diffusion 
        :: DiffSolver -> Delta -> Rate
        -> Field (Float, Float) 
        -> IO (Field (Float, Float)) #-}

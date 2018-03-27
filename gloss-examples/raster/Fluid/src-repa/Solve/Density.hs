
module Solve.Density
        (densitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Config
import Model

-- | Run the stages for processing the density field in one time step
-- | 密度場の計算
densitySteps 
        :: Config -- ^オプションの値
        -> Int -- ^　現在のステップ数。拡散係数がこの値のステップ数を境に変わる
        -> DensityField  -- ^密度場
        -> Maybe (SourceDensity Float) -- ^GUI上での密度入力。BatchモードだつねにNothing
        -> VelocityField -- ^ 速度場
        -> IO DensityField -- ^ 計算結果の密度場

densitySteps config step df ds vf 
 = {-# SCC "Solve.densitySteps" #-}
   do   df1     <- addSources   (configDelta config) (configDensity   config) 
                                ds df

        let diff = if  configDiffAfter config /= 0
                    && step >= configDiffAfter config 
                     then 0.0005
                     else configDiff config

        let diffSolver
                = if configUnstable config
                        then DiffUnstable
                        else DiffStable (configIters config)

        df2     <- diffusion    diffSolver (configDelta config) diff
                                df1

        df'     <- advection    (configDelta config) vf df2

        return  df'


module Solve.Velocity
        (velocitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Stage.Project
import Model
import Config

-- The pass that sets boundary conditions is buggy and 
-- currently disabled.
-- import Stage.Boundary
velocitySteps 
        :: Config
        -> Int -- ^つかってない
        -> VelocityField -- ^前のフレームの速度場
        -> Maybe (SourceDensity (Float, Float)) -- ^Interactiveモードで、GUI上で指定された位置に源が現れる。Batchモードだと常にNothing
        -> IO VelocityField -- ^更新後の速度場

velocitySteps config _step vf vs 
 = {-# SCC "Solve.velocitySteps" #-}
   do   
        vf1     <- addSources   (configDelta config) (configVelocity config)  --GUI上で指定したSourceDensityを加える
                                vs vf

        let diffSolver = DiffStable (configIters config)
        vf2     <- diffusion    diffSolver (configDelta config) (configVisc config) 
                                vf1 
--      vf3     <- setBoundary vf2

        vf4     <- project      (configIters config) vf2
--      vf5     <- setBoundary vf4

        vf6     <- advection    (configDelta config) vf4 vf4
--      vf7     <- setBoundary vf6

        vf8     <- project      (configIters config) vf6
--      vf'     <- setBoundary vf8

        return  vf8

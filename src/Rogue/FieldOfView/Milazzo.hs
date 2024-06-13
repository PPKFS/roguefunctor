module Rogue.FieldOfView.Milazzo where
{- INCOMPLETE
import Rogue.Prelude
import Rogue.Geometry.V2
import Rogue.FieldOfView.Visibility
import qualified Data.Vector as V
import Relude.Monad

newtype Slope = Slope V2

gte :: Slope -> Slope -> Bool
gte (Slope (V2 x y)) (Slope (V2 x2 y2)) = (y * x2) >= (x2 * y)

gt :: Slope -> Slope -> Bool
gt (Slope (V2 x y)) (Slope (V2 x2 y2)) = (y * x2) > (x2 * y)

slopeX :: Slope -> Int32
slopeX (Slope (V2 x _)) = x

slopeY :: Slope -> Int32
slopeY (Slope (V2 _ y)) = y

computeOctant :: tilemap -> OctantPosition -> V2 -> Int -> Int -> Slope -> Slope -> V.Vector V2
computeOctant tm oct origin range x' top@(Slope (V2 tx _)) bottom@(Slope (V2 bx by)) = flip (error "") runState $ forM_ [x'..range] $ \x -> do
  topY <- if tx == 1 then pure x
    else do
      let ty = ((x*2-1) * ty + tx) / (tx*2)
      if blocksLight x ty oct origin
      then
        if gte top (Slope (V2 (x*2) (ty * 2 + 1))) && not (blocksLight x (ty+1) oct origin)
          then return $ ty+1
          else return ty
      else do
        let ax = if blocksLight (x+1) (ty+1) oct origin then (x*2)+1 else x*2
        return $ if gt top (Slope (V2 ax (ty*2 + 1))) then ty+1 else ty
  bottomY <- if by == 0 then pure 0
    else do
      let by' = ((x*2 - 1) * by + bx) / (bx*2)
      if gte bottom (Slope (V2 (x*2) (by'*2 + 1))) && blocksLight x by' oct origin && not (blocksLight x (by'+1) oct origin)
        then return $ by'+1 else return by
  let wasOpaque = -1
  let r = reverse [bottomY .. topY]
  forM_ r $ \y -> do
    if range < 0 || (distance (V2 x y) origin <= range)
    then do
      let isOpaque = blocksLight x y octant origin
          isVisible = isOpaque || ((y /= topY || gt top (Slope (V2 (x*4+1) (y*4-1)))) && (y /= bottomY || gte (Slope (V2 (x*4-1) (y*4+1)))))
      when isVisible $ addVisible (x, y, oct, origin)
      when (x /= range) $ do
        if isOpaque then do
          if wasOpaque == 0 then do
            let nx = x*2
            let ny = y*2 + 1
            if gt top (Slope (V2 nx ny)) then
              do
                modify (\s -> s { bottom = Slope (V2 nx ny) })
            else
              computeOctant tm oct origin rangeLimit (x+1) top (Slope (V2 nx ny))
-}
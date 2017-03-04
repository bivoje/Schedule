
module Schedule.Core where

import Control.Monad

import Schedule.Types
import Schedule.DBServer
import qualified Data.Set as S


requirConflicts :: CrsidSet -> [Crsid] -> DB [(Crsid, CrsidSet)]
requirConflicts took crss =
  filter (null . S.intersection took . snd) . zip crss
    <$> mapM getRequir crss


retakeConfilits :: CrsidSet -> [Crsid] -> DB [(Crsid, CrsidSet)]
retakeConfilits took crss =
  filter (not . null . S.intersection took . snd) . zip crss
    <$> mapM getRetake crss

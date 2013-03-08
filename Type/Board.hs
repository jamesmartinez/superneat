module Type.Board where

import Type.Common
import Type.Pin (UserId, PinId)

data Board = Board { boardPins :: [PinId], boardOwner :: UserId, followers :: [UserId] }

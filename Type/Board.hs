module Type.Board where

import Type.Common

data Board = Board { boardPins :: [PinId], boardOwner :: UserId, followers :: [UserId] }

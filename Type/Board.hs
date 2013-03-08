module Type.Board where

import Type.Common

newtype UserId = UserId {unUserId :: Integer }
               deriving (Eq, Ord, Num, Typeable, SafeCopy)

newtype PinId = PinId { unPinId :: Integer }
              deriving (Eq, Ord, Num, Enum, Typeable, Safecopy)

data Board = Board { boardPins :: [PinId], boardOwner :: UserId, followers :: [UserId] }

module Week03.Converter ( posixTimeToSlotTestnetConverter ) where 

import Ledger          ( POSIXTime(POSIXTime), Slot(Slot) )
import Ledger.TimeSlot ( SlotConfig(SlotConfig), posixTimeToEnclosingSlot )

posixTimeToSlotTestnetConverter :: POSIXTime -> Slot
posixTimeToSlotTestnetConverter time = slotWhenSlotChangedTo1Sec + posixTimeToEnclosingSlot testnetConf time

timeWhenSlotChangedTo1Sec :: POSIXTime
timeWhenSlotChangedTo1Sec = POSIXTime 1595967616000  -- 2020/07/28 20:19:56 - epoch:73 - slot:1598399 - block:1597132  

slotWhenSlotChangedTo1Sec :: Slot
slotWhenSlotChangedTo1Sec = Slot 1598399

testnetConf :: SlotConfig
testnetConf = SlotConfig 1000 timeWhenSlotChangedTo1Sec

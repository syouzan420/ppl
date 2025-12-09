module Controls where

import SDL
import Foreign.C.Types (CInt)

data Controls = Controls {
    ctlA      :: !Bool,
    ctlB      :: !Bool,
    ctlDown   :: !Bool,
    ctlE      :: !Bool,
    ctlLeft   :: !Bool,
    ctlLMB    :: !Bool,
    ctlMMB    :: !Bool,
    ctlMouse  :: !(Int, Int),
    ctlQ      :: !Bool,
    ctlR      :: !Bool,
    ctlRight  :: !Bool,
    ctlRMB    :: !Bool,
    ctlSelect :: !Bool,
    ctlStart  :: !Bool,
    ctlTab    :: !Bool,
    ctlUp     :: !Bool,
    ctlW      :: !Bool
}

controlsDefault :: Controls
controlsDefault = Controls {
    ctlA      = False,
    ctlB      = False,
    ctlDown   = False,
    ctlE      = False,
    ctlLeft   = False,
    ctlLMB    = False,
    ctlMMB    = False,
    ctlMouse  = (0, 0),
    ctlQ      = False,
    ctlR      = False,
    ctlRight  = False,
    ctlRMB    = False,
    ctlSelect = False,
    ctlStart  = False,
    ctlTab    = False,
    ctlUp     = False,
    ctlW      = False
}

readControls :: (Scancode -> Bool) -> (MouseButton -> Bool) -> Point V2 CInt -> Controls
readControls keyHeld mouseButtonHeld (P (V2 x y)) = Controls {
    ctlA      = a,
    ctlB      = b,
    ctlDown   = down,
    ctlE      = e,
    ctlLeft   = left,
    ctlLMB    = lmb,
    ctlMMB    = mmb,
    ctlMouse  = mouse,
    ctlQ      = q,
    ctlR      = r,
    ctlRight  = right,
    ctlRMB    = rmb,
    ctlSelect = select,
    ctlStart  = start,
    ctlTab    = tab,
    ctlUp     = up,
    ctlW      = w
}
  where
    a      = keyHeld ScancodeSpace || keyHeld ScancodeZ
    b      = keyHeld ScancodeEscape || keyHeld ScancodeX
    down   = not up && (keyHeld ScancodeDown || keyHeld ScancodeJ)
    e      = keyHeld ScancodeE
    left   = not right && (keyHeld ScancodeLeft || keyHeld ScancodeH)
    lmb    = mouseButtonHeld ButtonLeft
    mmb    = mouseButtonHeld ButtonMiddle
    mouse  = (fromIntegral x, fromIntegral y)
    q      = keyHeld ScancodeQ
    r      = keyHeld ScancodeR
    right  = keyHeld ScancodeRight || keyHeld ScancodeL
    rmb    = mouseButtonHeld ButtonRight
    select = keyHeld ScancodeRShift
    start  = keyHeld ScancodeReturn
    tab    = keyHeld ScancodeTab
    up     = keyHeld ScancodeUp || keyHeld ScancodeK
    w      = keyHeld ScancodeW 

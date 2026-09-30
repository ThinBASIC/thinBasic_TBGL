'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles controls - mouse and keyboard
' =============================================================================

' -----------------------------------------------------------------------------

'GLOBAL tbgl_MouseWheelDelta AS LONG  ' -- defined in tbgl_Window.inc
'GLOBAL keyPressed( ) AS BYTE         ' -- defined in tbgl_Window.inc

' -----------------------------------------------------------------------------
' -- Mouse
' -----------------------------------------------------------------------------

$ERR_INVALID_ENUM = "Invalid enumeration passed to the function"

sub resource_input_init()
  dim keyPressed(0 to 256) as byte
  internal_resetKeyState()
end sub

' -- Controls visibility of cursor
SUB tbgl_ShowCursor ALIAS "tbgl_ShowCursor" ( BYVAL flag AS LONG ) EXPORT

  IF flag <> 0 THEN
    WHILE ShowCursor(%TRUE) < 0
    WEND
  ELSE
    WHILE ShowCursor(%FALSE) >= 0
    WEND
  END IF

END SUB

' -- Determines whether mouse is in client area
function MouseIsInClient(optional byref mouseCoordinates as POINTAPI, byref area as Rect) as long
  dim clientArea as Rect
  dim mouse      as PointApi

  GetCursorPos mouse
  GetClientrect( g_Win.handle, clientArea )

  if g_Win.controlParent = 0 then         ' Window
    ScreenToClient( g_Win.handle, mouse )
  else                                    ' Canvas
    dim Win as Rect
    GetWindowRect( g_Win.handle, Win )
    mouse.y = mouse.y - win.ntop
    mouse.x = mouse.x - win.nLeft
  end if

  if ( GetForegroundWindow = g_Win.handle or GetForegroundWindow = g_Win.controlParent ) and _
       mouse.x >= 0 and mouse.x < clientArea.nright and _
       mouse.y >= 0 and mouse.y < clientArea.nbottom then

    if varptr(mouseCoordinates) then
      type set mouseCoordinates = mouse
    end if

    if varptr(area) then
      type set area = clientArea
    end if

    function = 1
  else
    function = 0
  end if

end function

'
function tbgl_MouseInClient alias "tbgl_MouseInClient" ( ) export as long

  function = MouseIsInClient()

end function

' -- coordinateMode is %TBGL_CLIENTAREA or %TBGL_2D
function tbgl_MouseGetPosX alias "tbgl_MouseGetPosX" ( byval coordinateMode as long ) export as single

  dim mouse as PointApi
  dim area  as Rect

  if MouseIsInClient(mouse, area) = 0 then
    function = -1
    exit function
  end if

  select case coordinateMode

    case %TBGL_CLIENTAREA
      function = mouse.x

    case %TBGL_2D
      function = g_renderMatrix2D.left + mouse.x / (area.nRight - area.nLeft) * (g_renderMatrix2D.right - g_renderMatrix2D.left)

    case else
      tbglError $ERR_INVALID_ENUM

  end select

end function


' -- coordinateMode is %TBGL_CLIENTAREA or %TBGL_2D
function tbgl_MouseGetPosY alias "tbgl_MouseGetPosY" ( byval coordinateMode as long ) export as single

  dim mouse as PointApi
  dim area  as Rect

  if MouseIsInClient(mouse, area) = 0 then
    function = -1
    exit function
  end if

  select case coordinateMode

    case %TBGL_CLIENTAREA
      function = mouse.y

    case %TBGL_2D
      function = g_renderMatrix2D.top - g_renderMatrix2d.pixelOffsetY*2 + mouse.y / (area.nBottom - area.nTop) * (g_renderMatrix2D.bottom - g_renderMatrix2D.top)

    case else
      tbglError $ERR_INVALID_ENUM

  end select

end function


FUNCTION tbgl_MouseGetLButton ALIAS "tbgl_MouseGetLButton" ( ) EXPORT AS LONG
  LOCAL retVal AS LONG
  GetAsyncKeyState( %VK_LBUTTON )
  IF MouseIsInClient THEN retVal = IIF( GetAsyncKeyState( %VK_LBUTTON ), 1, 0 )
  FUNCTION = retVal
END FUNCTION


FUNCTION tbgl_MouseGetRButton ALIAS "tbgl_MouseGetRButton" ( ) EXPORT AS LONG
  LOCAL retVal AS LONG
  GetAsyncKeyState( %VK_RBUTTON )
  IF MouseIsInClient THEN retVal = IIF( GetAsyncKeyState( %VK_RBUTTON ), 1, 0 )
  FUNCTION = retVal
END FUNCTION


FUNCTION tbgl_MouseGetMButton ALIAS "tbgl_MouseGetMButton" ( ) EXPORT AS LONG
  LOCAL retVal AS LONG
  GetAsyncKeyState( %VK_MBUTTON )
  IF MouseIsInClient THEN retVal = IIF( GetAsyncKeyState( %VK_MBUTTON ), 1, 0 )
  FUNCTION = retVal
END FUNCTION


FUNCTION tbgl_MouseGetWheelDelta ALIAS "tbgl_MouseGetWheelDelta" ( ) EXPORT AS LONG
  LOCAL retVal AS LONG
  ' -- tbgl_MouseWheelDelta is filled with values in WndProc in tbgl_Window
  IF MouseIsInClient THEN retVal = tbgl_MouseWheelDelta
  tbgl_MouseWheelDelta = 0
  FUNCTION = retVal
END FUNCTION

' -- Centers cursor in rendering area
FUNCTION tbgl_CenterCursor ALIAS "tbgl_CenterCursor" ( ) EXPORT AS LONG
  LOCAL CenterPos AS POINTAPI
  LOCAL rClient AS RECT

  GetClientrect( g_Win.handle, rClient )

  IF ( GetForegroundWindow = g_Win.handle OR GetForegroundWindow = g_Win.controlParent ) THEN
    CenterPos.x = rClient.nRight / 2
    CenterPos.y = rClient.nBottom / 2
    ClientToScreen( g_Win.handle, CenterPos )
    SetCursorPos( CenterPos.x, CenterPos.y )
    FUNCTION = 0
  ELSE
    FUNCTION = - 1
  END IF

END FUNCTION

' -----------------------------------------------------------------------------
' -- Keyboard
' -----------------------------------------------------------------------------

sub internal_resetKeyState()
  register virtualKey as long

  for virtualKey = 0 to 256
    GetAsyncKeyState(virtualKey)
  next

  reset keyPressed()
end sub

' -- Resets key before use
sub tbgl_ResetKeyState alias "tbgl_ResetKeyState" ( ) export

  internal_resetKeyState()

end sub


function tbgl_GetAsyncKeyState alias "tbgl_GetAsyncKeyState" ( byval eKey as long ) export as long

  if eKey < 0 then exit function

  GetAsyncKeyState(eKey)
  function = iif(GetAsyncKeyState(eKey), 1, 0)

end function


function tbgl_GetWindowKeyState alias "tbgl_GetWindowKeyState" ( byval ehWnd as dword, byval eKey as long ) export as long

  if eKey < 0 then exit function

  GetAsyncKeyState(eKey)
  function = GetAsyncKeyState(eKey) AND (GetForegroundWindow = ehWnd OR GetForegroundWindow = g_Win.controlParent)

end function

' -- Returns non-zero only when the key was not pushed last time
function tbgl_GetWindowKeyOnce alias "tbgl_GetWindowKeyOnce" ( byval ehWnd as dword, byval eKey as long ) export as long
  local keyDown as long
  local windowActive as long

  if eKey < 0 then exit function

  GetAsyncKeyState(eKey)
  keyDown = iif(GetAsyncKeyState(eKey), 1, 0)
  windowActive = (GetForegroundWindow = ehWnd OR GetForegroundWindow = g_Win.controlParent)

  if windowActive and keyDown then
    if keyPressed(eKey) = 0 then
      keyPressed(eKey) = 1
      function = 1
    else
      function = 0
    end if
  else
    keyPressed(eKey) = 0
    function = 0
  end if

end function

' -- Returns %TRUE when all count keys in keys array are down
function tbgl_GetMultiAsyncKeyState alias "tbgl_GetMultiAsyncKeyState" ( byval keys as long ptr, byval keyCount as long ) export as long
  local Counter, AllKeysDown as long

  AllKeysDown = %TRUE

  for Counter = 0 to keyCount - 1

    GetAsyncKeyState(@keys[Counter])
    if GetAsyncKeyState(@keys[Counter]) = 0 then
      AllKeysDown = %FALSE
      exit for
    end if

  next

  function = AllKeysDown

end function


' -- Same as tbgl_GetMultiAsyncKeyState, hWnd is currently not checked
function tbgl_GetWindowMultiKeyState alias "tbgl_GetWindowMultiKeyState" ( byval ehWnd as dword, byval keys as long ptr, byval keyCount as long ) export as long
  function = tbgl_GetMultiAsyncKeyState(keys, keyCount)
end function

' -- Returns non-zero value, if any of passed keys was pressed in given window
' -- keyCount = 0 checks any key (except mouse buttons), regardless of window
function tbgl_GetWindowAnyKeyState alias "tbgl_GetWindowAnyKeyState" ( byval ehWnd as dword, byval keys as long ptr, byval keyCount as long ) export as long
  local Counter as long

  if keyCount then
    if (GetForegroundWindow = ehWnd OR GetForegroundWindow = g_Win.controlParent) then
      for Counter = 0 TO keyCount - 1
        GetAsyncKeyState(@keys[Counter])
        if GetAsyncKeyState(@keys[Counter]) THEN
          function = %TRUE
          exit for
        end if
      next
    end if
  else
    for Counter = 0 to 256
      if counter = %VK_LBUTTON or counter = %VK_MBUTTON or counter = %VK_RBUTTON then iterate for

      GetAsyncKeyState(Counter)
      if GetAsyncKeyState(Counter) THEN
        function = %TRUE
        exit for
      end if
    next
  end if
end function

'

%TBGL_KEY_PRESSED            = 1
%TBGL_KEY_RELEASED           = 2
%TBGL_KEY_PRESSED_FIRST_TIME = 4
function implementation_TBGL_GetKeys(byval hWnd as dword, byref keys() as long) as long

  if GetForegroundWindow <> hWnd then
    function = 0
    exit function
  end if

  register i as long
  local result as long
  local lastValue as long
  local keyDown as long

  for i = lbound(keys) to ubound(keys)
    GetAsyncKeyState(i)
    lastValue = keys(i)
    keyDown = iif(GetAsyncKeyState(i) <> 0, 1, 0)

    if lastValue = 0 and keyDown then
      keys(i) = %TBGL_KEY_PRESSED_FIRST_TIME
      result or= %TBGL_KEY_PRESSED_FIRST_TIME
    elseif lastValue = %TBGL_KEY_RELEASED and keyDown = 0 then
      keys(i) = 0
    elseif lastValue <> 0 and keyDown = 0 then
      keys(i) = %TBGL_KEY_RELEASED
      result or= %TBGL_KEY_RELEASED
    elseif keyDown then
      keys(i) = %TBGL_KEY_PRESSED
      result or= %TBGL_KEY_PRESSED
    else
      keys(i) = 0
    end if

  next

  function = result

end function

' -- Fills pressedKeys (LONG array of totalElements items) with keys pressed for the first time, returns their count
function tbgl_GetKeysPressed alias "tbgl_GetKeysPressed" ( byval hWnd as dword, byval pressedKeysPtr as long ptr, byval totalElements as long ) export as long

  dim pressedKeys(1 to totalElements) as long at pressedKeysPtr

  dim keys(1 to 256) as long
  dim result as long
  result = implementation_TBGL_GetKeys(hWnd, keys())

  register i as long

  dim keyNow as long
  for i = 1 to 256
    if keys(i) = %TBGL_KEY_PRESSED_FIRST_TIME then
      incr keyNow
      pressedKeys(keyNow) = i

      if keyNow = totalElements then exit for
    end if
  next

  for i = keyNow+1 to totalElements
    pressedKeys(i) = 0
  next

  function = keyNow

end function

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_ShowCursor ( )
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number( )
  tbgl_ShowCursor( ISTRUE flag )

END SUB

function Exec_TBGL_MouseInClient( ) as long
  function = tbgl_MouseInClient( )
end function

' -- Parses optional "( coordinateMode )", %TBGL_CLIENTAREA when omitted
function internal_ParseMouseCoordinateMode( ) as long
  dim coordinateMode as ext
  coordinateMode = %TBGL_CLIENTAREA

  if thinBasic_CheckOpenParens_Optional then
    thinBasic_ParseNumber coordinateMode
    thinBasic_CheckCloseParens_Mandatory
  end if

  function = coordinateMode
end function

function Exec_TBGL_MouseGetPosX( ) as single
  function = tbgl_MouseGetPosX( internal_ParseMouseCoordinateMode( ))
end function

function Exec_TBGL_MouseGetPosY( ) as single
  function = tbgl_MouseGetPosY( internal_ParseMouseCoordinateMode( ))
end function

FUNCTION Exec_TBGL_MouseGetLButton( ) AS LONG
  FUNCTION = tbgl_MouseGetLButton( )
END FUNCTION

FUNCTION Exec_TBGL_MouseGetRButton( ) AS LONG
  FUNCTION = tbgl_MouseGetRButton( )
END FUNCTION

FUNCTION Exec_TBGL_MouseGetMButton( ) AS LONG
  FUNCTION = tbgl_MouseGetMButton( )
END FUNCTION

FUNCTION Exec_TBGL_MouseGetWheelDelta( ) AS LONG
  FUNCTION = tbgl_MouseGetWheelDelta( )
END FUNCTION

FUNCTION Exec_TBGL_CenterCursor ( ) AS LONG
  FUNCTION = tbgl_CenterCursor( )
END FUNCTION

sub Exec_TBGL_ResetKeyState( )
  if thinBasic_CheckOpenParens_Optional then thinBasic_CheckCloseParens_Mandatory
  tbgl_ResetKeyState( )
end sub

function Exec_TBGL_GetAsyncKeyState( ) as long
  local eKey as ext

  eKey = thinBasic_Parse1Number
  if eKey < 0 then exit function

  function = tbgl_GetAsyncKeyState( eKey )
end function

function Exec_TBGL_GetWindowKeyState( ) as long
  local eKey, ehWnd as ext

  thinBasic_Parse2Numbers(ehWnd, eKey)
  if eKey < 0 then exit function

  function = tbgl_GetWindowKeyState( ehWnd, eKey )
end function

function Exec_TBGL_GetWindowKeyOnce( ) as long
  local eKey, ehWnd as ext

  thinBasic_Parse2Numbers(ehWnd, eKey)
  if eKey < 0 then exit function

  function = tbgl_GetWindowKeyOnce( ehWnd, eKey )
end function

' -- Parses "[(] [hWnd,] key1 [, key2 ...] [)]" into keys(), returns number of keys, 0 on error
function internal_parseMultiKeys(byval handleAsFirstParam as long, ehWnd as ext, keys() as long) as long
  dim eKeys(1 to 10) as ext
  local KeysFound, ParensPresent, Counter as long

  if thinBasic_CheckOpenParens_Optional then ParensPresent = %TRUE

  if handleAsFirstParam then
    thinBasic_ParseNumber ehWnd
    thinBasic_CheckComma_Mandatory
  end if

  KeysFound = 1
  thinBasic_ParseNumber eKeys(KeysFound)
  while thinBasic_CheckComma_Optional
    incr KeysFound

    if KeysFound > ubound(eKeys) then
      tbglError "The function does not support more than " + format$(ubound(eKeys)) + " concurrent keys"
      exit function
    end if

    thinBasic_ParseNumber eKeys(KeysFound)
  wend

  if ParensPresent then thinBasic_CheckCloseParens_Mandatory

  redim keys(0 to KeysFound - 1)
  for Counter = 1 to KeysFound
    keys(Counter - 1) = eKeys(Counter)
  next

  function = KeysFound

end function

function Exec_TBGL_GetMultiAsyncKeyState( ) as long
  local ehWnd as ext
  local keyCount as long
  dim keys() as long

  keyCount = internal_parseMultiKeys(%FALSE, ehWnd, keys())
  if keyCount then function = tbgl_GetMultiAsyncKeyState( varptr(keys(0)), keyCount )
end function

function Exec_TBGL_GetWindowMultiKeyState() as long
  local ehWnd as ext
  local keyCount as long
  dim keys() as long

  keyCount = internal_parseMultiKeys(%TRUE, ehWnd, keys())
  if keyCount then function = tbgl_GetWindowMultiKeyState( ehWnd, varptr(keys(0)), keyCount )
end function

function Exec_TBGL_GetWindowAnyKeyState() as long
  dim eKeys(1 to 10) as ext
  local ehWnd as ext
  local KeysFound, ParensPresent, Counter as long

  if thinBasic_CheckOpenParens_Optional = %TRUE then ParensPresent = %TRUE
  thinBasic_ParseNumber ehWnd

  if thinBasic_CheckComma_Optional then
    KeysFound = 1
    thinBasic_ParseNumber eKeys(KeysFound)
    while thinBasic_CheckComma_Optional
      incr KeysFound
      if KeysFound > ubound(eKeys) then
        redim preserve eKeys(1 to ubound(eKeys)*2)
      end if

      thinBasic_ParseNumber eKeys(KeysFound)
    wend

    IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

    dim keys(0 to KeysFound - 1) as long
    for Counter = 1 to KeysFound
      keys(Counter - 1) = eKeys(Counter)
    next

    function = tbgl_GetWindowAnyKeyState( ehWnd, varptr(keys(0)), KeysFound )
  elseif thinBasic_CheckCloseParens_Mandatory then
    function = tbgl_GetWindowAnyKeyState( ehWnd, 0, 0 )
  end if
end function

' -- Script array is expected to be LONG (it is overlaid directly)
function Exec_TBGL_GetKeysPressed() as long

  thinBasic_CheckOpenParens_Mandatory

  local hWnd as ext
  thinBasic_ParseNumber hWnd

  thinBasic_CheckComma_Mandatory

  local arrayCellPtr, mainType, subType, elementsAreFixed, totalElements, elementSize, variablePtrOdd, arrayCellAbsPos as long
  thinBasic_ParseVariableInfo(arrayCellPtr, mainType, subType, elementsAreFixed, totalElements, elementSize, variablePtrOdd, arrayCellAbsPos)

  thinBasic_CheckCloseParens_Mandatory

  function = tbgl_GetKeysPressed( hWnd, variablePtrOdd, totalElements )

end function

#ENDIF

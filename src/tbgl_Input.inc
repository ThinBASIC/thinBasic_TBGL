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

' -- Controls visibility of cursor
SUB Exec_TBGL_ShowCursor ( )
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number( )

  IF flag <> 0 THEN
    WHILE ShowCursor(%TRUE) < 0
    WEND
  ELSE
    WHILE ShowCursor(%FALSE) >= 0
    WEND
  END IF

END SUB

' -- Determines whether mouse is in client area
FUNCTION MouseIsInClient( ) AS LONG
  LOCAL mouse AS POINTAPI
  LOCAL clienta AS RECT
  GetCursorPos mouse
  GetClientrect( g_Win.handle, clienta )

  IF g_Win.controlParent = 0 THEN         ' Window
    ScreenToClient( g_Win.handle, mouse )
  ELSE                                    ' Canvas
    LOCAL Win AS RECT
    GetWindowRect( g_Win.handle, Win )
    mouse.y = mouse.y - win.ntop
    mouse.x = mouse.x - win.nLeft
  END IF

  IF ( GetForegroundWindow = g_Win.handle OR GetForegroundWindow = g_Win.controlParent ) AND _
    mouse.x >= 0 AND mouse.x < clienta.nright AND _
    mouse.y >= 0 AND mouse.y < clienta.nbottom THEN
    FUNCTION = 1
  ELSE
    FUNCTION = 0
  END IF

END FUNCTION

'
FUNCTION Exec_TBGL_MouseGetPosX( ) AS LONG

  LOCAL mouse AS POINTAPI

  GetCursorPos mouse

  IF g_Win.controlParent = 0 THEN         ' Window
    ScreenToClient( g_Win.handle, mouse )
  ELSE
    LOCAL Win AS RECT                     ' Canvas
    GetWindowRect( g_Win.handle, Win )
    mouse.y = mouse.y - win.ntop
    mouse.x = mouse.x - win.nLeft
  END IF

  IF MouseIsInClient THEN
    FUNCTION = mouse.x
  ELSE
    FUNCTION = - 1
  END IF
END FUNCTION


FUNCTION Exec_TBGL_MouseGetPosY( ) AS LONG
  LOCAL mouse AS POINTAPI

  GetCursorPos mouse

  IF g_Win.controlParent = 0 THEN           ' Window
    ScreenToClient( g_Win.handle, mouse )
  ELSE                                      ' Canvas
    LOCAL Win AS RECT
    GetWindowRect( g_Win.handle, Win )
    mouse.y = mouse.y - win.ntop
    mouse.x = mouse.x - win.nLeft
  END IF

  IF MouseIsInClient THEN
    FUNCTION = mouse.y
  ELSE
    FUNCTION = - 1
  END IF

END FUNCTION


FUNCTION Exec_TBGL_MouseGetLButton( ) AS LONG
  LOCAL retVal AS LONG
  GetAsyncKeyState( %VK_LBUTTON )
  IF MouseIsInClient THEN retVal = IIF( GetAsyncKeyState( %VK_LBUTTON ), 1, 0 )
  FUNCTION = retVal
END FUNCTION


FUNCTION Exec_TBGL_MouseGetRButton( ) AS LONG
  LOCAL retVal AS LONG
  GetAsyncKeyState( %VK_RBUTTON )
  IF MouseIsInClient THEN retVal = IIF( GetAsyncKeyState( %VK_RBUTTON ), 1, 0 )
  FUNCTION = retVal
END FUNCTION


FUNCTION Exec_TBGL_MouseGetMButton( ) AS LONG
  LOCAL retVal AS LONG
  GetAsyncKeyState( %VK_MBUTTON )
  IF MouseIsInClient THEN retVal = IIF( GetAsyncKeyState( %VK_MBUTTON ), 1, 0 )
  FUNCTION = retVal
END FUNCTION


FUNCTION Exec_TBGL_MouseGetWheelDelta( ) AS LONG
  LOCAL retVal AS LONG
  ' -- tbgl_MouseWheelDelta is filled with values in WndProc in tbgl_Window
  IF MouseIsInClient THEN retVal = tbgl_MouseWheelDelta
  tbgl_MouseWheelDelta = 0
  FUNCTION = retVal
END FUNCTION

' -- Centers cursor in rendering area
FUNCTION Exec_TBGL_CenterCursor ( ) AS LONG
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

' -- Resets key before use
SUB Exec_TBGL_ResetKeyState( )
  REGISTER Key AS LONG

  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FOR Key = 1 TO 256
    GetAsyncKeyState( Key )
  NEXT
  RESET keyPressed( )

END SUB


FUNCTION Exec_TBGL_GetAsyncKeyState( ) AS EXT
  LOCAL eKey AS EXT

  eKey = thinBasic_Parse1Number
  IF eKey < 0 THEN
    FOR eKey = 1 TO 256
      GetAsyncKeyState( eKey )
    NEXT
  ELSE
    FUNCTION = GetAsyncKeyState( eKey )
  END IF
END FUNCTION


FUNCTION Exec_TBGL_GetWindowKeyState( ) AS EXT
  LOCAL eKey AS EXT
  LOCAL ehWnd AS EXT

  thinBasic_Parse2Numbers( ehWnd, eKey )
  IF eKey < 0 THEN
    FOR eKey = 1 TO 256
      GetAsyncKeyState( eKey )
    NEXT
  ELSE
    FUNCTION = GetAsyncKeyState( eKey ) AND ( GetForegroundWindow = ehWnd OR GetForegroundWindow = g_Win.controlParent )    ' ! muze delat binec u multiwin
  END IF

END FUNCTION

' -- Returns non-zero only when the key was not pushed last time
FUNCTION Exec_TBGL_GetWindowKeyOnce( ) AS EXT
  LOCAL eKey AS EXT
  LOCAL ehWnd AS EXT
  LOCAL kstatus AS LONG

  thinBasic_Parse2Numbers( ehWnd, eKey )
  IF eKey < 0 THEN
    FOR eKey = 1 TO 256
      GetAsyncKeyState( eKey )
    NEXT
  ELSE
    kstatus = GetAsyncKeyState( eKey )

    IF ( GetForegroundWindow = ehWnd OR GetForegroundWindow = g_Win.controlParent ) THEN
      IF kstatus = 0 THEN
        keyPressed( eKey ) = 0
      ELSEIF keyPressed( eKey ) = 0 THEN
        keyPressed( eKey ) = 1
        FUNCTION = kstatus
      END IF
    END IF
  END IF

END FUNCTION

' -- Allows checking of multiple keys
FUNCTION Exec_TBGL_GetMultiAsyncKeyState( ) AS EXT
  LOCAL eKeys( ) AS EXT
  LOCAL MaxKeys AS LONG
  LOCAL KeysFound AS LONG
  LOCAL ParensPresent AS LONG
  LOCAL Counter AS LONG
  LOCAL AllKeysDown AS LONG

  MaxKeys = 3
  REDIM eKeys( 1& TO MaxKeys )
  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE

  KeysFound = 1&
  thinBasic_ParseNumber eKeys( KeysFound )
  WHILE thinBasic_CheckComma_Optional = %TRUE
    INCR KeysFound
    thinBasic_ParseNumber eKeys( KeysFound )
    IF KeysFound = MaxKeys THEN EXIT LOOP
  WEND

  FUNCTION = KeysFound

  IF KeysFound = 1& AND eKeys( 1& ) < 0 THEN
    FOR Counter = 1& TO 256&
      GetAsyncKeyState( Counter )
    NEXT
  ELSE
    AllKeysDown = %TRUE
    FOR Counter = 1 TO KeysFound
      IF AllKeysDown = %TRUE THEN
        IF GetAsyncKeyState( eKeys( Counter )) THEN
        ELSE
          AllKeysDown = %FALSE
          EXIT FOR
        END IF
      END IF
    NEXT

    FUNCTION = AllKeysDown
  END IF

  IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

END FUNCTION


FUNCTION Exec_TBGL_GetWindowMultiKeyState( ) AS EXT

  LOCAL eKeys( ) AS EXT
  LOCAL MaxKeys AS LONG
  LOCAL KeysFound AS LONG
  LOCAL ehWnd AS EXT
  LOCAL ParensPresent AS LONG
  LOCAL Counter AS LONG
  LOCAL AllKeysDown AS LONG

  MaxKeys = 3
  REDIM eKeys( 1& TO MaxKeys )
  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE
  thinBasic_ParseNumber ehWnd
  IF thinBasic_CheckComma_Mandatory THEN
    KeysFound = 1&
    thinBasic_ParseNumber eKeys( KeysFound )
    WHILE thinBasic_CheckComma_Optional
      INCR KeysFound
      thinBasic_ParseNumber eKeys( KeysFound )
      IF KeysFound = MaxKeys THEN EXIT LOOP
    WEND
    FUNCTION = KeysFound

    IF KeysFound = 1& AND eKeys( 1& ) < 0 THEN
      FOR Counter = 1& TO 256&
        GetAsyncKeyState( Counter )
      NEXT
    ELSE
      IF ( GetForegroundWindow = ehWnd OR GetForegroundWindow = g_Win.controlParent ) THEN
        AllKeysDown = %TRUE
        FOR Counter = 1 TO KeysFound
          IF AllKeysDown = %TRUE THEN
            IF GetAsyncKeyState( eKeys( Counter )) THEN
            ELSE
              AllKeysDown = %FALSE
              EXIT FOR
            END IF
          END IF
        NEXT

        FUNCTION = AllKeysDown

      END IF
    END IF

  END IF

  IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

END FUNCTION

' -- Returns non-zero value, if any of passed keys was pressed
FUNCTION Exec_TBGL_GetWindowAnyKeyState( ) AS EXT
  LOCAL eKeys( ) AS EXT
  LOCAL KeysFound AS LONG
  LOCAL ehWnd AS EXT
  LOCAL ParensPresent AS LONG
  LOCAL Counter AS LONG

  REDIM eKeys( 1& TO 1& )

  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE
  thinBasic_ParseNumber ehWnd
  IF thinBasic_CheckComma_Mandatory THEN
    KeysFound = 1%
    thinBasic_ParseNumber eKeys( KeysFound )
    WHILE thinBasic_CheckComma_Optional
      INCR KeysFound
      REDIM PRESERVE eKeys( 1& TO KeysFound )
      thinBasic_ParseNumber eKeys( KeysFound )
    WEND

    IF ( GetForegroundWindow = ehWnd OR GetForegroundWindow = g_Win.controlParent ) THEN
      FOR Counter = 1& TO KeysFound
        IF GetAsyncKeyState( eKeys( Counter )) THEN
          FUNCTION = %TRUE
          EXIT FOR
        END IF
      NEXT
    END IF
  END IF

  IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

END FUNCTION

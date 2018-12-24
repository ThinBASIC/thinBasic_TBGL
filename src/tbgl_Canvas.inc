'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles TBGL canvas
' =============================================================================

' -----------------------------------------------------------------------------

#INCLUDE ONCE "tbgl_Window.inc" ' -- Must be included first as it has something in common

' -----------------------------------------------------------------------------

' -- Retrieving handle of host control for rendering
FUNCTION Exec_TBGL_BindCanvas( ) AS EXT

  LOCAL hCtrl AS DWORD
  LOCAL r AS RECT

  ' -- Getting handle
  hCtrl = thinBasic_Parse1Number

  ' -- Creating device handle
  g_Win.hDC = GetDC( hCtrl )

  ' -- Dimensions of target control
  GetClientRect( hCtrl, r )
  g_Win.xRes      = r.nright
  g_Win.yRes      = r.nbottom
  g_Win.fullscreen= 0
  g_Win.handle    = hCtrl

  g_Win.controlParent = GetParent( hCtrl )
  internal_InitOpenGL()

  FUNCTION = 0
END FUNCTION


' -- Updates viewport size with actual control size
FUNCTION Exec_TBGL_UpdateCanvasProportions( ) AS EXT
  LOCAL cHandle AS EXT
  DIM r AS RECT
  IF thinBasic_CheckOpenParens_Optional THEN
    thinBASIC_ParseNumber( cHandle )
    thinBasic_CheckCloseParens_Mandatory
    GetClientRect( cHandle, r )
  ELSE
    GetClientRect( g_Win.handle, r )
  END IF

  g_Win.xRes = r.nright
  g_Win.yRes = r.nbottom
  internal_refreshViewport( )

END FUNCTION


' -- Is canvas bound for rendering?
FUNCTION Exec_TBGL_CanvasBound( ) AS EXT
  LOCAL cHandle AS EXT

  IF thinBasic_CheckOpenParens_Optional THEN
    thinBASIC_ParseNumber( cHandle )
    thinBasic_CheckCloseParens_Mandatory

    IF cHandle = g_Win.handle AND g_Win.ControlParent <> 0 THEN
      FUNCTION = %TRUE
    ELSE
      FUNCTION = %FALSE
    END IF
  ELSEIF g_Win.handle <> 0 AND g_Win.ControlParent <> 0 THEN
    FUNCTION = %TRUE
  END IF

END FUNCTION


' -- Releasing canvas results in releasing sources and refreshing control we captured
FUNCTION Exec_TBGL_ReleaseCanvas( ) AS EXT

  LOCAL cHandle AS EXT
  IF thinBasic_CheckOpenParens_Optional THEN
    thinBASIC_ParseNumber( cHandle )
    thinBasic_CheckCloseParens_Mandatory
    RedrawWindow( g_Win.handle, BYVAL 0, BYCOPY 0, %RDW_INVALIDATE )

    IF cHandle = g_Win.handle THEN
      Internal_TBGL_Free( )
    END IF

  ELSE
    Internal_TBGL_Free( )
  END IF

END FUNCTION

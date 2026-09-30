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
FUNCTION tbgl_BindCanvas ALIAS "tbgl_BindCanvas" ( BYVAL hCtrl AS DWORD ) EXPORT AS LONG

  LOCAL r AS RECT

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


' -- Updates viewport size with actual control size, hCtrl = 0 means currently bound canvas
FUNCTION tbgl_UpdateCanvasProportions ALIAS "tbgl_UpdateCanvasProportions" ( BYVAL hCtrl AS DWORD ) EXPORT AS LONG
  DIM r AS RECT

  IF hCtrl THEN
    GetClientRect( hCtrl, r )
  ELSE
    GetClientRect( g_Win.handle, r )
  END IF

  g_Win.xRes = r.nright
  g_Win.yRes = r.nbottom
  internal_refreshViewport( )

END FUNCTION


' -- Is canvas bound for rendering? hCtrl = 0 means any canvas
FUNCTION tbgl_CanvasBound ALIAS "tbgl_CanvasBound" ( BYVAL hCtrl AS DWORD ) EXPORT AS LONG

  IF hCtrl THEN
    IF hCtrl = g_Win.handle AND g_Win.ControlParent <> 0 THEN
      FUNCTION = %TRUE
    ELSE
      FUNCTION = %FALSE
    END IF
  ELSEIF g_Win.handle <> 0 AND g_Win.ControlParent <> 0 THEN
    FUNCTION = %TRUE
  END IF

END FUNCTION


' -- Releasing canvas results in releasing sources and refreshing control we captured, hCtrl = 0 means current canvas
FUNCTION tbgl_ReleaseCanvas ALIAS "tbgl_ReleaseCanvas" ( BYVAL hCtrl AS DWORD ) EXPORT AS LONG

  IF hCtrl THEN
    RedrawWindow( g_Win.handle, BYVAL 0, BYCOPY 0, %RDW_INVALIDATE )

    IF hCtrl = g_Win.handle THEN
      Internal_TBGL_Free( )
    END IF

  ELSE
    Internal_TBGL_Free( )
  END IF

END FUNCTION

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

FUNCTION Exec_TBGL_BindCanvas( ) AS EXT
  FUNCTION = tbgl_BindCanvas( thinBasic_Parse1Number )
END FUNCTION

' -- Parses optional "( hCtrl )", returns 0 when omitted
FUNCTION internal_ParseOptionalHandleInParens( ) AS DWORD
  LOCAL cHandle AS EXT

  IF thinBasic_CheckOpenParens_Optional THEN
    thinBASIC_ParseNumber( cHandle )
    thinBasic_CheckCloseParens_Mandatory
  END IF

  FUNCTION = cHandle
END FUNCTION

FUNCTION Exec_TBGL_UpdateCanvasProportions( ) AS EXT
  FUNCTION = tbgl_UpdateCanvasProportions( internal_ParseOptionalHandleInParens( ))
END FUNCTION

FUNCTION Exec_TBGL_CanvasBound( ) AS EXT
  FUNCTION = tbgl_CanvasBound( internal_ParseOptionalHandleInParens( ))
END FUNCTION

FUNCTION Exec_TBGL_ReleaseCanvas( ) AS EXT
  FUNCTION = tbgl_ReleaseCanvas( internal_ParseOptionalHandleInParens( ))
END FUNCTION

#ENDIF

'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles all geometry definition approaches
' =============================================================================

' -----------------------------------------------------------------------------

%LIMIT_MAX_DISPLAYLISTS = 1024

' -----------------------------------------------------------------------------

#INCLUDE "tbgl_ImmediateMode.inc"
#INCLUDE "tbgl_Primitives.inc"
#INCLUDE "tbgl_Models.inc"
#INCLUDE "tbgl_DisplayLists.inc"
#INCLUDE "tbgl_GeomBuffers.inc"

' -----------------------------------------------------------------------------
' -- General purpose geometry output modifiers
' -----------------------------------------------------------------------------

' -- Sets point size
SUB tbgl_PointSize ALIAS "tbgl_PointSize" ( BYVAL pointSize AS SINGLE ) EXPORT

  glPointSize pointSize

END SUB

' -- Sets line width in pixels
SUB tbgl_LineWidth ALIAS "tbgl_LineWidth" ( BYVAL lineWidth AS SINGLE ) EXPORT

  glLineWidth lineWidth

END SUB

' -- Setups pattern for stipple
SUB tbgl_LineStipple ALIAS "tbgl_LineStipple" ( BYVAL factor AS LONG, BYVAL pattern AS WORD ) EXPORT

  glLineStipple factor, pattern

END SUB

' -- Enables or disables use of line stipple
SUB tbgl_UseLineStipple ALIAS "tbgl_UseLineStipple" ( BYVAL flag AS LONG ) EXPORT

  IF flag THEN glEnable %GL_LINE_STIPPLE ELSE glDisable %GL_LINE_STIPPLE

END SUB

' -- Modifies polygon look
SUB tbgl_PolygonLook ALIAS "tbgl_PolygonLook" ( BYVAL polygonMode AS DWORD ) EXPORT

  glPolygonMode %GL_FRONT_AND_BACK, polygonMode

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_PointSize( )
  tbgl_PointSize( thinBasic_Parse1Number )
END SUB

SUB Exec_TBGL_LineWidth( )
  tbgl_LineWidth( thinBasic_Parse1Number )
END SUB

SUB Exec_TBGL_LineStipple( )
  LOCAL x, y AS EXT

  thinBasic_Parse2Numbers( x, y )
  tbgl_LineStipple( x, y )

END SUB

SUB Exec_TBGL_UseLineStipple( )
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  IF thinBasic_ErrorFree THEN tbgl_UseLineStipple( ISTRUE flag )

END SUB

SUB Exec_TBGL_PolygonLook( )
  tbgl_PolygonLook( thinBasic_Parse1Number )
END SUB

#ENDIF

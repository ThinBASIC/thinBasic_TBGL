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
SUB Exec_TBGL_PointSize( )

  LOCAL x AS EXT

  x = thinBasic_Parse1Number
  glPointSize x

END SUB

' -- Sets line width in pixels
SUB Exec_TBGL_LineWidth( )

  LOCAL x AS EXT

  x = thinBasic_Parse1Number

  glLineWidth x

END SUB

' -- Setups pattern for stipple
SUB Exec_TBGL_LineStipple( )

  LOCAL x, y AS EXT
  LOCAL lx, ly AS LONG

  thinBasic_Parse2Numbers( x, y )

  lx = x
  ly = y

  glLineStipple lx, ly

END SUB

' -- Enables or disables use of line stipple
SUB Exec_TBGL_UseLineStipple( )

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use1Param(%GL_LINE_STIPPLE)

END SUB

' -- Modifies polygon look
SUB Exec_TBGL_PolygonLook( )
  LOCAL x AS EXT

  x = thinBasic_Parse1Number

  glPolygonMode %GL_FRONT_AND_BACK, x

END SUB

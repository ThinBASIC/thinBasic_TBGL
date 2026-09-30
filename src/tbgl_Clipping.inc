'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to clipping
' =============================================================================

' -----------------------------------------------------------------------------

' -- Sets clip plane on or off
SUB tbgl_UseClipPlane ALIAS "tbgl_UseClipPlane" ( BYVAL whichPlane AS DWORD, BYVAL flag AS LONG ) EXPORT

  IF flag THEN glEnable whichPlane ELSE glDisable whichPlane

END SUB


' -- Sets equation for clip plane
SUB tbgl_SetupClipPlane ALIAS "tbgl_SetupClipPlane" ( BYVAL whichPlane AS DWORD, BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE, BYVAL w AS DOUBLE ) EXPORT

  DIM Plane( 0 TO 3 ) AS LOCAL DOUBLE

  ARRAY ASSIGN Plane() = x, y, z, w
  glClipPlane whichPlane, Plane(0)

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_UseClipPlane()
  LOCAL whichPlane, flag AS EXT

  thinBasic_Parse2Numbers( whichPlane, flag )
  IF thinBasic_ErrorFree THEN tbgl_UseClipPlane( whichPlane, ISTRUE flag )

END SUB

SUB Exec_TBGL_SetupClipPlane( )
  LOCAL x, y, z, w AS EXT
  LOCAL whichPlane AS EXT

  thinBasic_Parse5Numbers( whichPlane, x, y, z, w )
  tbgl_SetupClipPlane( whichPlane, x, y, z, w )

END SUB

#ENDIF

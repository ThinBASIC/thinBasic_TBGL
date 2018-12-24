'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to clipping
' =============================================================================

' -----------------------------------------------------------------------------

' -- Sets clip plane on or off
SUB Exec_TBGL_UseClipPlane()

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use2Param()

END SUB


' -- Sets equation for clip plane
SUB Exec_TBGL_SetupClipPlane( )

  LOCAL x, y, z, w AS EXT
  LOCAL whichPlane AS EXT

  DIM Plane( 0 TO 3 ) AS LOCAL DOUBLE

  thinBasic_Parse5Numbers( whichPlane, x, y, z, w )
  ARRAY ASSIGN Plane() = x, y, z, w
  glClipPlane whichPlane, Plane(0)

END SUB

'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to fog
' =============================================================================

' -----------------------------------------------------------------------------

' -- Very elemental fog setup
SUB Exec_TBGL_SetupFog( )

  LOCAL r, g, b, a, d AS EXT
  thinBasic_Parse5Numbers( r, g, b, a, d )

  DIM fogColor ( 0 TO 3 ) AS LOCAL SINGLE
  ARRAY ASSIGN fogColor ( ) = r / 255, g / 255, b / 255, a / 255

  d /= 100

  glFogI (%GL_FOG_MODE, %GL_EXP2)
  glFogFV (%GL_FOG_COLOR, fogColor ( 0 ))
  glFogF (%GL_FOG_DENSITY, d)

  glFogF (%GL_FOG_START, 0)
  glFogF (%GL_FOG_END, 1)

END SUB

' -- Sets fog on or off
SUB Exec_TBGL_UseFog()

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use1Param(%GL_FOG)

END SUB

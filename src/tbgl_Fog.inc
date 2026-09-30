'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to fog
' =============================================================================

' -----------------------------------------------------------------------------

' -- Very elemental fog setup
SUB tbgl_SetupFog ALIAS "tbgl_SetupFog" ( BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE, BYVAL density AS SINGLE ) EXPORT

  internal_SetupFogF( r / 255, g / 255, b / 255, a / 255, density / 100 )

END SUB

' -- Color components in 0..1, not clamped; glDensity is the GL fog density (TBGL density / 100)
SUB internal_SetupFogF( BYVAL r AS SINGLE, BYVAL g AS SINGLE, BYVAL b AS SINGLE, BYVAL a AS SINGLE, BYVAL glDensity AS SINGLE )

  DIM fogColor ( 0 TO 3 ) AS LOCAL SINGLE
  ARRAY ASSIGN fogColor ( ) = r, g, b, a

  glFogI (%GL_FOG_MODE, %GL_EXP2)
  glFogFV (%GL_FOG_COLOR, fogColor ( 0 ))
  glFogF (%GL_FOG_DENSITY, glDensity)

  glFogF (%GL_FOG_START, 0)
  glFogF (%GL_FOG_END, 1)

END SUB

' -- Sets fog on or off
SUB tbgl_UseFog ALIAS "tbgl_UseFog" ( BYVAL flag AS LONG ) EXPORT

  IF flag THEN glEnable %GL_FOG ELSE glDisable %GL_FOG

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_SetupFog( )
  LOCAL r, g, b, a, d AS EXT

  thinBasic_Parse5Numbers( r, g, b, a, d )
  internal_SetupFogF( r / 255, g / 255, b / 255, a / 255, d / 100 )

END SUB

SUB Exec_TBGL_UseFog()
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  IF thinBasic_ErrorFree THEN tbgl_UseFog( ISTRUE flag )

END SUB

#ENDIF

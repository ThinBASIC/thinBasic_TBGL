'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to lighting
' =============================================================================

' -- Function to specify normal, can be used with immediate mode
SUB tbgl_Normal ALIAS "tbgl_Normal" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE ) EXPORT

  glNormal3f x, y, z

END SUB

SUB tbgl_SetLightParameter ALIAS "tbgl_SetLightParameter" ( _
    BYVAL lightId AS DWORD _
  , BYVAL whatComponent AS DWORD _
  , BYVAL x1 AS SINGLE _
  , BYVAL x2 AS SINGLE _
  , BYVAL x3 AS SINGLE _
  , BYVAL x4 AS SINGLE _
  ) EXPORT

  DIM Arr( 3 ) AS LOCAL SINGLE
  ARRAY ASSIGN Arr( ) = x1, x2, x3, x4

  glLightfV lightId, whatComponent, Arr( 0 )

END SUB

' -- Sets lighting on or off
SUB tbgl_UseLighting ALIAS "tbgl_UseLighting" ( BYVAL flag AS LONG ) EXPORT

  IF flag THEN glEnable %GL_LIGHTING ELSE glDisable %GL_LIGHTING

END SUB

' -- Sets light source on or off
SUB tbgl_UseLightSource ALIAS "tbgl_UseLightSource" ( BYVAL lightId AS DWORD, BYVAL flag AS LONG ) EXPORT

  IF flag THEN glEnable lightId ELSE glDisable lightId

END SUB

' -- Is lighting on?
FUNCTION tbgl_GetUseLighting ALIAS "tbgl_GetUseLighting" ( ) EXPORT AS LONG
  FUNCTION = glIsEnabled( %GL_LIGHTING )
END FUNCTION

' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
' -- [O] obsolete function for light setup
SUB tbgl_SetupLightSource ALIAS "tbgl_SetupLightSource" ( _
    BYVAL lightId AS DWORD _
  , BYVAL x AS SINGLE _
  , BYVAL y AS SINGLE _
  , BYVAL z AS SINGLE _
  , BYVAL r AS BYTE _
  , BYVAL g AS BYTE _
  , BYVAL b AS BYTE _
  , BYVAL diff AS BYTE _
  ) EXPORT

  internal_SetupLightSourceF( lightId, x, y, z, r / 255, g / 255, b / 255, diff / 255 )

END SUB

' -- Color components in 0..1, not clamped (thinBasic keyword passes script values / 255 unrounded)
SUB internal_SetupLightSourceF( _
    BYVAL lightId AS DWORD _
  , BYVAL lx AS SINGLE _
  , BYVAL ly AS SINGLE _
  , BYVAL lz AS SINGLE _
  , BYVAL lr AS SINGLE _
  , BYVAL lg AS SINGLE _
  , BYVAL lb AS SINGLE _
  , BYVAL ldiff AS SINGLE _
  )

  DIM LightAmbient( 3 ) AS LOCAL SINGLE
  DIM LightDiffuse( 3 ) AS LOCAL SINGLE
  DIM LightPosition( 3 ) AS LOCAL SINGLE
  DIM LightSpecular( 3 ) AS LOCAL SINGLE

  ARRAY ASSIGN LightAmbient( ) = lr, lg, lb, 1.0!
  ARRAY ASSIGN LightDiffuse( ) = ldiff, ldiff, ldiff, 1.0!
  ARRAY ASSIGN LightPosition( ) = lx, ly, lz, 1.0!
  ARRAY ASSIGN LightSpecular( ) = lx, ly, lz, 1.0!

  GLLIGHTFV lightId, %GL_AMBIENT, LightAmbient( 0 )
  GLLIGHTFV lightId, %GL_DIFFUSE, LightDiffuse( 0 )
  GLLIGHTFV lightId, %GL_POSITION, LightPosition( 0 )
  GLLIGHTFV lightId, %GL_SPECULAR, LightSpecular( 0 )

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_Normal( )
  LOCAL x AS EXT, y AS EXT, z AS EXT

  thinBasic_Parse3Numbers( x, y, z )
  tbgl_Normal( x, y, z )

END SUB

SUB Exec_TBGL_SetLightParameter( )
  LOCAL lightid AS EXT
  LOCAL whatcomponent AS EXT
  LOCAL x1, x2, x3, x4 AS EXT

  thinBasic_Parse6Numbers( lightid, whatcomponent, x1, x2, x3, x4 )
  tbgl_SetLightParameter( lightid, whatcomponent, x1, x2, x3, x4 )

END SUB

SUB Exec_TBGL_UseLighting()
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  IF thinBasic_ErrorFree THEN tbgl_UseLighting( ISTRUE flag )

END SUB

SUB Exec_TBGL_UseLightSource()
  LOCAL lightId, flag AS EXT

  thinBasic_Parse2Numbers( lightId, flag )
  IF thinBasic_ErrorFree THEN tbgl_UseLightSource( lightId, ISTRUE flag )

END SUB

FUNCTION Exec_TBGL_GetUseLighting( ) AS EXT
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = tbgl_GetUseLighting( )
END FUNCTION

SUB Exec_TBGL_SetupLightSource( )
  LOCAL lightid AS EXT
  LOCAL x AS EXT, y AS EXT, z AS EXT
  LOCAL r AS EXT, g AS EXT, b AS EXT
  LOCAL diff AS EXT

  thinBASIC_ParseXNumbers( 8, 8, lightid, x, y, z, r, g, b, diff )
  internal_SetupLightSourceF( lightid, x, y, z, r / 255, g / 255, b / 255, diff / 255 )

END SUB

#ENDIF

'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to lighting
' =============================================================================

' -- Function to specify normal, can be used with immediate mode
SUB Exec_TBGL_Normal( )
  LOCAL x AS EXT, y AS EXT, z AS EXT

  thinBasic_Parse3Numbers( x, y, z )

  glNormal3f x, y, z

END SUB

SUB Exec_TBGL_SetLightParameter( )
  LOCAL lightid AS EXT
  LOCAL whatcomponent AS EXT

  LOCAL x1, x2, x3, x4 AS EXT
  thinBasic_Parse6Numbers( lightid, whatcomponent, x1, x2, x3, x4 )

  DIM Arr( 3 ) AS LOCAL SINGLE
  ARRAY ASSIGN Arr( ) = x1, x2, x3, x4

  glLightfV lightid, whatcomponent, Arr( 0 )

END SUB

' -- Sets lighting on or off
SUB Exec_TBGL_UseLighting()

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use1Param(%GL_LIGHTING)

END SUB

' -- Sets light source on or off
SUB Exec_TBGL_UseLightSource()

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use2Param()

END SUB

' -- Is lighting on?
FUNCTION Exec_TBGL_GetUseLighting( ) AS EXT
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = glIsEnabled( %GL_LIGHTING )
END FUNCTION

' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
' -- [O] obsolete function for light setup
SUB Exec_TBGL_SetupLightSource( )
  LOCAL lightid AS EXT, llightid AS LONG
  LOCAL x AS EXT, y AS EXT, z AS EXT
  LOCAL r AS EXT, g AS EXT, b AS EXT
  LOCAL diff AS EXT, ldiff AS SINGLE
  LOCAL lx AS SINGLE, ly AS SINGLE, lz AS SINGLE
  LOCAL lr AS SINGLE, lg AS SINGLE, lb AS SINGLE

  thinBASIC_ParseXNumbers( 8, 8, lightid, x, y, z, r, g, b, diff )

  llightid = lightid
  lx = x
  ly = y
  lz = z
  lr = r / 255
  lg = g / 255
  lb = b / 255
  ldiff = diff / 255

  DIM LightAmbient( 3 ) AS LOCAL SINGLE
  DIM LightDiffuse( 3 ) AS LOCAL SINGLE
  DIM LightPosition( 3 ) AS LOCAL SINGLE
  DIM LightSpecular( 3 ) AS LOCAL SINGLE

  ARRAY ASSIGN LightAmbient( ) = lr, lg, lb, 1.0!
  ARRAY ASSIGN LightDiffuse( ) = ldiff, ldiff, ldiff, 1.0!
  ARRAY ASSIGN LightPosition( ) = lx, ly, lz, 1.0!
  ARRAY ASSIGN LightSpecular( ) = lx, ly, lz, 1.0!

  GLLIGHTFV llightid, %GL_AMBIENT, LightAmbient( 0 )
  GLLIGHTFV llightid, %GL_DIFFUSE, LightDiffuse( 0 )
  GLLIGHTFV llightid, %GL_POSITION, LightPosition( 0 )
  GLLIGHTFV llightid, %GL_SPECULAR, LightSpecular( 0 )

END SUB

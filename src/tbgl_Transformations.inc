'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to transformations
' =============================================================================

' -----------------------------------------------------------------------------

' -- Elemental camera
SUB Exec_TBGL_Camera( )

  LOCAL x, y, z AS EXT
  LOCAL x2, y2, z2 AS EXT

  thinBasic_Parse6Numbers( x, y, z, x2, y2, z2 )

  gluLookAt (x, y, z, x2, y2, z2, 0.0!, 1.0!, 0.0!)

END SUB

SUB Exec_TBGL_Scale( )
  LOCAL x, y, z AS EXT
  LOCAL nParsed AS LONG

  ' -- Z = 1 is for 2 params specified
  z = 1
  nParsed = thinBasic_ParseXNumbers( 1, 3, x, y, z )

  IF nParsed = 1 THEN
    glScaleF x, x, x
  ELSE
    glScaleF x, y, z
  END IF

END SUB

' -- Allows 2D and 3D
SUB Exec_TBGL_Translate( )
  LOCAL x, y, z AS EXT
  thinBasic_ParseXNumbers( 2, 3, x, y, z )

  glTranslatef x, y, z

END SUB

' -- Allows 2D and 3D
SUB Exec_TBGL_Rotate( )
  LOCAL angle, x, y, z AS EXT
  LOCAL nParsed AS LONG

  nParsed = thinBasic_ParseXNumbers( 1, 4, angle, x, y, z )

  IF nParsed = 4 THEN
    glRotateF angle, x, y, z
  ELSE
    glRotateF angle, 0, 0, 1
  END IF

END SUB

SUB Exec_TBGL_PushMatrix( )

  glPushMatrix

END SUB

SUB Exec_TBGL_PopMatrix( )

  glPopMatrix

END SUB

SUB Exec_tbgl_ResetMatrix( )

  glLoadIdentity

END SUB

' -- Performs one rotation after another
SUB Exec_TBGL_RotateXYZ( )
  LOCAL x AS EXT
  LOCAL y AS EXT
  LOCAL z AS EXT

  thinBasic_Parse3Numbers( x, y, z )

  IF x THEN glRotateF x, 1, 0, 0
  IF y THEN glRotateF y, 0, 1, 0
  IF z THEN glRotateF z, 0, 0, 1

END SUB

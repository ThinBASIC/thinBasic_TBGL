'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to transformations
' =============================================================================

' -----------------------------------------------------------------------------

' -- Elemental camera
SUB tbgl_Camera ALIAS "tbgl_Camera" ( BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE, BYVAL x2 AS DOUBLE, BYVAL y2 AS DOUBLE, BYVAL z2 AS DOUBLE ) EXPORT

  gluLookAt (x, y, z, x2, y2, z2, 0.0!, 1.0!, 0.0!)

END SUB

SUB tbgl_Scale ALIAS "tbgl_Scale" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE ) EXPORT

  glScaleF x, y, z

END SUB

' -- Allows 2D and 3D
SUB tbgl_Translate ALIAS "tbgl_Translate" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE ) EXPORT

  glTranslatef x, y, z

END SUB

' -- Allows 2D and 3D
SUB tbgl_Rotate ALIAS "tbgl_Rotate" ( BYVAL angle AS SINGLE, BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE ) EXPORT

  glRotateF angle, x, y, z

END SUB

SUB tbgl_PushMatrix ALIAS "tbgl_PushMatrix" ( ) EXPORT

  glPushMatrix

END SUB

SUB tbgl_PopMatrix ALIAS "tbgl_PopMatrix" ( ) EXPORT

  glPopMatrix

END SUB

SUB tbgl_ResetMatrix ALIAS "tbgl_ResetMatrix" ( ) EXPORT

  glLoadIdentity

END SUB

' -- Performs one rotation after another
SUB tbgl_RotateXYZ ALIAS "tbgl_RotateXYZ" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE ) EXPORT

  IF x THEN glRotateF x, 1, 0, 0
  IF y THEN glRotateF y, 0, 1, 0
  IF z THEN glRotateF z, 0, 0, 1

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_Camera( )

  LOCAL x, y, z AS EXT
  LOCAL x2, y2, z2 AS EXT

  thinBasic_Parse6Numbers( x, y, z, x2, y2, z2 )
  tbgl_Camera( x, y, z, x2, y2, z2 )

END SUB

SUB Exec_TBGL_Scale( )
  LOCAL x, y, z AS EXT

  ' -- Z = 1 is for 2 params specified, single param scales uniformly
  z = 1
  IF thinBasic_ParseXNumbers( 1, 3, x, y, z ) = 1 THEN
    y = x
    z = x
  END IF

  tbgl_Scale( x, y, z )

END SUB

SUB Exec_TBGL_Translate( )
  LOCAL x, y, z AS EXT

  thinBasic_ParseXNumbers( 2, 3, x, y, z )
  tbgl_Translate( x, y, z )

END SUB

SUB Exec_TBGL_Rotate( )
  LOCAL angle, x, y, z AS EXT

  ' -- Without full axis, rotation happens around Z
  IF thinBasic_ParseXNumbers( 1, 4, angle, x, y, z ) <> 4 THEN
    x = 0
    y = 0
    z = 1
  END IF

  tbgl_Rotate( angle, x, y, z )

END SUB

SUB Exec_TBGL_PushMatrix( )
  tbgl_PushMatrix( )
END SUB

SUB Exec_TBGL_PopMatrix( )
  tbgl_PopMatrix( )
END SUB

SUB Exec_tbgl_ResetMatrix( )
  tbgl_ResetMatrix( )
END SUB

SUB Exec_TBGL_RotateXYZ( )
  LOCAL x, y, z AS EXT

  thinBasic_Parse3Numbers( x, y, z )
  tbgl_RotateXYZ( x, y, z )

END SUB

#ENDIF

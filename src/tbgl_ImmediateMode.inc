'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles immediate mode
' =============================================================================

' -----------------------------------------------------------------------------

SUB tbgl_BeginPoly ALIAS "tbgl_BeginPoly" ( BYVAL polyType AS DWORD ) EXPORT

  glBegin polyType

END SUB

SUB tbgl_EndPoly ALIAS "tbgl_EndPoly" ( ) EXPORT

  glEnd()

END SUB

SUB tbgl_Vertex ALIAS "tbgl_Vertex" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE ) EXPORT
  glVertex2f x, y
END SUB

SUB tbgl_Vertex3D ALIAS "tbgl_Vertex3D" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE ) EXPORT
  glVertex3f x, y, z
END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_BeginPoly( )
  tbgl_BeginPoly( thinBasic_Parse1Number )
END SUB

SUB Exec_TBGL_EndPoly( )
  tbgl_EndPoly( )
END SUB

' -- Hybrid vertex specification for both 3D and 2D
SUB Exec_TBGL_Vertex( )
  LOCAL x, y, z AS EXT

  IF thinBasic_ParseXNumbers( 2, 3, x, y, z ) = 3 THEN tbgl_Vertex3D( x, y, z ) ELSE tbgl_Vertex( x, y )
END SUB

#ENDIF

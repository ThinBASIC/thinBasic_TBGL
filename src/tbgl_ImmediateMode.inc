'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles immediate mode
' =============================================================================

' -----------------------------------------------------------------------------

SUB Exec_TBGL_BeginPoly( )
  LOCAL polytype AS EXT, spolytype AS LONG

  polyType = thinBasic_Parse1Number
  spolytype = polytype

  glBegin spolytype

END SUB

SUB Exec_TBGL_EndPoly( )

  glEnd()

END SUB

' -- Hybrid vertex specification for both 3D and 2D
SUB Exec_TBGL_Vertex( )
  LOCAL x, y, z AS EXT
  LOCAL nParsed AS LONG

  nParsed = thinBasic_ParseXNumbers( 2, 3, x, y, z )
  IF nParsed = 3 THEN glVertex3f x, y, z ELSE glVertex2f x, y
END SUB

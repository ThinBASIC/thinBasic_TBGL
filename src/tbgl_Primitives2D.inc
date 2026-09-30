'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles the 2D primitives point, line, rect, circle
' =============================================================================

  '*****************************************************************
  SUB tbgl_Rect ALIAS "tbgl_Rect" ( BYVAL a AS SINGLE, BYVAL b AS SINGLE, BYVAL c AS SINGLE, BYVAL d AS SINGLE ) EXPORT
  '*****************************************************************
    LOCAL x1, y1, x2, y2 AS SINGLE

    x1 = a
    y1 = b
    x2 = c
    y2 = d

    if g_renderMatrix2d.pixelPerfect then
      ' -- Ensuring pixel row/column, when needed
      local xdir, ydir as single
      xdir = iif(x1 < x2, 1, -1)
      ydir = iif(y1 < y2, 1, -1)
      if g_renderMatrix2d.pixelOffsetX*xdir > 0 then x2 += g_renderMatrix2d.pixelOffsetX else x1 += g_renderMatrix2d.pixelOffsetX
      if g_renderMatrix2d.pixelOffsetY*ydir > 0 then y2 += g_renderMatrix2d.pixelOffsetY else y1 += g_renderMatrix2d.pixelOffsetY
    end if

    glBegin %GL_QUADS
      glVertex2f x1, y2
      glVertex2f x2, y2
      glVertex2f x2, y1
      glVertex2f x1, y1
    glEnd

  END SUB


  '*****************************************************************
  SUB tbgl_Rect3D ALIAS "tbgl_Rect3D" ( _
      BYVAL a AS SINGLE _
    , BYVAL b AS SINGLE _
    , BYVAL c AS SINGLE _
    , BYVAL d AS SINGLE _
    , BYVAL e AS SINGLE _
    , BYVAL f AS SINGLE _
    , BYVAL planeAligned AS LONG _
    ) EXPORT
  '*****************************************************************
    LOCAL x1, y1, z1, x2, y2, z2 AS SINGLE

    x1 = a
    y1 = b
    z1 = c
    x2 = d
    y2 = e
    z2 = f

    glBegin %GL_QUADS
      IF planeAligned = 0 THEN
        glVertex3f x1, y2, z1
        glVertex3f x2, y2, z2
        glVertex3f x2, y1, z2
        glVertex3f x1, y1, z1
      ELSE
        IF x1=x2 THEN ' in yz plane

            glVertex3f x1,y1,z1
            glVertex3f x1,y2,z1
            glVertex3f x1,y2,z2
            glVertex3f x1,y1,z2

        ELSEIF y1=y2 THEN ' in zx plane

            glVertex3f x1,y1,z1
            glVertex3f x1,y1,z2
            glVertex3f x2,y1,z2
            glVertex3f x2,y1,z1

        ELSEIF z1=z2 THEN ' in xy plane

            glVertex3f x1,y1,z1
            glVertex3f x2,y1,z1
            glVertex3f x2,y2,z1
            glVertex3f x1,y2,z1

        ELSE
            glVertex3f x1, y2, z1
            glVertex3f x2, y2, z2
            glVertex3f x2, y1, z2
            glVertex3f x1, y1, z1
        END IF
      END IF
    glEnd

  END SUB


  '*****************************************************************
  SUB tbgl_Line ALIAS "tbgl_Line" ( BYVAL a AS SINGLE, BYVAL b AS SINGLE, BYVAL c AS SINGLE, BYVAL d AS SINGLE ) EXPORT
  '*****************************************************************

    if g_Win.RenderMatrixMode = %TBGL_2D and g_renderMatrix2d.pixelPerfect then
      ' -- Ensure the start and end of the line is visible
      glBegin %GL_POINTS
        glVertex2f  a, b
        glVertex2f  c, d
      glEnd
    end if

    glBegin %GL_LINES
      glVertex2f  a, b
      glVertex2f  c, d
    glEnd

  END SUB


  '*****************************************************************
  SUB tbgl_Line3D ALIAS "tbgl_Line3D" ( BYVAL a AS SINGLE, BYVAL b AS SINGLE, BYVAL c AS SINGLE, BYVAL d AS SINGLE, BYVAL e AS SINGLE, BYVAL f AS SINGLE ) EXPORT
  '*****************************************************************

    glBegin %GL_LINES
      glVertex3f  a, b, c
      glVertex3f  d, e, f
    glEnd

  END SUB


  '*****************************************************************
  SUB tbgl_Point ALIAS "tbgl_Point" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE ) EXPORT
  '*****************************************************************

    glBegin %GL_Points
      glVertex2f x, y
    glEnd

  END SUB


  '*****************************************************************
  SUB tbgl_Point3D ALIAS "tbgl_Point3D" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE ) EXPORT
  '*****************************************************************

    glBegin %GL_Points
      glVertex3f x, y, z
    glEnd

  END SUB


  '*****************************************************************
  SUB tbgl_NGon ALIAS "tbgl_NGon" ( BYVAL ex AS SINGLE, BYVAL ey AS SINGLE, BYVAL radius AS SINGLE, BYVAL sides AS LONG ) EXPORT
  '*****************************************************************
    LOCAL x, y          AS DOUBLE
    LOCAL i             AS LONG
    LOCAL angle,da      AS DOUBLE

    da = 2 * 3.14159/sides
    glBegin %GL_POLYGON
        FOR i = 1 TO sides
            angle = i * da
            x = ex+(COS(angle)*radius)
            y = ey+(SIN(angle)*radius)
            glVertex2f x,y
        NEXT
    glEnd

  END SUB


  '*****************************************************************
  SUB tbgl_NGon3D ALIAS "tbgl_NGon3D" ( BYVAL ex AS SINGLE, BYVAL ey AS SINGLE, BYVAL z AS SINGLE, BYVAL radius AS SINGLE, BYVAL sides AS LONG, BYVAL angleDeg AS SINGLE ) EXPORT
  '*****************************************************************
    LOCAL x, y          AS DOUBLE
    LOCAL i             AS LONG
    LOCAL angle,da      AS DOUBLE

    da = 2 * 3.14159/sides
    glBegin %GL_POLYGON
        FOR i = 1 TO sides
            angle = i * da + angleDeg * 0.0174532925
            x = ex+(COS(angle)*radius)
            y = ey+(SIN(angle)*radius)
            glVertex3f x,y,z
        NEXT
    glEnd

  END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

  ' -- 4 params: 2D rect, 6 params: 3D rect, 7th param: plane aligned
  SUB Exec_TBGL_Rect()
    LOCAL a, b, c, d, e, f, planeAligned  AS EXT

    IF thinBasic_ParseXNumbers(4,7, a, b, c, d, e, f, planeAligned) = 4 THEN
      tbgl_Rect(a, b, c, d)
    ELSE
      tbgl_Rect3D(a, b, c, d, e, f, planeAligned)
    END IF

  END SUB

  ' -- 4 or 5 params: 2D line, 6 params: 3D line
  SUB Exec_TBGL_Line()
    LOCAL a, b, c, d, e, f  AS EXT

    IF thinBasic_ParseXNumbers(4,6, a, b, c, d, e, f) < 6 THEN
      tbgl_Line(a, b, c, d)
    ELSE
      tbgl_Line3D(a, b, c, d, e, f)
    END IF

  END SUB

  ' -- 2 params: 2D point, 3 params: 3D point
  SUB Exec_TBGL_Point()
    LOCAL x, y, z       AS EXT

    IF thinBasic_ParseXNumbers(2,3, x, y, z) = 2 THEN
      tbgl_Point(x, y)
    ELSE
      tbgl_Point3D(x, y, z)
    END IF

  END SUB

  ' -- 4 params: x, y, radius, sides; 5-6 params: x, y, z, radius, sides [, angle]
  SUB Exec_TBGL_NGon()
    LOCAL ex, ey, c, d, e ,f     AS EXT

    IF thinBasic_ParseXNumbers(4,6, ex, ey, c, d, e,f) = 4 THEN
      tbgl_NGon(ex, ey, c, d)
    ELSE
      tbgl_NGon3D(ex, ey, c, d, e, f)
    END IF

  END SUB

#ENDIF

'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles the 2D primitives point, line, rect, circle
' =============================================================================

  '*****************************************************************
  SUB Exec_TBGL_Rect()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL a, b, c, d, e, f, planeAligned  AS EXT
    LOCAL x1, y1, z1, x2, y2, z2 AS SINGLE

    nParsed = thinBasic_ParseXNumbers(4,7, a, b, c, d, e, f, planeAligned)

    glBegin %GL_QUADS
      SELECT CASE AS LONG nParsed
        CASE 4
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

          glVertex2f x1, y2
          glVertex2f x2, y2
          glVertex2f x2, y1
          glVertex2f x1, y1
        CASE 6, 7
          x1 = a
          y1 = b
          z1 = c
          x2 = d
          y2 = e
          z2 = f

          IF nParsed = 6 OR planeAligned = 0 THEN
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
      END SELECT
    glEnd

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_Line()
  '*****************************************************************
    LOCAL nParsed       AS LONG
    LOCAL a, b, c, d, e, f  AS EXT

    nParsed = thinBasic_ParseXNumbers(4,6, a, b, c, d, e, f)

    IF nParsed < 6 THEN

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
    ELSE
      glBegin %GL_LINES
        glVertex3f  a, b, c
        glVertex3f  d, e, f
      glEnd
    END IF

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_Point()
  '*****************************************************************
    LOCAL nParsed       AS LONG
    LOCAL x, y, z       AS EXT

    nParsed = thinBasic_ParseXNumbers(2,3, x, y, z)

    glBegin %GL_Points
    IF nParsed = 2 THEN
        glVertex2f x, y
    ELSE
        glVertex3f x, y, z
    END IF
    glEnd

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_NGon()
  '*****************************************************************
    LOCAL nParsed       AS LONG
    'LOCAL x, y, ex, ey, radius, divider     AS EXT
    LOCAL x, y, z, ex, ey, c, d, e ,f     AS EXT
    LOCAL i,de             AS LONG
    LOCAL angle,da      AS DOUBLE

    'nParsed = thinBasic_ParseXNumbers(3,4, ex, ey, radius, divider)
    nParsed = thinBasic_ParseXNumbers(4,6, ex, ey, c, d, e,f)

    IF nPArsed = 4 THEN
        da = 2 * 3.14159/d
        de = d
        glBegin %GL_POLYGON
            FOR i = 1 TO de
                angle = i * da
                x = ex+(COS(angle)*c)
                y = ey+(SIN(angle)*c)
                glVertex2f x,y
            NEXT
        glEnd
    ELSEIF nParsed = 6 THEN
        da = 2 * 3.14159/e
        z = c
        de = e
        glBegin %GL_POLYGON
            FOR i = 1 TO de
                angle = i * da + f* 0.0174532925
                x = ex+(COS(angle)*d)
                y = ey+(SIN(angle)*d)
                glVertex3f x,y,z
            NEXT
        glEnd
    ELSE
        da = 2 * 3.14159/e
        z = c
        de = e
        glBegin %GL_POLYGON
            FOR i = 1 TO de
                angle = i * da
                x = ex+(COS(angle)*d)
                y = ey+(SIN(angle)*d)
                glVertex3f x,y,z
            NEXT
        glEnd
    END IF
  END SUB

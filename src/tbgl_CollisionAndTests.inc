'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to collision and tests
' =============================================================================

' -----------------------------------------------------------------------------

%TBGL_OBJ_SPHERE   = 1
%TBGL_OBJ_CUBE     = 2
%TBGL_OBJ_CUBE3    = 3
%TBGL_OBJ_CYLINDER = 4

' -----------------------------------------------------------------------------

' -- Function to calculate whether point is inside specified object
FUNCTION Exec_TBGL_PointInside3D( ) AS LONG

  LOCAL x, y, z, cType, cx, cy, cz, par1, par2, par3 AS EXT
  LOCAL lType AS LONG
  LOCAL numParsed AS LONG
  numParsed = thinBASIC_ParseXNumbers( 8, 10, x, y, z, cType, cx, cy, cz, par1, par2, par3 )

  lType = cType
  ' -- Note, there is used esoteric constant 0.05 everywhere, why?
  ' -- Because values returned from z-buffer are not so precise :]

  ' -- For 8 parsed parameters, only sphere and cube it can be
  IF numParsed = 8 THEN

    IF lType = %TBGL_OBJ_SPHERE THEN

      par1 += 0.05
      IF SQR(( x - cx ) * ( x - cx ) + ( y - cy ) * ( y - cy ) + ( z - cz ) * ( z - cz )) <= par1 THEN
        FUNCTION = 1
      END IF

    ELSEIF lType = %TBGL_OBJ_CUBE THEN
      par1 /= 2 + 0.05
      IF x <= cx + par1 AND x >= cx - par1 AND _
         y <= cy + par1 AND y >= cy - par1 AND _
         z <= cz + par1 AND z >= cz - par1 THEN

        FUNCTION = 1
      END IF
    END IF

  ELSEIF numParsed = 10 THEN

    IF lType = %TBGL_OBJ_CUBE3 THEN

      par1 /= 2 + 0.05
      par2 /= 2 + 0.05
      par3 /= 2 + 0.05

      IF x <= cx + par1 AND x >= cx - par1 AND _
         y <= cy + par2 AND y >= cy - par2 AND _
         z <= cz + par3 AND z >= cz - par3 THEN

        FUNCTION = 1
      END IF

    ELSEIF lType = %TBGL_OBJ_CYLINDER THEN

      IF par3 = 0 THEN EXIT FUNCTION

      LOCAL radiusAtY AS SINGLE
      LOCAL UpFactor AS SINGLE

      UpFactor = par2 - par1
      radiusAtY = par1 + (( y - cy ) / par3 ) * UpFactor + 0.05

      IF y <= cy + par3 AND y >= cy AND SQR(( x - cx ) * ( x - cx ) + ( z - cz ) * ( z - cz )) <= radiusAtY THEN FUNCTION = 1

    END IF

  END IF

END FUNCTION

' -- Test whether point is behind current view
FUNCTION Exec_TBGL_IsPointBehindView( ) AS EXT

  LOCAL x, y, z AS EXT
  LOCAL winx, winy, winz AS DOUBLE
  thinBasic_Parse3Numbers( x, y, z )

  ' -- This is kind of hack, but useful one
  ' -- we get OpenGL matrices, and then transform the passed point
  ' -- If new coordinate is bigger than one, it is behind us
  ' -- Trick stolen from OpenGL.org forums
  glGetIntegerV( %GL_VIEWPORT, VIEWPORT( 0 ))
  glGetDoubleV( %GL_MODELVIEW_MATRIX, mvmatrix( 0 ))
  glGetDoubleV( %GL_PROJECTION_MATRIX, projmatrix( 0 ))

  gluProject( x, y, z, mvmatrix( 0 ), projmatrix( 0 ), VIEWPORT( 0 ), winx, winy, winz )

  FUNCTION = IIF( winz > 1, 1, 0 )

END FUNCTION


' -- Tests whether passed coordinates are visible on screen
FUNCTION Exec_TBGL_IsPointVisible( ) AS EXT
  LOCAL x, y, z AS EXT
  LOCAL winx, winy, winz AS DOUBLE
  LOCAL bufferZ AS SINGLE

  thinBasic_Parse3Numbers( x, y, z )
  ' -- Similar trick to the one used in Exec_TBGL_IsPointBehindView
  glGetIntegerV( %GL_VIEWPORT, VIEWPORT( 0 ))
  glGetDoubleV( %GL_MODELVIEW_MATRIX, mvmatrix( 0 ))
  glGetDoubleV( %GL_PROJECTION_MATRIX, projmatrix( 0 ))

  gluProject( x, y, z, mvmatrix( 0 ), projmatrix( 0 ), VIEWPORT( 0 ), winx, winy, winz )

  glReadPixels( winx, winy, 1, 1, %GL_DEPTH_COMPONENT, %GL_FLOAT, bufferZ )

  FUNCTION = IIF( bufferZ < winZ, 0, 1 )
END FUNCTION

' -- Converting 3D position to screen coordinates
SUB Exec_TBGL_Pos3DtoPos2D( )

  REGISTER j AS LONG
  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL eResult1 AS EXT
  LOCAL eResult2 AS EXT

  LOCAL x, y, z AS EXT
  LOCAL dx, dy, dz AS DOUBLE
  LOCAL winx, winy, winz AS DOUBLE

  LOCAL ParensPresent AS LONG
  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE

  thinBasic_ParseNumber x
  IF thinBasic_CheckComma_Mandatory THEN
    thinBasic_ParseNumber y
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber z
      IF thinBasic_CheckComma_Mandatory THEN

        thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )

        IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )

          IF thinBasic_ErrorFree THEN
            glFlush
            glGetIntegerV( %GL_VIEWPORT, VIEWPORT( 0 ))
            glGetDoubleV( %GL_MODELVIEW_MATRIX, mvmatrix( 0 ))
            glGetDoubleV( %GL_PROJECTION_MATRIX, projmatrix( 0 ))

            dx = x
            dy = y
            dz = z

            ' -- We project the point
            gluProject( dx, dy, dz, mvmatrix( 0 ), projmatrix( 0 ), Viewport( 0 ), winx, winy, winz )

            eResult1 = winx
            eResult2 = winy

            ' -- And put what we get back to passed variables
            thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, eResult1 )
            thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, eResult2 )

          END IF
        END IF
      END IF
    END IF
  END IF

  IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

END SUB

' -----------------------------------------------------------------------------

TYPE TBGL_tUseInfo
  Fog AS LONG
  Texturing AS LONG
  Blend AS LONG
  Depth AS LONG
  DepthMask AS LONG
  Lighting AS LONG
  Alpha AS LONG
  LineStipple AS LONG
  RenderMatrixMode AS LONG
END TYPE

' -- Retrieves information on OpenGL states
SUB Exec_TBGL_UseQuery( )

  LOCAL pVar, pData AS LONG

  IF thinBasic_CheckOpenParens_Mandatory( ) THEN

    thinBasic_VariableParsePtr( pVar )
    pData = thinBasic_DirectPtrToDataPtr( pVar )

    thinBasic_CheckCloseParens_Mandatory( )

    IF thinBasic_ErrorFree THEN
      LOCAL b AS BYTE
      glGetBooleanV( %GL_DEPTH_WRITEMASK, b )
      REDIM MyUDT( 1& TO 1& ) AS TBGL_tUseInfo AT pData
      MyUDT( 1& ).Fog = glIsEnabled( %GL_FOG )
      MyUDT( 1& ).Texturing = glIsEnabled( %GL_TEXTURE_2D )
      MyUDT( 1& ).Blend = glIsEnabled( %GL_BLEND )
      MyUDT( 1& ).Depth = glIsEnabled( %GL_DEPTH_TEST )
      MyUDT( 1& ).DepthMask = b
      MyUDT( 1& ).Lighting = glIsEnabled( %GL_LIGHTING )
      MyUDT( 1& ).Alpha = glIsEnabled( %GL_ALPHA_TEST )
      MyUDT( 1& ).LineStipple = glIsEnabled( %GL_LINE_STIPPLE )
      MyUDT( 1& ).RenderMatrixMode = g_Win.RenderMatrixMode
    END IF

  END IF

END SUB

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
' -- %TBGL_OBJ_SPHERE, %TBGL_OBJ_CUBE use par1 only, %TBGL_OBJ_CUBE3, %TBGL_OBJ_CYLINDER use par1..par3
FUNCTION tbgl_PointInside3D ALIAS "tbgl_PointInside3D" ( BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE, BYVAL lType AS LONG, _
                                                         BYVAL cx AS DOUBLE, BYVAL cy AS DOUBLE, BYVAL cz AS DOUBLE, _
                                                         BYVAL par1 AS DOUBLE, BYVAL par2 AS DOUBLE, BYVAL par3 AS DOUBLE ) EXPORT AS LONG

  ' -- Note, there is used esoteric constant 0.05 everywhere, why?
  ' -- Because values returned from z-buffer are not so precise :]

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

  ELSEIF lType = %TBGL_OBJ_CUBE3 THEN

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

END FUNCTION

' -- Test whether point is behind current view
FUNCTION tbgl_IsPointBehindView ALIAS "tbgl_IsPointBehindView" ( BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE ) EXPORT AS LONG

  LOCAL winx, winy, winz AS DOUBLE

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
FUNCTION tbgl_IsPointVisible ALIAS "tbgl_IsPointVisible" ( BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE ) EXPORT AS LONG
  LOCAL winx, winy, winz AS DOUBLE
  LOCAL bufferZ AS SINGLE

  ' -- Similar trick to the one used in tbgl_IsPointBehindView
  glGetIntegerV( %GL_VIEWPORT, VIEWPORT( 0 ))
  glGetDoubleV( %GL_MODELVIEW_MATRIX, mvmatrix( 0 ))
  glGetDoubleV( %GL_PROJECTION_MATRIX, projmatrix( 0 ))

  gluProject( x, y, z, mvmatrix( 0 ), projmatrix( 0 ), VIEWPORT( 0 ), winx, winy, winz )

  glReadPixels( winx, winy, 1, 1, %GL_DEPTH_COMPONENT, %GL_FLOAT, bufferZ )

  FUNCTION = IIF( bufferZ < winZ, 0, 1 )
END FUNCTION

' -- Converting 3D position to screen coordinates
SUB tbgl_Pos3DtoPos2D ALIAS "tbgl_Pos3DtoPos2D" ( BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE, BYREF x2D AS DOUBLE, BYREF y2D AS DOUBLE ) EXPORT

  LOCAL winz AS DOUBLE

  glFlush
  glGetIntegerV( %GL_VIEWPORT, VIEWPORT( 0 ))
  glGetDoubleV( %GL_MODELVIEW_MATRIX, mvmatrix( 0 ))
  glGetDoubleV( %GL_PROJECTION_MATRIX, projmatrix( 0 ))

  ' -- We project the point
  gluProject( x, y, z, mvmatrix( 0 ), projmatrix( 0 ), Viewport( 0 ), x2D, y2D, winz )

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
SUB tbgl_UseQuery ALIAS "tbgl_UseQuery" ( BYREF useInfo AS TBGL_tUseInfo ) EXPORT

  LOCAL b AS BYTE
  glGetBooleanV( %GL_DEPTH_WRITEMASK, b )
  useInfo.Fog = glIsEnabled( %GL_FOG )
  useInfo.Texturing = glIsEnabled( %GL_TEXTURE_2D )
  useInfo.Blend = glIsEnabled( %GL_BLEND )
  useInfo.Depth = glIsEnabled( %GL_DEPTH_TEST )
  useInfo.DepthMask = b
  useInfo.Lighting = glIsEnabled( %GL_LIGHTING )
  useInfo.Alpha = glIsEnabled( %GL_ALPHA_TEST )
  useInfo.LineStipple = glIsEnabled( %GL_LINE_STIPPLE )
  useInfo.RenderMatrixMode = g_Win.RenderMatrixMode

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

' -- 8 params: sphere or cube, 10 params: cube3 or cylinder; other combinations report outside
FUNCTION Exec_TBGL_PointInside3D( ) AS LONG

  LOCAL x, y, z, cType, cx, cy, cz, par1, par2, par3 AS EXT
  LOCAL lType AS LONG
  LOCAL numParsed AS LONG
  numParsed = thinBASIC_ParseXNumbers( 8, 10, x, y, z, cType, cx, cy, cz, par1, par2, par3 )

  lType = cType

  IF ( numParsed = 8 AND ( lType = %TBGL_OBJ_SPHERE OR lType = %TBGL_OBJ_CUBE )) OR _
     ( numParsed = 10 AND ( lType = %TBGL_OBJ_CUBE3 OR lType = %TBGL_OBJ_CYLINDER )) THEN
    FUNCTION = tbgl_PointInside3D( x, y, z, lType, cx, cy, cz, par1, par2, par3 )
  END IF

END FUNCTION

FUNCTION Exec_TBGL_IsPointBehindView( ) AS EXT
  LOCAL x, y, z AS EXT

  thinBasic_Parse3Numbers( x, y, z )
  FUNCTION = tbgl_IsPointBehindView( x, y, z )

END FUNCTION

FUNCTION Exec_TBGL_IsPointVisible( ) AS EXT
  LOCAL x, y, z AS EXT

  thinBasic_Parse3Numbers( x, y, z )
  FUNCTION = tbgl_IsPointVisible( x, y, z )

END FUNCTION

SUB Exec_TBGL_Pos3DtoPos2D( )

  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL x, y, z AS EXT
  LOCAL x2D, y2D AS DOUBLE

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
            tbgl_Pos3DtoPos2D( x, y, z, x2D, y2D )

            ' -- And put what we get back to passed variables
            thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, x2D )
            thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, y2D )

          END IF
        END IF
      END IF
    END IF
  END IF

  IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

END SUB

SUB Exec_TBGL_UseQuery( )

  LOCAL pVar, pData AS LONG

  IF thinBasic_CheckOpenParens_Mandatory( ) THEN

    thinBasic_VariableParsePtr( pVar )
    pData = thinBasic_DirectPtrToDataPtr( pVar )

    thinBasic_CheckCloseParens_Mandatory( )

    IF thinBasic_ErrorFree THEN
      tbgl_UseQuery( BYVAL pData )
    END IF

  END IF

END SUB

#ENDIF

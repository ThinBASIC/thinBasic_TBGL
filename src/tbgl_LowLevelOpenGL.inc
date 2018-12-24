'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : Low level functions mostly for pure OpenGL coder
' =============================================================================

' -----------------------------------------------------------------------------
GLOBAL gOpenGLStarted AS LONG       ' -- Did OpenGL started already? 0 = not, <> 0 = yes
GLOBAL gOpenGLExtensions AS STRING  ' -- List of detected OpenGL extensions
GLOBAL gvOpenGL AS LONG             ' -- OpenGL version a*1000 + b*100 + c*10 + d
GLOBAL mvmatrix()   AS DOUBLE       ' -- OpenGL matrices
GLOBAL projmatrix() AS DOUBLE
GLOBAL Viewport()    AS LONG

' -- Prototype for V-Sync
GLOBAL ptr_wglSwapIntervalEXT AS DWORD
DECLARE FUNCTION prototype_wglSwapIntervalEXT( BYVAL NUMBER AS LONG ) AS LONG

' -- Tests support for passed extensions
FUNCTION Exec_TBGL_oglExtensionSupport( ) AS EXT

  LOCAL Extensions AS STRING
  Extensions = thinBasic_Parse1String
  FUNCTION = internal_IsExtensionSupported( Extensions )

END FUNCTION

' -- Tests whether AT LEAST passed version is supported
FUNCTION Exec_TBGL_oglVersionSupport( ) AS EXT

  LOCAL Major, Minor AS EXT

  thinBasic_Parse2Numbers( Major, Minor )

  FUNCTION = IIF( gvOpenGL >= Major * 1000 + Minor * 100, %TRUE, %FALSE )

END FUNCTION

' -- Returns OpenGL version
FUNCTION Exec_TBGL_oglVersion( ) AS STRING

  LOCAL Major, Minor AS EXT

  LOCAL pVersion AS ASCIIZ PTR
  LOCAL sVersion AS STRING
  pVersion = glGetString(%GL_VERSION)
  sVersion = TRIM$(@pVersion)
  FUNCTION = sVersion

  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  IF thinBasic_CheckOpenParens_Mandatory() THEN
    IF thinBasic_CheckCloseParens_Optional() = 0 AND thinBasic_ErrorFree THEN
      thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )
        IF thinBasic_CheckCloseParens_Mandatory() THEN

          '---Now assign values to referenced variables
          thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, VAL(PARSE$(sVersion, ".", 1)) )
          thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, VAL(PARSE$(sVersion, ".", 2)) )

        END IF
      END IF
    END IF
  END IF

END FUNCTION

' -- Returns OpenGL renderer
FUNCTION Exec_TBGL_oglRenderer( ) AS STRING

  LOCAL pRenderer AS ASCIIZ PTR
  LOCAL sRenderer AS STRING
  pRenderer = glGetString(%GL_RENDERER)
  sRenderer = TRIM$(@pRenderer)

  IF thinBasic_CheckOpenParens_Optional() THEN
    thinBasic_CheckCloseParens_Optional()
  END IF

  FUNCTION = sRenderer
END FUNCTION

' -- Returns >active< OpenGL rendering context
FUNCTION Exec_TBGL_oglRenderContext( ) AS EXT

  IF thinBasic_CheckOpenParens_Optional() THEN
    thinBasic_CheckCloseParens_Optional()
  END IF

  '[!] Needs to be changed for multiwindow with smarter query?
  FUNCTION = g_Win.hRC

'  LOCAL hPassed AS ext
'
'
'  hPassed = thinBasic_Parse1Number()
'
'  '[!] Needs to be changed for multiwindow with smarter query
'  if g_Win.handle = hPassed then
'    function = g_Win.hRC
'  else
'    FUNCTION = %NULL
'  end if

END FUNCTION

FUNCTION Exec_TBGL_oglDeviceContext( ) AS EXT

  IF thinBasic_CheckOpenParens_Optional() THEN
    thinBasic_CheckCloseParens_Optional()
  END IF

  '[!] Needs to be changed for multiwindow with smarter query?
  FUNCTION = g_Win.hDC

'  LOCAL hPassed AS ext
'
'
'  hPassed = thinBasic_Parse1Number()
'
'  '[!] Needs to be changed for multiwindow with smarter query
'  if g_Win.handle = hPassed then
'    function = g_Win.hRC
'  else
'    FUNCTION = %NULL
'  end if

END FUNCTION


' -- Returns whether openGL is accelerated renderer
FUNCTION Exec_TBGL_oglIsAccelerated( ) AS EXT

  LOCAL pVendor AS ASCIIZ PTR
  LOCAL sVendor AS STRING
  pVendor = glGetString(%GL_VENDOR)
  sVendor = UCASE$(TRIM$(@pVendor))

  IF thinBasic_CheckOpenParens_Optional() THEN
    thinBasic_CheckCloseParens_Optional()
  END IF

  FUNCTION = IIF(INSTR(sVendor, "MICROSOFT"), %FALSE, %TRUE)
END FUNCTION

' -- Returns OpenGL 1.1+ entry points
FUNCTION Exec_TBGL_GetProcAddress( ) AS EXT
  ' -- [!] Extend for TBGL extensions later
  LOCAL Extension AS STRING
  Extension = thinBasic_Parse1String
  FUNCTION = wglGetProcAddress( Extension + "" )

END FUNCTION

' -----------------------------------------------------------------------------
'
GLOBAL StateProtectStack() AS LONG
GLOBAL StateProtectStackDepth AS LONG

' -- Protects section from states
' -- It detects whether some of the states are active,
' -- if yes, then it disables them and only those it
' -- remember for re-activation
SUB Exec_TBGL_PushStateProtect( )

  LOCAL parameter AS EXT
  LOCAL bitMask AS LONG
  LOCAL b AS BYTE

  INCR StateProtectStackDepth
  IF StateProtectStackDepth > UBOUND(StateProtectStack) THEN REDIM PRESERVE StateProtectStack(1 TO StateProtectStackDepth+4)

  thinBasic_ParseNumber parameter
  bitMask = parameter

  ' -- Unwanted states list truly just those states, which must be turned back ( which ARE enabled in fact )
  LOCAL UnWantedStates AS LONG

  ' bits 0 to 7 are:
  ' 0 FOG
  ' 1 TEXTURING
  ' 2 BLEND
  ' 3 ALPHA
  ' 4 DEPTH
  ' 5 DEPTHMASK
  ' 6 LIGHTING
  ' 7 LINESTIPPLE
  IF BIT(bitMask, 0) AND glIsEnabled( %GL_FOG )         THEN UnWantedStates OR= %TBGL_FOG         : glDisable(%GL_FOG)
  IF BIT(bitMask, 1) AND glIsEnabled( %GL_TEXTURE_2D )  THEN UnWantedStates OR= %TBGL_TEXTURING   : glDisable(%GL_TEXTURE_2D)
  IF BIT(bitMask, 2) AND glIsEnabled( %GL_BLEND )       THEN UnWantedStates OR= %TBGL_BLEND       : glDisable(%GL_BLEND)
  IF BIT(bitMask, 3) AND glIsEnabled( %GL_ALPHA_TEST )  THEN UnWantedStates OR= %TBGL_ALPHA       : glDisable(%GL_ALPHA_TEST)
  IF BIT(bitMask, 4) AND glIsEnabled( %GL_DEPTH_TEST )  THEN UnWantedStates OR= %TBGL_DEPTH       : glDisable(%GL_DEPTH_TEST)
  IF BIT(bitMask, 5) THEN
    glGetBooleanV( %GL_DEPTH_WRITEMASK, b )
    IF b THEN UnWantedStates OR= %TBGL_DEPTHMASK : glDepthMask(%GL_FALSE)
  END IF
  IF BIT(bitMask, 6) AND glIsEnabled( %GL_LIGHTING )    THEN UnWantedStates OR= %TBGL_LIGHTING    : glDisable(%GL_LIGHTING)
  IF BIT(bitMask, 7) AND glIsEnabled( %GL_LINE_STIPPLE )THEN UnWantedStates OR= %TBGL_LINESTIPPLE : glDisable(%GL_LINE_STIPPLE)

  StateProtectStack(StateProtectStackDepth) = UnWantedStates

END SUB

' -- Ends protection from states
SUB Exec_TBGL_PopStateProtect( )

  IF StateProtectStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "TBGL_PopStateProtect caused stack underflow, check whether number of pairs of TBGL_PushStateProtect / TBGL_PopStateProtect match"
    END IF
    EXIT SUB
  END IF

  LOCAL bitMask AS LONG
  bitMask = StateProtectStack(StateProtectStackDepth)

  IF BIT(bitMask, 0) THEN glEnable(%GL_FOG)
  IF BIT(bitMask, 1) THEN glEnable(%GL_TEXTURE_2D)
  IF BIT(bitMask, 2) THEN glEnable(%GL_BLEND)
  IF BIT(bitMask, 3) THEN glEnable(%GL_ALPHA_TEST)
  IF BIT(bitMask, 4) THEN glEnable(%GL_DEPTH_TEST)
  IF BIT(bitMask, 5) THEN glDepthMask(%GL_TRUE)
  IF BIT(bitMask, 6) THEN glEnable(%GL_LIGHTING)
  IF BIT(bitMask, 7) THEN glEnable(%GL_LINE_STIPPLE)

  DECR StateProtectStackDepth

END SUB

' -----------------------------------------------------------------------------

GLOBAL StateStack() AS LONG
GLOBAL StateStackDepth AS LONG


' -- Enables states in given section, and makes sure they
' -- are disabled on the end
SUB Exec_TBGL_PushState( )

  LOCAL parameter AS EXT
  LOCAL bitMask AS LONG

  INCR StateStackDepth
  IF StateStackDepth > UBOUND(StateStack) THEN REDIM PRESERVE StateStack(1 TO StateStackDepth+4)

  thinBasic_ParseNumber parameter
  bitMask = parameter

  ' bits 0 to 7 are:
  ' 0 FOG
  ' 1 TEXTURING
  ' 2 BLEND
  ' 3 ALPHA
  ' 4 DEPTH
  ' 5 DEPTHMASK
  ' 6 LIGHTING
  ' 7 LINESTIPPLE
  IF BIT(bitMask, 0) THEN glEnable(%GL_FOG)
  IF BIT(bitMask, 1) THEN glEnable(%GL_TEXTURE_2D)
  IF BIT(bitMask, 2) THEN glEnable(%GL_BLEND)
  IF BIT(bitMask, 3) THEN glEnable(%GL_ALPHA_TEST)
  IF BIT(bitMask, 4) THEN glEnable(%GL_DEPTH_TEST)
  IF BIT(bitMask, 5) THEN glDepthMask(%GL_TRUE)
  IF BIT(bitMask, 6) THEN glEnable(%GL_LIGHTING)
  IF BIT(bitMask, 7) THEN glEnable(%GL_LINE_STIPPLE)

  StateStack(StateStackDepth) = bitMask

END SUB

' -- Ends states
SUB Exec_TBGL_PopState( )

  IF StateStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "TBGL_PopState caused stack underflow, check whether number of pairs of TBGL_PushState / TBGL_PopState match"
    END IF
    EXIT SUB
  END IF

  LOCAL bitMask AS LONG
  bitMask = StateStack(StateStackDepth)

  IF BIT(bitMask, 0) THEN glDisable(%GL_FOG)
  IF BIT(bitMask, 1) THEN glDisable(%GL_TEXTURE_2D)
  IF BIT(bitMask, 2) THEN glDisable(%GL_BLEND)
  IF BIT(bitMask, 3) THEN glDisable(%GL_ALPHA_TEST)
  IF BIT(bitMask, 4) THEN glDisable(%GL_DEPTH_TEST)
  IF BIT(bitMask, 5) THEN glDepthMask(%GL_FALSE)
  IF BIT(bitMask, 6) THEN glDisable(%GL_LIGHTING)
  IF BIT(bitMask, 7) THEN glDisable(%GL_LINE_STIPPLE)

  DECR StateStackDepth

END SUB

' -----------------------------------------------------------------------------

GLOBAL LineWidthStack() AS LONG
GLOBAL LineWidthStackDepth AS LONG


' -- Enables line width in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushLineWidth( )

  LOCAL parameter AS EXT

  INCR LineWidthStackDepth
  IF LineWidthStackDepth > UBOUND(LineWidthStack) THEN REDIM PRESERVE LineWidthStack(1 TO LineWidthStackDepth+4)

  thinBasic_ParseNumber parameter
  ' -- Backup the line width
  glGetIntegerV(%GL_LINE_WIDTH, LineWidthStack(LineWidthStackDepth))

  ' -- Set the new one
  glLineWidth(parameter)
END SUB

' -- Ends line width region
SUB Exec_TBGL_PopLineWidth( )

  IF LineWidthStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopLineWidth caused stack underflow, check whether number of pairs of TBGL_PushLineWidth / TBGL_PopLineWidth match"
    END IF
    EXIT SUB
  END IF

  glLineWidth(LineWidthStack(LineWidthStackDepth))

  DECR LineWidthStackDepth

END SUB

' -----------------------------------------------------------------------------
TYPE LineStipple_Item
  factor AS LONG
  pattern AS LONG
END TYPE

GLOBAL LineStippleStack() AS LineStipple_Item
GLOBAL LineStippleStackDepth AS LONG


' -- Enables line stipple in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushLineStipple( )

  LOCAL factor, pattern AS EXT

  INCR LineStippleStackDepth
  IF LineStippleStackDepth > UBOUND(LineStippleStack) THEN REDIM PRESERVE LineStippleStack(1 TO LineStippleStackDepth+4)

  thinBasic_Parse2Numbers factor, pattern
  ' -- Backup the line width
  glGetIntegerV(%GL_LINE_STIPPLE_REPEAT, LineStippleStack(LineStippleStackDepth).factor)
  glGetIntegerV(%GL_LINE_STIPPLE_PATTERN, LineStippleStack(LineStippleStackDepth).pattern)

  ' -- Set the new one
  glLineStipple(factor, pattern)
END SUB

' -- Ends line stipple region
SUB Exec_TBGL_PopLineStipple( )

  IF LineStippleStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopLineStipple caused stack underflow, check whether number of pairs of TBGL_PushLineStipple / TBGL_PopLineStipple match"
    END IF
    EXIT SUB
  END IF

  glLineStipple(LineStippleStack(LineStippleStackDepth).factor, LineStippleStack(LineStippleStackDepth).pattern)

  DECR LineStippleStackDepth

END SUB

' -----------------------------------------------------------------------------

GLOBAL PointSizeStack() AS LONG
GLOBAL PointSizeStackDepth AS LONG


' -- Enables point size in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushPointSize( )

  LOCAL parameter AS EXT

  INCR PointSizeStackDepth
  IF PointSizeStackDepth > UBOUND(PointSizeStack) THEN REDIM PRESERVE PointSizeStack(1 TO PointSizeStackDepth+4)

  thinBasic_ParseNumber parameter
  ' -- Backup the line width
  glGetIntegerV(%GL_POINT_SIZE, PointSizeStack(PointSizeStackDepth))

  ' -- Set the new one
  glPointSize(parameter)
END SUB

' -- Ends point size region
SUB Exec_TBGL_PopPointSize( )

  IF PointSizeStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopPointSize caused stack underflow, check whether number of pairs of TBGL_PushPointSize / TBGL_PopPointSize match"
    END IF
    EXIT SUB
  END IF

  glPointSize(PointSizeStack(PointSizeStackDepth))

  DECR PointSizeStackDepth

END SUB

' -----------------------------------------------------------------------------
TYPE ColorStack_Item
  R   AS SINGLE
  G   AS SINGLE
  B   AS SINGLE
  A   AS SINGLE
END TYPE

GLOBAL ColorStack() AS ColorStack_Item
GLOBAL ColorStackDepth AS LONG


' -- Enables point size in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushColor( )

  LOCAL r, g, b, a, n AS EXT

  INCR ColorStackDepth
  IF ColorStackDepth > UBOUND(ColorStack) THEN REDIM PRESERVE ColorStack(1 TO ColorStackDepth+4)

  a = 255
  n = thinBasic_ParseXNumbers( 3, 4, r, g, b, a )

  ' -- Backup the line width
  glGetFloatV(%GL_CURRENT_COLOR, BYVAL VARPTR(ColorStack(ColorStackDepth)))

  ' -- Set the new one
  glColor4f(r/255, g/255, b/255, a/255)

END SUB

' -- Ends point size region
SUB Exec_TBGL_PopColor( )

  IF ColorStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopColor caused stack underflow, check whether number of pairs of TBGL_PushColor / TBGL_PopColor match"
    END IF
    EXIT SUB
  END IF

  glColor4fv(BYVAL VARPTR(ColorStack(ColorStackDepth)))
  DECR ColorStackDepth

END SUB

' -----------------------------------------------------------------------------

GLOBAL PolygonLookStack() AS LONG
GLOBAL PolygonLookStackDepth AS LONG


' -- Enables line width in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushPolygonLook( )

  LOCAL parameter AS EXT

  INCR PolygonLookStackDepth
  IF PolygonLookStackDepth > UBOUND(PolygonLookStack) THEN REDIM PRESERVE PolygonLookStack(1 TO PolygonLookStackDepth+4)

  thinBasic_ParseNumber parameter
  ' -- Backup the line width
  glGetIntegerV(%GL_POLYGON_MODE, PolygonLookStack(PolygonLookStackDepth))

  ' -- Set the new one
  glPolygonMode(%GL_FRONT_AND_BACK, parameter)
END SUB

' -- Ends line width region
SUB Exec_TBGL_PopPolygonLook( )

  IF PolygonLookStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopPolygonLook caused stack underflow, check whether number of pairs of TBGL_PushPolygonLook / TBGL_PopPolygonLook match"
    END IF
    EXIT SUB
  END IF

  glPolygonMode(%GL_FRONT_AND_BACK, PolygonLookStack(PolygonLookStackDepth))

  DECR PolygonLookStackDepth

END SUB


' =============================================================================
' -- Checks whether extension is supported
FUNCTION internal_IsExtensionSupported( Extensions AS STRING ) AS EXT

  REGISTER i AS LONG, j AS LONG

  Extensions = UCASE$( Extensions )

  LOCAL n, n2 AS LONG
  LOCAL found AS BYTE
  n = PARSECOUNT( Extensions, $SPC )
  n2 = PARSECOUNT( gOpenGLExtensions, $SPC )
  DIM Token( 1 TO n ) AS STRING
  DIM Exts( 1 TO n2 ) AS STRING

  PARSE Extensions, Token( ), $SPC
  PARSE gOpenGLExtensions, Exts( ), $SPC

  FOR i = 1 TO n

    found = %FALSE

    FOR j = 1 TO n2
      IF Token( i ) = Exts( j ) THEN
        found = %TRUE
        EXIT FOR
      END IF
    NEXT

    IF found = %FALSE THEN EXIT FOR
  NEXT

  FUNCTION = found

END FUNCTION

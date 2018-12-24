'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to depth
' =============================================================================

' -----------------------------------------------------------------------------

' -- Sets depth testing on or off
SUB Exec_TBGL_UseDepth()

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use1Param(%GL_DEPTH_TEST)

END SUB

' -- Manages depth masking, useful when working with particles
SUB Exec_TBGL_UseDepthMask( )

  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  IF flag THEN glDepthMask( %GL_TRUE ) ELSE glDepthMask( %GL_FALSE )

END SUB

' -- Retrieves state of depth test
FUNCTION Exec_TBGL_GetUseDepth( ) AS EXT
  ' ()
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = glIsEnabled( %GL_DEPTH_TEST )
END FUNCTION

' -- Sets alpha func to be used when alpha testing is on
SUB Exec_TBGL_DepthFunc( )

  LOCAL func AS EXT

  func = thinBasic_Parse1Number()

  glDepthFunc( func )

END SUB

' -----------------------------------------------------------------------------

GLOBAL DepthFuncStack() AS LONG
GLOBAL DepthFuncStackDepth AS LONG


' -- Enables line width in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushDepthFunc( )

  LOCAL parameter AS EXT

  INCR DepthFuncStackDepth
  IF DepthFuncStackDepth > UBOUND(DepthFuncStack) THEN REDIM PRESERVE DepthFuncStack(1 TO DepthFuncStackDepth+4)

  thinBasic_ParseNumber parameter
  ' -- Backup the line width
  glGetIntegerV(%GL_DEPTH_FUNC, DepthFuncStack(DepthFuncStackDepth))

  ' -- Set the new one
  glDepthFunc(parameter)

END SUB

' -- Ends line width region
SUB Exec_TBGL_PopDepthFunc( )

  IF DepthFuncStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopDepthFunc caused stack underflow, check whether number of pairs of TBGL_PushDepthFunc / TBGL_PopDepthFunc match"
    END IF
    EXIT SUB
  END IF

  glDepthFunc(DepthFuncStack(DepthFuncStackDepth))

  DECR DepthFuncStackDepth

END SUB

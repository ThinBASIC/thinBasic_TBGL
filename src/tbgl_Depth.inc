'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to depth
' =============================================================================

' -----------------------------------------------------------------------------

' -- Sets depth testing on or off
SUB tbgl_UseDepth ALIAS "tbgl_UseDepth" ( BYVAL flag AS LONG ) EXPORT

  IF flag THEN glEnable %GL_DEPTH_TEST ELSE glDisable %GL_DEPTH_TEST

END SUB

' -- Manages depth masking, useful when working with particles
SUB tbgl_UseDepthMask ALIAS "tbgl_UseDepthMask" ( BYVAL flag AS LONG ) EXPORT

  IF flag THEN glDepthMask( %GL_TRUE ) ELSE glDepthMask( %GL_FALSE )

END SUB

' -- Retrieves state of depth test
FUNCTION tbgl_GetUseDepth ALIAS "tbgl_GetUseDepth" ( ) EXPORT AS LONG
  FUNCTION = glIsEnabled( %GL_DEPTH_TEST )
END FUNCTION

' -- Sets alpha func to be used when alpha testing is on
SUB tbgl_DepthFunc ALIAS "tbgl_DepthFunc" ( BYVAL func AS DWORD ) EXPORT

  glDepthFunc( func )

END SUB

' -----------------------------------------------------------------------------

GLOBAL DepthFuncStack() AS LONG
GLOBAL DepthFuncStackDepth AS LONG


' -- Enables line width in given section, and makes sure it is returned back on the end
SUB tbgl_PushDepthFunc ALIAS "tbgl_PushDepthFunc" ( BYVAL func AS DWORD ) EXPORT

  INCR DepthFuncStackDepth
  IF DepthFuncStackDepth > UBOUND(DepthFuncStack) THEN REDIM PRESERVE DepthFuncStack(1 TO DepthFuncStackDepth+4)

  ' -- Backup the line width
  glGetIntegerV(%GL_DEPTH_FUNC, DepthFuncStack(DepthFuncStackDepth))

  ' -- Set the new one
  glDepthFunc(func)

END SUB

' -- Ends line width region
SUB tbgl_PopDepthFunc ALIAS "tbgl_PopDepthFunc" ( ) EXPORT

  IF DepthFuncStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopDepthFunc caused stack underflow, check whether number of pairs of TBGL_PushDepthFunc / TBGL_PopDepthFunc match"
    END IF
    EXIT SUB
  END IF

  glDepthFunc(DepthFuncStack(DepthFuncStackDepth))

  DECR DepthFuncStackDepth

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_UseDepth()
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  IF thinBasic_ErrorFree THEN tbgl_UseDepth( ISTRUE flag )

END SUB

SUB Exec_TBGL_UseDepthMask( )
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  tbgl_UseDepthMask( ISTRUE flag )

END SUB

FUNCTION Exec_TBGL_GetUseDepth( ) AS EXT
  ' ()
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = tbgl_GetUseDepth( )
END FUNCTION

SUB Exec_TBGL_DepthFunc( )
  tbgl_DepthFunc( thinBasic_Parse1Number( ))
END SUB

SUB Exec_TBGL_PushDepthFunc( )
  LOCAL parameter AS EXT

  thinBasic_ParseNumber parameter
  tbgl_PushDepthFunc( parameter )

END SUB

SUB Exec_TBGL_PopDepthFunc( )
  tbgl_PopDepthFunc( )
END SUB

#ENDIF

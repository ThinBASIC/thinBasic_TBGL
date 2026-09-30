
'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to color+alpha
' =============================================================================

' -----------------------------------------------------------------------------

' -- Colors all subsequent primitives, fully opaque
SUB tbgl_Color ALIAS "tbgl_Color" ( BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE ) EXPORT

  tbgl_ColorAlpha( r, g, b, 255 )

END SUB


' -- Colors all subsequent primitives
SUB tbgl_ColorAlpha ALIAS "tbgl_ColorAlpha" ( BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE ) EXPORT

  internal_ColorAlphaF( r / 255, g / 255, b / 255, a / 255 )

END SUB

' -- Components in 0..1, not clamped (thinBasic keywords pass script values / 255 unrounded)
SUB internal_ColorAlphaF( BYVAL r AS SINGLE, BYVAL g AS SINGLE, BYVAL b AS SINGLE, BYVAL a AS SINGLE )

  ' -- [!] Going FP path for all, driver seems to do it this way
  glColor4f (r, g, b, a)
'  glColor4ub r, g, b, a

END SUB


' -- Sets background color for rendering
SUB tbgl_BackColor ALIAS "tbgl_BackColor" ( BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE ) EXPORT

  internal_BackColorF( r / 255, g / 255, b / 255, a / 255 )

END SUB

' -- Components in 0..1, not clamped
SUB internal_BackColorF( BYVAL r AS SINGLE, BYVAL g AS SINGLE, BYVAL b AS SINGLE, BYVAL a AS SINGLE )

  glClearColor r, g, b, a

END SUB

' -- Sets alpha testing on or off
SUB tbgl_UseAlphaTest ALIAS "tbgl_UseAlphaTest" ( BYVAL flag AS LONG ) EXPORT

  IF flag THEN glEnable %GL_ALPHA_TEST ELSE glDisable %GL_ALPHA_TEST

END SUB

' -- Sets alpha func to be used when alpha testing is on
SUB tbgl_AlphaFunc ALIAS "tbgl_AlphaFunc" ( BYVAL func AS DWORD, BYVAL value AS SINGLE ) EXPORT

  glAlphaFunc func, value

END SUB

' -----------------------------------------------------------------------------
TYPE AlphaFunc_Item
  func  AS LONG
  value AS SINGLE
END TYPE

GLOBAL AlphaFuncStack() AS AlphaFunc_Item
GLOBAL AlphaFuncStackDepth AS LONG


' -- Enables line stipple in given section, and makes sure it is returned back on the end
SUB tbgl_PushAlphaFunc ALIAS "tbgl_PushAlphaFunc" ( BYVAL func AS DWORD, BYVAL value AS SINGLE ) EXPORT

  INCR AlphaFuncStackDepth
  IF AlphaFuncStackDepth > UBOUND(AlphaFuncStack) THEN REDIM PRESERVE AlphaFuncStack(1 TO AlphaFuncStackDepth+4)

  ' -- Backup the line width
  glGetIntegerV(%GL_ALPHA_TEST_FUNC, AlphaFuncStack(AlphaFuncStackDepth).func)
  glGetFloatV(%GL_ALPHA_TEST_REF, AlphaFuncStack(AlphaFuncStackDepth).value)

  ' -- Set the new one
  glAlphaFunc(func, value)

END SUB

' -- Ends line stipple region
SUB tbgl_PopAlphaFunc ALIAS "tbgl_PopAlphaFunc" ( ) EXPORT

  IF AlphaFuncStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "TBGL_PopAlphaFunc caused stack underflow, check whether number of pairs of TBGL_PushAlphaFunc / TBGL_PopAlphaFunc match"
    END IF
    EXIT SUB
  END IF

  glAlphaFunc(AlphaFuncStack(AlphaFuncStackDepth).func, AlphaFuncStack(AlphaFuncStackDepth).value)

  DECR AlphaFuncStackDepth

END SUB

' ----------------------------------------------------------------------------
GLOBAL LogicOpStack() AS LONG
GLOBAL LogicOpStackDepth AS LONG

SUB tbgl_PushLogicOp ALIAS "tbgl_PushLogicOp" ( BYVAL logicOp AS DWORD ) EXPORT

  INCR LogicOpStackDepth
  IF LogicOpStackDepth > UBOUND(LogicOpStack) THEN
    REDIM PRESERVE LogicOpStack(1 TO LogicOpStackDepth+4)
  END IF

  LogicOpStack(LogicOpStackDepth) = logicOp

  IF LogicOpStackDepth = 1 THEN
    glEnable(%GL_COLOR_LOGIC_OP)
  END IF
  glLogicOp(LogicOpStack(LogicOpStackDepth))

END SUB


SUB tbgl_PopLogicOp ALIAS "tbgl_PopLogicOp" ( ) EXPORT

  DECR LogicOpStackDepth

  IF LogicOpStackDepth < 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "TBGL_PopLogicOp caused stack underflow, check whether number of pairs of TBGL_PushLogicOp / TBGL_PopLogicOp match"
    END IF
    EXIT SUB

  ELSEIF LogicOpStackDepth = 0 THEN
    glDisable(%GL_COLOR_LOGIC_OP)
    glLogicOp(%GL_COPY)

  ELSE
    glLogicOp(LogicOpStack(LogicOpStackDepth))
  END IF

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

' -- Optional 4th parameter is alpha
' -- Colors go through the float path unrounded and unclamped, as script values always did
SUB Exec_TBGL_Color ( )
  LOCAL r, g, b, a AS EXT

  a = 255
  thinBasic_ParseXNumbers( 3, 4, r, g, b, a )
  internal_ColorAlphaF( r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB Exec_TBGL_ColorAlpha( )
  LOCAL r, g, b, a AS EXT

  thinBasic_Parse4Numbers( r, g, b, a )
  internal_ColorAlphaF( r / 255, g / 255, b / 255, a / 255 )

END SUB

' -- Alpha defaults to 0
SUB Exec_TBGL_BackColor ( )
  LOCAL r, g, b, a AS EXT

  thinBASIC_ParseXNumbers( 3, 4, r, g, b, a )
  internal_BackColorF( r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB Exec_TBGL_UseAlphaTest()
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  IF thinBasic_ErrorFree THEN tbgl_UseAlphaTest( ISTRUE flag )

END SUB

SUB Exec_TBGL_AlphaFunc( )
  LOCAL fnc, param AS EXT

  thinBasic_Parse2Numbers( fnc, param )
  tbgl_AlphaFunc( fnc, param )

END SUB

SUB Exec_TBGL_PushAlphaFunc( )
  LOCAL func, value AS EXT

  thinBasic_Parse2Numbers func, value
  tbgl_PushAlphaFunc( func, value )

END SUB

SUB Exec_TBGL_PopAlphaFunc( )
  tbgl_PopAlphaFunc( )
END SUB

SUB Exec_TBGL_PushLogicOp( )
  tbgl_PushLogicOp( thinBasic_Parse1Number )
END SUB

SUB Exec_TBGL_PopLogicOp( )
  tbgl_PopLogicOp( )
END SUB

#ENDIF

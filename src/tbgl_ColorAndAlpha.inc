
'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to color+alpha
' =============================================================================

' -----------------------------------------------------------------------------

' -- Colors all subsequent primitives
SUB Exec_TBGL_Color ( )
  LOCAL r, g, b, a, n AS EXT
  a = 255
  n = thinBasic_ParseXNumbers( 3, 4, r, g, b, a )

  ' -- Depending on the input value, we go for floating point, or integer based call
  ' -- [!] Going FP path for all, driver seems to do it this way
  r /= 255
  g /= 255
  b /= 255
  a /= 255
  glColor4f(r, g, b, a)

END SUB


' -- Colors all subsequent primitives
SUB Exec_TBGL_ColorAlpha( )
  LOCAL r, g, b, a AS EXT

  thinBasic_Parse4Numbers( r, g, b, a )

  r /= 255
  g /= 255
  b /= 255
  a /= 255

  glColor4f (r, g, b, a)
'  glColor4ub r, g, b, a

END SUB


' -- Sets background color for rendering
SUB Exec_TBGL_BackColor ( )
  LOCAL r, g, b, a AS EXT

  thinBASIC_ParseXNumbers( 3, 4, r, g, b, a )

  r /= 255
  g /= 255
  b /= 255
  a /= 255

  glClearColor r, g, b, a

END SUB

' -- Sets alpha testing on or off
SUB Exec_TBGL_UseAlphaTest()

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use1Param(%GL_ALPHA_TEST)

END SUB

' -- Sets alpha func to be used when alpha testing is on
SUB Exec_TBGL_AlphaFunc( )

  LOCAL fnc, param AS EXT

  thinBasic_Parse2Numbers( fnc, param )

  glAlphaFunc fnc, param

END SUB

' -----------------------------------------------------------------------------
TYPE AlphaFunc_Item
  func  AS LONG
  value AS SINGLE
END TYPE

GLOBAL AlphaFuncStack() AS AlphaFunc_Item
GLOBAL AlphaFuncStackDepth AS LONG


' -- Enables line stipple in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushAlphaFunc( )

  LOCAL func, value AS EXT

  INCR AlphaFuncStackDepth
  IF AlphaFuncStackDepth > UBOUND(AlphaFuncStack) THEN REDIM PRESERVE AlphaFuncStack(1 TO AlphaFuncStackDepth+4)

  thinBasic_Parse2Numbers func, value
  ' -- Backup the line width
  glGetIntegerV(%GL_ALPHA_TEST_FUNC, AlphaFuncStack(AlphaFuncStackDepth).func)
  glGetFloatV(%GL_ALPHA_TEST_REF, AlphaFuncStack(AlphaFuncStackDepth).value)

  ' -- Set the new one
  glAlphaFunc(func, value)

END SUB

' -- Ends line stipple region
SUB Exec_TBGL_PopAlphaFunc( )

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

SUB Exec_TBGL_PushLogicOp( )

  LOCAL LogicOp AS EXT

  INCR LogicOpStackDepth
  IF LogicOpStackDepth > UBOUND(LogicOpStack) THEN
    REDIM PRESERVE LogicOpStack(1 TO LogicOpStackDepth+4)
  END IF

  LogicOp = thinBasic_Parse1Number

  LogicOpStack(LogicOpStackDepth) = LogicOp

  IF LogicOpStackDepth = 1 THEN
    glEnable(%GL_COLOR_LOGIC_OP)
  END IF
  glLogicOp(LogicOpStack(LogicOpStackDepth))

END SUB


SUB Exec_TBGL_PopLogicOp( )

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

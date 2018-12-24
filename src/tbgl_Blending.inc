'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to blending
' =============================================================================

' -----------------------------------------------------------------------------

' -- Sets blending on or off
SUB Exec_TBGL_UseBlend()

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use1Param(%GL_BLEND)

END SUB

' -- Setups blend func
SUB Exec_TBGL_BlendFunc( )

  LOCAL x, y AS EXT
  thinBasic_Parse2Numbers( x, y )
  glBlendFunc x, y

END SUB

' -- Is blending enabled?
FUNCTION Exec_TBGL_GetUseBlend( ) AS EXT
  ' () check
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = glIsEnabled( %GL_BLEND )
END FUNCTION

' -----------------------------------------------------------------------------
TYPE BlendFunc_Item
  src AS LONG
  dst AS LONG
END TYPE

GLOBAL BlendFuncStack() AS BlendFunc_Item
GLOBAL BlendFuncStackDepth AS LONG


' -- Enables line stipple in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushBlendFunc( )

  LOCAL src, dst AS EXT

  INCR BlendFuncStackDepth
  IF BlendFuncStackDepth > UBOUND(BlendFuncStack) THEN REDIM PRESERVE BlendFuncStack(1 TO BlendFuncStackDepth+4)

  thinBasic_Parse2Numbers src, dst
  ' -- Backup the line width
  glGetIntegerV(%GL_BLEND_SRC, BlendFuncStack(BlendFuncStackDepth).src)
  glGetIntegerV(%GL_BLEND_DST, BlendFuncStack(BlendFuncStackDepth).dst)

  ' -- Set the new one
  glBlendFunc(src, dst)
END SUB

' -- Ends line stipple region
SUB Exec_TBGL_PopBlendFunc( )

  IF BlendFuncStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopBlendFunc caused stack underflow, check whether number of pairs of TBGL_PushBlendFunc / TBGL_PopBlendFunc match"
    END IF
    EXIT SUB
  END IF

  glBlendFunc(BlendFuncStack(BlendFuncStackDepth).src, BlendFuncStack(BlendFuncStackDepth).dst)

  DECR BlendFuncStackDepth

END SUB

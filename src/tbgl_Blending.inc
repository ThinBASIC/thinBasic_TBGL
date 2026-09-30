'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to blending
' =============================================================================

' -----------------------------------------------------------------------------

' -- Sets blending on or off
SUB tbgl_UseBlend ALIAS "tbgl_UseBlend" ( BYVAL flag AS LONG ) EXPORT

  IF flag THEN glEnable %GL_BLEND ELSE glDisable %GL_BLEND

END SUB

' -- Setups blend func
SUB tbgl_BlendFunc ALIAS "tbgl_BlendFunc" ( BYVAL srcFactor AS DWORD, BYVAL dstFactor AS DWORD ) EXPORT

  glBlendFunc srcFactor, dstFactor

END SUB

' -- Is blending enabled?
FUNCTION tbgl_GetUseBlend ALIAS "tbgl_GetUseBlend" ( ) EXPORT AS LONG
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
SUB tbgl_PushBlendFunc ALIAS "tbgl_PushBlendFunc" ( BYVAL srcFactor AS DWORD, BYVAL dstFactor AS DWORD ) EXPORT

  INCR BlendFuncStackDepth
  IF BlendFuncStackDepth > UBOUND(BlendFuncStack) THEN REDIM PRESERVE BlendFuncStack(1 TO BlendFuncStackDepth+4)

  ' -- Backup the line width
  glGetIntegerV(%GL_BLEND_SRC, BlendFuncStack(BlendFuncStackDepth).src)
  glGetIntegerV(%GL_BLEND_DST, BlendFuncStack(BlendFuncStackDepth).dst)

  ' -- Set the new one
  glBlendFunc(srcFactor, dstFactor)
END SUB

' -- Ends line stipple region
SUB tbgl_PopBlendFunc ALIAS "tbgl_PopBlendFunc" ( ) EXPORT

  IF BlendFuncStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopBlendFunc caused stack underflow, check whether number of pairs of TBGL_PushBlendFunc / TBGL_PopBlendFunc match"
    END IF
    EXIT SUB
  END IF

  glBlendFunc(BlendFuncStack(BlendFuncStackDepth).src, BlendFuncStack(BlendFuncStackDepth).dst)

  DECR BlendFuncStackDepth

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

SUB Exec_TBGL_UseBlend()
  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  IF thinBasic_ErrorFree THEN tbgl_UseBlend( ISTRUE flag )

END SUB

SUB Exec_TBGL_BlendFunc( )
  LOCAL x, y AS EXT

  thinBasic_Parse2Numbers( x, y )
  tbgl_BlendFunc( x, y )

END SUB

FUNCTION Exec_TBGL_GetUseBlend( ) AS EXT
  ' () check
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = tbgl_GetUseBlend( )
END FUNCTION

SUB Exec_TBGL_PushBlendFunc( )
  LOCAL src, dst AS EXT

  thinBasic_Parse2Numbers src, dst
  tbgl_PushBlendFunc( src, dst )

END SUB

SUB Exec_TBGL_PopBlendFunc( )
  tbgl_PopBlendFunc( )
END SUB

#ENDIF

'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : Services which differ between the thinBasic module build
'                 and the general purpose DLL build (see %TBGL_THINBASIC_INTERPRETER_INTERFACE)
' =============================================================================

' -----------------------------------------------------------------------------

#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE = 0
' -- Callbacks passed to the general purpose DLL are parameterless stdcall SUBs
DECLARE SUB TBGL_CallbackPrototype()
#ENDIF

' -- Calls user callback: script function for thinBasic module, code pointer for general purpose DLL
SUB internal_InvokeCallback( BYVAL funcPtr AS LONG, OPTIONAL BYVAL ptrResult AS EXT PTR )

  IF funcPtr = 0 THEN EXIT SUB

  #IF %TBGL_THINBASIC_INTERPRETER_INTERFACE
    thinBasic_FunctionSimpleCall_ByPtr( funcPtr, ptrResult )
  #ELSE
    CALL DWORD funcPtr USING TBGL_CallbackPrototype()
  #ENDIF

END SUB

' -- Base path for relative file names: script path for thinBasic module, EXE path for general purpose DLL
FUNCTION internal_GetBasePath( ) AS STRING

  #IF %TBGL_THINBASIC_INTERPRETER_INTERFACE
    FUNCTION = thinBasic_GetRunTimeInfo( "SCRIPT_PATH" )
  #ELSE
    LOCAL exePath AS ASCIIZ * %MAX_PATH
    GetModuleFileName( %NULL, exePath, SIZEOF( exePath ))
    FUNCTION = LEFT$( exePath, INSTR( -1, exePath, "\" ))
  #ENDIF

END FUNCTION

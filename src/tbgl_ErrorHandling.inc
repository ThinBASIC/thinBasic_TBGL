'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include takes care of error handling
' =============================================================================

' -----------------------------------------------------------------------------

%TBGL_FUNCTION_SUCCESS  = 0
%TBGL_FUNCTION_ERROR    = 1

%TBGL_ERROR_NONE   = 0
%TBGL_ERROR_MSGBOX = 1
%TBGL_ERROR_FILE   = 2

$ERR_BADTEXTUREINDEX  = "Texture index out of bounds [1, 1024]"    ' %LIMIT_MAX_TEXTUREINDEX
$ERR_TEXTURENOTFOUND  = "Problem:" + $TAB + $TAB + "Texture not found" + $CRLF + $CRLF + "File:" + $TAB
$ERR_TEXTUREBADFORMAT = "Problem:" + $TAB + $TAB + "Unsupported format / version" + $CRLF + $CRLF + "File:" + $TAB
$ERR_DISPLISTINDEX    = "Display list index out of bounds [1, 1024] or not allocated by TBGL_NewListSpace"    ' %LIMIT_MAX_DISPLAYLISTS
$ERR_MODELBUFFER      = "Problem" + $TAB + "Usage of tbgl_m15* statement without dimensioning model buffers using TBGL_m15InitModelBuffer first"

' -----------------------------------------------------------------------------

GLOBAL error_display AS LONG

' -----------------------------------------------------------------------------

FUNCTION Exec_TBGL_ErrorMessages( ) AS LONG

  LOCAL x AS EXT

  x = thinBasic_Parse1Number

  IF x = < %TBGL_ERROR_FILE AND x >= %TBGL_ERROR_NONE THEN
    error_display = x
  ELSE
    FUNCTION = %TBGL_FUNCTION_ERROR
  END IF
END FUNCTION

' -- Internal function to handle error situation according to setup by TBGL_ErrorMessages
FUNCTION TBGLError ( mtext AS STRING ) AS LONG

  STATIC ErrorCount AS LONG

  IF error_display = %TBGL_ERROR_MSGBOX THEN
    thinbasic_RunTimeError ( %ERR__MODULE_SPECIFIC, mText )

  ELSEIF error_display = %TBGL_ERROR_FILE THEN

    LOCAL x AS LONG
    x = FREEFILE

    INCR ErrorCount

    IF ErrorCount = 1 THEN KILL thinBasic_GetRunTimeInfo( "SCRIPT_PATH" ) + "TBGLModuleErrors.log"

    OPEN thinBasic_GetRunTimeInfo( "SCRIPT_PATH" ) + "TBGLModuleErrors.log" FOR APPEND AS #x
      IF ErrorCount = 1 THEN PRINT #x, "This file contains list of problems which occured in TBGL module during the run of the script" : PRINT #x, "Log started on " + DATE$ + " at " + TIME$
      PRINT #x, mText
    CLOSE #x

  END IF

END FUNCTION

sub ReportError(errorContext as string, description as string)
  dim errorMessage as string
  errorMessage = "Context:" + $TAB + errorContext + $CRLF + "Problem:" + $TAB + description

  TBGLError(errorMessage)
end sub

' -- Returns last error in OpenGL
FUNCTION Exec_TBGL_GetLastGLError ( ) AS STRING

  LOCAL errorCode AS LONG
  errorCode = glGetError ( )

  IF errorCode <> %GL_NO_ERROR THEN
    LOCAL ErrorInfoPTR AS ASCIIZ PTR
    ErrorInfoPTR = gluErrorString( errorCode )
    FUNCTION = @ErrorInfoPTR
  END IF

END FUNCTION

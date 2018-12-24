'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : Functions to manage display lists
' =============================================================================
'
' -----------------------------------------------------------------------------

' >RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES
GLOBAL g_DisplayListSpaceHandles AS STRING

SUB resource_DisplayListSpace_AddHandle( dlHandle AS TBGL_HANDLE )
  g_DisplayListSpaceHandles += MKL$(dlHandle)
END SUB

SUB resource_DisplayListSpace_RemoveHandle( dlHandle AS TBGL_HANDLE )
  REGISTER i AS LONG
  LOCAL handlePtr AS DWORD
  LOCAL handleCount AS LONG
  LOCAL matchesFound AS LONG
  LOCAL memBuffer AS STRING
  LOCAL handleLen AS LONG

  handleLen   = LEN(g_DisplayListSpaceHandles)
  handleCount = handleLen/TBGL_HANDLE_SIZE

  IF handleCount THEN
    handlePtr = STRPTR(g_DisplayListSpaceHandles)

    DIM dlHandles(1 TO handleLen) AS TBGL_HANDLE AT handlePtr

    FOR i = handleCount TO 1 STEP -1

      IF dlHandles(i) = dlHandle THEN
        matchesFound += 1
        IF matchesFound = 1 THEN glDeleteLists(dlHandles(i), 1)

        ' -- Fast shift, if it is last we don't need to do anything
        IF i < handleCount THEN
          memBuffer = PEEK$(handlePtr + (TBGL_HANDLE_SIZE * i), _
                            TBGL_HANDLE_SIZE * (handleCount-i))

          POKE$ handlePtr + (TBGL_HANDLE_SIZE * (i-1)), _
                memBuffer
        END IF

      END IF

    NEXT

    IF matchesFound THEN g_DisplayListSpaceHandles = LEFT$(g_DisplayListSpaceHandles, handleLen - TBGL_HANDLE_SIZE*matchesFound)

  END IF

END SUB

SUB resource_DisplayListSpace_RemoveAll( )
  REGISTER i AS LONG
  LOCAL handlePtr AS DWORD
  LOCAL handleCount AS LONG

  LOCAL memBuffer AS STRING
  LOCAL handleLen AS LONG

  handleLen   = LEN(g_DisplayListSpaceHandles)
  handleCount = handleLen/TBGL_HANDLE_SIZE

  IF handleCount THEN
    handlePtr = STRPTR(g_DisplayListSpaceHandles)

    DIM dlHandles(1 TO handleLen) AS TBGL_HANDLE AT handlePtr

    FOR i = handleCount TO 1 STEP -1

      IF resource_DisplayLists_IsGenerated(dlHandles(i)) THEN glDeleteLists(dlHandles(i), 1)

    NEXT

    g_DisplayListSpaceHandles = ""

  END IF

END SUB

'MACRO IsDLIndexValid( someIndex ) = (someIndex < 1 OR someIndex > %LIMIT_MAX_DISPLAYLISTS)

SUB resource_DisplayLists_Alloc()
  glGenLists( %LIMIT_MAX_DISPLAYLISTS )
  g_DisplayListSpaceHandles = ""
END SUB

SUB resource_DisplayLists_Release()
  REGISTER i AS LONG

  ' -- Release display lists
  FOR i = %LIMIT_MAX_DISPLAYLISTS TO 1 STEP - 1
    IF resource_DisplayLists_IsGenerated( i ) THEN glDeleteLists i, 1
  NEXT

  ' -- Release display lists from user extensions
  resource_DisplayListSpace_RemoveAll()

END SUB

FUNCTION resource_DisplayLists_IsGenerated( BYVAL index AS LONG ) AS LONG
  REGISTER i AS LONG

  IF index >= 1 AND index <= %LIMIT_MAX_DISPLAYLISTS THEN
    FUNCTION = 1
    EXIT FUNCTION
  END IF

  LOCAL handleLen AS LONG, handleCount AS LONG
  LOCAL handlePtr AS DWORD

  handleLen   = LEN(g_DisplayListSpaceHandles)
  handleCount = handleLen/TBGL_HANDLE_SIZE

  IF handleCount THEN
    handlePtr = STRPTR(g_DisplayListSpaceHandles)

    DIM dlHandles(1 TO handleLen) AS TBGL_HANDLE AT handlePtr

    FOR i = 1 TO handleLen
      IF dlHandles(i) = index THEN
        FUNCTION = 1
        EXIT FUNCTION
      END IF
    NEXT
  END IF

  FUNCTION = 0

END FUNCTION
' <RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES

SUB Exec_TBGL_NewListSpace()
  REGISTER i AS LONG
  LOCAL lArrayCellPtr AS LONG
  LOCAL lArrayCellAbsPos AS LONG
  LOCAL howManyLists AS EXT
  LOCAL VariablePtrOdd, MainType, SubType, ElementsAreFixed, TotElements, ElementSize AS LONG


  IF thinBasic_CheckOpenParens_Mandatory() THEN
    thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

    howManyLists = TotElements

    IF thinBasic_CheckComma_Optional AND thinBasic_ErrorFree THEN
      thinBasic_ParseNumber( howManyLists )
    END IF

    IF thinBasic_CheckCloseParens_Mandatory() THEN

      LOCAL StartID, OldID AS TBGL_HANDLE
      LOCAL AbsolutePosition AS LONG
      StartID = glGenLists(howManyLists)

      FOR i = 1 TO howManyLists
        AbsolutePosition = lArrayCellAbsPos+(i-1)
        IF AbsolutePosition > TotElements THEN EXIT FOR
        ' -- Do we overwrite something which was? Release it please!
        OldID = thinBasic_GetVariableNumberDirect(lArrayCellPtr, AbsolutePosition)
        IF OldID THEN resource_DisplayListSpace_RemoveHandle(OldID)

        '---Now assign values to referenced variables
        thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, AbsolutePosition, StartID )
        resource_DisplayListSpace_AddHandle(StartID)
        StartID       += 1
      NEXT

    END IF
  END IF

END SUB

SUB Exec_TBGL_DeleteListSpace()

  REGISTER i AS LONG
  LOCAL lArrayCellPtr AS LONG
  LOCAL lArrayCellAbsPos AS LONG
  LOCAL howManyLists AS EXT

  LOCAL VariablePtrOdd, MainType, SubType, ElementsAreFixed, TotElements, ElementSize AS LONG
  IF thinBasic_CheckOpenParens_Mandatory() THEN


    thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)
    howManyLists = TotElements

    IF thinBasic_CheckComma_Optional AND thinBasic_ErrorFree THEN
      thinBasic_ParseNumber( howManyLists )
    END IF

    IF thinBasic_CheckCloseParens_Mandatory() THEN

      LOCAL StartID AS TBGL_HANDLE
      LOCAL AbsolutePosition AS LONG

      FOR i = 1 TO howManyLists
        AbsolutePosition = lArrayCellAbsPos+(i-1)
        IF AbsolutePosition > TotElements THEN EXIT FOR

        StartID = thinBasic_GetVariableNumberDirect(lArrayCellPtr, AbsolutePosition)
        resource_DisplayListSpace_RemoveHandle(StartID)
        thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, AbsolutePosition, 0 )
      NEXT

    END IF
  END IF


END SUB

FUNCTION Exec_TBGL_NewList( ) AS LONG
  LOCAL list AS EXT

  list = thinBasic_Parse1Number

  IF resource_DisplayLists_IsGenerated( list ) = 0 THEN
    TBGLError "Statement:" + $TAB + "tbgl_NewList" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_DISPLISTINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( list )
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  glNewList list, %GL_COMPILE_AND_EXECUTE

END FUNCTION

SUB Exec_TBGL_EndList( )

  glEndList()

END SUB

FUNCTION Exec_TBGL_CallList( ) AS LONG
  LOCAL list AS EXT

  list = thinBasic_Parse1Number

  IF resource_DisplayLists_IsGenerated( list ) = 0 THEN
    TBGLError "Statement:" + $TAB + "tbgl_CallList" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_DISPLISTINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( list )
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  glCallList list

END FUNCTION

SUB Exec_TBGL_DeleteList( )

  LOCAL list AS LONG

  list = thinBasic_Parse1Number

  ' -- Looks odd, but it is safe to do it this way
  ' -- If it is not a list, no need to clear it, as it is allocated, but empty
  IF glIsList( list ) THEN
    glDeleteLists list, 1
    glGenLists(1)
  END IF

END SUB

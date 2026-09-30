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

' -- Allocates count new display lists and stores their IDs to listIDs array (non-zero IDs present are released first)
SUB tbgl_NewListSpace ALIAS "tbgl_NewListSpace" ( BYVAL listIDs AS LONG PTR, BYVAL count AS LONG ) EXPORT
  REGISTER i AS LONG
  LOCAL StartID, OldID AS TBGL_HANDLE

  StartID = glGenLists(count)

  FOR i = 0 TO count - 1
    ' -- Do we overwrite something which was? Release it please!
    OldID = @listIDs[i]
    IF OldID THEN resource_DisplayListSpace_RemoveHandle(OldID)

    @listIDs[i] = StartID
    resource_DisplayListSpace_AddHandle(StartID)
    StartID       += 1
  NEXT

END SUB

' -- Releases count display lists whose IDs are in listIDs array, and zeroes the array items
SUB tbgl_DeleteListSpace ALIAS "tbgl_DeleteListSpace" ( BYVAL listIDs AS LONG PTR, BYVAL count AS LONG ) EXPORT
  REGISTER i AS LONG
  LOCAL StartID AS TBGL_HANDLE

  FOR i = 0 TO count - 1
    StartID = @listIDs[i]
    resource_DisplayListSpace_RemoveHandle(StartID)
    @listIDs[i] = 0
  NEXT

END SUB

FUNCTION tbgl_NewList ALIAS "tbgl_NewList" ( BYVAL list AS LONG ) EXPORT AS LONG

  IF resource_DisplayLists_IsGenerated( list ) = 0 THEN
    TBGLError "Statement:" + $TAB + "tbgl_NewList" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_DISPLISTINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( list )
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  glNewList list, %GL_COMPILE_AND_EXECUTE

END FUNCTION

SUB tbgl_EndList ALIAS "tbgl_EndList" ( ) EXPORT

  glEndList()

END SUB

FUNCTION tbgl_CallList ALIAS "tbgl_CallList" ( BYVAL list AS LONG ) EXPORT AS LONG

  IF resource_DisplayLists_IsGenerated( list ) = 0 THEN
    TBGLError "Statement:" + $TAB + "tbgl_CallList" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_DISPLISTINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( list )
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  glCallList list

END FUNCTION

SUB tbgl_DeleteList ALIAS "tbgl_DeleteList" ( BYVAL list AS LONG ) EXPORT

  ' -- Looks odd, but it is safe to do it this way
  ' -- If it is not a list, no need to clear it, as it is allocated, but empty
  IF glIsList( list ) THEN
    glDeleteLists list, 1
    glGenLists(1)
  END IF

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

' -- Parses "( array [, howMany] )", returns number of usable items, loads them into ids()
FUNCTION internal_ParseListSpaceArray( ids() AS LONG, lArrayCellPtr AS LONG, lArrayCellAbsPos AS LONG ) AS LONG
  REGISTER i AS LONG
  LOCAL howManyLists AS EXT
  LOCAL VariablePtrOdd, MainType, SubType, ElementsAreFixed, TotElements, ElementSize AS LONG
  LOCAL count AS LONG

  IF thinBasic_CheckOpenParens_Mandatory() THEN
    thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

    howManyLists = TotElements

    IF thinBasic_CheckComma_Optional AND thinBasic_ErrorFree THEN
      thinBasic_ParseNumber( howManyLists )
    END IF

    IF thinBasic_CheckCloseParens_Mandatory() THEN
      ' -- Only items which fit in the array are processed
      count = MIN&(howManyLists, TotElements - lArrayCellAbsPos + 1)
      IF count > 0 THEN
        REDIM ids(0 TO count - 1)
        FOR i = 0 TO count - 1
          ids(i) = thinBasic_GetVariableNumberDirect(lArrayCellPtr, lArrayCellAbsPos + i)
        NEXT
        FUNCTION = count
      END IF
    END IF
  END IF

END FUNCTION

SUB internal_StoreListSpaceArray( ids() AS LONG, BYVAL count AS LONG, BYVAL lArrayCellPtr AS LONG, BYVAL lArrayCellAbsPos AS LONG )
  REGISTER i AS LONG

  FOR i = 0 TO count - 1
    thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, lArrayCellAbsPos + i, ids(i) )
  NEXT

END SUB

SUB Exec_TBGL_NewListSpace()
  LOCAL lArrayCellPtr, lArrayCellAbsPos, count AS LONG
  DIM ids() AS LONG

  count = internal_ParseListSpaceArray( ids(), lArrayCellPtr, lArrayCellAbsPos )
  IF count THEN
    tbgl_NewListSpace( VARPTR(ids(0)), count )
    internal_StoreListSpaceArray( ids(), count, lArrayCellPtr, lArrayCellAbsPos )
  END IF

END SUB

SUB Exec_TBGL_DeleteListSpace()
  LOCAL lArrayCellPtr, lArrayCellAbsPos, count AS LONG
  DIM ids() AS LONG

  count = internal_ParseListSpaceArray( ids(), lArrayCellPtr, lArrayCellAbsPos )
  IF count THEN
    tbgl_DeleteListSpace( VARPTR(ids(0)), count )
    internal_StoreListSpaceArray( ids(), count, lArrayCellPtr, lArrayCellAbsPos )
  END IF

END SUB

FUNCTION Exec_TBGL_NewList( ) AS LONG
  FUNCTION = tbgl_NewList( thinBasic_Parse1Number )
END FUNCTION

SUB Exec_TBGL_EndList( )
  tbgl_EndList( )
END SUB

FUNCTION Exec_TBGL_CallList( ) AS LONG
  FUNCTION = tbgl_CallList( thinBasic_Parse1Number )
END FUNCTION

SUB Exec_TBGL_DeleteList( )
  tbgl_DeleteList( thinBasic_Parse1Number )
END SUB

#ENDIF

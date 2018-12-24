#INCLUDE ONCE "oleidl.inc"

'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles drag and drop for TBGL window
' =============================================================================

' ########################################################################################
' *** Custom implementation of the IDropTarget interface.
' The IDropTarget interface is one of the interfaces you implement to provide
' drag-and-drop operations in your application. It contains methods used in any
' application that can be a target for data during a drag-and-drop operation. A
' drop-target application is responsible for:

'  * Determining the effect of the drop on the target application.
'  * Incorporating any valid dropped data when the drop occurs.
'  * Communicating target feedback to the source so the source application can provide
'    appropriate visual feedback such as setting the cursor.
'  * Implementing drag scrolling.
'  * Registering and revoking its application windows as drop targets.

' The IDropTarget interface contains methods that handle all these responsibilities except
' registering and revoking the application window as a drop target, for which you must
' call the RegisterDragDrop and the RevokeDragDrop functions.
' You do not call the methods of IDropTarget directly. The DoDragDrop function calls the
' IDropTarget methods during the drag-and-drop operation.
' ########################################################################################

$CLSID_CDropTarget = GUID$("{F9E4BF70-EFA8-411E-A142-F4B02D89D619}")
$IID_IDropTarget = GUID$("{00000122-0000-0000-C000-000000000046}")

' // Need to declare it as common to avoid removal of methods
CLASS CDropTarget $CLSID_CDropTarget AS COMMON

   INSTANCE hr AS LONG
   INSTANCE hwnd AS DWORD
   INSTANCE bAllowDrop AS LONG
   INSTANCE fmtc AS FORMATETC
   INSTANCE stgmed AS STGMEDIUM
   INSTANCE pData AS ASCIIZ PTR
   INSTANCE strData AS STRING

   INTERFACE IDropTargetImpl $IID_IDropTarget

      INHERIT IUNKNOWN

      ' ----------------------------------------------------------------------------------
      ' Determines whether a drop can be accepted and its effect if it is accepted
      ' ----------------------------------------------------------------------------------
      METHOD DragEnter ( _                      ' VTable offset = 12
        BYVAL pDataObject AS IDataObject _      ' // Pointer to the interface of the source data object
      , BYVAL grfKeyState AS DWORD _            ' // Current state of keyboard modifier keys
      , BYVAL pt AS POINTL _                    ' // Current cursor coordinates (Must be BYVAL)
      , BYREF pdwEffect AS DWORD _              ' // Pointer to the effect of the drag-and-drop operation
      ) AS LONG                                 ' HRESULT

         pdwEffect = %DROPEFFECT_NONE
         IF ISFALSE ISOBJECT(pDataObject) THEN
            METHOD = %E_FAIL
            EXIT METHOD
         END IF

         ' Check if the data object contains the data we want
         bAllowDrop = ME.QueryDataObject(pDataObject)
         IF bAllowDrop THEN
            ' Get the dropeffect based on keyboard state
            pdwEffect = ME.DropEffect(grfKeyState, pt, pdwEffect)
            ' Bring the window to the foregroung
            IF hwnd THEN SetForegroundWindow hwnd
         END IF

         ' Return success
         METHOD = %S_OK

      END METHOD
      ' ----------------------------------------------------------------------------------

      ' ----------------------------------------------------------------------------------
      ' Provides target feedback to the user through the DoDragDrop function
      ' ----------------------------------------------------------------------------------
      METHOD DragOver ( _                       ' VTable offset = 16
        BYVAL grfKeyState AS DWORD _            ' // Current state of keyboard modifier keys
      , BYVAL pt AS POINTL _                    ' // Current cursor coordinates (Must be BYVAL)
      , BYREF pdwEffect AS DWORD _              ' // Pointer to the effect of the drag-and-drop operation
      ) AS LONG                                 ' HRESULT

         IF bAllowDrop THEN
            ' Get the dropeffect based on keyboard state
            pdwEffect = ME.DropEffect(grfKeyState, pt, pdwEffect)
         ELSE
            pdwEffect = %DROPEFFECT_NONE
         END IF
         METHOD = %S_OK

      END METHOD
      ' ----------------------------------------------------------------------------------

      ' ----------------------------------------------------------------------------------
      ' Causes the drop target to suspend its feedback actions
      ' ----------------------------------------------------------------------------------
      METHOD DragLeave ( _                      ' VTable offset = 20
      ) AS LONG                                 ' HRESULT

         METHOD = %S_OK

      END METHOD
      ' ----------------------------------------------------------------------------------

      ' ----------------------------------------------------------------------------------
      ' Drops the data into the target window
      ' ----------------------------------------------------------------------------------
      METHOD Drop ( _                           ' VTable offset = 24
        BYVAL pDataObject AS IDataObject _      ' // Pointer to the interface of the source data object
      , BYVAL grfKeyState AS DWORD _            ' // Current state of keyboard modifier keys
      , BYVAL pt AS POINTL _                    ' // Current cursor coordinates (Must be BYVAL)
      , BYREF pdwEffect AS DWORD _              ' // Pointer to the effect of the drag-and-drop operation
      ) AS LONG                                 ' HRESULT

         pdwEffect = %DROPEFFECT_NONE
         IF ISFALSE ISOBJECT(pDataObject) THEN
            METHOD = %E_FAIL
            EXIT METHOD
         END IF

         ' Get the dropeffect based on keyboard state
         pdwEffect = ME.DropEffect(grfKeyState, pt, pdwEffect)
         '  Ask IDataObject for some CF_TEXT data, stored as a HGLOBAL in the clipboard
         IF bAllowDrop THEN
            fmtc.cfFormat = %CF_TEXT
            fmtc.ptd = %NULL
            fmtc.dwAspect = %DVASPECT_CONTENT
            fmtc.lindex = -1
            fmtc.tymed = %TYMED_HGLOBAL
            hr = pDataObject.GetData(fmtc, stgmed)
            IF hr = %S_OK THEN
               IF stgmed.hGlobal THEN
                  ' Lock the hGlobal handle just in case isn't fixed memory
                  pData = GlobalLock(stgmed.hGlobal)
                  ' Store the data in a string variable
                  strData = @pData
                  ' Show the data in the window
                  MSGBOX strData
                  ' Unlock the global data
                  GlobalUnlock stgmed.hGlobal
               END IF
               ' Free the memory used by the STGMEDIUM structure
               ReleaseStgMedium stgmed
            END IF
         END IF

         ' Return success
         METHOD = %S_OK

      END METHOD
      ' ----------------------------------------------------------------------------------

      ' ==================================================================================
      ' *** We can add custom methods and properties here ***
      ' ==================================================================================

      ' ----------------------------------------------------------------------------------
      ' Window handle of the control that has been registered for drag and drop operations
      ' ----------------------------------------------------------------------------------
      METHOD SetHwnd (BYVAL hndl AS DWORD) AS LONG
         hwnd = hndl
         METHOD = %S_OK
      END METHOD
      ' ----------------------------------------------------------------------------------

      ' ----------------------------------------------------------------------------------
      ' Returns an string containing the text of the dropped link or text
      ' ----------------------------------------------------------------------------------
      METHOD GetData (BYREF pstrData AS STRING) AS LONG
         pstrData = strData
         METHOD = %S_OK
      END METHOD
      ' ----------------------------------------------------------------------------------

      ' ----------------------------------------------------------------------------------
      ' Retrieves the allowed drop effect
      ' ----------------------------------------------------------------------------------
      METHOD DropEffect (BYVAL grfKeyState AS DWORD, BYVAL pt AS POINTL, BYVAL dwAllowed AS DWORD) AS DWORD

         LOCAL dwEffect  AS DWORD

         ' 1. Check "pt" -> Is a  drop allowed at the specified coordinates?
         ' 2. Work out that the drop-effect should be based on grfKeyState
         IF (grfKeyState AND %MK_CONTROL) THEN
            dwEffect = dwAllowed AND %DROPEFFECT_COPY
         ELSEIF (grfKeyState AND %MK_SHIFT) THEN
            dwEffect = dwAllowed AND %DROPEFFECT_MOVE
         END IF

         ' 3. No key-modifiers were specified (or drop effect not allowed), so
         '    base the effect on those allowed by the dropsource
         IF dwEffect = 0 THEN
            IF (dwAllowed AND %DROPEFFECT_COPY) THEN dwEffect = %DROPEFFECT_COPY
            IF (dwAllowed AND %DROPEFFECT_MOVE) THEN dwEffect = %DROPEFFECT_MOVE
            IF (dwAllowed AND %DROPEFFECT_LINK) THEN dwEffect = %DROPEFFECT_LINK
         END IF
         METHOD = dwEffect

      END METHOD
      ' ----------------------------------------------------------------------------------

      ' ----------------------------------------------------------------------------------
      ' Checks if the data object contains the data we want.
      ' In this example, asks for some CF_TEXT data, stored as a HGLOBAL in the clipboard
      ' ----------------------------------------------------------------------------------
      METHOD QueryDataObject (BYVAL pDataObject AS IDataObject) AS LONG

         LOCAL hr AS LONG
         LOCAL fmtc AS FORMATETC
         LOCAL stgmed AS STGMEDIUM

         fmtc.cfFormat = %CF_TEXT
         fmtc.ptd = %NULL
         fmtc.dwAspect = %DVASPECT_CONTENT
         fmtc.lindex = -1
         fmtc.tymed = %TYMED_HGLOBAL
         hr = pDataObject.GetData(fmtc, stgmed)
         IF hr = %S_OK THEN
            IF stgmed.hGlobal THEN METHOD = %TRUE
            ReleaseStgMedium stgmed
         END IF

      END METHOD
      ' ----------------------------------------------------------------------------------

   END INTERFACE

END CLASS
' ########################################################################################

DECLARE FUNCTION Sub1(p1 AS ANY) AS DWORD
DECLARE FUNCTION Sub2(p1 AS ANY, p2 AS ANY) AS DWORD
DECLARE FUNCTION Sub3(p1 AS ANY, p2 AS ANY, p3 AS ANY) AS DWORD
DECLARE FUNCTION Sub5(p1 AS ANY, p2 AS ANY, p3 AS ANY, p4 AS ANY, p5 AS ANY) AS DWORD


'------------------------------------------------------------------------------
' Returns details from a link (LNK) file, based on iType.
' 1 = link file name, 2 = link path name, 3 = link parameters
'
' Note: parts of the following function were found on PB forum,
'       so credit goes to the original authors.
'------------------------------------------------------------------------------
FUNCTION GetLinkInfo(BYVAL sLinkPath AS STRING, BYVAL nType AS LONG) AS STRING

  LOCAL CLSCTX_INPROC_SERVER AS DWORD, CLSID_ShellLink AS GUIDAPI
  LOCAL FileData AS WIN32_FIND_DATA, Flags AS DWORD
  LOCAL IID_IShellLink AS GUIDAPI, IID_Persist AS STRING * 16
  LOCAL lResult AS DWORD, outvalue AS ASCIIZ * %MAX_PATH, nRet AS LONG
  LOCAL pp AS DWORD PTR, ppf AS DWORD PTR, psl AS DWORD PTR
  LOCAL TmpAsciiz AS ASCIIZ * %MAX_PATH, TmpWide AS ASCIIZ * (%MAX_PATH*2)

  POKE$ VARPTR(CLSID_ShellLink), MKL$(&H00021401) + CHR$(0, 0, 0, 0, &HC0, 0, 0, 0, 0, 0, 0, &H46)
  POKE$ VARPTR(IID_IShellLink), MKL$(&H000214EE) + CHR$(0, 0, 0, 0, &HC0, 0, 0, 0, 0, 0, 0, &H46)

  IID_Persist     = MKL$(&H0000010B) + CHR$(0, 0, 0, 0, &HC0, 0, 0, 0, 0, 0, 0, &H46)
  CLSCTX_INPROC_SERVER = 1

  nRet = CoCreateInstance(CLSID_ShellLink, NOTHING, CLSCTX_INPROC_SERVER, IID_IShellLink, psl)
  IF (nRet = %S_OK) THEN
     pp = @psl
     CALL DWORD @pp USING Sub3(BYVAL psl, IID_Persist, ppf) TO lResult
     TmpAsciiz = sLinkPath
     MultiByteToWideChar %CP_ACP, 0, TmpAsciiz, -1, BYVAL VARPTR(TmpWide), %MAX_PATH
     pp = @ppf + 20
     CALL DWORD @pp USING Sub3(BYVAL ppf, TmpWide, BYVAL %True)
     SELECT CASE nType
            CASE 2
                 pp = @psl + 32
                 CALL DWORD @pp USING Sub3(BYVAL psl, outvalue, BYVAL %MAX_PATH)
            CASE 3
                 pp = @psl + 40
                 CALL DWORD @pp USING Sub3(BYVAL psl, outvalue, BYVAL %MAX_PATH)
            CASE ELSE
                 pp = @psl + 12
                 CALL DWORD @pp USING Sub5(BYVAL psl, outvalue, BYVAL %MAX_PATH, FileData, Flags)
     END SELECT
     ' Release the persistant file
     pp = @ppf + 8
     CALL DWORD @pp USING Sub1(BYVAL ppf)
     ' Unbind the shell link object from the persistent file
     pp = @psl + 8
     CALL DWORD @pp USING Sub1(BYVAL psl)
     FUNCTION = outValue
  END IF

END FUNCTION

'------------------------------------------------------------------------------
' Return a list of drag/drop filenames, separated by | (See WM_DROPFILES message)
' Note: this function also finishes the specified drag/drop operation.
'------------------------------------------------------------------------------
FUNCTION GetDropFiles(BYVAL hDropParam AS DWORD) AS STRING
  LOCAL sDropFiles AS STRING, sFile AS STRING, i AS LONG

  FOR i = 0 TO DragQueryFile(hDropParam, &HFFFFFFFF&, "", 0)-1

      sFile = SPACE$(DragQueryFile(hDropParam, i, "", 0)+1)
      DragQueryFile hDropParam, i, BYVAL STRPTR(sFile), LEN(sFile)
      sFile = LEFT$(sFile, LEN(sFile)-1)
      IF UCASE$(RIGHT$(sFile, 4)) = ".LNK" THEN sFile = GetLinkInfo(sFile, 1)
      sDropFiles = sDropFiles + sFile + "|"
  NEXT i
  FUNCTION = RTRIM$(sDropFiles, "|")

END FUNCTION

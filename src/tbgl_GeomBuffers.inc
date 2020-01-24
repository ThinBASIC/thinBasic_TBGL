'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles geom buffers,
'                 optimized geometry primitives
' =============================================================================
'
' -----------------------------------------------------------------------------

' >RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES
TYPE TBGL_tDataDescriptor
  pData         AS DWORD
  nComponents   AS LONG
  componentType AS LONG
END TYPE

TYPE TBGL_tGBuffer
  gType             AS LONG
  gDims             AS LONG
  gKind             AS LONG

  items             AS LONG

  vertexInfo        AS TBGL_tDataDescriptor
  colorInfo         AS TBGL_tDataDescriptor
  normalInfo        AS TBGL_tDataDescriptor
  coordInfo(1 TO 4) AS TBGL_tDataDescriptor
END TYPE

TYPE TBGL_tVector3F
  x AS SINGLE
  y AS SINGLE
  z AS SINGLE
END TYPE

TYPE TBGL_tVector2F
  x AS SINGLE
  y AS SINGLE
END TYPE

TYPE TBGL_tRGB
  R AS BYTE
  G AS BYTE
  B AS BYTE
END TYPE

TYPE TBGL_tRGBA
  R AS BYTE
  G AS BYTE
  B AS BYTE
  A AS BYTE
END TYPE

TYPE TBGL_tRGB_f32
  R AS SINGLE
  G AS SINGLE
  B AS SINGLE
END TYPE

TYPE TBGL_tRGBA_f32
  R AS SINGLE
  G AS SINGLE
  B AS SINGLE
  A AS SINGLE
END TYPE

%TBGL_Dynamic = 1
%TBGL_Static  = 2

%TBGL_Points        = %GL_POINTS

%TBGL_Lines         = %GL_LINES
%TBGL_LineStrip     = %GL_LINE_STRIP
%TBGL_LineLoop      = %GL_LINE_LOOP

%TBGL_Triangles     = %GL_TRIANGLES
%TBGL_TriangleStrip = %GL_TRIANGLE_STRIP
%TBGL_TriangleFan   = %GL_TRIANGLE_FAN

%TBGL_Quads         = %GL_QUADS
%TBGL_QuadStrip     = %GL_QUAD_STRIP

%TBGL_Polygon       = %GL_POLYGON

GLOBAL g_GBuffersHandles AS STRING

SUB resource_GBuffers_Alloc()
  g_GBuffersHandles = ""
END SUB

SUB resource_GBuffer_ReleaseSingle(BYVAL gbHandle AS TBGL_HANDLE,OPTIONAL BYVAL justData AS LONG)

  DIM gBuffer(0) AS TBGL_tGBuffer AT gbHandle

  ' -- We will not release the arrays on the thinBASIC side, just custom allocations
  IF gBuffer(0).gKind <> %TBGL_DYNAMIC THEN
    Mem_Free(gBuffer(0).vertexInfo.pData)
    gBuffer(0).vertexInfo.pData = 0

    Mem_Free(gBuffer(0).colorInfo.pData)
    gBuffer(0).colorInfo.pData = 0

    Mem_Free(gBuffer(0).normalInfo.pData)
    gBuffer(0).normalInfo.pData = 0

    Mem_Free(gBuffer(0).coordInfo(1).pData)
    gBuffer(0).coordInfo(1).pData = 0

    Mem_Free(gBuffer(0).coordInfo(2).pData)
    gBuffer(0).coordInfo(2).pData = 0

    Mem_Free(gBuffer(0).coordInfo(3).pData)
    gBuffer(0).coordInfo(3).pData = 0

    Mem_Free(gBuffer(0).coordInfo(4).pData)
    gBuffer(0).coordInfo(4).pData = 0
  END IF

  IF justData = 0 THEN
    Mem_Free(gbHandle)
  END IF
END SUB

SUB resource_GBuffers_Release(OPTIONAL BYVAL specific AS TBGL_HANDLE) ' -- IF TBGL_HANDLE = 0 -> all, otherwise just the one specified

  REGISTER i AS LONG

  LOCAL numGBuffers AS LONG
  LOCAL hRes AS LONG

  numGBuffers = LEN(g_GBuffersHandles) / TBGL_HANDLE_SIZE
  DIM GBufferHandles(1 TO numGBuffers) AS TBGL_HANDLE AT STRPTR(g_GBuffersHandles)

  IF specific = 0 THEN
    FOR i = 1 TO numGBuffers
      resource_GBuffer_ReleaseSingle(GBufferHandles(i))
    NEXT

    g_GBuffersHandles = ""
  ELSE

    FOR i = 1 TO numGBuffers
      IF GBufferHandles(i) = specific THEN
        resource_GBuffer_ReleaseSingle(GBufferHandles(i))
        ARRAY DELETE GBufferHandles(i), 0
        g_GBuffersHandles = LEFT$(g_GBuffersHandles, LEN(g_GBuffersHandles)-TBGL_HANDLE_SIZE)
        EXIT FOR
      END IF
    NEXT

  END IF

END SUB
' <RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES

SUB declare_GBuffers()

  thinBasic_AddUDT( _
  "TYPE TBGL_tVector3F"+$CRLF+ _
  "  x AS SINGLE"+$CRLF+ _
  "  y AS SINGLE"+$CRLF+ _
  "  z AS SINGLE"+$CRLF+ _
  "END TYPE" _
  )

  thinBasic_AddUDT( _
  "TYPE TBGL_tVector2F"+$CRLF+ _
  "  x AS SINGLE"+$CRLF+ _
  "  y AS SINGLE"+$CRLF+ _
  "END TYPE" _
  )

  thinBasic_AddUDT( _
  "TYPE TBGL_tRGB"+$CRLF+ _
  "  R AS BYTE"+$CRLF+ _
  "  G AS BYTE"+$CRLF+ _
  "  B AS BYTE"+$CRLF+ _
  "END TYPE" _
  )

  thinBasic_AddUDT( _
  "TYPE TBGL_tRGBA"+$CRLF+ _
  "  R AS BYTE"+$CRLF+ _
  "  G AS BYTE"+$CRLF+ _
  "  B AS BYTE"+$CRLF+ _
  "  A AS BYTE"+$CRLF+ _
  "END TYPE" _
  )

  thinBasic_AddUDT( _
  "TYPE TBGL_tRGB_f32"+$CRLF+ _
  "  R AS SINGLE"+$CRLF+ _
  "  G AS SINGLE"+$CRLF+ _
  "  B AS SINGLE"+$CRLF+ _
  "END TYPE" _
  )

  thinBasic_AddUDT( _
  "TYPE TBGL_tRGBA_f32"+$CRLF+ _
  "  R AS SINGLE"+$CRLF+ _
  "  G AS SINGLE"+$CRLF+ _
  "  B AS SINGLE"+$CRLF+ _
  "  A AS SINGLE"+$CRLF+ _
  "END TYPE" _
  )

  ' -----

  thinBasic_LoadSymbolEx  "TBGL_GBufferCreate"               , %thinBasic_ReturnCodeLong             , CODEPTR( Exec_TBGL_GBufferCreate )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_GBufferCreate(BYVAL gType AS NUMBER, BYVAL gDims AS NUMBER) AS DWORD", _
                          "Function to create new GBuffer"

  thinBasic_LoadSymbolEx  "TBGL_GBufferDestroy"              , %thinBasic_ReturnCodeLong             , CODEPTR( Exec_TBGL_GBufferDestroy )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_GBufferDestroy(BYVAL gbHandle AS DWORD)"                             , _
                          "Function to destroy contents of given GBuffer"

  thinBasic_LoadSymbolEx  "TBGL_GBufferDefineFromArray"      , %thinBasic_ReturnCodeLong             , CODEPTR( Exec_TBGL_GBufferDefineFromArray )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_GBufferDefineFromArray(BYVAL gbHandle AS DWORD, BYVAL kind AS LONG, BYVAL items AS LONG, BYREF VertexArray() AS TBGL_tVector3D, OPTIONAL BYREF ColorArray() AS TBGL_tRGB, BYREF NormalArray() AS TBGL_tVector3D,"+_
                          ", BYREF TexCoordArray1() AS TBGL_tVector2D, BYREF TexCoordArray2() AS TBGL_tVector2D, BYREF TexCoordArray3() AS TBGL_tVector2D, BYREF TexCoordArray4() AS TBGL_tVector2D )", _
                          "Function to define data for GBuffer"

  thinBasic_LoadSymbolEx  "TBGL_GBufferRender"               , %thinBasic_ReturnCodeLong             , CODEPTR( Exec_TBGL_GBufferRender )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_GBufferRender(BYVAL gbHandle AS DWORD)"                             , _
                          "Function to render contents of specified GBuffer"

  ' -----

  thinBasic_AddEquate   "%TBGL_Points"                          , "" , %TBGL_Points
  thinBasic_AddEquate   "%TBGL_Lines"                           , "" , %TBGL_Lines
  thinBasic_AddEquate   "%TBGL_LineStrip"                       , "" , %TBGL_LineStrip
  thinBasic_AddEquate   "%TBGL_LineLoop"                        , "" , %TBGL_LineLoop
  thinBasic_AddEquate   "%TBGL_Triangles"                       , "" , %TBGL_Triangles
  thinBasic_AddEquate   "%TBGL_TriangleStrip"                   , "" , %TBGL_TriangleStrip
  thinBasic_AddEquate   "%TBGL_TriangleFan"                     , "" , %TBGL_TriangleFan
  thinBasic_AddEquate   "%TBGL_Quads"                           , "" , %TBGL_Quads
  thinBasic_AddEquate   "%TBGL_QuadStrip"                       , "" , %TBGL_QuadStrip
  thinBasic_AddEquate   "%TBGL_Polygon"                         , "" , %TBGL_Polygon

  thinBasic_AddEquate   "%TBGL_Dynamic"                         , "" , %TBGL_Dynamic
  thinBasic_AddEquate   "%TBGL_Static"                          , "" , %TBGL_Static

END SUB

' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

FUNCTION Exec_TBGL_GBufferCreate() AS LONG

  LOCAL gbHandle AS TBGL_HANDLE

  LOCAL eType, eDims AS EXT

  thinBasic_Parse2Numbers(eType, eDims)

  gbHandle = Mem_Alloc(SIZEOF(TBGL_tGBuffer))

  g_GBuffersHandles += MKL$(gbHandle)

  DIM gBuffer(0) AS TBGL_tGBuffer AT gbHandle

  gBuffer(0).gType        = eType
  gBuffer(0).gDims        = eDims
  gBuffer(0).gKind        = 0

  gBuffer(0).items        = 0 ' -- Filled once data are passed

  gBuffer(0).vertexInfo.pData           = 0
  gBuffer(0).vertexInfo.nComponents     = IIF(eDims = %TBGL_2D, 2, 3)
  gBuffer(0).vertexInfo.componentType   = %GL_FLOAT

  gBuffer(0).colorInfo.pData            = 0
  gBuffer(0).colorInfo.nComponents      = 0 ' -- To be determined on assignment
  gBuffer(0).colorInfo.componentType    = 0 ' -- To be determined on assignment

  gBuffer(0).normalInfo.pData           = 0
  gBuffer(0).normalInfo.nComponents     = 3 ' Always
  gBuffer(0).normalInfo.componentType   = %GL_FLOAT

  gBuffer(0).coordInfo(1).pData         = 0
  gBuffer(0).coordInfo(1).nComponents   = 2 ' Always
  gBuffer(0).coordInfo(1).componentType = %GL_FLOAT

  gBuffer(0).coordInfo(2).pData         = 0
  gBuffer(0).coordInfo(2).nComponents   = 2 ' Always
  gBuffer(0).coordInfo(2).componentType = %GL_FLOAT

  gBuffer(0).coordInfo(3).pData         = 0
  gBuffer(0).coordInfo(3).nComponents   = 2 ' Always
  gBuffer(0).coordInfo(3).componentType = %GL_FLOAT

  gBuffer(0).coordInfo(4).pData         = 0
  gBuffer(0).coordInfo(4).nComponents   = 2 ' Always
  gBuffer(0).coordInfo(4).componentType = %GL_FLOAT

  FUNCTION = gbHandle

END FUNCTION

FUNCTION Exec_TBGL_GBufferDestroy() AS LONG

  LOCAL gbHandle AS TBGL_HANDLE

  gbHandle = thinbasic_Parse1Number()

  resource_GBuffers_Release(gbHandle)

END FUNCTION

' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

FUNCTION Exec_TBGL_GBufferDefineFromArray() AS LONG

  LOCAL gbHandle AS TBGL_HANDLE
  LOCAL i AS LONG

  LOCAL eHandle, eKind, eItems AS EXT
  LOCAL lArrayCellPtr AS LONG
  LOCAL lArrayCellAbsPos AS LONG
  LOCAL howManyLists AS EXT
  LOCAL VariablePtrOdd, MainType, SubType, ElementsAreFixed, TotElements, ElementSize AS LONG
  LOCAL helpBuffer AS STRING


  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber eHandle
    gbHandle = eHandle

    resource_GBuffer_ReleaseSingle(gbHandle, 1)
    DIM gBuffer(0) AS TBGL_tGBuffer AT gbHandle

    IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
      thinBasic_ParseNumber eKind
      gBuffer(0).gKind = eKind
      IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
        thinBasic_ParseNumber eItems
        gBuffer(0).items = eItems

        ' -- VERTEX
        IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
          thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

          if eItems > TotElements then
              ReportError "tbgl_gBufferDefineFromArray, VertexArray() parameter", _
                          "Passed number of items ("+format$(eItems)+") is higher than number of items in passed array ("+format$(TotElements)+")"
          end if

          VariablePtrOdd = VariablePtrOdd + (lArrayCellAbsPos-1)*ElementSize
          IF gBuffer(0).gKind = %TBGL_DYNAMIC THEN
            gBuffer(0).vertexInfo.pData = VariablePtrOdd
          ELSE
            gBuffer(0).vertexInfo.pData = Mem_Alloc(eItems*ElementSize)
            Mem_Copy(VariablePtrOdd, gBuffer(0).vertexInfo.pData, eItems*ElementSize)
          END IF

          ' -- COLOR
          IF thinBasic_CheckComma_Optional( ) AND thinBasic_ErrorFree THEN
            thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

            if eItems > TotElements then
                ReportError "tbgl_gBufferDefineFromArray, ColorArray() parameter", _
                            "Passed number of items ("+format$(eItems)+") is higher than number of items in passed array ("+format$(TotElements)+")"
            end if

            VariablePtrOdd = VariablePtrOdd + (lArrayCellAbsPos-1)*ElementSize
            IF gBuffer(0).gKind = %TBGL_DYNAMIC THEN
              gBuffer(0).colorInfo.pData       = VariablePtrOdd
            ELSE
              gBuffer(0).colorInfo.pData = Mem_Alloc(eItems*ElementSize)
              Mem_Copy(VariablePtrOdd, gBuffer(0).colorInfo.pData, eItems*ElementSize)
            END IF

            select case as long ElementSize
              case sizeOf(TBGL_tRGB)
                gBuffer(0).colorInfo.nComponents   = 3
                gBuffer(0).colorInfo.componentType = %GL_UNSIGNED_BYTE

              case sizeOf(TBGL_tRGBA)
                gBuffer(0).colorInfo.nComponents   = 4
                gBuffer(0).colorInfo.componentType = %GL_UNSIGNED_BYTE

              case sizeOf(TBGL_tRGB_f32)
                gBuffer(0).colorInfo.nComponents   = 3
                gBuffer(0).colorInfo.componentType = %GL_FLOAT

              case sizeOf(TBGL_tRGBA_f32)
                gBuffer(0).colorInfo.nComponents   = 4
                gBuffer(0).colorInfo.componentType = %GL_FLOAT
            end select

            ' -- NORMAL
            IF thinBasic_CheckComma_Optional( ) AND thinBasic_ErrorFree THEN
              thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

              if eItems > TotElements then
                  ReportError "tbgl_gBufferDefineFromArray, NormalArray() parameter", _
                              "Passed number of items ("+format$(eItems)+") is higher than number of items in passed array ("+format$(TotElements)+")"
              end if

              VariablePtrOdd = VariablePtrOdd + (lArrayCellAbsPos-1)*ElementSize
              IF gBuffer(0).gKind = %TBGL_DYNAMIC THEN
                gBuffer(0).normalInfo.pData = VariablePtrOdd
              ELSE
                gBuffer(0).normalInfo.pData = Mem_Alloc(eItems*ElementSize)
                Mem_Copy(VariablePtrOdd, gBuffer(0).normalInfo.pData, eItems*ElementSize)
              END IF

              ' -- TEXCOORD
              FOR i = 1 TO 4
                IF thinBasic_CheckComma_Optional( ) AND thinBasic_ErrorFree THEN
                  thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

                  if eItems > TotElements then
                      ReportError "tbgl_gBufferDefineFromArray, TexCoordArray() parameter", _
                                  "Passed number of items ("+format$(eItems)+") is higher than number of items in passed array ("+format$(TotElements)+")"
                  end if

                  VariablePtrOdd = VariablePtrOdd + (lArrayCellAbsPos-1)*ElementSize
                  IF gBuffer(0).gKind = %TBGL_DYNAMIC THEN
                    gBuffer(0).coordInfo(i).pData = VariablePtrOdd
                  ELSE
                    gBuffer(0).coordInfo(i).pData = Mem_Alloc(eItems*ElementSize)
                    Mem_Copy(VariablePtrOdd, gBuffer(0).coordInfo(i).pData, eItems*ElementSize)
                  END IF
                ELSE
                  EXIT FOR
                END IF
              NEXT
            END IF
          END IF
        END IF
      END IF
    END IF
  END IF

  thinBasic_CheckCloseParens_Mandatory()

END FUNCTION

' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

FUNCTION Exec_TBGL_GBufferRender() AS LONG

  LOCAL gbHandle AS TBGL_HANDLE

  gbHandle = thinbasic_Parse1Number()

  DIM gBuffer(0) AS TBGL_tGBuffer AT gbHandle

  IF gBuffer(0).items = 0 THEN EXIT FUNCTION

  ' -- Bind the data
  IF gBuffer(0).normalInfo.pData THEN
    glEnableClientState(%GL_NORMAL_ARRAY)
    glNormalPointer  (gBuffer(0).normalInfo.componentType, 0, BYVAL gBuffer(0).normalInfo.pData)
  END IF

  IF gBuffer(0).colorInfo.pData THEN
    glEnableClientState(%GL_COLOR_ARRAY)
    glColorPointer(gBuffer(0).colorInfo.nComponents, gBuffer(0).colorInfo.componentType, 0, BYVAL gBuffer(0).colorInfo.pData)
  END IF

  IF gBuffer(0).coordInfo(1).pData THEN
    glEnableClientState(%GL_TEXTURE_COORD_ARRAY)
    glTexCoordPointer(gBuffer(0).coordInfo(1).nComponents, gBuffer(0).coordInfo(1).componentType, 0, BYVAL gBuffer(0).coordInfo(1).pData)
  END IF

  IF gBuffer(0).vertexInfo.pData THEN
    glEnableClientState(%GL_VERTEX_ARRAY)
    glVertexPointer(gBuffer(0).vertexInfo.nComponents, gBuffer(0).vertexInfo.componentType, 0, BYVAL gBuffer(0).vertexInfo.pData)
  END IF

  ' -- Render it
  glDrawArrays(gBuffer(0).gType, 0, gBuffer(0).items)

  ' -- Un bind it
  IF gBuffer(0).vertexInfo.pData   THEN glDisableClientState(%GL_VERTEX_ARRAY)
  IF gBuffer(0).coordInfo(1).pData THEN glDisableClientState(%GL_TEXTURE_COORD_ARRAY)
  IF gBuffer(0).colorInfo.pData    THEN glDisableClientState(%GL_COLOR_ARRAY)
  IF gBuffer(0).normalInfo.pData   THEN glDisableClientState(%GL_NORMAL_ARRAY)
END FUNCTION

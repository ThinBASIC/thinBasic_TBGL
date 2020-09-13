' =============================================================================
'  Part of     : thinBASIC_TBGL
'
'  Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : Functions to manage M15 and its (old) bones
' =============================================================================
'
' -----------------------------------------------------------------------------

  GLOBAL m15ModelNum AS LONG
  GLOBAL m15ModelVtx AS LONG

  TYPE tM15Data
    vertexCount   AS LONG
    isTextured    as long
    ' -- isTriangles, isQuads ...
  END TYPE

  GLOBAL m15ModelData() AS tM15Data
  GLOBAL m15ModelLayers() AS STRING

' -----------------------------------------------------------------------------

TYPE Tm15ImmediateData
  TexXY(1)  AS SINGLE  ' [!] 0 to 1
  clr(2)    AS BYTE    ' [!] 0 to 2
  vtx(2)    AS SINGLE  ' [!] 0 to 2
  n(2)      AS SINGLE  ' [!] 0 TO 2

  pStop     AS BYTE    ' polygon stop flag
  TexN      AS LONG    ' texture number
  Layer     AS LONG    ' layer number
END TYPE

GLOBAL m15ModelBuffer() AS STRING
GLOBAL m15ModelBuffer_Overlay() AS Tm15ImmediateData

' -----------------------------------------------------------------------------

  #INCLUDE ONCE "class_Models.inc"
  #INCLUDE ONCE "tbgl_OldBones.inc"

' -----------------------------------------------------------------------------
'
' [TODO]: Vertex arrays implemented, would be nice to support VBO as well
' [TODO]: BONES right!

' -----------------------------------------------------------------------------

%TBGL_M15X        = 1
%TBGL_M15Y        = 2
%TBGL_M15Z        = 3
%TBGL_M15PSTOP    = 4
%TBGL_M15TEXN     = 5
%TBGL_M15TEXX     = 6
%TBGL_M15TEXY     = 7
%TBGL_M15R        = 8
%TBGL_M15G        = 9
%TBGL_M15B        = 10
%TBGL_M15LAYER    = 11

%TBGL_NORMAL_NONE    = 0
%TBGL_NORMAL_PRECISE = 1
%TBGL_NORMAL_SMOOTH  = 2
%TBGL_NORMAL_FROMFILE= 3
' -----------------------------------------------------------------------------

' -- Used in normal calculations
%MAX_BUFFERITO_INDEX = 128
TYPE tCommonVertex
  x   AS SINGLE                 ' coordinates
  y   AS SINGLE
  z   AS SINGLE

  nx  AS SINGLE                 ' normals
  ny  AS SINGLE
  nz  AS SINGLE

  vertexCount AS LONG                 ' how much

  bufferito(1 TO %MAX_BUFFERITO_INDEX)  AS LONG  ' to store vertex index
END TYPE

' -----------------------------------------------------------------------------

MACRO ParsedVal(src, position) = VAL( PARSE$( src, ",", position ))

' -----------------------------------------------------------------------------

' -- Initializes structures for models and bones
FUNCTION Exec_TBGL_m15InitModelBuffers() AS EXT
  REGISTER i AS LONG
  LOCAL n AS EXT, n2 AS EXT

  thinBASIC_Parse2Numbers( n, n2 )

  IF n > 0 AND n2 > 3 THEN
    m15ModelNum = n
    m15ModelVtx = n2

    REDIM m15ModelBuffer(1 TO m15ModelNum)

    FOR i = 1 TO m15ModelNum
      m15ModelBuffer(i) = STRING$( m15ModelVtx * SIZEOF(Tm15ImmediateData), 0 )
    NEXT
    REDIM m15ModelData(1 TO m15ModelNum)

    REDIM m15ModelLayers(1 TO m15ModelNum)

    ' ---------------------------------------------------------

    REDIM BoneBox(1 TO m15ModelNum, 1 TO %MAXBONES)
    REDIM BoneTree(1 TO m15ModelNum, 1 TO %MAXBONES)

    ' Used in apply bones
    REDIM vtx_x(1 TO m15ModelVtx)
    REDIM vtx_y(1 TO m15ModelVtx)
    REDIM vtx_z(1 TO m15ModelVtx)

  ELSE
    tbglerror "Invalid parameter"+IIF$(n <= 0 AND n2 < 3, "s", "")+" for TBGL_m15InitModelBuffers"+$CRLF+$CRLF+IIF$(n <= 0 ,"Number of models:"+STR$(n)+$CRLF, "")+IIF$(n2 < 3, "Number of vertices:"+STR$(n2), "")
  END IF

END FUNCTION

' -- Default filter used when loading textures for models
SUB Exec_TBGL_m15SetDefaultTextureFilter()

  LOCAL eFilter, eAniso AS EXT
  LOCAL n AS LONG

  n = thinBASIC_ParseXNumbers(1, 2, eFilter, eAniso )
  texture_DefaultModelFilter = eFilter
  IF n > 1 THEN texture_DefaultModelFilterMultiplier = IIF(eAniso > texture_AnisotropyMax, texture_AnisotropyMax, eAniso )

END SUB

' -- Loads M15 model from file
FUNCTION Exec_TBGL_m15LoadModel() AS LONG

  REGISTER i AS LONG, e AS LONG

  LOCAL fKeyword AS STRING, totalCount AS LONG
  LOCAL FileContent AS STRING
  DIM TextureMoved(0 TO %LIMIT_MAX_TEXTUREINDEX) AS LOCAL LONG
  LOCAL j AS LONG     , temp AS LONG
  LOCAL texList AS STRING, TextureToLoad AS STRING

  LOCAL SrcPath AS STRING
  LOCAL StartTexSearch AS LONG
  LOCAL ff AS LONG

  LOCAL VertexCountNow, MaxVertexCount AS LONG

  LOCAL FileName AS STRING, TextureDirectory AS STRING,Model AS EXT, MakeList AS EXT, NormalMethod AS EXT

  ' -- Features of new versions
  LOCAL supportNormals AS LONG

  thinBasic_ParseString FileName

  IF thinBasic_CheckComma_Mandatory THEN
    thinBasic_ParseString TextureDirectory

    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber Model

      IF Model < 1 THEN tbglError filename+":"+$CRLF+"Model index must be positive number (used "+FORMAT$(model)+")" : EXIT FUNCTION

      IF thinBasic_CheckComma_Mandatory THEN
        thinBasic_ParseNumber Makelist

        IF thinBasic_CheckComma_Mandatory THEN
          thinBasic_ParseNumber NormalMethod

          ' -------------------------------------------------------------------------
          IF UBOUND(m15ModelBuffer) < Model THEN
            REDIM PRESERVE m15ModelBuffer (1 TO Model)
            REDIM PRESERVE m15ModelData   (1 TO Model)
            REDIM PRESERVE m15ModelLayers (1 TO Model)
            m15ModelNum = Model

          END IF
          ' -------------------------------------------------------------------------
          DIM Token(15) AS STRING
          ' -------------------------------------------------------------------------

          SrcPath = thinBasic_GetRunTimeInfo("SCRIPT_PATH")

          ' -- Texture path setup
          LOCAL TextureCounterMini AS LONG
          IF LEN(TextureDirectory) = 0          THEN TextureDirectory = "Textures\"
          IF RIGHT$(TextureDirectory,1) <> "\"  THEN TextureDirectory = TextureDirectory + "\"
          IF INSTR(TextureDirectory, ":") = 0   THEN TextureDirectory = SrcPath+TextureDirectory

          ' -- Layer setup
          m15ModelLayers(Model) = "<GENERALLAYER>"

          ' -- Filename preprocessing
          ff = FREEFILE
          REPLACE ".\" WITH "" IN FileName
          IF File_Exists(FileName) = 0 THEN FileName = SrcPath+FileName
          IF File_Exists(FileName) = 0 THEN TBGLERROR "Cannot locate model file:"+$CRLF+FileName : EXIT FUNCTION

          ' -- Open the file
          OPEN FileName FOR BINARY AS #ff
            GET$ #ff, LOF(#ff), FileContent
          CLOSE #ff

          ' -- Get the lines out of it
          totalCount = PARSECOUNT(FileContent, $CRLF)
          DIM fSRCLine(1 TO totalCount) AS LOCAL STRING
          PARSE FileContent, fSRCLine(), $CRLF

          ' -- Detect version
          DIM version(3) AS LONG
          version(0) = VAL(PARSE$(fSRCLine(1), ANY "{.", 2))
          version(1) = VAL(PARSE$(fSRCLine(1),      ".", 2))
          version(2) = VAL(PARSE$(fSRCLine(1),      ".", 3))
          version(3) = VAL(PARSE$(fSRCLine(1), ANY ".}", 4))

          IF version(0) >= 1 AND version(1) >= 0 AND version(2) >= 0 AND version(3) >= 7 THEN supportNormals = %TRUE
          IF normalMethod = %TBGL_NORMAL_FROMFILE AND supportNormals = %FALSE THEN TBGLERROR "File does not contain any normal vector information" : EXIT FUNCTION
          ' -- [!] VERTEXNUM must be defined in file !!!

          ' -- Let CPU take a breath
          SLEEP 1

          FOR i = 1 TO totalCount

            PARSE fSRCLine(i), Token(), ","
            fKeyword    = Token(0)

            SELECT CASE fKeyword
              CASE "POLY"
                  IF VertexCountNow < MaxVertexCount THEN
                    INCR VertexCountNow
                  ELSE
                    INCR VertexCountNow
                    m15ModelBuffer(Model) = m15ModelBuffer(Model) + STRING$( SIZEOF(Tm15ImmediateData), 0 )
                    REDIM m15ModelBuffer_Overlay(1 TO MaxVertexCount) AT STRPTR(m15ModelBuffer(Model))
                  END IF

                  m15ModelBuffer_Overlay(VertexCountNow).vtx(0)   = VAL(Token(1))
                  m15ModelBuffer_Overlay(VertexCountNow).vtx(1)   = VAL(Token(2))
                  m15ModelBuffer_Overlay(VertexCountNow).vtx(2)   = VAL(Token(3))
                  m15ModelBuffer_Overlay(VertexCountNow).PStop    = VAL(Token(4))
                  m15ModelBuffer_Overlay(VertexCountNow).TexN     = TextureMoved(VAL(Token(5)))
                  m15ModelBuffer_Overlay(VertexCountNow).TexXY(0) = VAL(Token(6))
                  m15ModelBuffer_Overlay(VertexCountNow).TexXY(1) = VAL(Token(7))
                  m15ModelBuffer_Overlay(VertexCountNow).clr(0)   = VAL(Token(8))
                  m15ModelBuffer_Overlay(VertexCountNow).clr(1)   = VAL(Token(9))
                  m15ModelBuffer_Overlay(VertexCountNow).clr(2)   = VAL(Token(10))
                  m15ModelBuffer_Overlay(VertexCountNow).Layer    = VAL(Token(11))

                  IF supportNormals = %TRUE AND NormalMethod = %TBGL_NORMAL_FROMFILE THEN
                    m15ModelBuffer_Overlay(VertexCountNow).n(0)   = VAL(Token(12))
                    m15ModelBuffer_Overlay(VertexCountNow).n(1)   = VAL(Token(13))
                    m15ModelBuffer_Overlay(VertexCountNow).n(2)   = VAL(Token(14))
                  END IF

              CASE "VERTEXNUM"
                IF MaxVertexCount = 0 THEN
                  MaxVertexCount = VAL(Token(1))
                  ' -- Pre allocate space
                  m15ModelBuffer(Model) = STRING$( MaxVertexCount * SIZEOF(Tm15ImmediateData), 0 )
                  REDIM m15ModelBuffer_Overlay(1 TO MaxVertexCount) AT STRPTR(m15ModelBuffer(Model))
                END IF

              CASE "LAYERS"
                LOCAL layerstring AS STRING
                layerstring = UCASE$(MID$(fSRCLine(i), INSTR(fSRCLine(i), ",")+1, LEN(fSRCLine(i))))

                FOR j = 1 TO PARSECOUNT(layerstring, ",")
                  m15ModelLayers(Model) = m15ModelLayers(Model) + "," + TRIM$(PARSE$(layerstring, ",", j))
                NEXT

              CASE "TEXLIST"
                  ' -- Loading textures
                  texList = MID$(fSRCLine(i), INSTR(fSRCLine(i), ",")+1, LEN(fSRCLine(i)))

                  m15ModelData(Model).isTextured = %TRUE

                  FOR j = 1 TO PARSECOUNT(texList, ",")
                    TextureToLoad = PARSE$(texList, ",", j)

                    IF LEN(TextureToLoad) THEN
                      IF INSTR(TextureToLoad, ":") = 0 THEN TextureToLoad = TextureDirectory+TextureToLoad
                      TextureToLoad = UCASE$(TextureToLoad)

                      temp = meIsTextureLoaded(TextureToLoad)

                      IF temp = 0 THEN

                          INCR TextureCounterMini
                          StartTexSearch = TextureCounterMini
                          WHILE LEN(TextureList(TextureCounterMini))
                            INCR TextureCounterMini
                            IF TextureCounterMini + 1 = StartTexSearch THEN EXIT LOOP ' Silly situation, nothing to place
                            IF TextureCounterMini > %LIMIT_MAX_TEXTUREINDEX THEN TextureCounterMini = 1
                          WEND

                          IF UCASE$(TRIM$(PARSE$( TextureToLoad, ".", -1))) = "BMP" THEN
                            TextureLoadBMP(0, TextureToLoad, TextureCounterMini, texture_DefaultModelFilter, texture_DefaultModelFilterMultiplier, -1 )
                          ELSEIF UCASE$(TRIM$(PARSE$( TextureToLoad, ".", -1))) = "TGA" THEN
                            TextureLoadTGA(0, TextureToLoad, TextureCounterMini, texture_DefaultModelFilter, texture_DefaultModelFilterMultiplier)
                          END IF

                        TextureMoved(j+1) = TextureCounterMini
                      ELSE
                        TextureMoved(j+1) = temp  ' Texture with original index j is moved to meIsTextureLoaded returned value
                      END IF
                    ELSE
                      EXIT FOR
                    END IF
                  NEXT

            END SELECT

          NEXT

          m15ModelData(Model).vertexCount = VertexCountNow

          FUNCTION = 0

          ' -- When calculation of normals is requested
          IF NormalMethod > %TBGL_NORMAL_NONE AND NormalMethod < %TBGL_NORMAL_FROMFILE THEN internal_RecalcNormals(Model, NormalMethod)

          IF MakeList > 0 THEN
            IF resource_DisplayLists_IsGenerated(MakeList) = 0 THEN TBGLError "Statement:"+$TAB+"tbgl_m15LoadModel"+$CRLF+"Problem:"+$TAB+$TAB+$ERR_DISPLISTINDEX+$CRLF+$CRLF+"Passed:"+$TAB+FORMAT$(MakeList)
            glNewList MakeList, %GL_COMPILE
              ' -- Drawing model, with no forced settings
              internal_DrawModel (Model, -1, -1, -1, -1)
            glEndList
          END IF
          ' -------------------------------------------------------------------------
        END IF
      END IF
    END IF
  END IF

END FUNCTION

' -- Renders model
SUB Exec_TBGL_m15DrawModel(  )

  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  LOCAL ModelID AS EXT

  ModelID = thinBasic_Parse1Number
  internal_DrawModel(ModelID, -1, -1, -1, -1)

END SUB

' -- Recalculates normals
SUB Exec_TBGL_m15RecalcNormals()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  LOCAL Model AS EXT
  LOCAL nType AS EXT
  LOCAL Layer AS STRING


  thinBasic_ParseNumber Model
  IF thinBasic_CheckComma_Mandatory THEN
    thinBasic_ParseNumber nType
    IF thinBasic_CheckComma_Optional THEN thinBasic_ParseString Layer

    IF nType > 0 THEN
      IF LEN(Layer) = 0 THEN
        internal_RecalcNormals(Model, nType)
      ELSE
        internal_RecalcNormalsLayer(Model, nType, Layer)
      END IF
    END IF

  END IF
END SUB

' -- Prepares model slot for reuse
SUB Exec_TBGL_m15ClearModel()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  REGISTER i AS LONG
  LOCAL Model AS EXT

  Model = thinBasic_Parse1Number

  LOCAL vertexcount AS LONG
  vertexcount = m15ModelData(model).vertexCount

  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))

  FOR i = 1 TO Vertexcount
    m15ModelBuffer_Overlay(i).vtx(0) = 0
    m15ModelBuffer_Overlay(i).vtx(1) = 0
    m15ModelBuffer_Overlay(i).vtx(2) = 0
    m15ModelBuffer_Overlay(i).PStop = 0
    m15ModelBuffer_Overlay(i).clr(0) = 0
    m15ModelBuffer_Overlay(i).clr(1) = 0
    m15ModelBuffer_Overlay(i).clr(2) = 0
    m15ModelBuffer_Overlay(i).TexN = 0
    m15ModelBuffer_Overlay(i).TexXY(0) = 0
    m15ModelBuffer_Overlay(i).TexXY(1) = 0
    m15ModelBuffer_Overlay(i).Layer = 0
  NEXT

  m15ModelLayers(Model) = ""
  m15ModelData(model).vertexCount = 0

END SUB

' -----------------------------------------------------------------------------
' -- PARAMETERS
' -----------------------------------------------------------------------------

' -----------------------------------------------------------------------------
' -- STATISTICS
' -----------------------------------------------------------------------------

' -- Returns model vertex count
FUNCTION Exec_TBGL_m15GetModelVertexcount() AS LONG
  IF m15ModelNum = 0 THEN tbglError "Invalid function call to M15 related function"+$CRLF+"You must specify model buffer using TBGL_m15InitModelBuffer" : EXIT FUNCTION
  LOCAL Model AS EXT

  model = thinBasic_Parse1Number()
  FUNCTION = m15ModelData(model).vertexCount

END FUNCTION

' -- Sets model vertex count, useful when dealing with custom on-the-fly models
SUB Exec_TBGL_m15SetModelVertexcount()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  LOCAL Model AS EXT
  LOCAL i AS EXT

  thinBasic_Parse2Numbers( Model, i)

  m15ModelData(model).vertexCount = i

END SUB

' -- Gets number of polygons in models
FUNCTION Exec_TBGL_m15GetModelPolycount() AS EXT
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT FUNCTION

  REGISTER i AS LONG, vtxs AS LONG
  LOCAL polys AS LONG
  LOCAL Model AS EXT

  Model = thinBasic_Parse1Number
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  vtxs = m15ModelData(model).vertexCount
  FOR i = 1 TO vtxs
    IF m15ModelBuffer_Overlay(i).PStop = 1 THEN INCR polys
  NEXT

  FUNCTION = polys

END FUNCTION

' -- Gets width, height, length of model
SUB EXEC_TBGL_m15GetModelDimensions()
  REGISTER i AS LONG
  LOCAL Model AS EXT

  LOCAL lVariablePtr1     AS LONG
  LOCAL lVariableAbsPos1  AS LONG

  LOCAL lVariablePtr2     AS LONG
  LOCAL lVariableAbsPos2  AS LONG

  LOCAL lVariablePtr3     AS LONG
  LOCAL lVariableAbsPos3  AS LONG

  LOCAL x,y,z AS EXT

  LOCAL minx, miny, minz, maxx, maxy, maxz AS DOUBLE
  minx =  1000000
  miny =  1000000
  minz =  1000000
  maxx = -1000000
  maxy = -1000000
  maxz = -1000000

  IF thinBasic_CheckOpenParens THEN
    thinBasic_ParseNumber Model
    REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
    IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
      thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)
          IF thinBasic_CheckCloseParens THEN

            FOR i = 1 TO m15ModelData(Model).vertexCount

              x = m15ModelBuffer_Overlay(i).vtx(0)
              y = m15ModelBuffer_Overlay(i).vtx(1)
              z = m15ModelBuffer_Overlay(i).vtx(2)

              IF x < minx THEN minx = x
              IF y < miny THEN miny = y
              IF z < minz THEN minz = z

              IF x > maxx THEN maxx = x
              IF y > maxy THEN maxy = y
              IF z > maxz THEN maxz = z

            NEXT
            x = maxx - minx
            y = maxy - miny
            z = maxz - minz

            '---Now assign values to referenced variables
            thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, x)
            thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, y)
            thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, z)

          END IF
        END IF
      END IF
    END IF
  END IF

END SUB

' -----------------------------------------------------------------------------
' -- POSTION
' -----------------------------------------------------------------------------

' -- Multifunctional ... function to set all parameters
' -- I wanted to save some bytes in final DLL by not doing separate function for each
' -- param, but this command is not very pleasant to use ... lesson learned
SUB Exec_TBGL_m15SetVertexParam()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  LOCAL Model AS EXT
  LOCAL i AS EXT
  LOCAL param AS EXT
  LOCAL lngParam AS LONG
  LOCAL value AS EXT

  thinBasic_Parse4Numbers( Model, i, Param, value )
  lngParam = Param
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))

  ' -- According to type, we assign parameter
  SELECT CASE AS LONG lngParam
    CASE %TBGL_M15X
      m15ModelBuffer_Overlay(i).vtx(0) = value
    CASE %TBGL_M15Y
      m15ModelBuffer_Overlay(i).vtx(1) = value
    CASE %TBGL_M15Z
      m15ModelBuffer_Overlay(i).vtx(2) = value
    CASE %TBGL_M15PSTOP
      m15ModelBuffer_Overlay(i).PStop = value
    CASE %TBGL_M15TEXN
      m15ModelBuffer_Overlay(i).TexN = value
      m15ModelData(Model).isTextured = m15ModelBuffer_Overlay(i).TexN > 0
    CASE %TBGL_M15TEXX
      m15ModelBuffer_Overlay(i).TexXY(0) = value
    CASE %TBGL_M15TEXY
      m15ModelBuffer_Overlay(i).TexXY(1) = value
    CASE %TBGL_M15R
      m15ModelBuffer_Overlay(i).clr(0) = value
    CASE %TBGL_M15G
      m15ModelBuffer_Overlay(i).clr(1) = value
    CASE %TBGL_M15B
      m15ModelBuffer_Overlay(i).clr(2) = value
    CASE %TBGL_M15LAYER
      m15ModelBuffer_Overlay(i).Layer = value
  END SELECT

END SUB

' -- Multifunctional ... function to get all parameters
' -- I wanted to save some bytes in final DLL by not doing separate function for each
' -- param, but this command is not very pleasant to use ... lesson learned
FUNCTION Exec_TBGL_m15GetVertexParam() AS SINGLE
  IF m15ModelNum = 0 THEN tbglError "Invalid function call to M15 related function"+$CRLF+"You must specify model buffer using TBGL_m15InitModelBuffer" : EXIT FUNCTION
  LOCAL Model AS EXT
  LOCAL i AS EXT
  LOCAL param AS EXT
  LOCAL lngParam AS LONG

  thinBasic_Parse3Numbers( Model, i, Param)
  lngParam = param
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))

  SELECT CASE AS LONG lngParam
    CASE %TBGL_M15X
      FUNCTION = m15ModelBuffer_Overlay(i).vtx(0)
    CASE %TBGL_M15Y
      FUNCTION = m15ModelBuffer_Overlay(i).vtx(1)
    CASE %TBGL_M15Z
      FUNCTION = m15ModelBuffer_Overlay(i).vtx(2)
    CASE %TBGL_M15PSTOP
      FUNCTION = m15ModelBuffer_Overlay(i).PStop
    CASE %TBGL_M15TEXN
      FUNCTION = m15ModelBuffer_Overlay(i).TexN
    CASE %TBGL_M15TEXX
      FUNCTION = m15ModelBuffer_Overlay(i).TexXY(0)
    CASE %TBGL_M15TEXY
      FUNCTION = m15ModelBuffer_Overlay(i).TexXY(1)
    CASE %TBGL_M15R
      FUNCTION = m15ModelBuffer_Overlay(i).clr(0)
    CASE %TBGL_M15G
      FUNCTION = m15ModelBuffer_Overlay(i).clr(1)
    CASE %TBGL_M15B
      FUNCTION = m15ModelBuffer_Overlay(i).clr(2)
    CASE %TBGL_M15LAYER
      FUNCTION = m15ModelBuffer_Overlay(i).Layer
  END SELECT

END FUNCTION

' -- Setting X coordinate
SUB Exec_TBGL_m15SetVertexX()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).vtx(0) = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).vtx(0) = Num2
    NEXT

  END IF

END SUB

' -- Gets X position
FUNCTION Exec_TBGL_m15GetVertexX() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).vtx(0)

END FUNCTION

' -- Setting Y coordinate
SUB Exec_TBGL_m15SetVertexY()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).vtx(1) = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).vtx(1) = Num2
    NEXT

  END IF

END SUB

' -- Gets Y position
FUNCTION Exec_TBGL_m15GetVertexY() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).vtx(1)

END FUNCTION


' -- Setting Z coordinate
SUB Exec_TBGL_m15SetVertexZ()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))

  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).vtx(2) = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).vtx(2) = Num2
    NEXT

  END IF

END SUB

' -- Gets Z position
FUNCTION Exec_TBGL_m15GetVertexZ() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).vtx(2)

END FUNCTION


' -- Setting all coordinates
SUB Exec_TBGL_m15SetVertexXYZ()

  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, Num3, Num4, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(5, 6, Model, index, Num1, Num2, Num3, Num4)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 5 THEN
    m15ModelBuffer_Overlay(index).vtx(0) = Num1
    m15ModelBuffer_Overlay(index).vtx(1) = Num2
    m15ModelBuffer_Overlay(index).vtx(2) = Num3

  ELSEIF nFound = 6 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).vtx(0) = Num2
      m15ModelBuffer_Overlay(i).vtx(1) = Num3
      m15ModelBuffer_Overlay(i).vtx(2) = Num4
    NEXT

  END IF

END SUB

' -- Gets complete XYZ
SUB Exec_TBGL_m15GetVertexXYZ()

  LOCAL Model AS EXT
  LOCAL i AS EXT

  LOCAL lVariablePtr1     AS LONG
  LOCAL lVariableAbsPos1  AS LONG

  LOCAL lVariablePtr2     AS LONG
  LOCAL lVariableAbsPos2  AS LONG

  LOCAL lVariablePtr3     AS LONG
  LOCAL lVariableAbsPos3  AS LONG

  LOCAL x,y,z AS EXT

  IF thinBasic_CheckOpenParens THEN
    thinBasic_ParseNumber Model
    REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber i
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
          IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
            thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)
            IF thinBasic_CheckCloseParens THEN

              x = m15ModelBuffer_Overlay(i).vtx(0)
              y = m15ModelBuffer_Overlay(i).vtx(1)
              z = m15ModelBuffer_Overlay(i).vtx(2)

              '---Now assign values to referenced variables
              thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, x)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, y)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, z)

            END IF
          END IF
        END IF
      END IF
    END IF
  END IF

END SUB

' -- Polygon stop flag
SUB Exec_TBGL_m15SetVertexPStop()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).PStop = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).PStop = Num2
    NEXT

  END IF

END SUB

' -- Retrieves Polygon stop flag
FUNCTION Exec_TBGL_m15GetVertexPStop() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).PStop

END FUNCTION

' -----------------------------------------------------------------------------
' -- COLORS
' -----------------------------------------------------------------------------

' -- Vertex Red color channel
SUB Exec_TBGL_m15SetVertexR()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).clr(0) = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).clr(0) = Num2
    NEXT

  END IF
END SUB

FUNCTION Exec_TBGL_m15GetVertexR() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).clr(0)

END FUNCTION

' -- Vertex Green color channel
SUB Exec_TBGL_m15SetVertexG()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).clr(1) = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).clr(1) = Num2
    NEXT

  END IF
END SUB

FUNCTION Exec_TBGL_m15GetVertexG() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).clr(1)

END FUNCTION

' -- Vertex Blue color channel
SUB Exec_TBGL_m15SetVertexB()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).clr(2) = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).clr(2) = Num2
    NEXT

  END IF
END SUB

FUNCTION Exec_TBGL_m15GetVertexB() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).clr(2)

END FUNCTION

' -- All color channels at once
SUB Exec_TBGL_m15SetVertexRGB()

  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, Num3, Num4, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(5, 6, Model, index, Num1, Num2, Num3, Num4)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))



  IF nFound = 5 THEN
    m15ModelBuffer_Overlay(index).clr(0) = Num1
    m15ModelBuffer_Overlay(index).clr(1) = Num2
    m15ModelBuffer_Overlay(index).clr(2) = Num3

  ELSEIF nFound = 6 THEN

    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).clr(0) = Num2
      m15ModelBuffer_Overlay(i).clr(1) = Num3
      m15ModelBuffer_Overlay(i).clr(2) = Num4
    NEXT

  END IF

END SUB

' -- Retrieving RGB color
SUB Exec_TBGL_m15GetVertexRGB()

  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  LOCAL Model AS EXT
  LOCAL i AS EXT

  LOCAL lVariablePtr1     AS LONG
  LOCAL lVariableAbsPos1  AS LONG

  LOCAL lVariablePtr2     AS LONG
  LOCAL lVariableAbsPos2  AS LONG

  LOCAL lVariablePtr3     AS LONG
  LOCAL lVariableAbsPos3  AS LONG


  LOCAL r,g,b AS EXT

  IF thinBasic_CheckOpenParens THEN
    thinBasic_ParseNumber Model
    REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber i
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
          IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
            thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)
            IF thinBasic_CheckCloseParens THEN

              R = m15ModelBuffer_Overlay(i).clr(0)
              G = m15ModelBuffer_Overlay(i).clr(1)
              B = m15ModelBuffer_Overlay(i).clr(2)

              '---Now assign values to referenced variables
              thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, R)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, G)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, B)

            END IF
          END IF
        END IF
      END IF
    END IF
  END IF

END SUB

' -----------------------------------------------------------------------------
' -- Textures
' -----------------------------------------------------------------------------

' -- Texture number
SUB Exec_TBGL_m15SetVertexTexN()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).TexN = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).TexN = Num2
    NEXT

  END IF

  m15ModelData(Model).isTextured = m15ModelBuffer_Overlay(index).TexN > 0

END SUB

FUNCTION Exec_TBGL_m15GetVertexTexN() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).TexN

END FUNCTION

' -- Setting texture U coordinate
SUB Exec_TBGL_m15SetVertexTexX()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).TexXY(0) = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).TexXY(0) = Num2
    NEXT

  END IF
END SUB

FUNCTION Exec_TBGL_m15GetVertexTexX() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).TexXY(0)

END FUNCTION

' -- Setting texture V coordinate
SUB Exec_TBGL_m15SetVertexTexY()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).TexXY(1) = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).TexXY(1) = Num2
    NEXT

  END IF
END SUB

FUNCTION Exec_TBGL_m15GetVertexTexY() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).TexXY(1)

END FUNCTION

' -- Setting texture UV coordinate
SUB Exec_TBGL_m15SetVertexTexXY()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, Num3, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(4, 5, Model, index, Num1, Num2, Num3)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 4 THEN
    m15ModelBuffer_Overlay(index).TexXY(0) = Num1
    m15ModelBuffer_Overlay(index).TexXY(1) = Num2

  ELSEIF nFound = 5 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).TexXY(0) = Num2
      m15ModelBuffer_Overlay(i).TexXY(1) = Num3
    NEXT

  END IF

END SUB

SUB Exec_TBGL_m15GetVertexTexXY()

  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  LOCAL Model AS EXT
  LOCAL i AS EXT

  LOCAL lVariablePtr1     AS LONG
  LOCAL lVariableAbsPos1  AS LONG

  LOCAL lVariablePtr2     AS LONG
  LOCAL lVariableAbsPos2  AS LONG

  LOCAL x,y AS EXT

  IF thinBasic_CheckOpenParens THEN
    thinBasic_ParseNumber Model
    REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber i
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
            IF thinBasic_CheckCloseParens THEN

              x = m15ModelBuffer_Overlay(i).TexXY(0)
              y = m15ModelBuffer_Overlay(i).TexXY(1)

              '---Now assign values to referenced variables
              thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, x)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, y)

            END IF
          END IF
        END IF
      END IF
    END IF


END SUB


' -----------------------------------------------------------------------------
' -- LAYERS
' -----------------------------------------------------------------------------

' -- Layer number
SUB Exec_TBGL_m15SetVertexLayer()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG
  LOCAL Model, index, Num1, Num2, nFound AS EXT

  nFound = thinBasic_ParseXNumbers(3, 4, Model, index, Num1, Num2)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  IF nFound = 3 THEN
    m15ModelBuffer_Overlay(index).Layer = Num1

  ELSEIF nFound = 4 THEN
    FOR i = index TO Num1
      m15ModelBuffer_Overlay(i).Layer = Num2
    NEXT

  END IF
END SUB

FUNCTION Exec_TBGL_m15GetVertexLayer() AS EXT

  LOCAL Model, index AS EXT

  thinBasic_Parse2Numbers(Model, index)
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  FUNCTION = m15ModelBuffer_Overlay(index).Layer

END FUNCTION

' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUB internal_RecalcNormals( BYVAL Model AS LONG, BYVAL nType AS LONG )

  REGISTER i AS LONG, j AS LONG

  LOCAL ux AS SINGLE, uy AS SINGLE, uz AS SINGLE
  LOCAL vx AS SINGLE, vy AS SINGLE, vz AS SINGLE
  LOCAL nx AS SINGLE, ny AS SINGLE, nz AS SINGLE, nl AS SINGLE

  LOCAL x AS SINGLE, y AS SINGLE, z AS SINGLE
  LOCAL condition AS LONG

  LOCAL VertexCount AS LONG
  VertexCount = m15ModelData(model).vertexCount
  REDIM m15ModelBuffer_Overlay(1 TO VertexCount) AT STRPTR(m15ModelBuffer(Model))

  ' -- When condition = 1, we need to calculate normal from first 3 vertices of polygon
  ' -- (which can be 3+ vertexed)
  condition = 1
  FOR i = 1 TO VertexCount

    IF condition = 1 THEN

      ux = m15ModelBuffer_Overlay(i+1).vtx(0) - m15ModelBuffer_Overlay(i).vtx(0)
      uy = m15ModelBuffer_Overlay(i+1).vtx(1) - m15ModelBuffer_Overlay(i).vtx(1)
      uz = m15ModelBuffer_Overlay(i+1).vtx(2) - m15ModelBuffer_Overlay(i).vtx(2)

      vx = m15ModelBuffer_Overlay(i+2).vtx(0) - m15ModelBuffer_Overlay(i).vtx(0)
      vy = m15ModelBuffer_Overlay(i+2).vtx(1) - m15ModelBuffer_Overlay(i).vtx(1)
      vz = m15ModelBuffer_Overlay(i+2).vtx(2) - m15ModelBuffer_Overlay(i).vtx(2)

      nx = (uy * vz - uz * vy)
      ny = -(ux * vz - uz * vx)
      nz = (ux * vy - uy * vx)

      nl = SQR(nx*nx + ny*ny + nz*nz)

      nx /= nl
      ny /= nl
      nz /= nl

    END IF

    m15ModelBuffer_Overlay(i).n(0) = nx
    m15ModelBuffer_Overlay(i).n(1) = ny
    m15ModelBuffer_Overlay(i).n(2) = nz

    condition = m15ModelBuffer_Overlay(i).PStop
  NEXT

  ' -- End of creation of precise normals
  IF nType <> 2 THEN EXIT SUB     ' instead of mega IF / END IF this is faster !

  ' -- Here come calculation of interpolation from already precise normals
  DIM CommonVertex(1 TO VertexCount) AS LOCAL tCommonVertex
  LOCAL CommonVertexID AS LONG, CommonVertexGroup AS LONG

  CommonVertexID = 1
  CommonVertex(1).x  = m15ModelBuffer_Overlay(1).vtx(0)
  CommonVertex(1).y  = m15ModelBuffer_Overlay(1).vtx(1)
  CommonVertex(1).z  = m15ModelBuffer_Overlay(1).vtx(2)
  CommonVertex(1).nx = m15ModelBuffer_Overlay(1).n(0)
  CommonVertex(1).ny = m15ModelBuffer_Overlay(1).n(1)
  CommonVertex(1).nz = m15ModelBuffer_Overlay(1).n(2)
  CommonVertex(1).vertexCount = 1
  CommonVertex(1).bufferito(1) = 1
  FOR i = 2 TO VertexCount

    FOR j = CommonVertexID TO 1 STEP -1
      CommonVertexGroup = 0
      IF CommonVertex(j).x = m15ModelBuffer_Overlay(i).vtx(0) AND _
         CommonVertex(j).y = m15ModelBuffer_Overlay(i).vtx(1) AND _
           CommonVertex(j).z = m15ModelBuffer_Overlay(i).vtx(2) THEN
            IF CommonVertex(j).vertexCount = %MAX_BUFFERITO_INDEX THEN EXIT FOR ' [!] In case more than 128 vertices share some position we are busted
            INCR CommonVertex(j).vertexCount
            CommonVertex(j).bufferito(CommonVertex(j).vertexCount) = i
            CommonVertex(j).nx += m15ModelBuffer_Overlay(i).n(0)
            CommonVertex(j).ny += m15ModelBuffer_Overlay(i).n(1)
            CommonVertex(j).nz += m15ModelBuffer_Overlay(i).n(2)
            CommonVertexGroup = j
            EXIT FOR
       END IF
    NEXT

    IF CommonVertexGroup = 0 THEN
      INCR CommonVertexID
      CommonVertex(CommonVertexID).x            = m15ModelBuffer_Overlay(i).vtx(0)
      CommonVertex(CommonVertexID).y            = m15ModelBuffer_Overlay(i).vtx(1)
      CommonVertex(CommonVertexID).z            = m15ModelBuffer_Overlay(i).vtx(2)
      CommonVertex(CommonVertexID).nx           = m15ModelBuffer_Overlay(i).n(0)
      CommonVertex(CommonVertexID).ny           = m15ModelBuffer_Overlay(i).n(1)
      CommonVertex(CommonVertexID).nz           = m15ModelBuffer_Overlay(i).n(2)
      CommonVertex(CommonVertexID).vertexCount        = 1
      CommonVertex(CommonVertexID).bufferito(1) = i
    END IF

  NEXT

  FOR i = 1 TO CommonVertexID
    IF CommonVertex(i).vertexCount > 1 THEN
      CommonVertex(i).nx /= CommonVertex(i).vertexCount
      CommonVertex(i).ny /= CommonVertex(i).vertexCount
      CommonVertex(i).nz /= CommonVertex(i).vertexCount

      FOR j = 1 TO CommonVertex(i).vertexCount
        m15ModelBuffer_Overlay(CommonVertex(i).bufferito(j) ).n(0) = CommonVertex(i).nx
        m15ModelBuffer_Overlay(CommonVertex(i).bufferito(j) ).n(1) = CommonVertex(i).ny
        m15ModelBuffer_Overlay(CommonVertex(i).bufferito(j) ).n(2) = CommonVertex(i).nz
      NEXT
    END IF

  NEXT

END SUB

SUB internal_RecalcNormalsLayer( BYVAL Model AS LONG, BYVAL nType AS LONG, BYVAL LayerName AS STRING )

  IF nType = 0 THEN EXIT SUB

  REGISTER i AS LONG, j AS LONG

  LOCAL ux AS SINGLE, uy AS SINGLE, uz AS SINGLE
  LOCAL vx AS SINGLE, vy AS SINGLE, vz AS SINGLE
  LOCAL nx AS SINGLE, ny AS SINGLE, nz AS SINGLE, nl AS SINGLE

  LOCAL x AS SINGLE, y AS SINGLE, z AS SINGLE

  LOCAL condition AS LONG
  LOCAL LayerExact AS STRING
  LOCAL NumLayers AS LONG
  LOCAL LayerLoop AS LONG

  LayerName = UCASE$(LayerName)

  NumLayers = PARSECOUNT(LayerName, ",")

  LOCAL wantedLayer AS LONG

  LOCAL VertexCount AS LONG
  VertexCount = m15ModelData(model).vertexCount
  REDIM m15ModelBuffer_Overlay(1 TO VertexCount) AT STRPTR(m15ModelBuffer(Model))


  FOR LayerLoop = 1 TO NumLayers

    wantedLayer = -10
    LayerExact = TRIM$(PARSE$(LayerName, ",", LayerLoop))


    FOR i = 1 TO PARSECOUNT( m15ModelLayers(Model), "," )
      IF PARSE$(m15ModelLayers(Model), ",", i) = LayerExact THEN
        wantedLayer = i - 1
        EXIT FOR
      END IF
    NEXT

    IF wantedLayer = -10 THEN
      IF NumLayers = 1 OR NumLayers = LayerLoop THEN
        EXIT SUB
      ELSE
        ITERATE FOR
      END IF
    END IF

    FOR i = 1 TO VertexCount
      IF i > 1 THEN
        condition = m15ModelBuffer_Overlay(i-1).PStop = 1 AND m15ModelBuffer_Overlay(i).layer = wantedLayer
      ELSE
        condition = m15ModelBuffer_Overlay(i).layer = wantedLayer
      END IF

      IF condition THEN

      ux = m15ModelBuffer_Overlay(i+1).vtx(0) - m15ModelBuffer_Overlay(i).vtx(0)
      uy = m15ModelBuffer_Overlay(i+1).vtx(1) - m15ModelBuffer_Overlay(i).vtx(1)
      uz = m15ModelBuffer_Overlay(i+1).vtx(2) - m15ModelBuffer_Overlay(i).vtx(2)

      vx = m15ModelBuffer_Overlay(i+2).vtx(0) - m15ModelBuffer_Overlay(i).vtx(0)
      vy = m15ModelBuffer_Overlay(i+2).vtx(1) - m15ModelBuffer_Overlay(i).vtx(1)
      vz = m15ModelBuffer_Overlay(i+2).vtx(2) - m15ModelBuffer_Overlay(i).vtx(2)

      nx = (uy * vz - uz * vy)
      ny = -(ux * vz - uz * vx)
      nz = (ux * vy - uy * vx)

      nl = SQR(nx*nx + ny*ny + nz*nz)

      nx /= nl
      ny /= nl
      nz /= nl

    END IF

    IF m15ModelBuffer_Overlay(i).layer = wantedLayer THEN
      m15ModelBuffer_Overlay(i).n(0) = nx
      m15ModelBuffer_Overlay(i).n(1) = ny
      m15ModelBuffer_Overlay(i).n(2) = nz
    END IF


  NEXT
  NEXT

  ' --- End of creation of precise normals
  IF nType <> 2 THEN EXIT SUB     ' instead of mega IF / END IF this is faster !

  DIM CommonVertex(1 TO VertexCount) AS LOCAL tCommonVertex
  LOCAL CommonVertexID AS LONG, CommonVertexGroup AS LONG

  CommonVertexID = 1
  CommonVertex(1).x = m15ModelBuffer_Overlay(1).vtx(0)
  CommonVertex(1).y = m15ModelBuffer_Overlay(1).vtx(1)
  CommonVertex(1).z = m15ModelBuffer_Overlay(1).vtx(2)
  CommonVertex(1).nx = m15ModelBuffer_Overlay(1).n(0)
  CommonVertex(1).ny = m15ModelBuffer_Overlay(1).n(1)
  CommonVertex(1).nz = m15ModelBuffer_Overlay(1).n(2)
  CommonVertex(1).vertexCount = 1
  CommonVertex(1).bufferito(1) = 1
  FOR i = 2 TO VertexCount

    FOR j = CommonVertexID TO 1 STEP -1
      CommonVertexGroup = 0
      IF CommonVertex(j).x = m15ModelBuffer_Overlay(i).vtx(0) AND _
         CommonVertex(j).y = m15ModelBuffer_Overlay(i).vtx(1) AND _
           CommonVertex(j).z = m15ModelBuffer_Overlay(i).vtx(2) AND _
           wantedlayer = m15ModelBuffer_Overlay(i).layer THEN
            IF CommonVertex(j).vertexCount = %MAX_BUFFERITO_INDEX THEN EXIT FOR ' [!] In case more than 128 vertices share some position we are busted
            INCR CommonVertex(j).vertexCount
            CommonVertex(j).bufferito(CommonVertex(j).vertexCount) = i
            CommonVertex(j).nx += m15ModelBuffer_Overlay(i).n(0)')/2
            CommonVertex(j).ny += m15ModelBuffer_Overlay(i).n(1)')/2
            CommonVertex(j).nz += m15ModelBuffer_Overlay(i).n(2)')/2
            CommonVertexGroup = j
            EXIT FOR
       END IF
    NEXT

    IF CommonVertexGroup = 0 THEN
      INCR CommonVertexID
      CommonVertex(CommonVertexID).x = m15ModelBuffer_Overlay(i).vtx(0)
      CommonVertex(CommonVertexID).y = m15ModelBuffer_Overlay(i).vtx(1)
      CommonVertex(CommonVertexID).z = m15ModelBuffer_Overlay(i).vtx(2)
      CommonVertex(CommonVertexID).nx = m15ModelBuffer_Overlay(i).n(0)
      CommonVertex(CommonVertexID).ny = m15ModelBuffer_Overlay(i).n(1)
      CommonVertex(CommonVertexID).nz = m15ModelBuffer_Overlay(i).n(2)
      CommonVertex(CommonVertexID).vertexCount = 1
      CommonVertex(CommonVertexID).bufferito(1) = i
    END IF

  NEXT

  LOCAL tempIndex AS LONG

  FOR i = 1 TO CommonVertexID
    IF CommonVertex(i).vertexCount > 1 THEN
      CommonVertex(i).nx /= CommonVertex(i).vertexCount
      CommonVertex(i).ny /= CommonVertex(i).vertexCount
      CommonVertex(i).nz /= CommonVertex(i).vertexCount

      FOR j = 1 TO CommonVertex(i).vertexCount
        tempIndex = CommonVertex(i).bufferito(j)
        m15ModelBuffer_Overlay(tempIndex).n(0) = CommonVertex(i).nx
        m15ModelBuffer_Overlay(tempIndex).n(1) = CommonVertex(i).ny
        m15ModelBuffer_Overlay(tempIndex).n(2) = CommonVertex(i).nz
      NEXT
    END IF

  NEXT

END SUB

' -- Drawing model is done via vertex arrays, it is very, VERY efficient technique, only better one is VBO, which is not so
' -- widely supported
SUB internal_DrawModel( BYVAL ModelID AS LONG, ForceTexture AS LONG, ForceR AS LONG, ForceG AS LONG, ForceB AS LONG )

  REGISTER i AS LONG
  LOCAL VertexCount AS LONG
  local TexturingNeeded as long
  VertexCount = m15ModelData(ModelID).vertexCount
  TexturingNeeded = m15ModelData(ModelID).isTextured or (ForceTexture > 0)

  REDIM m15ModelBuffer_Overlay(1 TO VertexCount) AT STRPTR(m15ModelBuffer(ModelID))

  IF VertexCount = 0 THEN EXIT SUB

  ' -- If model uses texture, we enable it
  if TexturingNeeded then
    IF gUseTexture = 0 THEN glEnable (%GL_TEXTURE_2D)
    glBindTexture %GL_TEXTURE_2D, lpTexture( IIF( ForceTexture > 0, ForceTexture, m15ModelBuffer_Overlay(1).TexN ))
  end if

  ' -- Enabling vertex arrays
  glEnableClientState( %GL_NORMAL_ARRAY )
  glNormalPointer  (%GL_FLOAT, SIZEOF(Tm15ImmediateData), m15ModelBuffer_Overlay(1).n(0)     )

  ' -- We will use color array just and only when there is no forcing
  IF ForceR < 0 THEN
    glEnableClientState( %GL_COLOR_ARRAY )
    glColorPointer   ( 3, %GL_UNSIGNED_BYTE, SIZEOF(Tm15ImmediateData), m15ModelBuffer_Overlay(1).clr(0)   )
  ELSE
    glColor3ub  ForceR, ForceG, ForceB
  END IF

  glEnableClientState( %GL_TEXTURE_COORD_ARRAY )
  glTexCoordPointer( 2, %GL_FLOAT, SIZEOF(Tm15ImmediateData), m15ModelBuffer_Overlay(1).texXY(0) )

  glEnableClientState( %GL_VERTEX_ARRAY )
  glVertexPointer  ( 3, %GL_FLOAT, SIZEOF(Tm15ImmediateData), m15ModelBuffer_Overlay(1).vtx(0)   )

  LOCAL j, baseI, lastI, numElements AS LONG
  i = 1

  ' -- Initialisation
    IF m15ModelBuffer_Overlay(i+2).pStop = 1 THEN

        baseI = i-1
        numElements = 1

        i += 3
        WHILE i < VertexCount-1 AND m15ModelBuffer_Overlay(i+2).pStop = 1
          IF ForceTexture < 0 AND m15ModelBuffer_Overlay(i).TexN <> m15ModelBuffer_Overlay(i-1).TexN THEN EXIT LOOP
          INCR numElements
          i += 3

        WEND

        glDrawArrays(%GL_TRIANGLES,baseI,numElements*3)

    ELSEIF m15ModelBuffer_Overlay(i+3).pStop = 1 THEN

        baseI = i-1
        numElements = 1

        i += 4
        WHILE i < VertexCount-2 AND m15ModelBuffer_Overlay(i+3).pStop = 1
          IF ForceTexture < 0 AND m15ModelBuffer_Overlay(i).TexN <> m15ModelBuffer_Overlay(i-1).TexN THEN EXIT LOOP
          INCR numElements
          i += 4

        WEND

        glDrawArrays(%GL_QUADS,baseI,numElements*4)
    ELSE

      FOR j = i+1 TO VertexCount

        IF m15ModelBuffer_Overlay(j).pStop THEN lastI = j : EXIT FOR
        IF j = VertexCount THEN GOTO PerformCleanup ' -- Should fix problem when model has incorrectly specified vertex count
        ' -- NOT NEEDED
        'IF ForceTexture < 0 AND m15ModelBuffer_Overlay(j).TexN <> m15ModelBuffer_Overlay(j-1).TexN THEN lastI = j-1 : EXIT FOR
      NEXT


      glDrawArrays(%GL_TRIANGLE_FAN,i-1,lastI-i+1)
      i = lastI+1
    END IF

    IF i < VertexCount AND i > 1 THEN
      IF ForceTexture < 0 AND m15ModelBuffer_Overlay(i).TexN <> m15ModelBuffer_Overlay(i-1).TexN THEN glBindTexture %GL_TEXTURE_2D, lpTexture(m15ModelBuffer_Overlay( i).TexN)
    END IF

  ' -- Full render
  ' -- Why so much WHILEs? Because more polygons we spit out at once, more performance gain
  ' -- If model is just triangles, or just quads, we get the best performance

  WHILE i < VertexCount

    IF ForceTexture < 0 AND m15ModelBuffer_Overlay(i).TexN <> m15ModelBuffer_Overlay(i-1).TexN THEN glBindTexture %GL_TEXTURE_2D, lpTexture(m15ModelBuffer_Overlay( i).TexN)

    ' -- Triangles
    IF m15ModelBuffer_Overlay(i+2).pStop = 1 THEN

        baseI = i-1
        numElements = 1

        i += 3
        WHILE i < VertexCount-1 AND m15ModelBuffer_Overlay(i+2).pStop = 1
          IF ForceTexture < 0 AND m15ModelBuffer_Overlay(i).TexN <> m15ModelBuffer_Overlay(i-1).TexN THEN EXIT LOOP
          INCR numElements
          i += 3

        WEND

        glDrawArrays(%GL_TRIANGLES,baseI,numElements*3)

    ' -- Quads
    ELSEIF m15ModelBuffer_Overlay(i+3).pStop = 1 THEN

        baseI = i-1
        numElements = 1

        i += 4
        WHILE i < VertexCount-2 AND m15ModelBuffer_Overlay(i+3).pStop = 1
          IF ForceTexture < 0 AND m15ModelBuffer_Overlay(i).TexN <> m15ModelBuffer_Overlay(i-1).TexN THEN EXIT LOOP
          INCR numElements
          i += 4

        WEND

        glDrawArrays(%GL_QUADS,baseI,numElements*4)

    ' -- Worst case, polygon with more than 4 vertices cannot be chained
    ELSE

      FOR j = i+1 TO VertexCount
        IF m15ModelBuffer_Overlay(j).pStop THEN lastI = j : EXIT FOR
        IF j = VertexCount THEN GOTO PerformCleanup ' -- Should fix problem when model has incorrectly specified vertex count
        ' -- NOT NEEDED
        'IF ForceTexture < 0 AND m15ModelBuffer_Overlay(j).TexN <> m15ModelBuffer_Overlay(j-1).TexN THEN lastI = j-1 : EXIT FOR
      NEXT

      glDrawArrays(%GL_TRIANGLE_FAN,i-1,lastI-i+1)
      i = lastI+1
    END IF

  WEND

  PerformCleanup:
  ' -- Returning vertex array rendering to off
  glDisableClientState( %GL_VERTEX_ARRAY )
  glDisableClientState( %GL_TEXTURE_COORD_ARRAY )
  IF ForceR < 0 THEN glDisableClientState( %GL_COLOR_ARRAY )
  glDisableClientState( %GL_NORMAL_ARRAY )

  if TexturingNeeded then
    IF gUseTexture = 0 THEN glDisable %GL_TEXTURE_2D
  end if

END SUB

'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles texturing in general
' =============================================================================

' -----------------------------------------------------------------------------

%TEXTURE_LOAD_SUCCESS = 0
%TBGL_TEX_NEAREST    = 0
%TBGL_TEX_LINEAR     = 1
%TBGL_TEX_MIPMAP     = 2
%TBGL_TEX_ANISO      = 3
%TBGL_TEX_CRISPANISO = 4

%TBGL_DATA_RGB    = 0
%TBGL_DATA_RGBA   = 1
%TBGL_FILE_BMP    = 2
%TBGL_FILE_TGA    = 3
%TBGL_DATA_BGR    = 4
%TBGL_DATA_BGRA   = 5

%TBGL_REPEAT      = 1
%TBGL_CLAMPTOEDGE = 2

%LIMIT_MAX_TEXTUREINDEX = 1024

' -----------------------------------------------------------------------------

TYPE c_BGRA
  b AS BYTE
  g AS BYTE
  r AS BYTE
  a AS BYTE
END TYPE

TYPE AUX_RGBImageRec
  sizeX AS LONG
  sizeY AS LONG
  nData AS LONG PTR
END TYPE

TYPE TBGL_tTexturingInfo
  maxWidth AS LONG
  maxHeight AS LONG
  NPOTSupport AS LONG
END TYPE

' -----------------------------------------------------------------------------

GLOBAL gUseTexture AS LONG
GLOBAL lpTexture( ) AS AUX_RGBImageRec PTR
GLOBAL lpTextureFont( ) AS AUX_RGBImageRec PTR

GLOBAL TextureList( ) AS STRING
GLOBAL LastBindedTexture AS LONG

GLOBAL texture_AnisotropyAllowed AS BYTE
GLOBAL texture_AnisotropyMax AS SINGLE
GLOBAL texture_DefaultModelFilter AS LONG
GLOBAL texture_DefaultModelFilterMultiplier AS SINGLE

' -----------------------------------------------------------------------------

' >RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES
SUB resource_Texturing_Alloc()
  glGenTextures %LIMIT_MAX_TEXTUREINDEX, lpTexture(0)
END SUB

SUB resource_Texturing_Release()
  glDeleteTextures %LIMIT_MAX_TEXTUREINDEX, lpTexture(0)
  RESET lpTexture()
  RESET TextureList()
END SUB
' <RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES

' -----------------------------------------------------------------------------
' -- CREATION
' -----------------------------------------------------------------------------

function internal_LoadTextureFileFromDisk(byref fileName as string, byref descriptionCandidate as string) as string

  dim srcPath as string
  srcPath = thinBasic_GetRunTimeInfo( "SCRIPT_PATH" )

  dim isFullPath as long
  dim isNetworkPath as long
  isFullPath = INSTR(fileName, ":") > 0
  isNetworkPath = (left$(fileName, 2) = "\\")

  fileName = remove$(fileName, ".\")
  if isFullPath or isNetworkPath then
    ' leave fileName unaltered
  else
    fileName = srcPath + fileName
  end if

  IF file_exists(fileName) = 0 THEN
    tbglError $ERR_TEXTURENOTFOUND + fileName
    exit function
  end if

  descriptionCandidate = uCase$(fileName)

  ' Loading file
  dim n as long
  n = freeFile
  dim contents as string
  open fileName for binary as #n
    get$ #n, lof( n ), contents
  close #n

  function = contents
end function

' -- [!] Loads BMP or TGA, will be optimized via GDIPlus for wider format support
function internal_LoadTexture(TexName as string, TexNum as long, TexFilter as long, eAnisoLevel as ext, lRGB as long) as long
  if TexNum < 0 OR TexNum > %LIMIT_MAX_TEXTUREINDEX then
    TBGLError "Statement:" + $TAB + "tbgl_LoadTexture" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_BADTEXTUREINDEX + $CRLF + $CRLF + "Passed:" + $TAB + format$(TexNum)
    function = %TBGL_FUNCTION_ERROR
    exit function
  end if

  select case ucase$(pathName$(extn, texName))
    case ".BMP"
      TextureLoadBMP( 0, TexName, TexNum, TexFilter, eAnisoLevel, lRGB)

    case ".TGA"
      TextureLoadTGA( 0, TexName, TexNum, TexFilter, eAnisoLevel )
  end select

end function

FUNCTION Exec_TBGL_LoadTexture( ) AS LONG
  LOCAL eTexNum, eTexFilter, eAnisoLevel, eRGB AS EXT
  LOCAL TexName AS STRING, TexNum, TexFilter, lRGB AS LONG
  LOCAL numParsed AS LONG
  numParsed = thinBASIC_Parse1StringXNumbers( 2, 4, TexName, eTexNum, eTexFilter, eAnisoLevel, eRGB )
  lRGB = eRGB
  TexNum = eTexNum
  TexFilter = eTexFilter

  IF numParsed <> 4 THEN lRGB = - 1
  FUNCTION = internal_LoadTexture(TexName, TexNum, TexFilter, eAnisoLevel, lRGB )

END FUNCTION

' -- [!] Loads BMP or TGA, will be optimized via GDIPlus for wider format support
FUNCTION internal_LoadTexturesFromTiles(TexName AS STRING, TexNum AS LONG, numCols AS LONG, numRows AS LONG, TexFilter AS LONG, eAnisoLevel AS EXT, lRGB AS LONG ) AS LONG
  IF TexNum < 0 OR TexNum > %LIMIT_MAX_TEXTUREINDEX THEN
    TBGLError "Statement:" + $TAB + "tbgl_LoadTexture" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_BADTEXTUREINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( TexNum )
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  IF UCASE$( TRIM$( PARSE$( TexName, ".", - 1 ))) = "BMP" THEN
    TextureLoadBMP_Tiled( 0, TexName, TexNum, numCols, numRows, TexFilter, eAnisoLevel, lRGB)
  ELSEIF UCASE$( TRIM$( PARSE$( TexName, ".", - 1 ))) = "TGA" THEN
    TextureLoadTGA_Tiled( 0, TexName, TexNum, numCols, numRows, TexFilter, eAnisoLevel )
  END IF

END FUNCTION

FUNCTION Exec_TBGL_LoadTexturesFromTiles( ) AS LONG
  LOCAL eTexNum, eCols, eRows, eTexFilter, eAnisoLevel, eRGB AS EXT
  LOCAL TexName AS STRING, TexNum, TexFilter, lRGB, lCols, lRows AS LONG
  LOCAL numParsed AS LONG

  eRGB = - 1
  numParsed = thinBASIC_Parse1StringXNumbers( 4, 7, TexName, eTexNum, eCols, eRows, eTexFilter, eAnisoLevel, eRGB )
  lRGB = eRGB
  TexNum = eTexNum
  TexFilter = eTexFilter
  lCols = eCols
  lRows = eRows


  FUNCTION = internal_LoadTexturesFromTiles(TexName, TexNum, lCols, lRows, TexFilter, eAnisoLevel, lRGB )

END FUNCTION

' -- Makes texture from memory buffer
FUNCTION Exec_TBGL_MakeTexture( ) AS LONG

  REGISTER i AS LONG

  LOCAL eTexType, eX, eY, eTexNum, eTexFilter, eAnisoLevel, eRGB AS EXT
  LOCAL TexBuffer AS STRING, TexNum, TexFilter, lRGB AS LONG
  LOCAL numParsed AS LONG
  LOCAL bytePointer AS BYTE PTR
  LOCAL lenTexBuffer AS LONG
  LOCAL pTex AS BYTE PTR
  LOCAL RGBStringIn AS STRING * 4
  LOCAL rgbPos AS LONG
  LOCAL nTexSize AS LONG
  LOCAL rPointer AS BYTE PTR
  LOCAL bPointer AS BYTE PTR

  numParsed = thinBASIC_Parse1StringXNumbers( 5, 7, TexBuffer, eTexType, eX, eY, eTexNum, eTexFilter, eAnisoLevel, eRGB )

  IF numParsed < 7 THEN
    lRGB = - 1
  ELSE
    lRGB = eRGB
  END IF

  TexNum = eTexNum
  TexFilter = eTexFilter

  IF TexNum < 0 OR TexNum > %LIMIT_MAX_TEXTUREINDEX THEN
    TBGLError "Statement:" + $TAB + "tbgl_MakeTexture" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_BADTEXTUREINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( TexNum )
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  LOCAL x, y AS LONG, AnisoX AS SINGLE
  x = ex
  y = ey
  AnisoX = eAnisoLevel

  SELECT CASE AS LONG eTexType
    ' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
    CASE %TBGL_FILE_BMP
      TextureLoadBMP( 0, TexBuffer, TexNum, TexFilter, eAnisoLevel, IIF( numParsed = 4, lRGB, - 1 ))

    ' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
    CASE %TBGL_FILE_TGA
      TextureLoadTGA( 0, TexBuffer, TexNum, TexFilter, eAnisoLevel )

    ' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
    CASE %TBGL_DATA_RGB

      bytePointer = STRPTR( TexBuffer )
      lenTexBuffer = LEN( TexBuffer )
      IF lenTexBuffer MOD 3 <> 0 THEN tbglerror "Statement:" + $TAB + "tbgl_MakeTexture" + $CRLF + "Problem:" + $TAB + $TAB + _
      "Invalid length of RGB buffer" + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( LEN( TexBuffer ), "#," ) + "bytes ( not divisible by 3 )" : EXIT FUNCTION
      IF lenTexBuffer / 3 <> x * y THEN _
        tbglerror "Statement:" + $TAB + "tbgl_MakeTexture" + $CRLF + "Problem:" + $TAB + $TAB + "Length of buffer does not match passed dimensions" + _
        $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( LEN( TexBuffer ), "#," ) + "bytes, " + FORMAT$( x ) + "x" +FORMAT$( y ) : EXIT FUNCTION

      DIM RGBBuffer( 1 TO lenTexBuffer / 3 ) AS LOCAL STRING * 3 AT STRPTR( TexBuffer )
      TexBuffer = JOIN$( RGBBuffer( ), CHR$( 255 )) + CHR$( 255 )
      lenTexBuffer = LEN( TexBuffer )

      ' -- Adding transparency
      IF lRGB >= 0 THEN
        RGBStringIn = MKL$( lRGB )
        ASC( RGBStringIn, 4 ) = 255
        rgbPos = 1

        lenTexBuffer = lenTexBuffer - 4    ' !!! accelerate things up :)
        DO
          rgbPos = INSTR( rgbPos, TexBuffer, RGBStringIn )

          IF rgbPos = 0 THEN
            EXIT LOOP
          ELSEIF rgbPos MOD 4 = 1 THEN
            ASC( TexBuffer, rgbPos + 3 ) = 0
          END IF
          INCR rgbpos
          IF rgbPos > lenTexBuffer THEN EXIT LOOP

        LOOP

      END IF
      bytePointer = STRPTR( TexBuffer )
      createGLtexture( 0, bytePointer, %GL_RGBA8, X, Y, TexNum, TexFilter, AnisoX )

    ' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
    CASE %TBGL_DATA_RGBA
      IF LEN( TexBuffer ) MOD 4 <> 0 THEN tbglerror "Statement:" + $TAB + "tbgl_MakeTexture" + $CRLF + "Problem:" + $TAB + $TAB + "Invalid length of RGBA buffer" + $CRLF + $CRLF + "Passed:" + $TAB + _
                                                    FORMAT$( LEN( TexBuffer )) + " ( not divisible by 4 )": EXIT FUNCTION
      bytePointer = STRPTR( TexBuffer )
      createGLtexture( 0, bytePointer, %GL_RGBA8, X, Y, TexNum, TexFilter, AnisoX )

    ' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
    CASE %TBGL_DATA_BGR

      bytePointer = STRPTR( TexBuffer )
      lenTexBuffer = LEN( TexBuffer )
      nTexSize = x * y * 4

      IF lenTexBuffer MOD 3 <> 0 THEN tbglerror "Statement:" + $TAB + "tbgl_MakeTexture" + $CRLF + "Problem:" + $TAB + $TAB + _
      "Invalid length of BGR buffer" + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( LEN( TexBuffer ), "#," ) + "bytes ( not divisible by 3 )" : EXIT FUNCTION

      IF lenTexBuffer / 3 <> x * y THEN _
        tbglerror "Statement:" + $TAB + "tbgl_MakeTexture" + $CRLF + "Problem:" + $TAB + $TAB + "Length of buffer does not match passed dimensions" + _
        $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( LEN( TexBuffer ), "#," ) + "bytes, " + FORMAT$( x ) + "x" +FORMAT$( y ) : EXIT FUNCTION

      DIM RGBBuffer( 1 TO lenTexBuffer / 3 ) AS LOCAL STRING * 3 AT STRPTR( TexBuffer )
      TexBuffer = JOIN$( RGBBuffer( ), CHR$( 255 )) + CHR$( 255 )
      lenTexBuffer = LEN( TexBuffer )

      ' -- BGR -> RGB
      bytePointer = STRPTR( TexBuffer )
      rPointer = bytePointer+2
      bPointer = bytePointer
      FOR i = 1 TO x*y

        SWAP @bPointer, @rPointer
        rPointer += 4
        bPointer += 4

      NEXT

      ' -- Adding transparency
      IF lRGB >= 0 THEN

        RGBStringIn = MKL$( lRGB )
        ASC( RGBStringIn, 4 ) = 255
        rgbPos = 1

        lenTexBuffer = lenTexBuffer - 4    ' !!! accelerate things up :)
        DO
          rgbPos = INSTR( rgbPos, TexBuffer, RGBStringIn )

          IF rgbPos = 0 THEN
            EXIT LOOP
          ELSEIF rgbPos MOD 4 = 1 THEN
            ASC( TexBuffer, rgbPos + 3 ) = 0
          END IF
          INCR rgbpos
          IF rgbPos > lenTexBuffer THEN EXIT LOOP

        LOOP

      END IF
      bytePointer = STRPTR( TexBuffer )
      createGLtexture( 0, bytePointer, %GL_RGBA8, X, Y, TexNum, TexFilter, AnisoX )

    ' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
    CASE %TBGL_DATA_BGRA
      IF LEN( TexBuffer ) MOD 4 <> 0 THEN tbglerror "Statement:" + $TAB + "tbgl_MakeTexture" + $CRLF + "Problem:" + $TAB + $TAB + "Invalid length of BGRA buffer" + $CRLF + $CRLF + "Passed:" + $TAB + _
                                                    FORMAT$( LEN( TexBuffer )) + " ( not divisible by 4 )": EXIT FUNCTION
      ' -- BGR -> RGB
      bytePointer = STRPTR( TexBuffer )
      rPointer = bytePointer+2
      bPointer = bytePointer
      FOR i = 1 TO x*y

        SWAP @bPointer, @rPointer
        rPointer += 4
        bPointer += 4

      NEXT
      createGLtexture( 0, bytePointer, %GL_RGBA8, X, Y, TexNum, TexFilter, AnisoX )


  END SELECT

END FUNCTION

' -----------------------------------------------------------------------------
' -- FLAGS'n'STATES
' -----------------------------------------------------------------------------

' -- Enables texture mapping
SUB Exec_TBGL_UseTexturing()

  ' MACRO placed in thinbasic_tbgl
  MACRO_Use1Param(%GL_TEXTURE_2D)

END SUB

' -- Is texturing enabled?
FUNCTION Exec_TBGL_GetUseTexturing( ) AS EXT
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = glIsEnabled( %GL_TEXTURE_2D )
END FUNCTION

' -- Sets active texure
FUNCTION Exec_TBGL_BindTexture( ) AS LONG
  LOCAL TexNum AS EXT

  texNum = thinBasic_Parse1Number

  IF TexNum < 0 OR TexNum > %LIMIT_MAX_TEXTUREINDEX THEN
    TBGLError "Statement:" + $TAB + "tbgl_BindTexture" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_BADTEXTUREINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( TexNum )
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  LastBindedTexture = TexNum
  glBindTexture %GL_TEXTURE_2D, lpTexture( TexNum )

END FUNCTION

' -----------------------------------------------------------------------------

GLOBAL BindTextureStack() AS LONG
GLOBAL BindTextureStackDepth AS LONG


' -- Enables texture in given section, and makes sure it is returned back on the end
SUB Exec_TBGL_PushTexture( )

  LOCAL parameter AS EXT

  INCR BindTextureStackDepth
  IF BindTextureStackDepth > UBOUND(BindTextureStack) THEN REDIM PRESERVE BindTextureStack(1 TO BindTextureStackDepth+4)

  thinBasic_ParseNumber parameter
  ' -- Backup the texture
  glGetIntegerV(%GL_TEXTURE_BINDING_2D, BindTextureStack(BindTextureStackDepth))

  ' -- Set the new one
  IF parameter < 0 OR parameter > %LIMIT_MAX_TEXTUREINDEX THEN
    TBGLError "Statement:" + $TAB + "tbgl_BindTexture" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_BADTEXTUREINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( parameter )
    EXIT SUB
  END IF

  LastBindedTexture = parameter
  glBindTexture(%GL_TEXTURE_2D, lpTexture( parameter ))

END SUB

' -- Ends texture region
SUB Exec_TBGL_PopTexture( )

  IF BindTextureStackDepth = 0 THEN
    IF gOpenGLStarted THEN
      TBGLError "Exec_TBGL_PopTexture caused stack underflow, check whether number of pairs of TBGL_PushTexture / TBGL_PopTexture match"
    END IF
    EXIT SUB
  END IF

  REGISTER i AS LONG

  FOR i = LBOUND(lpTexture) TO UBOUND(lpTexture)
    IF lpTexture(i) = BindTextureStack(BindTextureStackDepth) THEN
      LastBindedTexture = i
      EXIT FOR
    END IF
  NEXT
  glBindTexture(%GL_TEXTURE_2D, BindTextureStack(BindTextureStackDepth) )

  DECR BindTextureStackDepth

END SUB


' -- 2D texture coordinate for use with immediate mode
SUB Exec_TBGL_TexCoord2D( )
  LOCAL u, v AS EXT

  thinBasic_Parse2Numbers( u, v )

  glTexCoord2f u, v

END SUB

' -----------------------------------------------------------------------------
' -- Rendering to texture
' -----------------------------------------------------------------------------

' -- [!] Compatible rendering to texture, would benefit from using FBO on better hardware
SUB Exec_TBGL_RenderToTexture( )

  LOCAL destTexture AS EXT, x, y, twidth, theight AS EXT

  thinBasic_Parse5Numbers( destTexture, x, y, twidth, theight )
  glFinish
  glBindTexture %GL_TEXTURE_2D, lpTexture( destTexture )
  glGetIntegerV( %GL_VIEWPORT, Viewport( 0 ))
  y = Viewport( 3 ) - y - tHeight
  glCopyTexImage2D( %GL_TEXTURE_2D, 0, %GL_RGBA8, x, y, tWidth, tHeight, 0 )
  IF LastBindedTexture >= 0 THEN glBindTexture %GL_TEXTURE_2D, lpTexture( LastBindedTexture )

END SUB

' -- Very useful for render to texture on OLD hardware, returns best match of non-power of two rectangle
' -- to power of two rectangle
SUB Exec_TBGL_EvaluatePOTMatch( )

  REGISTER i AS LONG

  LOCAL resX, resY, fitX, fitY AS EXT
  LOCAL tempX, tempY AS LONG


  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL eResult1 AS EXT
  LOCAL eResult2 AS EXT

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber resX
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber resY

      IF thinBasic_CheckComma_Mandatory THEN
        thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )

        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )
          IF thinBasic_CheckCloseParens_Mandatory THEN
            IF thinBasic_ErrorFree THEN
              i = 1
              tempX = 1
              WHILE tempX <= resX
                fitX = tempX
                tempX = 2^ i
                INCR i
              WEND

              i = 1
              tempY = 1
              WHILE tempY <= resY
                fitY = tempY
                tempY = 2^ i
                INCR i
              WEND

              eResult1 = fitX
              eResult2 = fitY
              thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, eResult1 )
              thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, eResult2 )

            END IF
          END IF
        END IF
      END IF
    END IF
  END IF

END SUB

' -----------------------------------------------------------------------------
' -- INFORMATIONAL
' -----------------------------------------------------------------------------

' -- Returns texture list
FUNCTION Exec_TBGL_GetTextureList( ) AS STRING
  REGISTER i AS LONG
  LOCAL Temp AS STRING

  Temp = JOIN$(TextureList(), ",")
  Temp = RTRIM$(Temp, ",")

  FUNCTION = Temp
END FUNCTION

' -- Returns name of texture in given slot
FUNCTION Exec_TBGL_GetTextureName( ) AS STRING
  LOCAL i AS EXT

  i = thinBASIC_Parse1Number

  IF i < %LIMIT_MAX_TEXTUREINDEX AND i > 0 THEN
    FUNCTION = TextureList( i )
  ELSE
    FUNCTION = "!"
  END IF

END FUNCTION

' -- Gets texture width x height
SUB Exec_TBGL_GetTextureResolution( )

  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL eResult1 AS EXT
  LOCAL eResult2 AS EXT


  LOCAL nTex AS EXT, nWidth, nHeight AS LONG

  LOCAL ParensPresent AS LONG
  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE

  thinBasic_ParseNumber nTex
  IF thinBasic_CheckComma_Mandatory THEN
    thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )

    IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
      thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )

      IF thinBasic_ErrorFree THEN
        glBindTexture( %GL_TEXTURE_2D, lpTexture( nTex ))

        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, nWidth )
        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, nHeight )
        eResult1 = nWidth
        eResult2 = nHeight
        thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, eResult1 )
        thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, eResult2 )

      END IF

    END IF
  END IF

  IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory
  IF LastBindedTexture >= 0 THEN glBindTexture %GL_TEXTURE_2D, lpTexture( LastBindedTexture )

END SUB

FUNCTION Exec_TBGL_GetTextureHandle( ) AS EXT
  LOCAL TexNum AS EXT

  TexNum = thinBASIC_Parse1Number

  IF TexNum < %LIMIT_MAX_TEXTUREINDEX AND TexNum > 0 THEN
    FUNCTION = lpTexture( TexNum )
  ELSE
    FUNCTION = 0
  END IF

END FUNCTION

FUNCTION Exec_TBGL_GetFreeTextureIndex( ) AS EXT
  REGISTER i AS LONG, nWidth AS LONG

  IF thinBasic_CheckOpenParens_Mandatory( ) THEN
    IF thinBasic_CheckCloseParens_Mandatory( ) THEN
      FOR i = 1 TO %LIMIT_MAX_TEXTUREINDEX-1
        IF glIsTexture( lpTexture(i) ) = %GL_FALSE THEN
          FUNCTION = i
          EXIT FUNCTION
        END IF
      NEXT
      FUNCTION = 0
    END IF
  END IF
  FUNCTION = 0

END FUNCTION

' -- Gets texture width x height
FUNCTION Exec_TBGL_GetTextureData( ) AS STRING

  LOCAL nTex, nFormat AS EXT
  LOCAL nFormatGL, nWidth, nHeight, nBlockSize AS LONG

  thinBASIC_Parse2Numbers(nTex, nFormat)

  IF thinBasic_ErrorFree THEN

    ' -- Parameter validation
    IF nTex > %LIMIT_MAX_TEXTUREINDEX AND nTex < 0 THEN
      TBGLError "TBGL_GetTextureData: " + $ERR_BADTEXTUREINDEX + "( Passed value = " + FORMAT$( nTex ) + ")"
      FUNCTION = ""
      EXIT FUNCTION
    END IF

    SELECT CASE nFormat
      CASE %TBGL_DATA_RGB
        nFormatGL = %GL_RGB
        nBlockSize = 3

      CASE %TBGL_DATA_RGBA
        nFormatGL = %GL_RGBA
        nBlockSize = 4

      CASE %TBGL_DATA_BGR
        nFormatGL = %GL_BGR
        nBlockSize = 3

      CASE %TBGL_DATA_BGRA
        nFormatGL = %GL_BGRA
        nBlockSize = 4

      CASE ELSE
        TBGLError "TBGL_GetTextureData: Invalid data format. The accepted values are %TBGL_RGB, %TBGL_RGBA, %TBGL_BGR, %TBGL_BGRA ( Passed value = " + FORMAT$( nFormat ) + ")"
        FUNCTION = ""
        EXIT FUNCTION

    END SELECT

    ' -- Bind the texture
    glBindTexture( %GL_TEXTURE_2D, lpTexture( nTex ))

    ' -- Get the resolution
    glGetTexLevelParameterIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, nWidth )
    glGetTexLevelParameterIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, nHeight )

    ' -- Get the image data
    LOCAL sBuffer AS STRING
    sBuffer = STRING$(nWidth * nHeight * nBlockSize, $SPC)
    glGetTexImage( %GL_TEXTURE_2D, 0, nFormatGL, %GL_UNSIGNED_BYTE, BYVAL STRPTR(sBuffer))

    IF LastBindedTexture >= 0 THEN glBindTexture %GL_TEXTURE_2D, lpTexture( LastBindedTexture )
    FUNCTION = sBuffer
  ELSE
    FUNCTION = ""

  END IF

END FUNCTION

' -- Query on texturing abilities of given hardware
SUB Exec_TBGL_TexturingQuery( )

  LOCAL resolution AS LONG
  LOCAL pVar, pData AS LONG

  IF thinBasic_CheckOpenParens_Mandatory( ) THEN

    thinBasic_VariableParsePtr( pVar )
    pData = thinBasic_DirectPtrToDataPtr( pVar )

    thinBasic_CheckCloseParens_Mandatory( )

    IF thinBasic_ErrorFree THEN

      glGetIntegerV( %GL_MAX_TEXTURE_SIZE, resolution )

      REDIM MyUDT( 1& TO 1& ) AS TBGL_tTexturingInfo AT pData
      MyUDT( 1& ).maxWidth = resolution
      MyUDT( 1& ).maxHeight = resolution
      MyUDT( 1& ).NPOTSupport = internal_IsExtensionSupported( "GL_ARB_TEXTURE_NON_POWER_OF_TWO" )
    END IF


  END IF

END SUB

' -- Deletes texture
SUB Exec_TBGL_DeleteTexture()
  LOCAL texID AS LONG

  texID = thinBasic_Parse1Number

  ' -- Looks odd, but it is safe to do it this way
  IF glIsTexture( lpTexture( texID )  ) THEN
    glDeleteTextures 1, lpTexture( texID )
    glGenTextures 1, lpTexture( texID )
  END IF

END SUB

' -- Sets repeating mode
SUB Exec_TBGL_SetTextureRepeatMode()
  DIM nParams AS LONG
  DIM textureIndex AS EXT, param1 AS EXT, param2 AS EXT

  nParams = thinBasic_ParseXNumbers( 2, 3, textureIndex, param1, param2 )

  IF nParams = 2 THEN
    param2 = param1
  END IF

  DIM textureHandleCurrent AS LONG
  glGetIntegerV(%GL_TEXTURE_BINDING_2D, textureHandleCurrent)

  ' -- Bind the texture temporarily
  IF textureHandleCurrent <> lpTexture(textureIndex) THEN
    glBindTexture(%GL_TEXTURE_2D, lpTexture(textureIndex))
  END IF

  ' -- U coordinate
  SELECT CASE param1
    CASE %TBGL_REPEAT
      glTexParameterI(%GL_TEXTURE_2D, %GL_TEXTURE_WRAP_S, %GL_REPEAT)

    CASE %TBGL_CLAMPTOEDGE
      glTexParameterI(%GL_TEXTURE_2D, %GL_TEXTURE_WRAP_S, %GL_CLAMP_TO_EDGE)

  END SELECT

  ' -- V coordinate
  SELECT CASE param2
    CASE %TBGL_REPEAT
      glTexParameterI(%GL_TEXTURE_2D, %GL_TEXTURE_WRAP_T, %GL_REPEAT)

    CASE %TBGL_CLAMPTOEDGE
      glTexParameterI(%GL_TEXTURE_2D, %GL_TEXTURE_WRAP_T, %GL_CLAMP_TO_EDGE)

  END SELECT

  ' -- Put back the original texture
  IF textureHandleCurrent <> lpTexture(textureIndex) THEN
    glBindTexture(%GL_TEXTURE_2D, textureHandleCurrent)
  END IF
END SUB

' ===============================================================================================

FUNCTION TextureLoadBMP( asFont AS BYTE, TexName AS STRING, TexNum AS LONG, TexFilter AS LONG, OPTIONAL BYVAL AnisoX AS SINGLE, BYVAL clrRGB AS LONG ) AS EXT

  REGISTER i AS LONG, j AS LONG

  LOCAL n, nWidth, nHeight, nDepth, nTexSize, nColors, diff AS LONG
  LOCAL colors( ) AS c_BGRA
  LOCAL nTexData, sTex, descriptionCandidate, WholeBMP AS STRING
  LOCAL pTexData, pTex AS BYTE PTR

  ' -- Determine whether it is physical file or memory image
  IF ucase$(pathName$(extn, texName)) = ".BMP" THEN
    wholeBMP = internal_loadTextureFileFromDisk(texName, descriptionCandidate)
    if len(wholeBMP) = 0 then
      function = %TBGL_FUNCTION_ERROR
      exit function
    end if
  ELSE
    descriptionCandidate = "_MEMORY_FILE_BMP_" + FORMAT$( STRPTR( TexName ))
    wholeBMP = TexName
  END IF

  DIM bmfh( 0 TO 0 ) AS BitmapFileHeader AT STRPTR( WholeBMP )
  IF bmfh( 0 ).bfType <> 19778 THEN
    TBGLError $ERR_TEXTUREBADFORMAT + TexName
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  DIM bmih( 0 TO 0 ) AS BitmapInfoHeader AT STRPTR( WholeBMP ) + SIZEOF( BitmapFileHeader )


  nWidth = bmih( 0 ).biWidth
  nHeight = bmih( 0 ).biHeight
  nDepth = bmih( 0 ).biBitCount

  ' Calculate the size of our texture
  nTexSize = nWidth * nHeight * ( nDepth \ 8 )
  ' Calculate the number of available colors
  nColors = 1
  SHIFT SIGNED LEFT nColors, nDepth
  IF nDepth < 8 THEN
    TBGLError $ERR_TEXTUREBADFORMAT + TexName
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  ' Load the palette for 8 bits per pixel
  IF nDepth = 8 THEN
    REDIM colors( nColors ) AT STRPTR( WholeBMP ) + SIZEOF( BitmapFileHeader ) + SIZEOF( BitmapInfoHeader )
    nTexData = MID$( WholeBMP, 1 + SIZEOF( BitmapFileHeader ) + SIZEOF( BitmapInfoHeader ) + nColors * 4, LEN( WholeBMP ))

  ELSE
    nTexData = MID$( WholeBMP, 1 + SIZEOF( BitmapFileHeader ) + SIZEOF( BitmapInfoHeader ), LEN( WholeBMP ))
  END IF

  ' --

  ' Calculate the size of this texture once converted to 32-bit RGBA
  diff = nWidth * nHeight * 4

  ' Change format from BGRA to RGBA
  IF nDepth = 8 THEN
    ' Point to the data we loaded in
    pTexData = STRPTR( nTexData )

    ' Allocate memory for our texture
    sTex = SPACE$( diff )
    pTex = STRPTR( sTex )
    IF nHeight > 0 THEN
      j = 0
      ' Count backwards so you start at the front on the texture

      FOR i = 0 TO diff - 1 STEP 4
        ' Transfer the data
        @pTex [ i ] = colors( @pTexData [ j ] ).r
        @pTex [ i + 1 ] = colors( @pTexData [ j ] ).g
        @pTex [ i + 2 ] = colors( @pTexData [ j ] ).b
        IF clrRGB = - 1& THEN
          @pTex [ i + 3 ] = 255
        ELSE
          @pTex [ i + 3 ] = IIF( RGB( @pTex [ i ], @pTex [ i + 1 ], @pTex [ i + 2 ] ) = clrRGB, 0, 255 )
        END IF
        INCR j
      NEXT i

    ELSE
      ' Texture parser for a forward texture
      j = nTexSize - 1

      ' Count backwards so you start at the front of the texture
      FOR i = 0 TO diff - 1 STEP 4
        ' Transfer the data
        @pTex [ i ] = colors( @pTexData [ j ] ).r
        @pTex [ i + 1 ] = colors( @pTexData [ j ] ).g
        @pTex [ i + 2 ] = colors( @pTexData [ j ] ).b
        IF clrRGB = - 1& THEN
          @pTex [ i + 3 ] = 255
        ELSE
          @pTex [ i + 3 ] = IIF( RGB( @pTex [ i ], @pTex [ i + 1 ], @pTex [ i + 2 ] ) = clrRGB, 0, 255 )
        END IF

        DECR j
      NEXT i
    END IF

  ELSE

    ' Allocate memory for our texture
    pTexData = STRPTR( nTexData )
    sTex = SPACE$( diff )
    pTex = STRPTR( sTex ) : IF pTex = 0 THEN EXIT FUNCTION

    IF nHeight > 0 THEN
      j = 0

      ' Count backwards so you start at the front of the texture
      FOR i = 0 TO nTexSize - 1 STEP 3
        ' Transfer the data
        @pTex [ j ] = @pTexData [ i + 2 ]
        @pTex [ j + 1 ] = @pTexData [ i + 1 ]
        @pTex [ j + 2 ] = @pTexData [ i ]
        IF clrRGB = - 1& THEN
          @pTex [ j + 3 ] = 255
        ELSE
          @pTex [ j + 3 ] = IIF( RGB( @pTex [ j ], @pTex [ j + 1 ], @pTex [ j + 2 ] ) = clrRGB, 0, 255 )
        END IF

        j = j + 4
      NEXT i
    ELSE
      ' Texture parser for a forward texture
      j = diff - 4

      ' Count backwards so you start at the front of the texture
      FOR i = 0 TO nTexSize - 1 STEP 3
        ' Transfer the data
        @pTex [ j ] = @pTexData [ i + 2 ]
        @pTex [ j + 1 ] = @pTexData [ i + 1 ]
        @pTex [ j + 2 ] = @pTexData [ i ]
        IF clrRGB = - 1& THEN
          @pTex [ j + 3 ] = 255
        ELSE
          @pTex [ j + 3 ] = IIF( RGB( @pTex [ j ], @pTex [ j + 1 ], @pTex [ j + 2 ] ) = clrRGB, 0, 255 )
        END IF

        j = j - 4
      NEXT i
    END IF
  END IF

  createGLTexture( asFont, pTex, %GL_RGBA8, nWidth, nHeight, TexNum, TexFilter, AnisoX )

  IF asFont = 0 THEN TextureList( TexNum ) = DescriptionCandidate
  FUNCTION = %TEXTURE_LOAD_SUCCESS

END FUNCTION

FUNCTION TextureLoadBMP_Tiled( asFont AS BYTE, TexName AS STRING, TexNum AS LONG, numCols AS LONG, numRows AS LONG, TexFilter AS LONG, OPTIONAL BYVAL AnisoX AS SINGLE, BYVAL clrRGB AS LONG ) AS EXT

  REGISTER i AS LONG, j AS LONG

  LOCAL n, nWidth, nHeight, nDepth, nTexSize, nColors, diff AS LONG
  LOCAL colors( ) AS c_BGRA
  LOCAL nTexData, sTex, DescriptionCandidate, WholeBMP AS STRING
  LOCAL pTexData, pTex AS BYTE PTR

  ' -- Determine whether it is physical file or memory image
  if ucase$(pathName$(extn, texName)) = ".BMP" then
    wholeBMP = internal_loadTextureFileFromDisk(texName, descriptionCandidate)
    if len(wholeBMP) = 0 then
      function = %TBGL_FUNCTION_ERROR
      exit function
    end if
  else
    DescriptionCandidate = "_MEMORY_FILE_BMP_" + FORMAT$( STRPTR( TexName ))
    WholeBMP = TexName
  end if

  DIM bmfh( 0 TO 0 ) AS BitmapFileHeader AT STRPTR( WholeBMP )

  IF bmfh( 0 ).bfType <> 19778 THEN
TextureLoadingError :
    TBGLError $ERR_TEXTUREBADFORMAT + TexName
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF

  DIM bmih( 0 TO 0 ) AS BitmapInfoHeader AT STRPTR( WholeBMP ) + SIZEOF( BitmapFileHeader )


  nWidth = bmih( 0 ).biWidth
  nHeight = bmih( 0 ).biHeight
  nDepth = bmih( 0 ).biBitCount

  ' Calculate the size of our texture
  nTexSize = nWidth * nHeight * ( nDepth \ 8 )
  ' Calculate the number of available colors
  nColors = 1
  SHIFT SIGNED LEFT nColors, nDepth
  IF nDepth < 8 THEN
    GOTO TextureLoadingError
  END IF

  ' Load the palette for 8 bits per pixel
  IF nDepth = 8 THEN
    REDIM colors( nColors ) AT STRPTR( WholeBMP ) + SIZEOF( BitmapFileHeader ) + SIZEOF( BitmapInfoHeader )
    nTexData = MID$( WholeBMP, 1 + SIZEOF( BitmapFileHeader ) + SIZEOF( BitmapInfoHeader ) + nColors * 4, LEN( WholeBMP ))

  ELSE
    nTexData = MID$( WholeBMP, 1 + SIZEOF( BitmapFileHeader ) + SIZEOF( BitmapInfoHeader ), LEN( WholeBMP ))
  END IF

  ' --

  ' Calculate the size of this texture once converted to 32-bit RGBA
  diff = nWidth * nHeight * 4

  ' Change format from BGRA to RGBA
  IF nDepth = 8 THEN
    ' Point to the data we loaded in
    pTexData = STRPTR( nTexData )

    ' Allocate memory for our texture
    sTex = SPACE$( diff )
    pTex = STRPTR( sTex )
    IF nHeight > 0 THEN
      j = 0
      ' Count backwards so you start at the front on the texture

      FOR i = 0 TO diff - 1 STEP 4
        ' Transfer the data
        @pTex [ i ] = colors( @pTexData [ j ] ).r
        @pTex [ i + 1 ] = colors( @pTexData [ j ] ).g
        @pTex [ i + 2 ] = colors( @pTexData [ j ] ).b
        IF clrRGB = - 1& THEN
          @pTex [ i + 3 ] = 255
        ELSE
          @pTex [ i + 3 ] = IIF( RGB( @pTex [ i ], @pTex [ i + 1 ], @pTex [ i + 2 ] ) = clrRGB, 0, 255 )
        END IF
        INCR j
      NEXT i

    ELSE
      ' Texture parser for a forward texture
      j = nTexSize - 1

      ' Count backwards so you start at the front of the texture
      FOR i = 0 TO diff - 1 STEP 4
        ' Transfer the data
        @pTex [ i ] = colors( @pTexData [ j ] ).r
        @pTex [ i + 1 ] = colors( @pTexData [ j ] ).g
        @pTex [ i + 2 ] = colors( @pTexData [ j ] ).b
        IF clrRGB = - 1& THEN
          @pTex [ i + 3 ] = 255
        ELSE
          @pTex [ i + 3 ] = IIF( RGB( @pTex [ i ], @pTex [ i + 1 ], @pTex [ i + 2 ] ) = clrRGB, 0, 255 )
        END IF

        DECR j
      NEXT i
    END IF

  ELSE

    ' Allocate memory for our texture
    pTexData = STRPTR( nTexData )
    sTex = SPACE$( diff )
    pTex = STRPTR( sTex ) : IF pTex = 0 THEN EXIT FUNCTION

    IF nHeight > 0 THEN
      j = 0

      ' Count backwards so you start at the front of the texture
      FOR i = 0 TO nTexSize - 1 STEP 3
        ' Transfer the data
        @pTex [ j ] = @pTexData [ i + 2 ]
        @pTex [ j + 1 ] = @pTexData [ i + 1 ]
        @pTex [ j + 2 ] = @pTexData [ i ]
        IF clrRGB = - 1& THEN
          @pTex [ j + 3 ] = 255
        ELSE
          @pTex [ j + 3 ] = IIF( RGB( @pTex [ j ], @pTex [ j + 1 ], @pTex [ j + 2 ] ) = clrRGB, 0, 255 )
        END IF

        j = j + 4
      NEXT i
    ELSE
      ' Texture parser for a forward texture
      j = diff - 4

      ' Count backwards so you start at the front of the texture
      FOR i = 0 TO nTexSize - 1 STEP 3
        ' Transfer the data
        @pTex [ j ] = @pTexData [ i + 2 ]
        @pTex [ j + 1 ] = @pTexData [ i + 1 ]
        @pTex [ j + 2 ] = @pTexData [ i ]
        IF clrRGB = - 1& THEN
          @pTex [ j + 3 ] = 255
        ELSE
          @pTex [ j + 3 ] = IIF( RGB( @pTex [ j ], @pTex [ j + 1 ], @pTex [ j + 2 ] ) = clrRGB, 0, 255 )
        END IF

        j = j - 4
      NEXT i
    END IF
  END IF

  LOCAL miniPTex AS BYTE PTR
  LOCAL miniBuffer AS STRING
  LOCAL miniW, miniH AS LONG

  miniW = nWidth / numCols
  miniH = nHeight / numRows

  FOR j = 1 TO numRows
    FOR i = 1 TO numCols
      pTex       = STRPTR( sTex )
      miniBuffer = internal_GetTextureTile(pTex, nWidth, nHeight, numCols, numRows, i, j )
      miniPTex   = STRPTR(miniBuffer)
      createGLTexture( asFont, miniPTex, %GL_RGBA8, miniW, miniH, TexNum, TexFilter, AnisoX )
      INCR TexNum
    NEXT
  NEXT

  IF asFont = 0 THEN TextureList( TexNum ) = DescriptionCandidate
  FUNCTION = %TEXTURE_LOAD_SUCCESS

END FUNCTION

' -- Spravit indexovani
FUNCTION internal_GetTextureTile( BYVAL rawBuffer AS BYTE PTR, nWidth AS LONG, nHeight AS LONG, BYVAL nTilesW AS LONG, BYVAL nTilesH AS LONG, BYVAL nCol AS LONG, BYVAL nRow AS LONG ) AS STRING
  REGISTER i AS LONG
  LOCAL miniWidth, miniHeight AS LONG

  nRow = nTilesH+1 - nRow

  miniWidth = nWidth / nTilesW
  miniHeight= nHeight / nTilesH

  LOCAL hJump AS LONG
  hJump = nWidth * 4

  ' -- Distance from beginning
  LOCAL offset AS LONG
  offset = (nCol-1) * miniWidth * 4 + ((nRow-1) * nWidth * 4)*miniHeight

  ' -- Jump on first (RGBA=4bytes)
  rawBuffer += offset

  ' -- Jump horizontaly as many times as many tiles we have
  DIM sBuffer AS STRING

  FOR i = 1 TO miniHeight
    sBuffer   += PEEK$(rawBuffer, miniWidth * 4)
    rawBuffer += hJump
  NEXT

  FUNCTION = sBuffer

END FUNCTION


FUNCTION TextureLoadTGA( asFont AS BYTE, TexName AS STRING, TexNum AS LONG, TexFilter AS LONG, OPTIONAL BYVAL AnisoX AS SINGLE ) AS LONG

  #REGISTER NONE
  ' Loads a Targa (*.TGA) texture from disk

  LOCAL n, nWidth, nHeight, nDepth, nTexSize AS LONG
  LOCAL TGAheader, TGAcompare, auxHeader, nTexData, sTex, DescriptionCandidate, WholeTGA AS STRING
  LOCAL pTex AS BYTE PTR

  if ucase$(pathName$(extn, texName)) = ".TGA" then
    wholeTGA = internal_loadTextureFileFromDisk(texName, descriptionCandidate)
    if len(wholeTGA) = 0 then
      function = %TBGL_FUNCTION_ERROR
      exit function
    end if
  ELSE
    DescriptionCandidate = "_MEMORY_FILE_TGA_" + FORMAT$( STRPTR( TexName ))
    WholeTGA = TexName

  END IF

  ' Compare the header to ensure we have an uncompressed Targa file
  TGAheader = CHR$( 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0 )    ' STRING$(12, 0)
  TGAcompare = LEFT$( WholeTGA, 12 )
  IF TGAcompare <> TGAheader THEN
    TBGLError $ERR_TEXTUREBADFORMAT + TexName + $CRLF + "[ 0 0 2 0 0 0 0 0 0 0 0 0 ]" + $CRLF + "[" + _
      STR$( ASC( TGAcompare, 1 )) + STR$( ASC( TGAcompare, 2 )) + STR$( ASC( TGAcompare, 3 )) + STR$( ASC( TGAcompare, 4 )) + STR$( ASC( TGAcompare, 5 )) + STR$( ASC( TGAcompare, 6 )) + _
      STR$( ASC( TGAcompare, 7 )) + STR$( ASC( TGAcompare, 8 )) + STR$( ASC( TGAcompare, 9 )) + STR$( ASC( TGAcompare, 10 )) + STR$( ASC( TGAcompare, 11 )) + STR$( ASC( TGAcompare, 12 )) + " ]"
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF


  ' Calculate the size of our texture
  auxHeader = MID$( WholeTGA, 13, 6 )
  nWidth = ASC( auxHeader, 2 ) * 256 + ASC( auxHeader, 1 )
  nHeight = ASC( auxHeader, 4 ) * 256 + ASC( auxHeader, 3 )
  nDepth = ASC( auxHeader, 5 )
  IF nDepth <> 32 THEN
    TBGLError $ERR_TEXTUREBADFORMAT + TexName + "Problem:" + $TAB + "Wrong bit depth"
    EXIT FUNCTION
  END IF

  ' Calculate the amount of memory required to store our entire texture
  nTexSize = nWidth * nHeight * 4

  ' Load in the actual pixel data that makes up the texture
  nTexData = MID$( WholeTGA, 19, nWidth * nHeight * 4 )

  ' Allocate memory for our texture
  sTex = SPACE$( nTexSize )
  pTex = STRPTR( sTex )

  ' Store our texture
  POKE$ pTex, nTexData

  ' Swap the red and blue components of the texture
  ! push      ebx
  ! mov       ebx, pTex                               ' point to the texture
  ! mov       ecx, nTexSize                           ' set up our counter...
  ! shr       ecx, 2                                  ' ...(divide nTexSize by 4)
swapRGBvalues :
  ! mov       al, [ebx]                               ' swap the red and blue components
  ! mov       ah, [ebx + 2]
  ! mov       [ebx], ah
  ! mov       [ebx + 2], al
  ! Add       ebx, 4
  ! Sub       ecx, 1
  ! jnz       swapRGBvalues                           ' loop through until ECX is 0
  ! pop       ebx


  ' Assign an OpenGL handle to this texture
  createGLTexture( asFont, pTex, %GL_RGBA8, nWidth, nHeight, TexNum, TexFilter, AnisoX )

  IF asFont = 0 THEN TextureList( TexNum ) = DescriptionCandidate
  FUNCTION = %TEXTURE_LOAD_SUCCESS

END FUNCTION

FUNCTION TextureLoadTGA_Tiled( asFont AS BYTE, TexName AS STRING, TexNum AS LONG, numCols AS LONG, numRows AS LONG, TexFilter AS LONG, OPTIONAL BYVAL AnisoX AS SINGLE ) AS LONG

  #REGISTER NONE
  ' Loads a Targa (*.TGA) texture from disk

  LOCAL n, nWidth, nHeight, nDepth, nTexSize AS LONG
  LOCAL TGAheader, TGAcompare, auxHeader, nTexData, sTex, DescriptionCandidate, WholeTGA AS STRING
  LOCAL pTex AS BYTE PTR

  if ucase$(pathName$(extn, texName)) = ".TGA" then
    wholeTGA = internal_loadTextureFileFromDisk(texName, descriptionCandidate)
    if len(wholeTGA) = 0 then
      function = %TBGL_FUNCTION_ERROR
      exit function
    end if
  ELSE
    DescriptionCandidate = "_MEMORY_FILE_TGA_" + FORMAT$( STRPTR( TexName ))
    WholeTGA = TexName

  END IF

  ' Compare the header to ensure we have an uncompressed Targa file
  TGAheader = CHR$( 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0 )    ' STRING$(12, 0)
  TGAcompare = LEFT$( WholeTGA, 12 )
  IF TGAcompare <> TGAheader THEN
    TBGLError $ERR_TEXTUREBADFORMAT + TexName + $CRLF + "[ 0 0 2 0 0 0 0 0 0 0 0 0 ]" + $CRLF + "[" + _
      STR$( ASC( TGAcompare, 1 )) + STR$( ASC( TGAcompare, 2 )) + STR$( ASC( TGAcompare, 3 )) + STR$( ASC( TGAcompare, 4 )) + STR$( ASC( TGAcompare, 5 )) + STR$( ASC( TGAcompare, 6 )) + _
      STR$( ASC( TGAcompare, 7 )) + STR$( ASC( TGAcompare, 8 )) + STR$( ASC( TGAcompare, 9 )) + STR$( ASC( TGAcompare, 10 )) + STR$( ASC( TGAcompare, 11 )) + STR$( ASC( TGAcompare, 12 )) + " ]"
    FUNCTION = %TBGL_FUNCTION_ERROR
    EXIT FUNCTION
  END IF


  ' Calculate the size of our texture
  auxHeader = MID$( WholeTGA, 13, 6 )
  nWidth = ASC( auxHeader, 2 ) * 256 + ASC( auxHeader, 1 )
  nHeight = ASC( auxHeader, 4 ) * 256 + ASC( auxHeader, 3 )
  nDepth = ASC( auxHeader, 5 )
  IF nDepth <> 32 THEN
    TBGLError $ERR_TEXTUREBADFORMAT + TexName + "Problem:" + $TAB + "Wrong bit depth"
    EXIT FUNCTION
  END IF

  ' Calculate the amount of memory required to store our entire texture
  nTexSize = nWidth * nHeight * 4

  ' Load in the actual pixel data that makes up the texture
  nTexData = MID$( WholeTGA, 19, nWidth * nHeight * 4 )

  ' Allocate memory for our texture
  sTex = SPACE$( nTexSize )
  pTex = STRPTR( sTex )

  ' Store our texture
  POKE$ pTex, nTexData

  ' Swap the red and blue components of the texture
  ! push      ebx
  ! mov       ebx, pTex                               ' point to the texture
  ! mov       ecx, nTexSize                           ' set up our counter...
  ! shr       ecx, 2                                  ' ...(divide nTexSize by 4)
swapRGBvalues :
  ! mov       al, [ebx]                               ' swap the red and blue components
  ! mov       ah, [ebx + 2]
  ! mov       [ebx], ah
  ! mov       [ebx + 2], al
  ! Add       ebx, 4
  ! Sub       ecx, 1
  ! jnz       swapRGBvalues                           ' loop through until ECX is 0
  ! pop       ebx



  ' Assign an OpenGL handle to this texture
  LOCAL miniPTex AS BYTE PTR
  LOCAL miniBuffer AS STRING
  LOCAL miniW, miniH AS LONG

  miniW = nWidth / numCols
  miniH = nHeight / numRows
  LOCAL i,j AS LONG
  FOR j = 1 TO numRows
    FOR i = 1 TO numCols
      pTex       = STRPTR( sTex )
      miniBuffer = internal_GetTextureTile(pTex, nWidth, nHeight, numCols, numRows, i, j )
      miniPTex   = STRPTR(miniBuffer)
      createGLTexture( asFont, miniPTex, %GL_RGBA8, miniW, miniH, TexNum, TexFilter, AnisoX )
      INCR TexNum
    NEXT
  NEXT
  'createGLTexture( asFont, pTex, %GL_RGBA8, nWidth, nHeight, TexNum, TexFilter, AnisoX )

  IF asFont = 0 THEN TextureList( TexNum ) = DescriptionCandidate
  FUNCTION = %TEXTURE_LOAD_SUCCESS

END FUNCTION


' -----------------------------------------------------------------------------
SUB createGLTexture( asFont AS BYTE, BYVAL rawBuffer AS BYTE PTR, dataType AS LONG, xSize AS LONG, ySize AS LONG, TexNum AS LONG, TexFilter AS LONG, AnisoX AS SINGLE )

  IF asFont = 0 THEN
    glBindTexture %GL_TEXTURE_2D, lpTexture( TexNum )
  ELSE
    glBindTexture %GL_TEXTURE_2D, lpTextureFont( TexNum )
  END IF

  IF TexFilter > %TBGL_TEX_CRISPANISO THEN
    TexFilter = %TBGL_TEX_CRISPANISO
  ELSEIF TexFilter < %TBGL_TEX_NEAREST THEN
    TexFilter = %TBGL_TEX_NEAREST
  END IF

  IF TexFilter = %TBGL_TEX_ANISO THEN
    IF AnisoX < 1 THEN AnisoX = 2
    IF AnisoX > texture_AnisotropyMax THEN AnisoX = texture_AnisotropyMax
    IF texture_AnisotropyAllowed = 0 THEN TexFilter = %TBGL_TEX_MIPMAP
  END IF

  IF TexFilter = %TBGL_TEX_CRISPANISO THEN
    IF AnisoX < 1 THEN AnisoX = 2
    IF AnisoX > texture_AnisotropyMax THEN AnisoX = texture_AnisotropyMax
    IF texture_AnisotropyAllowed = 0 THEN TexFilter = %TBGL_TEX_LINEAR
  END IF

  SELECT CASE AS LONG TexFilter
    CASE %TBGL_TEX_NEAREST
      GLTEXPARAMETERI %GL_TEXTURE_2D, %GL_TEXTURE_MAG_FILTER, %GL_NEAREST
      GLTEXPARAMETERI %GL_TEXTURE_2D, %GL_TEXTURE_MIN_FILTER, %GL_NEAREST
      GLTEXIMAGE2D %GL_TEXTURE_2D, 0, dataType, xSize, ySize, 0, %GL_RGBA, %GL_UNSIGNED_BYTE, @rawBuffer

    CASE %TBGL_TEX_LINEAR
      GLTEXPARAMETERI %GL_TEXTURE_2D, %GL_TEXTURE_MAG_FILTER, %GL_LINEAR
      GLTEXPARAMETERI %GL_TEXTURE_2D, %GL_TEXTURE_MIN_FILTER, %GL_LINEAR
      GLTEXIMAGE2D %GL_TEXTURE_2D, 0, dataType, xSize, ySize, 0, %GL_RGBA, %GL_UNSIGNED_BYTE, @rawBuffer

    CASE %TBGL_TEX_MIPMAP, %TBGL_TEX_ANISO
      GLTEXPARAMETERI %GL_TEXTURE_2D, %GL_TEXTURE_MAG_FILTER, %GL_LINEAR
      GLTEXPARAMETERI %GL_TEXTURE_2D, %GL_TEXTURE_MIN_FILTER, %GL_LINEAR_MIPMAP_LINEAR
      IF TexFilter = %TBGL_TEX_ANISO THEN GLTEXPARAMETERF( %GL_TEXTURE_2D, %GL_TEXTURE_MAX_ANISOTROPY_EXT, AnisoX )
      GLUBUILD2DMIPMAPS %GL_TEXTURE_2D, dataType, xSize, ySize, %GL_RGBA, %GL_UNSIGNED_BYTE, @rawBuffer    ' -- Nesouhlasi se specifikaci ?

    CASE %TBGL_TEX_CRISPANISO
      GLTEXPARAMETERI %GL_TEXTURE_2D, %GL_TEXTURE_MAG_FILTER, %GL_LINEAR
      GLTEXPARAMETERI %GL_TEXTURE_2D, %GL_TEXTURE_MIN_FILTER, %GL_LINEAR
      GLTEXPARAMETERF( %GL_TEXTURE_2D, %GL_TEXTURE_MAX_ANISOTROPY_EXT, AnisoX )
      GLTEXIMAGE2D %GL_TEXTURE_2D, 0, dataType, xSize, ySize, 0, %GL_RGBA, %GL_UNSIGNED_BYTE, @rawBuffer

  END SELECT

  IF LastBindedTexture >= 0 THEN glBindTexture %GL_TEXTURE_2D, lpTexture( LastBindedTexture )

END SUB

' -- Used in Models
FUNCTION meIsTextureLoaded( texName AS STRING ) AS LONG
  LOCAL texindex AS LONG

  FUNCTION = 0
  FOR texindex = 1 TO %LIMIT_MAX_TEXTUREINDEX
    IF TextureList( texindex ) = texName THEN FUNCTION = texindex
  NEXT
END FUNCTION

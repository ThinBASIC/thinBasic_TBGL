' =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles fonts in general
' =============================================================================

' -----------------------------------------------------------------------------

#INCLUDE ONCE "tbgl_Texturing.inc"  ' -- Fonts use some of texturing resources

' -----------------------------------------------------------------------------

%TBGL_ALIGN_LEFT        = -10
%TBGL_ALIGN_LEFT_UP     = -11
%TBGL_ALIGN_LEFT_DOWN   = -12
%TBGL_ALIGN_LEFT_CENTER = -13

%TBGL_ALIGN_CENTER        = 100
%TBGL_ALIGN_CENTER_UP     = 101
%TBGL_ALIGN_CENTER_DOWN   = 102
%TBGL_ALIGN_CENTER_CENTER = 103

%TBGL_ALIGN_RIGHT         = 10
%TBGL_ALIGN_RIGHT_UP      = 11
%TBGL_ALIGN_RIGHT_DOWN    = 12
%TBGL_ALIGN_RIGHT_CENTER  = 13

%TBGL_ALIGN_NONE          = -1


%TBGL_BOLD    = 1
%TBGL_ITALICS = 2

%LIMIT_MAX_FONTINDEX = 3

' -----------------------------------------------------------------------------

TYPE tWinFont
  ListBase AS LONG
  fHandle AS DWORD
END TYPE

GLOBAL g_WinFont( ) AS tWinFont
GLOBAL g_TBGL_SetActiveFont AS LONG

GLOBAL g_FontProjMatrix() AS DOUBLE
GLOBAL g_FontModeMatrix() AS DOUBLE
GLOBAL g_FontBeginEnd AS LONG

TYPE tBMPFont
  ListBase AS LONG
  fSize AS SINGLE
  fMaxX AS SINGLE
  fMaxY AS SINGLE
  fLines AS SINGLE
  fType AS LONG
END TYPE

GLOBAL g_BMPFont( ) AS tBMPFont
GLOBAL g_TBGL_SetActiveBMPFont AS LONG

'GLOBAL lpTextureFont( ) AS AUX_RGBImageRec PTR  ' -- defined in tbgl_texturing

' -----------------------------------------------------------------------------

' >RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES
SUB resource_Fonts_Alloc()
  glGenTextures %LIMIT_MAX_FONTINDEX, lpTextureFont( 0 )
END SUB

SUB resource_Fonts_Release()
  LOCAL i AS LONG

  glDeleteTextures %LIMIT_MAX_FONTINDEX, lpTextureFont( 0 )
  RESET lpTextureFont( )

  FOR i = 1 TO 4
    internal_FontCleanup( i )
    internal_FontBMPCleanup( i )
  NEXT

END SUB
' <RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES

' -----------------------------------------------------------------------------
' -- TTF fonts
' -----------------------------------------------------------------------------

' -- Creates windows font and returns its handle
FUNCTION Exec_TBGL_FontHandle( ) AS EXT

  LOCAL sFntName AS STRING
  LOCAL fFntSize, fStyle AS EXT
  LOCAL lStyle AS LONG
  LOCAL lWeight, lItalic, lCharSet AS LONG
  lCharset = %DEFAULT_CHARSET

  LOCAL n AS LONG

  n = thinBasic_Parse1StringXNumbers( 1, 2, sFntName, fFntSize, fStyle )

  lStyle = fStyle
  lWeight = IIF( BIT( lStyle, 0 ) = 0, %FW_NORMAL, %FW_BOLD )
  lItalic = IIF( BIT( lStyle, 1 ) = 0, %FALSE, %TRUE )

  LOCAL lf AS LOGFONT

  LOCAL lCyPixels AS LONG
  LOCAL fHDc AS DWORD
  fHDc = GetDC( %HWND_DESKTOP )
  lCyPixels = GetDeviceCaps( fHDc, %LOGPIXELSY )
  ReleaseDC %HWND_DESKTOP, fHDc

  lf.lfHeight = - ( fFntSize * lCyPixels ) \ 72
  lf.lfFaceName = sFntName
  lf.lfPitchAndFamily = %FF_DONTCARE
  IF lWeight = %FW_DONTCARE THEN
    lf.lfWeight = %FW_NORMAL
  ELSE
    lf.lfWeight = lWeight
  END IF
  lf.lfUnderline = %FALSE    'lUnderlined
  lf.lfItalic = lItalic
  lf.lfStrikeOut = %FALSE    'lStrike
  lf.lfCharSet = lCharSet

  FUNCTION = CreateFontIndirect( lf )

END FUNCTION

' -- Creates OpenGL vector font from Windows font handle
FUNCTION Exec_TBGL_BuildFont( ) AS EXT

  LOCAL x, SlotNumber AS EXT
  SlotNumber = 1
  thinBasic_ParseXNumbers( 1, 2, x, SlotNumber )

  IF SlotNumber < 1 OR SlotNumber > %LIMIT_MAX_FONTINDEX + 1 THEN
    TBGLError "Statement:" + $TAB + "TBGL_BuildFont" + $CRLF + "Problem:" + $TAB + $TAB + "Font slot ID must be in range [1," + STR$( %LIMIT_MAX_FONTINDEX + 1 ) + "]" + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( SlotNumber )
  END IF

  internal_FontCleanup( slotNumber )

  g_WinFont( SlotNumber ).fHandle = x
  g_WinFont( SlotNumber ).ListBase = GLGENLISTS( 256 )

  SelectObject g_Win.hDC, g_WinFont( SlotNumber ).fHandle
  wglUseFontBitmaps g_Win.hDC, 0, 256, g_WinFont( SlotNumber ).ListBase

  FUNCTION = SlotNumber

END FUNCTION

' -- Sets active TTF font
SUB Exec_TBGL_SetActiveFont( )
  LOCAL SlotNumber AS LONG

  SlotNumber = thinBASIC_Parse1Number( )
  IF SlotNumber < 1 OR SlotNumber > %LIMIT_MAX_FONTINDEX + 1 THEN
    TBGLError "Statement:" + $TAB + "TBGL_SetActiveFont" + $CRLF + "Problem:" + $TAB + $TAB + "Font slot ID must be in range [1," + STR$( %LIMIT_MAX_FONTINDEX + 1 ) + "]" + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( SlotNumber )
  ELSE
    g_TBGL_SetActiveFont = SlotNumber
  END IF

END SUB

' -- Prints font on screen
SUB Exec_TBGL_PrintFont( )

  LOCAL textik AS ASCIIZ * 256, stext AS STRING
  LOCAL x, y, z AS EXT
  LOCAL nParams AS LONG

  nParams = thinBasic_Parse1StringXNumbers( 2, 3, stext, x, y, z )

  textik = stext

  IF nParams = 2 THEN
    internal_ManagedRasterPos(x, y, 0)
  ELSE
    glRasterPos3f(x, y, z)
  END IF

  glPushAttrib %GL_LIST_BIT
    glListBase g_WinFont( g_TBGL_SetActiveFont ).ListBase
    glCallLists LEN( textik ), %GL_UNSIGNED_BYTE, textik
    glListBase 0
  glPopAttrib

END SUB

' -- Prints font on screen
SUB Exec_TBGL_PrintFont2D( )

  IF g_Win.RenderMatrixMode <> %TBGL_2D THEN
    TBGLError "Statement:" + $TAB + "TBGL_PrintFont2D" + $CRLF + "Problem:" + $TAB + $TAB + "You can use this command only in TBGL_RenderMatrix2D mode"
    EXIT SUB
  END IF

  LOCAL textik AS ASCIIZ * 256, stext AS STRING
  LOCAL x, y, screenAlignment, anchorAlignment, maximumWidth AS EXT
  LOCAL l AS sizel
  l.cx            = -1
  maximumWidth    = -1
  screenAlignment = %TBGL_ALIGN_NONE
  anchorAlignment = %TBGL_ALIGN_LEFT_DOWN

  thinBasic_Parse1StringXNumbers( 2, 6, stext, x, y, screenAlignment, anchorAlignment, maximumWidth )

  ' -- Text width correction
  IF maximumWidth <> -1 THEN
    LOCAL textChars AS LONG

    textChars = LEN(sText)

    SelectObject(g_Win.hDC, g_WinFont( g_TBGL_SetActiveFont ).fHandle    )
    GetTextExtentPoint32(g_Win.hDC, sText+"", textChars, l)

    WHILE l.cx > maximumWidth
      textChars -= 1
      IF textChars = 0 THEN EXIT LOOP

      GetTextExtentPoint32(g_Win.hDC, sText+"", textChars, l)
    WEND

    sText = LEFT$(sText, textChars)
  END IF

  ' -- Screen alignment
  IF screenAlignment <> -1 THEN

    ' -- Retrieve text dimensions
    IF l.cx = -1 THEN
      SelectObject(g_Win.hDC, g_WinFont( g_TBGL_SetActiveFont ).fHandle    )
      GetTextExtentPoint32(g_Win.hDC, sText+"", LEN(sText), l)
    END IF

    ' -- Assign possition presuming lower left corner
    ' X
    SELECT CASE AS LONG (screenAlignment)
      CASE %TBGL_ALIGN_LEFT, %TBGL_ALIGN_LEFT_UP, %TBGL_ALIGN_LEFT_CENTER, %TBGL_ALIGN_LEFT_DOWN
        x += g_renderMatrix2D.Left

      CASE %TBGL_ALIGN_CENTER, %TBGL_ALIGN_CENTER_UP, %TBGL_ALIGN_CENTER_CENTER, %TBGL_ALIGN_CENTER_DOWN
        x += g_renderMatrix2D.left+(g_renderMatrix2D.right - g_renderMatrix2D.left)/2

      CASE %TBGL_ALIGN_RIGHT, %TBGL_ALIGN_RIGHT_UP, %TBGL_ALIGN_RIGHT_CENTER, %TBGL_ALIGN_RIGHT_DOWN
        x += g_renderMatrix2D.right
    END SELECT

    ' Y
    SELECT CASE AS LONG (screenAlignment)
      CASE %TBGL_ALIGN_LEFT_UP, %TBGL_ALIGN_CENTER_UP, %TBGL_ALIGN_RIGHT_UP
        y += g_renderMatrix2D.top

      CASE %TBGL_ALIGN_LEFT_CENTER, %TBGL_ALIGN_CENTER_CENTER, %TBGL_ALIGN_RIGHT_CENTER
        y += g_renderMatrix2D.top+(g_renderMatrix2D.bottom - g_renderMatrix2D.top)/2

      CASE %TBGL_ALIGN_LEFT_DOWN, %TBGL_ALIGN_CENTER_DOWN, %TBGL_ALIGN_RIGHT_DOWN
        y += g_renderMatrix2D.bottom
    END SELECT

  END IF

  ' -- Anchor correction
  ' >>

    ' -- Retrieve text dimensions
    IF l.cx = -1 AND anchorAlignment <> %TBGL_ALIGN_LEFT_DOWN THEN
      SelectObject(g_Win.hDC, g_WinFont( g_TBGL_SetActiveFont ).fHandle    )
      GetTextExtentPoint32(g_Win.hDC, sText+"", LEN(sText), l)
    END IF

    ' -- Assign possition presuming lower left corner
    ' X
    SELECT CASE AS LONG (anchorAlignment)
      CASE %TBGL_ALIGN_LEFT_UP, %TBGL_ALIGN_LEFT_CENTER, %TBGL_ALIGN_LEFT_DOWN
        ' -- Nothing, as it is default

      CASE %TBGL_ALIGN_CENTER_UP, %TBGL_ALIGN_CENTER_CENTER, %TBGL_ALIGN_CENTER_DOWN
        x -= l.cx / 2

      CASE %TBGL_ALIGN_RIGHT_UP, %TBGL_ALIGN_RIGHT_CENTER, %TBGL_ALIGN_RIGHT_DOWN
        x -= l.cx
    END SELECT

    ' Y
    SELECT CASE AS LONG (anchorAlignment)
      CASE %TBGL_ALIGN_LEFT_UP, %TBGL_ALIGN_CENTER_UP, %TBGL_ALIGN_RIGHT_UP
        y -= l.cy

      CASE %TBGL_ALIGN_LEFT_CENTER, %TBGL_ALIGN_CENTER_CENTER, %TBGL_ALIGN_RIGHT_CENTER
        y -= l.cy / 2

      CASE %TBGL_ALIGN_LEFT_DOWN, %TBGL_ALIGN_CENTER_DOWN, %TBGL_ALIGN_RIGHT_DOWN
        ' -- Nothing, as it is default

    END SELECT

  ' <<

  ' -- Let's print the result...
  textik = stext

  internal_ManagedRasterPos(x, y, 0)
  glPushAttrib %GL_LIST_BIT
    glListBase g_WinFont( g_TBGL_SetActiveFont ).ListBase
    glCallLists LEN( textik ), %GL_UNSIGNED_BYTE, textik
    glListBase 0
  glPopAttrib

  ' -- OLD CODE FROM SCREEN ALIGNMENT
'    DIM mxProjection(0 TO 15) AS DOUBLE
'
'    glGetDoubleV( %GL_PROJECTION_MATRIX, mxProjection( 0 ))
'
'    n0 = mxProjection(0)
'    n3 = mxProjection(3)
'    n5 = mxProjection(5)
'    n7 = mxProjection(7)
'
'    ' -- Derived by solving system of equations from here: http://www.songho.ca/opengl/gl_projectionmatrix.html
'    A = (2/n0)
'    B = ((n3+1)/(n3-1))
'    nRight  = A/(1-B)
'    nLeft   = nRight * B
'
'    C = (2/n5)
'    D = ((n7+1)/(n7-1))
'    nTop    = C/(1-D)
'    nBottom = nTop * D

END SUB

' -- Measures size of the text in specified font in pixels
SUB Exec_TBGL_GetFontTextSize()
  LOCAL outX, outY AS EXT
  LOCAL sText AS STRING
  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseString sText
    IF thinBasic_Checkcomma_Mandatory THEN
    thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )
      IF thinBasic_Checkcomma_Mandatory THEN
        thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )
        IF thinBasic_CheckCloseParens_Mandatory() THEN
          LOCAL l AS sizel

          SelectObject(g_Win.hDC, g_WinFont( g_TBGL_SetActiveFont ).fHandle    )
          GetTextExtentPoint32(g_Win.hDC, sText+"", LEN(sText), l)
          outX = l.cx
          outY = l.cy

          thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, outX )
          thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, outY )
        END IF
      END IF
    END IF
  END IF

END SUB

' -- Kills font
SUB Exec_TBGL_KillFont( )

  LOCAL SlotNumber AS EXT

  IF thinBasic_CheckOpenParens_Optional THEN
    thinBASIC_ParseNumber( SlotNumber )
    thinBasic_CheckCloseParens_Mandatory
  ELSE
    SlotNumber = 1
  END IF

  IF SlotNumber < 1 OR SlotNumber > %LIMIT_MAX_FONTINDEX + 1 THEN
    TBGLError "Statement:" + $TAB + "TBGL_KillFont" + $CRLF + "Problem:" + $TAB + $TAB + "Font slot ID must be in range [1," + STR$( %LIMIT_MAX_FONTINDEX + 1 ) + "]" + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( SlotNumber )
  END IF

  internal_FontCleanup( SlotNumber )

END SUB

' -----------------------------------------------------------------------------
' -- Fonts from bitmap
' -----------------------------------------------------------------------------

' -- Ehm ... coefficients
MACRO HausNumero    = 0.414214
MACRO HausNumero2x  = 0.828428

SUB Exec_TBGL_LoadBMPFont( )
  REGISTER e AS LONG, i AS LONG

  LOCAL TextureName AS STRING
  LOCAL LinesSuggested AS EXT
  LOCAL SlotNumber AS EXT
  LOCAL filtersuggested AS EXT
  LOCAL ID AS LONG

  filtersuggested = %TBGL_TEX_MIPMAP
  thinBasic_ParseString TextureName

  ' -- Defaults
  LinesSuggested = 26
  SlotNumber = 1

  IF thinBasic_CheckComma_Optional THEN thinBasic_ParseNumber LinesSuggested
  IF thinBasic_CheckComma_Optional THEN thinBasic_ParseNumber SlotNumber

  IF SlotNumber < 1 OR SlotNumber > %LIMIT_MAX_FONTINDEX + 1 THEN
    TBGLError "Statement:" + $TAB + "TBGL_LoadBMPFont" + $CRLF + "Problem:" + $TAB + $TAB + "Font slot ID must be in range [1," + STR$( %LIMIT_MAX_FONTINDEX + 1 ) + "]" + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( SlotNumber )
  END IF

  internal_FontBMPCleanup( slotNumber )

  g_BMPFont( SlotNumber ).fSize = HausNumero2x / LinesSuggested
  g_BMPFont( SlotNumber ).fMaxX = ( HausNumero2x * g_Win.xRes / g_Win.yRes ) / g_BMPFont( SlotNumber ).fSize
  g_BMPFont( SlotNumber ).fMaxY = LinesSuggested

  IF UCASE$( TRIM$( PARSE$( TextureName, ".", - 1 ))) = "BMP" THEN
    TextureLoadBMP( 1, TextureName, SlotNumber - 1, BYCOPY filtersuggested, 1, - 1 )
  ELSEIF UCASE$( TRIM$( PARSE$( TextureName, ".", - 1 ))) = "TGA" THEN
    TextureLoadTGA( 1, TextureName, SlotNumber - 1, BYCOPY filtersuggested, 1 )
  END IF

  g_BMPFont( SlotNumber ).Listbase = glGenLists( 256 )
  g_BMPFont( SlotNumber ).fType = %TBGL_3D

  ' -- Creating display lists with characters
  FOR e = 0 TO 15
    FOR i = 1 TO 16

      glNewList g_BMPFont( SlotNumber ).Listbase + ID, %GL_COMPILE

        glBegin %GL_QUADS

          glTexCoord2f 0.0625 * i - 0.0625, 1 - 0.0625 * e              : glVertex3f 0, g_BMPFont( SlotNumber ).fSize, 0
          glTexCoord2f 0.0625 * i, 1 - 0.0625 * e                       : glVertex3f g_BMPFont( SlotNumber ).fSize, g_BMPFont( SlotNumber ).fSize, 0
          glTexCoord2f 0.0625 * i, 1 - ( 0.0625 * e ) - 0.0625          : glVertex3f g_BMPFont( SlotNumber ).fSize, 0, 0
          glTexCoord2f 0.0625 * i - 0.0625, 1 - ( 0.0625 * e ) - 0.0625 : glVertex3f 0, 0, 0

        glEnd

      glEndList
      INCR ID
    NEXT i
  NEXT e

END SUB

SUB Exec_TBGL_LoadBMPFont2D( )
  REGISTER e AS LONG, i AS LONG

  LOCAL TextureName AS STRING
  LOCAL LinesSuggested AS EXT
  LOCAL ColumnsSuggested AS EXT
  LOCAL SlotNumber AS EXT
  LOCAL filtersuggested AS EXT
  LOCAL ID AS LONG

  filtersuggested = %TBGL_TEX_MIPMAP
  thinBasic_ParseString TextureName

  ' -- Defaults
  LinesSuggested  = 30
  ColumnsSuggested= 40

  SlotNumber = 1

  IF thinBasic_CheckComma_Optional THEN thinBasic_ParseNumber LinesSuggested
  IF thinBasic_CheckComma_Optional THEN thinBasic_ParseNumber ColumnsSuggested
  IF thinBasic_CheckComma_Optional THEN thinBasic_ParseNumber SlotNumber

  IF SlotNumber < 1 OR SlotNumber > %LIMIT_MAX_FONTINDEX + 1 THEN
    TBGLError "Statement:" + $TAB + "TBGL_LoadBMPFont2D" + $CRLF + "Problem:" + $TAB + $TAB + "Font slot ID must be in range [1," + STR$( %LIMIT_MAX_FONTINDEX + 1 ) + "]" + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( SlotNumber )
  END IF

  internal_FontBMPCleanup( slotNumber )

  g_BMPFont( SlotNumber ).fSize = 1
  g_BMPFont( SlotNumber ).fMaxX = ColumnsSuggested
  g_BMPFont( SlotNumber ).fMaxY = LinesSuggested

  IF UCASE$( TRIM$( PARSE$( TextureName, ".", - 1 ))) = "BMP" THEN
    TextureLoadBMP( 1, TextureName, SlotNumber - 1, BYCOPY filtersuggested, 1, - 1 )
  ELSEIF UCASE$( TRIM$( PARSE$( TextureName, ".", - 1 ))) = "TGA" THEN
    TextureLoadTGA( 1, TextureName, SlotNumber - 1, BYCOPY filtersuggested, 1 )
  END IF

  g_BMPFont( SlotNumber ).Listbase = glGenLists( 256 )
  g_BMPFont( SlotNumber ).fType = %TBGL_2D

  ' -- Creating display lists with characters
  FOR e = 0 TO 15
    FOR i = 1 TO 16

      glNewList g_BMPFont( SlotNumber ).Listbase + ID, %GL_COMPILE

        glBegin %GL_QUADS

          glTexCoord2f 0.0625 * i - 0.0625, 1 - 0.0625 * e              : glVertex2f -1, 0
          glTexCoord2f 0.0625 * i, 1 - 0.0625 * e                       : glVertex2f  0, 0
          glTexCoord2f 0.0625 * i, 1 - ( 0.0625 * e ) - 0.0625          : glVertex2f  0, 1
          glTexCoord2f 0.0625 * i - 0.0625, 1 - ( 0.0625 * e ) - 0.0625 : glVertex2f -1, 1

        glEnd

      glEndList
      INCR ID
    NEXT i
  NEXT e

END SUB


SUB Exec_TBGL_BeginPrintBMP
  g_FontBeginEnd = 1

  glGetIntegerV(%GL_TEXTURE_2D, gUseTexture)
  IF gUseTexture = 0 THEN GLENABLE %GL_TEXTURE_2D
  glBindTexture %GL_TEXTURE_2D, lpTextureFont( g_TBGL_SetActiveBMPFont - 1 )    ' -- Texture array from zero

  ' -- Back up temp projection
  glGetDoubleV( %GL_PROJECTION_MATRIX, g_FontProjMatrix( 0 ))
'  glGetDoubleV( %GL_PROJECTION_MATRIX, g_FontProjMatrix( 0 ))

  IF g_Win.RenderMatrixMode = %TBGL_3D THEN

      ' -- 3D projection
      glLoadIdentity( )

  ELSE

      ' -- 2D projection
      glMatrixMode( %GL_PROJECTION )

        glLoadIdentity( )
        glOrtho( 1, g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxX+1, g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxY+1, 1, -1, 1 )

      glMatrixMode( %GL_MODELVIEW )
        glLoadIdentity( )

  END IF

END SUB

SUB Exec_TBGL_EndPrintBMP

  glMatrixMode( %GL_PROJECTION )

    glLoadMatrixD(g_FontProjMatrix(0))

  glMatrixMode( %GL_MODELVIEW )
  'glMatrixMode( %GL_MODELVIEW )

  '  glLoadMatrixD(g_FontModeMatrix(0))

  IF gUseTexture = 0 THEN GLDISABLE %GL_TEXTURE_2D
  IF LastBindedTexture > 0 THEN glBindTexture %GL_TEXTURE_2D, lpTexture( LastBindedTexture )

  g_FontBeginEnd = 0

END SUB

' -- Prints font
SUB Exec_TBGL_PrintBMP( )
  REGISTER e AS LONG, lenText AS LONG, position AS EXT
  LOCAL TXT AS STRING, x AS EXT, y AS EXT, tAlign AS EXT


  LOCAL FLine AS SINGLE, fcode AS LONG
  LOCAL nParsed AS LONG

  nParsed = thinBasic_Parse1StringXNumbers( 2, 3, TXT, x, y, tAlign )

  IF nParsed = 3 THEN

    IF g_BMPFont( g_TBGL_SetActiveBMPFont ).fType = %TBGL_3D THEN
      ' -- RIGHT
      IF tAlign > 0 AND tAlign < 100 THEN

        tAlign = tAlign - 10
        x = HausNumero2x * ( g_Win.xRes / g_Win.yRes ) / g_BMPFont( g_TBGL_SetActiveBMPFont ).fSize - LEN( TXT ) + x + 1

        ' -- LEFT
      ELSEIF tAlign < 0 THEN

        tAlign = ABS( tAlign ) - 10
        x = 1 + x

        ' -- CENTER
      ELSEIF tAlign > 99 THEN

        tAlign = tAlign - 100
        x = ( g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxX - LEN( TXT )) / 2 + x + 1

      END IF
      IF tAlign THEN y = CHOOSE( tAlign, 1, g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxY, g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxY / 2 + 0.5 ) + y    ' -- Mysterious, isn't it
    ELSE
      ' -- RIGHT
      IF tAlign > 0 AND tAlign < 100 THEN

        tAlign = tAlign - 10
        x = g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxX - LEN( TXT ) + x + 1

        ' -- LEFT
      ELSEIF tAlign < 0 THEN

        tAlign = ABS( tAlign ) - 10
        x = 1 + x

        ' -- CENTER
      ELSEIF tAlign > 99 THEN

        tAlign = tAlign - 100
        x = ( g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxX - LEN( TXT )) / 2 + x + 1

      END IF
      IF tAlign THEN y = CHOOSE( tAlign, 1, g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxY, g_BMPFont( g_TBGL_SetActiveBMPFont ).fMaxY / 2 + 0.5 ) + y    ' -- Mysterious, isn't it
    END IF
  END IF

  IF g_BMPFont( g_TBGL_SetActiveBMPFont ).fType = %TBGL_3D THEN
    Fline = 0.414! - (( y ) * g_BMPFont( g_TBGL_SetActiveBMPFont ).fSize )
    position = (( - 0.414 ) * ( g_Win.xRes / g_Win.yRes )) + ( g_BMPFont( g_TBGL_SetActiveBMPFont ).fSize * ( x - 2 ))    ' (fSize*(1.5+lx))
  ELSE
    Fline = y
    position   = x
  END IF

  lenText = LEN( TXT )

  IF g_FontBeginEnd = 0 THEN
    glGetIntegerV(%GL_TEXTURE_2D, gUseTexture)
    IF gUseTexture = 0 THEN GLENABLE %GL_TEXTURE_2D
    glBindTexture %GL_TEXTURE_2D, lpTextureFont( g_TBGL_SetActiveBMPFont - 1 )    ' -- Texture array from zero
  END IF

  glPushMatrix
    glTranslatef position, FLine, IIF(g_BMPFont( g_TBGL_SetActiveBMPFont ).fType = %TBGL_3D, -1, 0)

    FOR e = 1 TO lenText
      fcode = g_BMPFont( g_TBGL_SetActiveBMPFont ).Listbase + ASC( TXT, e )
      glTranslatef g_BMPFont( g_TBGL_SetActiveBMPFont ).fSize, 0, 0
      IF fcode <> 289 THEN glCallList fcode    ' -- $SPC = ASC32 is NOT rendered
    NEXT e

  glPopMatrix

  IF g_FontBeginEnd = 0 THEN
    IF gUseTexture = 0 THEN GLDISABLE %GL_TEXTURE_2D
    IF LastBindedTexture > 0 THEN glBindTexture %GL_TEXTURE_2D, lpTexture( LastBindedTexture )
  END IF

END SUB

' -- Sets active bitmap font
SUB Exec_TBGL_SetActiveBMPFont( )
  LOCAL SlotNumber AS LONG

  SlotNumber = thinBASIC_Parse1Number( )
  IF SlotNumber < 1 OR SlotNumber > %LIMIT_MAX_FONTINDEX + 1 THEN
    TBGLError "Statement:" + $TAB + "TBGL_SetActiveBMPFont" + $CRLF + "Problem:" + $TAB + $TAB + "Font slot ID must be in range [1," + STR$( %LIMIT_MAX_FONTINDEX + 1 ) + "]" + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( SlotNumber )
  ELSE
    g_TBGL_SetActiveBMPFont = SlotNumber
  END IF

END SUB

' =============================================================================

SUB internal_FontBMPCleanup( BYVAL slotNumber AS LONG )
  IF g_BMPFont( slotNumber ).ListBase <> 0 THEN
    glDeleteLists g_BMPFont( slotNumber ).ListBase, 256
    RESET g_BMPFont( slotNumber )
  END IF
END SUB

SUB internal_FontCleanup( BYVAL slotNumber AS LONG )

  IF g_WinFont( SlotNumber ).fHandle > 0 THEN

    GLDELETELISTS g_WinFont( SlotNumber ).ListBase, 256

    SelectObject g_Win.hDC, g_WinFont( SlotNumber ).fHandle
    DeleteObject g_WinFont( SlotNumber ).fHandle

    RESET g_WinFont( SlotNumber )
  END IF

END SUB

SUB internal_ManagedRasterPos(BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)

  IF g_renderMatrix2D.top < g_renderMatrix2D.bottom THEN y = ABS(g_renderMatrix2D.bottom-g_renderMatrix2D.top) - y

  glPushAttrib(%GL_TRANSFORM_BIT OR %GL_VIEWPORT_BIT)

    glMatrixMode(%GL_PROJECTION)
    glPushMatrix()
      glLoadIdentity()

      glMatrixMode(%GL_MODELVIEW)
      glPushMatrix

        glLoadIdentity()

        glDepthRange(z, z)
        glViewport(INT(x-1), INT(y-1), 2, 2)

        LOCAL fx, fy AS SINGLE
        fx = x - INT(x)
        fy = y - INT(y)

        glRasterPos3f(fx, fy, 0)

      glPopMatrix()
      glMatrixMode(%GL_PROJECTION)

    glPopMatrix()

  glPopAttrib()

END SUB

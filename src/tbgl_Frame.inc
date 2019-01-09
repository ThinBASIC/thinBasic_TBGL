'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to rendering of frame
' =============================================================================

' -----------------------------------------------------------------------------

%TBGL_CLEAR_COLOR  = 1
%TBGL_CLEAR_DEPTH  = 2
%TBGL_RESET_MATRIX = 4

%TBGL_PINFO_RGB = 123
%TBGL_PINFO_XYZ = 456

' -- Related to measuring FrameRate
GLOBAL freq AS QUAD
GLOBAL qLastTime AS QUAD
GLOBAL qThisTime AS QUAD
GLOBAL dLastTime AS DWORD
GLOBAL dThisTime AS DWORD
GLOBAL qFPS AS EXT
GLOBAL qDivisor AS QUAD
GLOBAL dDivisor AS QUAD

' -----------------------------------------------------------------------------

%TBGL_OPENGL_ORTHO_2D  = 0
%TBGL_PIXEL_PERFECT_2D = 1

type tRenderMatrix2D
  pixelPerfect as long
  pixelOffsetX as single
  pixelOffsetY as single

  left         as single
  right        as single
  top          as single
  bottom       as single
end type

global g_renderMatrix2D as tRenderMatrix2D

sub resource_Frame_Init()
  reset g_renderMatrix2D
end sub

' -----------------------------------------------------------------------------

' -- Clears specified buffers and can reset matrix
SUB Exec_TBGL_ClearFrame( )

  LOCAL eFlags AS EXT
  IF thinBasic_CheckOpenParens_Optional THEN
    thinBASIC_ParseNumber( eFlags )
    thinBasic_CheckCloseParens_Mandatory

    LOCAL LFlags, GLFlags AS LONG
    LFlags = eFlags

    IF BIT( LFlags, 0 ) THEN GLFlags = %GL_COLOR_BUFFER_BIT
    IF BIT( LFlags, 1 ) THEN GLFlags = GLFlags OR %GL_DEPTH_BUFFER_BIT
    glClear GLFlags
    IF BIT( LFlags, 2 ) THEN glLoadIdentity

  ELSE

    glClear( %GL_COLOR_BUFFER_BIT OR %GL_DEPTH_BUFFER_BIT )
    glLoadIdentity( )

  END IF

END SUB

' -- Swaps back and front buffer
SUB Exec_TBGL_DrawFrame( )

  wglSwapBuffers( g_Win.hDC )

END SUB

' -- Sets rendering matrix to 3D
SUB Exec_TBGL_RenderMatrix3D( )
  LOCAL Ratio AS DOUBLE
  LOCAL eType, eRatio AS EXT
  LOCAL c AS RECT

  glLoadIdentity( )    ' [+]
  glMatrixMode %GL_PROJECTION    'Switch to Projection-Matrix mode.
  glLoadIdentity( )    'Reset the Projection-Matrix.

  g_customViewportRatioActive = %FALSE
  IF thinBasic_CheckOpenParens_Optional THEN

    thinBasic_ParseNumber eType
    IF thinBasic_CheckComma_Optional( ) THEN
      thinBasic_ParseNumber eRatio

      g_customViewportRatio       = eRatio
      g_customViewportRatioActive = %TRUE
    ELSE

      IF eType = %TBGL_VIEWPORT THEN
        GLGETINTEGERV( %GL_VIEWPORT, VIEWPORT( 0 ))
        eRatio = VIEWPORT( 2 ) / VIEWPORT( 3 )
      ELSE    ' %TBGL_CLIENTAREA
        GetClientRect( g_Win.handle, c )
        eRatio = c.nRight / c.nBottom

      END IF
    END IF

    Ratio = eRatio
    thinBasic_CheckCloseParens_Mandatory

  ELSE
    GetClientRect( g_Win.handle, c )
    Ratio = c.nRight / c.nBottom

  END IF

  gluPerspective( g_defaultFOV, Ratio, 0.09, g_Win.DrawDistance )
  glMatrixMode %GL_MODELVIEW    'Select the Modelview-Matrix

  g_Win.RenderMatrixMode = %TBGL_3D

END SUB


' -- Sets 2D mode, can have custom coordinate system
sub Exec_TBGL_RenderMatrix2D( )
  local minx, miny, maxy, maxx, mode2d as ext

  mode2d = %TBGL_OPENGL_ORTHO_2D

  if thinBasic_CheckOpenParens_Optional then
    thinBasic_ParseNumber minx
    if thinBasic_CheckComma_Mandatory( ) then
      thinBasic_ParseNumber miny
      if thinBasic_CheckComma_Mandatory( ) then
        thinBasic_ParseNumber maxx
        if thinBasic_CheckComma_Mandatory( ) then
          thinBasic_ParseNumber maxy

          if thinBasic_CheckComma_Optional( ) then
            thinBasic_ParseNumber mode2d
          end if

          thinBasic_CheckCloseParens_Mandatory

          ' -- This overrides default off-by-one OpenGL rasterization via simple hack
          g_renderMatrix2d.pixelPerfect = mode2d
          if mode2d = %TBGL_PIXEL_PERFECT_2D then
            if minx < maxx then
              incr maxx
              g_renderMatrix2d.pixelOffsetX = 0.5
            else
              decr maxx
              g_renderMatrix2d.pixelOffsetX =-0.5
            end if

            if miny < maxy then
              incr maxy
              g_renderMatrix2d.pixelOffsetY = 0.5
            else
              decr maxy
              g_renderMatrix2d.pixelOffsetY =-0.5
            end if
          end if

        end if
      end if
    end if

  else

    local c as RECT
    GetClientRect( g_Win.handle, c )

    minx = 0
    miny = 0
    maxx = c.nRight
    maxy = c.nBottom

  end if

  glLoadIdentity( )
  glMatrixMode( %GL_PROJECTION )

  glLoadIdentity( )
  glOrtho(minx-g_renderMatrix2d.pixelOffsetX, maxx-g_renderMatrix2d.pixelOffsetX, miny-g_renderMatrix2d.pixelOffsetY, maxy-g_renderMatrix2d.pixelOffsetY, 0, 1)
  glMatrixMode( %GL_MODELVIEW )

  g_Win.RenderMatrixMode = %TBGL_2D

  g_renderMatrix2D.left   = minX
  g_renderMatrix2D.bottom = minY
  g_renderMatrix2D.right  = maxX
  g_renderMatrix2D.top    = maxY

end sub


' -- Retrieves rendering mode
FUNCTION Exec_TBGL_GetRenderMatrixMode( ) AS EXT
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = g_Win.RenderMatrixMode
END FUNCTION


' -- Uses V-Sync, if supported
FUNCTION Exec_TBGL_UseVSync( ) AS EXT

  LOCAL flag AS EXT
  LOCAL returnVal AS LONG
  flag = thinBasic_Parse1Number

  IF ptr_wglSwapIntervalEXT = 0 THEN FUNCTION = 1 : EXIT FUNCTION

  CALL DWORD ptr_wglSwapIntervalEXT USING prototype_wglSwapIntervalEXT( flag ) TO ReturnVal
  FUNCTION = ReturnVal

END FUNCTION

FUNCTION Exec_TBGL_GetVSyncMaxFramerate( ) AS EXT
  LOCAL ScrnMode AS DEVMODE

  IF thinBasic_CheckOpenParens_Optional() THEN thinBasic_CheckCloseParens_Optional()

  EnumDisplaySettings(BYVAL 0, %ENUM_CURRENT_SETTINGS, ScrnMode)
  FUNCTION = ScrnMode.dmDisplayFrequency

END FUNCTION

' -- Saves BMP screenshot
FUNCTION Exec_TBGL_SaveScreenShot( ) AS EXT

  LOCAL ret AS LONG
  LOCAL sBmpFile AS STRING
  LOCAL hBMPDC, hMemDC, hMemBmp AS LONG
  LOCAL rc AS RECT
  LOCAL bm AS BITMAP
  LOCAL bmi AS BITMAPINFO
  LOCAL bmpFileHdr AS BITMAPFILEHEADER
  LOCAL bmpInfoHdr AS BITMAPINFOHEADER
  LOCAL hFile AS LONG
  LOCAL hasParens AS LONG

  LOCAL capturemode AS EXT
  capturemode = %TBGL_WINDOWAREA

  'thinBasic_Parse1StringXNumbers(0,1, sBmpFile, capturemode)
  IF thinBasic_CheckOpenParens_Optional THEN hasParens = %TRUE
  thinBasic_ParseString sBmpFile
  IF thinBasic_CheckComma_Optional THEN   thinBasic_ParseNumber capturemode
  IF hasParens = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

  IF INSTR( UCASE$( sBmpFile ), ".BMP" ) = 0 THEN sBmpFile = sBmpFile + ".Bmp"

  IF capturemode = %TBGL_WINDOWAREA THEN
    GetWindowRect g_Win.handle, rc    ' Window position
  ELSE
    GetClientRect g_Win.handle, rc    ' Window position

    LOCAL p AS POINT
    p.x = rc.nleft
    p.y = rc.ntop
    ClientToScreen( g_Win.handle, p )
    rc.nLeft = p.x
    rc.nTop = p.y

    p.x = rc.nright
    p.y = rc.nbottom
    ClientToScreen( g_Win.handle, p )
    rc.nright = p.x
    rc.nbottom = p.y

  END IF

  ' -- Create hidden DC and copy window contents
  hBMPDC = CreateDC( "DISPLAY", BYVAL %Null, BYVAL %Null, BYVAL %Null )
  hMemDC = CreateCompatibleDC( hBMPDC )
  bmi.bmiHeader.biSize = SIZEOF( bmi.bmiHeader )
  bmi.bmiHeader.biWidth = ( rc.nRight - rc.nLeft )
  bmi.bmiHeader.biHeight = ( rc.nBottom - rc.nTop )
  bmi.bmiHeader.biPlanes = 1
  bmi.bmiHeader.biBitCount = 24
  bmi.bmiHeader.biCompression = %BI_RGB

  hMemBmp = CreateDIBSection( hMemDC, bmi, %DIB_RGB_COLORS, 0, 0, 0 )
  GlobalLock hMemBmp
  SelectObject hMemDC, hMemBmp
  GetObject hMemBmp, SIZEOF( bm ), bm
  BitBlt hMemDC, 0, 0, bm.bmWidth, bm.bmHeight, hBMPDC, rc.nLeft, rc.nTop, %SRCCOPY
  bmpFileHdr.bfType = CVI( "BM" )
  bmpFileHdr.bfSize = LEN( bmpFileHdr ) + LEN( bmpInfoHdr ) + bm.bmWidthBytes * bm.bmHeight
  bmpFileHdr.bfOffBits = 54
  bmpInfoHdr.biSize = 40
  bmpInfoHdr.biWidth = bm.bmWidth
  bmpInfoHdr.biHeight = bm.bmHeight
  bmpInfoHdr.biPlanes = 1
  bmpInfoHdr.biBitCount = 24
  bmpInfoHdr.biSizeImage = 54 + bm.bmWidthBytes * bm.bmHeight

  DIM sBuffer AS STRING
  IF capturemode = %TBGL_CLIENTAREA THEN
    sBuffer = SPACE$(bm.bmWidth * bm.bmHeight * 3)
    glPixelStorei(%GL_PACK_ALIGNMENT, 1)
    glReadPixels(0, 0, bm.bmWidth, bm.bmHeight, %GL_BGR, %GL_UNSIGNED_BYTE, BYVAL STRPTR(sBuffer))
  END IF

  ' -- Write to disk
  hFile = FREEFILE
  ERRCLEAR
  OPEN sBmpFile FOR OUTPUT AS #hFile
    IF capturemode = %TBGL_CLIENTAREA THEN
      PRINT #hFile, bmpFileHdr bmpInfoHdr sBuffer
    ELSE
      PRINT #hFile, bmpFileHdr bmpInfoHdr PEEK$( bm.bmBits, bm.bmWidthBytes * bm.bmHeight )
    END IF
    FLUSH #hFile
  CLOSE hFile

  DeleteDC hBMPDC : DeleteDC hMemDC : GlobalUnlock hMemBmp : DeleteObject hMemBmp

  FUNCTION = ERR

END FUNCTION

' -- Returns window BMP as string
FUNCTION Exec_TBGL_GetWindowBMP( ) AS STRING

  LOCAL hBMPDC AS LONG
  LOCAL hMemDC AS LONG
  LOCAL rc AS RECT
  LOCAL hMemBmp AS LONG
  LOCAL bm AS BITMAP
  LOCAL bmi AS BITMAPINFO
  LOCAL bmpFileHdr AS BITMAPFILEHEADER
  LOCAL bmpInfoHdr AS BITMAPINFOHEADER

  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL handleW AS EXT
  LOCAL captureMode AS EXT
  capturemode = %TBGL_WINDOWAREA

  LOCAL ParensPresent AS LONG

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber handleW
    IF thinBasic_CheckComma_Optional THEN
      thinBasic_ParseNumber capturemode
      IF thinBasic_CheckComma_Optional THEN
        thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )
        IF thinBasic_CheckComma_Optional THEN
          thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )

        END IF
      END IF
    END IF
  END IF

  IF thinBasic_CheckCloseParens_Mandatory THEN

    IF capturemode = %TBGL_WINDOWAREA THEN
      'GetWindowRect g_Win.handle, rc    ' Window position
      GetWindowRect handleW, rc    ' Window position
    ELSE
      'GetWindowRect g_Win.HANDLE, rc    ' Window position
      GetWindowRect handleW, rc    ' Window position

      LOCAL p AS POINT
      p.x = rc.nleft
      p.y = rc.ntop
      'ClientToScreen( g_Win.handle, p )
      ClientToScreen( handleW, p )
      rc.nLeft = p.x
      rc.nTop = p.y

      p.x = rc.nright
      p.y = rc.nbottom
      'ClientToScreen( g_Win.handle, p )
      ClientToScreen( handleW, p )
      rc.nright = p.x
      rc.nbottom = p.y

    END IF

    hBMPDC = CreateDC( "DISPLAY", BYVAL %Null, BYVAL %Null, BYVAL %Null )
    hMemDC = CreateCompatibleDC( hBMPDC )
    bmi.bmiHeader.biSize = SIZEOF( bmi.bmiHeader )
    bmi.bmiHeader.biWidth = ( rc.nRight - rc.nLeft )
    bmi.bmiHeader.biHeight = ( rc.nBottom - rc.nTop )
    bmi.bmiHeader.biPlanes = 1
    bmi.bmiHeader.biBitCount = 24
    bmi.bmiHeader.biCompression = %BI_RGB

    hMemBmp = CreateDIBSection( hMemDC, bmi, %DIB_RGB_COLORS, 0, 0, 0 )
    GlobalLock hMemBmp
    SelectObject hMemDC, hMemBmp
    GetObject hMemBmp, SIZEOF( bm ), bm
    BitBlt hMemDC, 0, 0, bm.bmWidth, bm.bmHeight, hBMPDC, rc.nLeft, rc.nTop, %SRCCOPY
    bmpFileHdr.bfType = CVI( "BM" )
    bmpFileHdr.bfSize = LEN( bmpFileHdr ) + LEN( bmpInfoHdr ) + bm.bmWidthBytes * bm.bmHeight
    bmpFileHdr.bfOffBits = 54
    bmpInfoHdr.biSize = 40
    bmpInfoHdr.biWidth = bm.bmWidth
    bmpInfoHdr.biHeight = bm.bmHeight
    bmpInfoHdr.biPlanes = 1
    bmpInfoHdr.biBitCount = 24
    bmpInfoHdr.biSizeImage = 54 + bm.bmWidthBytes * bm.bmHeight

    ' -- Write to return value
    LOCAL sBuffer AS STRING
    sBuffer = PEEK$( VARPTR( bmpFileHdr ), SIZEOF( bmpFileHdr )) + PEEK$( VARPTR( bmpInfoHdr ), SIZEOF( bmpInfoHdr )) + PEEK$( bm.bmBits, bm.bmWidthBytes * bm.bmHeight )

    DeleteDC hBMPDC : DeleteDC hMemDC : GlobalUnlock hMemBmp : DeleteObject hMemBmp

    IF lVariablePtr1 THEN thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, bm.bmWidth  )
    IF lVariablePtr2 THEN thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, bm.bmHeight )

    FUNCTION = sBuffer
  END IF

END FUNCTION

' -- Sets visible distance
SUB Exec_TBGL_SetDrawDistance( )

  LOCAL flag AS EXT

  flag = thinBasic_Parse1Number
  g_Win.DrawDistance = flag

  internal_RefreshViewport( )    ' !!!

END SUB

' -- Sets viewport using absolute or relative coordinates
SUB Exec_TBGL_Viewport( )

  LOCAL xs AS LONG, ys AS LONG
  LOCAL x, y, xx, yy AS EXT
  LOCAL vunits AS EXT
  LOCAL dimension AS RECT

  thinBasic_ParseXNumbers( 4, 5, x, y, xx, yy, vunits )

  IF vunits = %TBGL_PARAM_RELATIVE THEN
    GetClientRect( g_Win.handle, dimension )

    xs = dimension.nRight
    ys = dimension.nBottom

    x = x * xs
    y = ys - ( y * ys ) - ( yy * ys )

    xx = xx * xs
    yy = yy * ys
  ELSEIF vunits = %TBGL_PARAM_PIXELS THEN
    GetClientRect( g_Win.handle, dimension )
    ys = dimension.nBottom

''    y = Viewport( 3 ) - y - yy    ' -- That is openGL matrix, dangerous because of recursion
    y = ys - y - yy    ' -- That is openGL matrix
  END IF
  glViewport x, y, xx, yy

END SUB

' -- [!] This function is used for measuring FPS in case HW supports performance counters
FUNCTION Exec_TBGL_GetFrameRate_PERFCOUNTER( ) AS EXT

  QueryPerformanceCounter( qThisTime )
  qDivisor = qThisTime - qLastTime

  IF qDivisor = 0 THEN qDivisor = freq
  qFPS = freq / qDivisor

  IF qFPS < 1 THEN qFPS = 1
  qLastTime = qThisTime

  FUNCTION = qFPS

END FUNCTION

' -- [!] This function is used for measuring FPS in case HW DOES NOT SUPPORT performance counters
FUNCTION Exec_TBGL_GetFrameRate_GETTICKCOUNT( ) AS EXT

  dThisTime = GetTickCount
  dDivisor = dThisTime - dLastTime

  IF dDivisor = 0 THEN dDivisor = 1
  FUNCTION = 1000 / dDivisor

  dLastTime = dThisTime

END FUNCTION

' -- Retrieves info on RGB or position of fragment at given pixel
SUB Exec_TBGL_GetPixelInfo( )
  LOCAL x, y AS EXT
  LOCAL returnWhat AS EXT


  '---Declare needed variables. To change a variable we need
  '---a reference pointer and an aboslute position (in case of arrays)
  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL lVariablePtr3 AS LONG
  LOCAL lVariableAbsPos3 AS LONG

  LOCAL eResult1 AS EXT
  LOCAL eResult2 AS EXT
  LOCAL eResult3 AS EXT

  LOCAL winz AS SINGLE

  LOCAL ParensPresent AS LONG
  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE

  LOCAL PleaseIgnore AS LONG

  GLFLUSH

  thinBasic_ParseNumber x
  IF thinBasic_CheckComma( ) THEN
    thinBasic_ParseNumber y
    IF thinBasic_CheckComma( ) THEN
      thinBasic_ParseNumber returnWhat
      IF thinBasic_CheckComma( ) THEN
        '---Check if any error
        IF thinBasic_ErrorFree THEN
          '---Get reference of first variable
          thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )

          IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
            thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )

            IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
              thinBasic_VariableParse( lVariablePtr3, lVariableAbsPos3 )

              IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory


              IF thinBasic_ErrorFree THEN

                LOCAL tx, ty AS LONG
                LOCAL rc AS rect
                GetClientRect( g_Win.handle, rc )
                tx = rc.nright
                ty = rc.nbottom

                IF x = - 1 OR y = - 1 THEN PleaseIgnore = 1

                y = ty - y - 1

                IF windowsizing THEN EXIT SUB

                '---Here do what you need to do
                IF PleaseIgnore = 0 THEN
                  SELECT CASE AS LONG returnWhat

                    CASE %TBGL_PINFO_RGB

                      DIM retValRGB( 1 TO 4 ) AS LOCAL BYTE
                      IF gvOpenGL >= 1200 THEN    ' -- OpenGL 1.2+
                        GLREADPIXELS( x, y, 1, 1, %GL_BGRA, %GL_UNSIGNED_BYTE, retValRGB( 1 ))
                        eResult1 = retValRGB( 3 )
                        eResult2 = retValRGB( 2 )
                        eResult3 = retValRGB( 1 )
                      ELSE
                        GLREADPIXELS( x, y, 1, 1, %GL_RGB, %GL_UNSIGNED_BYTE, retValRGB( 1 ))    ' 0.2.1
                        eResult1 = retValRGB( 1 )
                        eResult2 = retValRGB( 2 )
                        eResult3 = retValRGB( 3 )
                      END IF


                    CASE %TBGL_PINFO_XYZ
                      LOCAL dX, dy, dz AS DOUBLE
                      GLREADPIXELS( x, y, 1, 1, %GL_DEPTH_COMPONENT, %GL_FLOAT, winz )

                      GLGETINTEGERV( %GL_VIEWPORT, VIEWPORT( 0 ))
                      GLGETDOUBLEV( %GL_MODELVIEW_MATRIX, mvmatrix( 0 ))
                      GLGETDOUBLEV( %GL_PROJECTION_MATRIX, projmatrix( 0 ))

                      GLUUNPROJECT( x, y, winz, mvmatrix( 0 ), projmatrix( 0 ), VIEWPORT( 0 ), dX, dY, dZ )    '
                      eResult1 = dx
                      eResult2 = dy
                      eResult3 = dz

                  END SELECT
                END IF

                '---Now assign values to referenced variables
                thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, eResult1 )
                thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, eResult2 )
                thinBasic_ChangeVariableNumberDirect( lVariablePtr3, lVariableAbsPos3, eResult3 )

              END IF
            END IF
          END IF
        END IF
      END IF
    END IF
  END IF

END SUB

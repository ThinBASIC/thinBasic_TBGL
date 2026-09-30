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

' -- Clears specified buffers and can reset matrix, flags combine %TBGL_CLEAR_COLOR, %TBGL_CLEAR_DEPTH, %TBGL_RESET_MATRIX
SUB tbgl_ClearFrame ALIAS "tbgl_ClearFrame" ( BYVAL LFlags AS LONG ) EXPORT

  LOCAL GLFlags AS DWORD

  IF BIT( LFlags, 0 ) THEN GLFlags = %GL_COLOR_BUFFER_BIT
  IF BIT( LFlags, 1 ) THEN GLFlags = GLFlags OR %GL_DEPTH_BUFFER_BIT
  glClear GLFlags
  IF BIT( LFlags, 2 ) THEN glLoadIdentity

END SUB

' -- Swaps back and front buffer
SUB tbgl_DrawFrame ALIAS "tbgl_DrawFrame" ( ) EXPORT

  wglSwapBuffers( g_Win.hDC )
  dialog doevents

END SUB

' -- Sets rendering matrix to 3D
' -- ratioType: %TBGL_CLIENTAREA, %TBGL_VIEWPORT, or %TBGL_CUSTOM to use customRatio
SUB tbgl_RenderMatrix3D ALIAS "tbgl_RenderMatrix3D" ( BYVAL ratioType AS LONG, BYVAL customRatio AS DOUBLE ) EXPORT
  LOCAL Ratio AS DOUBLE
  LOCAL c AS RECT

  glLoadIdentity( )    ' [+]
  glMatrixMode %GL_PROJECTION    'Switch to Projection-Matrix mode.
  glLoadIdentity( )    'Reset the Projection-Matrix.

  g_customViewportRatioActive = %FALSE

  IF ratioType = %TBGL_CUSTOM THEN
    g_customViewportRatio       = customRatio
    g_customViewportRatioActive = %TRUE
    Ratio = customRatio

  ELSEIF ratioType = %TBGL_VIEWPORT THEN
    GLGETINTEGERV( %GL_VIEWPORT, VIEWPORT( 0 ))
    Ratio = VIEWPORT( 2 ) / VIEWPORT( 3 )

  ELSE    ' %TBGL_CLIENTAREA
    GetClientRect( g_Win.handle, c )
    Ratio = c.nRight / c.nBottom

  END IF

  gluPerspective( g_defaultFOV, Ratio, 0.09, g_Win.DrawDistance )
  glMatrixMode %GL_MODELVIEW    'Select the Modelview-Matrix

  g_Win.RenderMatrixMode = %TBGL_3D

END SUB


' -- Applies 2D projection for given coordinate system, stores it
SUB internal_ApplyRenderMatrix2D( BYVAL minx AS SINGLE, BYVAL miny AS SINGLE, BYVAL maxx AS SINGLE, BYVAL maxy AS SINGLE )

  glLoadIdentity( )
  glMatrixMode( %GL_PROJECTION )

  glLoadIdentity( )

  ' [!] The nearVal, farVal combo of -1, 1 is essential for Sprite functionality
  glOrtho(minx-g_renderMatrix2d.pixelOffsetX, maxx-g_renderMatrix2d.pixelOffsetX, miny-g_renderMatrix2d.pixelOffsetY, maxy-g_renderMatrix2d.pixelOffsetY, -1, 1)

  glMatrixMode( %GL_MODELVIEW )

  g_Win.RenderMatrixMode = %TBGL_2D

  g_renderMatrix2D.left   = minX
  g_renderMatrix2D.bottom = minY
  g_renderMatrix2D.right  = maxX
  g_renderMatrix2D.top    = maxY

end sub

' -- Sets 2D mode with coordinates matching client area pixels, keeps current pixel perfect setting
sub tbgl_RenderMatrix2D alias "tbgl_RenderMatrix2D" ( ) export

  local c as RECT
  GetClientRect( g_Win.handle, c )

  internal_ApplyRenderMatrix2D( 0, 0, c.nRight, c.nBottom )

end sub

' -- Sets 2D mode with custom coordinate system, mode2d is %TBGL_OPENGL_ORTHO_2D or %TBGL_PIXEL_PERFECT_2D
sub tbgl_RenderMatrix2DEx alias "tbgl_RenderMatrix2DEx" ( byval minx as single, byval miny as single, byval maxx as single, byval maxy as single, byval mode2d as long ) export

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
  else
    g_renderMatrix2d.pixelOffsetX = 0
    g_renderMatrix2d.pixelOffsetY = 0
  end if

  internal_ApplyRenderMatrix2D( minx, miny, maxx, maxy )

end sub


' -- Retrieves rendering mode
FUNCTION tbgl_GetRenderMatrixMode ALIAS "tbgl_GetRenderMatrixMode" ( ) EXPORT AS LONG
  FUNCTION = g_Win.RenderMatrixMode
END FUNCTION


' -- Uses V-Sync, if supported; interval is number of vertical retraces between swaps (0 = off)
FUNCTION tbgl_UseVSync ALIAS "tbgl_UseVSync" ( BYVAL interval AS LONG ) EXPORT AS LONG

  LOCAL returnVal AS LONG

  IF ptr_wglSwapIntervalEXT = 0 THEN FUNCTION = 1 : EXIT FUNCTION

  CALL DWORD ptr_wglSwapIntervalEXT USING prototype_wglSwapIntervalEXT( interval ) TO ReturnVal
  FUNCTION = ReturnVal

END FUNCTION

FUNCTION tbgl_GetVSyncMaxFramerate ALIAS "tbgl_GetVSyncMaxFramerate" ( ) EXPORT AS LONG
  LOCAL ScrnMode AS DEVMODE

  EnumDisplaySettings(BYVAL 0, %ENUM_CURRENT_SETTINGS, ScrnMode)
  FUNCTION = ScrnMode.dmDisplayFrequency

END FUNCTION

' -- Saves BMP screenshot, captureMode is %TBGL_WINDOWAREA or %TBGL_CLIENTAREA
FUNCTION tbgl_SaveScreenShot ALIAS "tbgl_SaveScreenShot" ( BYVAL sBmpFile AS STRING, BYVAL capturemode AS LONG ) EXPORT AS LONG

  LOCAL ret AS LONG
  LOCAL hBMPDC, hMemDC, hMemBmp AS LONG
  LOCAL rc AS RECT
  LOCAL bm AS BITMAP
  LOCAL bmi AS BITMAPINFO
  LOCAL bmpFileHdr AS BITMAPFILEHEADER
  LOCAL bmpInfoHdr AS BITMAPINFOHEADER
  LOCAL hFile AS LONG

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

' -- Returns window BMP as string, bitmap width and height are returned via parameters
FUNCTION tbgl_GetWindowBMP ALIAS "tbgl_GetWindowBMP" ( BYVAL handleW AS DWORD, BYVAL captureMode AS LONG, BYREF bmpWidth AS LONG, BYREF bmpHeight AS LONG ) EXPORT AS STRING

  LOCAL hBMPDC AS LONG
  LOCAL hMemDC AS LONG
  LOCAL rc AS RECT
  LOCAL hMemBmp AS LONG
  LOCAL bm AS BITMAP
  LOCAL bmi AS BITMAPINFO
  LOCAL bmpFileHdr AS BITMAPFILEHEADER
  LOCAL bmpInfoHdr AS BITMAPINFOHEADER

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

  bmpWidth  = bm.bmWidth
  bmpHeight = bm.bmHeight

  FUNCTION = sBuffer

END FUNCTION

' -- Sets visible distance
SUB tbgl_SetDrawDistance ALIAS "tbgl_SetDrawDistance" ( BYVAL drawDistance AS LONG ) EXPORT

  g_Win.DrawDistance = drawDistance

  internal_RefreshViewport( )    ' !!!

END SUB

' -- Sets viewport using absolute or relative coordinates, vunits is %TBGL_PARAM_RELATIVE or %TBGL_PARAM_PIXELS
SUB tbgl_Viewport ALIAS "tbgl_Viewport" ( BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL xx AS SINGLE, BYVAL yy AS SINGLE, BYVAL vunits AS LONG ) EXPORT

  LOCAL xs AS LONG, ys AS LONG
  LOCAL dimension AS RECT

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
FUNCTION internal_GetFrameRate_PERFCOUNTER( ) AS DOUBLE

  QueryPerformanceCounter( qThisTime )
  qDivisor = qThisTime - qLastTime

  IF qDivisor = 0 THEN qDivisor = freq
  qFPS = freq / qDivisor

  IF qFPS < 1 THEN qFPS = 1
  qLastTime = qThisTime

  FUNCTION = qFPS

END FUNCTION

' -- [!] This function is used for measuring FPS in case HW DOES NOT SUPPORT performance counters
FUNCTION internal_GetFrameRate_GETTICKCOUNT( ) AS DOUBLE

  dThisTime = GetTickCount
  dDivisor = dThisTime - dLastTime

  IF dDivisor = 0 THEN dDivisor = 1
  FUNCTION = 1000 / dDivisor

  dLastTime = dThisTime

END FUNCTION

' -- Returns current framerate based on time gap between last call and this one
FUNCTION tbgl_GetFrameRate ALIAS "tbgl_GetFrameRate" ( ) EXPORT AS DOUBLE

  IF freq THEN
    FUNCTION = internal_GetFrameRate_PERFCOUNTER( )
  ELSE
    FUNCTION = internal_GetFrameRate_GETTICKCOUNT( )
  END IF

END FUNCTION

' -- Prepares window coordinates for pixel read, returns 0 when info is not available (window resizing)
FUNCTION internal_PixelInfoPrepare( x AS LONG, y AS LONG, PleaseIgnore AS LONG ) AS LONG

  LOCAL ty AS LONG
  LOCAL rc AS rect

  GLFLUSH

  GetClientRect( g_Win.handle, rc )
  ty = rc.nbottom

  IF x = - 1 OR y = - 1 THEN PleaseIgnore = 1

  y = ty - y - 1

  IF windowsizing THEN EXIT FUNCTION

  FUNCTION = 1

END FUNCTION

' -- Retrieves RGB of pixel at given client coordinates, returns 0 (and leaves r, g, b untouched) when window is being resized
FUNCTION tbgl_GetPixelInfoRGB ALIAS "tbgl_GetPixelInfoRGB" ( BYVAL x AS LONG, BYVAL y AS LONG, BYREF r AS BYTE, BYREF g AS BYTE, BYREF b AS BYTE ) EXPORT AS LONG

  LOCAL PleaseIgnore AS LONG

  IF internal_PixelInfoPrepare( x, y, PleaseIgnore ) = 0 THEN EXIT FUNCTION

  r = 0 : g = 0 : b = 0

  IF PleaseIgnore = 0 THEN
    DIM retValRGB( 1 TO 4 ) AS LOCAL BYTE
    IF gvOpenGL >= 1200 THEN    ' -- OpenGL 1.2+
      GLREADPIXELS( x, y, 1, 1, %GL_BGRA, %GL_UNSIGNED_BYTE, retValRGB( 1 ))
      r = retValRGB( 3 )
      g = retValRGB( 2 )
      b = retValRGB( 1 )
    ELSE
      GLREADPIXELS( x, y, 1, 1, %GL_RGB, %GL_UNSIGNED_BYTE, retValRGB( 1 ))    ' 0.2.1
      r = retValRGB( 1 )
      g = retValRGB( 2 )
      b = retValRGB( 3 )
    END IF
  END IF

  FUNCTION = 1

END FUNCTION

' -- Retrieves 3D position of fragment at given client coordinates, returns 0 (and leaves outputs untouched) when window is being resized
FUNCTION tbgl_GetPixelInfoXYZ ALIAS "tbgl_GetPixelInfoXYZ" ( BYVAL x AS LONG, BYVAL y AS LONG, BYREF dX AS DOUBLE, BYREF dY AS DOUBLE, BYREF dZ AS DOUBLE ) EXPORT AS LONG

  LOCAL PleaseIgnore AS LONG
  LOCAL winz AS SINGLE

  IF internal_PixelInfoPrepare( x, y, PleaseIgnore ) = 0 THEN EXIT FUNCTION

  dX = 0 : dY = 0 : dZ = 0

  IF PleaseIgnore = 0 THEN
    GLREADPIXELS( x, y, 1, 1, %GL_DEPTH_COMPONENT, %GL_FLOAT, winz )

    GLGETINTEGERV( %GL_VIEWPORT, VIEWPORT( 0 ))
    GLGETDOUBLEV( %GL_MODELVIEW_MATRIX, mvmatrix( 0 ))
    GLGETDOUBLEV( %GL_PROJECTION_MATRIX, projmatrix( 0 ))

    GLUUNPROJECT( x, y, winz, mvmatrix( 0 ), projmatrix( 0 ), VIEWPORT( 0 ), dX, dY, dZ )    '
  END IF

  FUNCTION = 1

END FUNCTION

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

' -- Without parameters clears color and depth and resets matrix
SUB Exec_TBGL_ClearFrame( )

  LOCAL eFlags AS EXT
  IF thinBasic_CheckOpenParens_Optional THEN
    thinBASIC_ParseNumber( eFlags )
    thinBasic_CheckCloseParens_Mandatory
    tbgl_ClearFrame( eFlags )
  ELSE
    tbgl_ClearFrame( %TBGL_CLEAR_COLOR OR %TBGL_CLEAR_DEPTH OR %TBGL_RESET_MATRIX )
  END IF

END SUB

SUB Exec_TBGL_DrawFrame( )
  tbgl_DrawFrame( )
END SUB

' -- [( type [, ratio] )]: ratio given means custom ratio
SUB Exec_TBGL_RenderMatrix3D( )
  LOCAL eType, eRatio AS EXT

  IF thinBasic_CheckOpenParens_Optional THEN

    thinBasic_ParseNumber eType
    IF thinBasic_CheckComma_Optional( ) THEN
      thinBasic_ParseNumber eRatio
      eType = %TBGL_CUSTOM
    ELSEIF eType = %TBGL_CUSTOM THEN
      eType = %TBGL_CLIENTAREA
    END IF

    thinBasic_CheckCloseParens_Mandatory

  ELSE
    eType = %TBGL_CLIENTAREA
  END IF

  tbgl_RenderMatrix3D( eType, eRatio )

END SUB

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

          tbgl_RenderMatrix2DEx( minx, miny, maxx, maxy, mode2d )

        end if
      end if
    end if

  else
    tbgl_RenderMatrix2D( )
  end if

end sub

FUNCTION Exec_TBGL_GetRenderMatrixMode( ) AS EXT
  IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = tbgl_GetRenderMatrixMode( )
END FUNCTION

FUNCTION Exec_TBGL_UseVSync( ) AS EXT
  FUNCTION = tbgl_UseVSync( thinBasic_Parse1Number )
END FUNCTION

FUNCTION Exec_TBGL_GetVSyncMaxFramerate( ) AS EXT
  IF thinBasic_CheckOpenParens_Optional() THEN thinBasic_CheckCloseParens_Optional()
  FUNCTION = tbgl_GetVSyncMaxFramerate( )
END FUNCTION

' -- Capture mode defaults to %TBGL_WINDOWAREA
FUNCTION Exec_TBGL_SaveScreenShot( ) AS EXT

  LOCAL sBmpFile AS STRING
  LOCAL hasParens AS LONG
  LOCAL capturemode AS EXT
  capturemode = %TBGL_WINDOWAREA

  IF thinBasic_CheckOpenParens_Optional THEN hasParens = %TRUE
  thinBasic_ParseString sBmpFile
  IF thinBasic_CheckComma_Optional THEN   thinBasic_ParseNumber capturemode
  IF hasParens = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

  FUNCTION = tbgl_SaveScreenShot( sBmpFile, capturemode )

END FUNCTION

' -- ( hWnd [, captureMode [, widthVariable [, heightVariable ]]] )
FUNCTION Exec_TBGL_GetWindowBMP( ) AS STRING

  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL handleW AS EXT
  LOCAL captureMode AS EXT
  LOCAL bmpWidth, bmpHeight AS LONG
  capturemode = %TBGL_WINDOWAREA

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

    FUNCTION = tbgl_GetWindowBMP( handleW, captureMode, bmpWidth, bmpHeight )

    IF lVariablePtr1 THEN thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, bmpWidth  )
    IF lVariablePtr2 THEN thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, bmpHeight )

  END IF

END FUNCTION

SUB Exec_TBGL_SetDrawDistance( )
  tbgl_SetDrawDistance( thinBasic_Parse1Number )
END SUB

' -- Units default to %TBGL_PARAM_RELATIVE
SUB Exec_TBGL_Viewport( )
  LOCAL x, y, xx, yy AS EXT
  LOCAL vunits AS EXT

  thinBasic_ParseXNumbers( 4, 5, x, y, xx, yy, vunits )
  tbgl_Viewport( x, y, xx, yy, vunits )

END SUB

FUNCTION Exec_TBGL_GetFrameRate_PERFCOUNTER( ) AS EXT
  FUNCTION = internal_GetFrameRate_PERFCOUNTER( )
END FUNCTION

FUNCTION Exec_TBGL_GetFrameRate_GETTICKCOUNT( ) AS EXT
  FUNCTION = internal_GetFrameRate_GETTICKCOUNT( )
END FUNCTION

' -- ( x, y, %TBGL_PINFO_RGB | %TBGL_PINFO_XYZ, variable1, variable2, variable3 )
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

  LOCAL r, g, b AS BYTE
  LOCAL dX, dY, dZ AS DOUBLE
  LOCAL eResult1, eResult2, eResult3 AS EXT
  LOCAL available AS LONG

  LOCAL ParensPresent AS LONG
  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE

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

                SELECT CASE AS LONG returnWhat
                  CASE %TBGL_PINFO_RGB
                    available = tbgl_GetPixelInfoRGB( x, y, r, g, b )
                    eResult1 = r : eResult2 = g : eResult3 = b

                  CASE %TBGL_PINFO_XYZ
                    available = tbgl_GetPixelInfoXYZ( x, y, dX, dY, dZ )
                    eResult1 = dX : eResult2 = dY : eResult3 = dZ

                  CASE ELSE
                    ' -- Unknown request results in zeros, unless window is being resized
                    available = ISFALSE windowsizing
                END SELECT

                IF available = 0 THEN EXIT SUB

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

#ENDIF

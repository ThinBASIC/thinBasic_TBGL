'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles TBGL window
' =============================================================================

' -----------------------------------------------------------------------------
#INCLUDE "tbgl_Window.CDropTarget.inc"
' -----------------------------------------------------------------------------

GLOBAL windowSizing AS LONG   ' -- Is window sized now?
GLOBAL keyPressed() AS BYTE
GLOBAL tbgl_MouseWheelDelta AS LONG

%PeriodicFuncTimer = 1000
GLOBAL CallingWindow AS LONG
' -----------------------------------------------------------------------------

%TBGL_WS_WINDOWED            = 0
%TBGL_WS_FULLSCREEN          = 1
%TBGL_WS_MINIMIZEBOX         = 2
%TBGL_WS_MAXIMIZEBOX         = 4
%TBGL_WS_CLOSEBOX            = 8
%TBGL_WS_DONTKEEPASPECTRATIO = 16
%TBGL_WS_DONTSIZE            = 32

ENUM TBGL
  OnDropFiles
END ENUM

TYPE TBGL_OnEventPointers
  OnDropFiles AS LONG
END TYPE
GLOBAL g_OnEventPointers AS TBGL_OnEventPointers
GLOBAL g_OnDropFiles_Files AS STRING
' -----------------------------------------------------------------------------
SUB Resource_Window_Alloc()
  RESET g_OnEventPointers
  g_OnDropFiles_Files = ""

END SUB

SUB Resource_Window_Release()

  TBGL_UnBindWindowEvent(g_Win.handle, %TBGL.OnDropFiles)
  g_OnDropFiles_Files = ""

END SUB
' -----------------------------------------------------------------------------

' -- Macro to set bit depth to safe value when specified wrong one
MACRO m_Validate_gWin_Bitdepth
  IF g_Win.BitDepth <> 16 AND g_Win.BitDepth <> 24 AND g_Win.BitDepth <> 32 THEN g_Win.BitDepth = 16
END MACRO

' -----------------------------------------------------------------------------

' -- Function to create TBGL window, flags combine %TBGL_WS_* values, xPos = 0 and yPos = 0 center the window
FUNCTION tbgl_CreateWindowEx ALIAS "tbgl_CreateWindowEx" ( _
    BYVAL CAPTION AS STRING _
  , BYVAL xRes AS LONG _
  , BYVAL yRes AS LONG _
  , BYVAL Bpp AS BYTE _
  , BYVAL flags AS LONG _
  , BYVAL xPos AS LONG _
  , BYVAL yPos AS LONG _
  ) EXPORT AS DWORD

  LOCAL wce AS WndClassEx

  LOCAL flagsWindowed AS LONG

  ' -- Define and register OpenGL-Application class:
  g_Win.ClassName   = "OpenGL"
  wce.hInstance     = g_ModuleInstance
  wce.cbSize        = SIZEOF( wce )
  wce.lpszClassName = VARPTR( g_Win.ClassName )
  wce.lpfnWndProc   = CODEPTR( WndProc )
  wce.Style         = %CS_HREDRAW OR %CS_VREDRAW OR %CS_OWNDC
  wce.hCursor       = LoadCursor ( %Null, BYVAL %IDC_ARROW )
  wce.hIcon         = LoadIcon( g_ModuleInstance, "PROGRAM" )
  wce.hIconSm       = LoadIcon( g_ModuleInstance, "PROGRAM" )

  IF ISFALSE( RegisterClassEx( wce )) THEN
    TBGLERROR "TBGL_CreateWindowEx : Cannot register window class, Windows error" + STR$( GetLastError )
    EXIT FUNCTION
  END IF

  ' -- Set parameters
  ' -- [!] information is stored on bit positions according to %TBGL_WS_* flags

  g_Win.FullScreen = BIT( flags, 0 )  ' -- Fullscreen is specified by least significant bit

  g_Win.xRes = xRes
  g_Win.yRes = yRes

  g_Win.xPos = xPos
  g_Win.yPos = yPos

  g_Win.BitDepth = Bpp
  m_Validate_gWin_BitDepth

  g_Win.aspectRat# = g_Win.xRes / g_Win.yRes

  ' -- Creation of fullscreen window
  IF g_Win.FullScreen = 1 THEN
    g_Win.handle = CreateWindowEx( %WS_EX_LEFT, g_Win.ClassName, BYCOPY CAPTION, %WS_POPUP OR %WS_VISIBLE OR %WS_CLIPCHILDREN OR %WS_CLIPSIBLINGS, 0, 0, xRes, yRes, 0, 0, g_ModuleInstance, BYVAL 0 )
    internal_SetFullscreenMode( "TBGL_CreateWindowEx" )
  ELSE
  ' -- Creation of standard window
    flagsWindowed = g_Win.flagsWindowed

    ' -- [!] information is stored on bit positions according to %TBGL_WS_* flags
    IF BIT( flags, 1 ) OR BIT( flags, 2 ) OR BIT( flags, 3 ) THEN flagsWindowed = flagsWindowed OR %WS_SYSMENU

    IF BIT( flags, 1 )     THEN flagsWindowed    = flagsWindowed OR %WS_MINIMIZEBOX
    IF BIT( flags, 2 )     THEN flagsWindowed    = flagsWindowed OR %WS_MAXIMIZEBOX
    IF BIT( flags, 4 )     THEN g_Win.ignrAspect = 1
    IF BIT( flags, 5 ) = 0 THEN flagsWindowed    = flagsWindowed OR %WS_THICKFRAME

    g_Win.handle = CreateWindowEx( %WS_EX_LEFT, g_Win.ClassName, BYCOPY CAPTION, _
                                   flagsWindowed OR %WS_CLIPCHILDREN OR %WS_CLIPSIBLINGS, _
                                   xPos, yPos, xRes, yRes, %Null, %Null, g_ModuleInstance, BYVAL %Null )

    IF BIT( flags, 3 ) = 0 THEN    ' -- Close functionality is associated with system menu
      LOCAL hMenu, wFlags AS DWORD
      hMenu = GetSystemMenu( g_Win.handle, 0 )
      wFlags = %MF_BYCOMMAND OR %MF_GRAYED
      EnableMenuItem( hMenu, %SC_CLOSE, wFlags )
    END IF

    g_Win.flagsWindowed = flagsWindowed

  END IF

  IF ISFALSE( g_Win.handle ) THEN
    TBGLERROR "TBGL_CreateWindowEx : Cannot create window"
    EXIT FUNCTION
  END IF

  g_Win.controlParent = 0 ' -- This value is non 0 only for canvas
  FUNCTION = g_Win.handle

END FUNCTION

' -- TBGL window callback
FUNCTION WndProc( BYVAL hWnd AS DWORD, BYVAL wMsg AS DWORD, BYVAL wParam AS LONG, BYVAL lParam AS LONG ) AS LONG
  LOCAL wr AS RECT, cr AS RECT, x AS LONG, y AS LONG, xx AS LONG, yy AS LONG
  STATIC currentWidth, currentHeight, lastWidth, lastHeight AS LONG

  windowSizing = 0

  SELECT CASE AS LONG wMsg
    CASE %WM_ERASEBKGND           ' -- Avoid flickering
      FUNCTION = %TRUE

    CASE %WM_CREATE

      ' -- Check if created window has really the requested client area, if not, resize
      IF g_Win.FullScreen = 0 THEN
        LOCAL sx, sy AS LONG
        sx = GetSystemMetrics( %SM_CXSCREEN )
        sy = GetSystemMetrics( %SM_CYSCREEN )

        GetWindowRect hWnd, wr
        GetClientRect hWnd, cr

        x = wr.nLeft
        y = wr.nTop
        xx = ( wr.nRight - wr.nLeft ) + ( g_Win.Xres - ( cr.nRight - cr.nLeft ))
        yy = ( wr.nBottom - wr.nTop ) + ( g_Win.Yres - ( cr.nBottom - cr.nTop ))

        IF x = 0 THEN x = ( sx - xx ) / 2
        IF y = 0 THEN y = ( sy - yy ) / 2
        MoveWindow hWnd, x, y, xx, yy, %TRUE

        lastWidth  = xx
        lastHeight = yy

      END IF

      ' -- Initializes openGL
      g_Win.hDC = GetDC( hWnd )
      internal_InitOpenGL()

      FUNCTION = 0    ' -- to proceed as usual
      EXIT FUNCTION

    CASE %WM_MOUSEWHEEL
      tbgl_MouseWheelDelta = SGN( HI( INTEGER, wParam ))

'    CASE %WM_SIZING


    CASE %WM_SIZE

      IF gOpenGLStarted THEN
        GetWindowRect hWnd, wr
        currentWidth = ( wr.nRight - wr.nLeft )
        currentHeight= ( wr.nBottom - wr.nTop )

        IF wParam = %SIZE_MAXIMIZED THEN
          internal_MaximizeKeepAspectRatio()
        ELSE
          ''if currentWidth - lastWidth > currentHeight - lastHeight then
            internal_KeepAspectRatio( 0 )
          ''elseIF lastWidth - currentWidth > currentHeight - lastHeight THEN
          ''  internal_KeepAspectRatio( 0 )
          ''eLSE
          ''  internal_KeepAspectRatio( 1 )
          ''end if
        END IF

        ' -- Render dark frame during resizing
        internal_RenderFrame
        internal_RenderFrame

        windowSizing = 1

        GetWindowRect hWnd, wr
        lastWidth = ( wr.nRight - wr.nLeft )
        lastHeight= ( wr.nBottom - wr.nTop )

        FUNCTION = 0
        EXIT FUNCTION
      END IF

    CASE %WM_MOVE

      windowSizing = 1
      IF g_Win.periodicFuncPointer = 0 THEN
        ' -- Render dark frame during dragging
        IF gOpenGLStarted THEN internal_RenderFrame
      END IF

    CASE %WM_TIMER
      IF g_Win.periodicFuncPointer THEN
        CallingWindow = hWnd
        internal_InvokeCallback(g_Win.periodicFuncPointer)
        CallingWindow = 0
      END IF
      #IF %TBGL_THINBASIC_INTERPRETER_INTERFACE
      IF thinBasic_ErrorFlag THEN
        Internal_TBGL_Free()
      END IF
      #ENDIF

    CASE %WM_CLOSE
      ' -- Release everything
      Internal_TBGL_Free( )
      FUNCTION = 0
      EXIT FUNCTION


    CASE %WM_DESTROY
      FUNCTION = 0
      EXIT FUNCTION


    CASE %WM_SYSCOMMAND
      IF ( wParam <> %SC_SCREENSAVE ) OR ( wParam <> %SC_MONITORPOWER ) THEN FUNCTION = DefWindowProc( hWnd, wMsg, wParam, lParam )
      EXIT FUNCTION


    CASE %WM_DROPFILES
      ' Test for files "dragged" onto window...
      g_OnDropFiles_Files = GetDropFiles(wParam)
      DragFinish(wParam)
      CallingWindow = hWnd

      internal_InvokeCallback(g_OnEventPointers.OnDropFiles)
      FUNCTION = 0


    CASE ELSE
      FUNCTION = DefWindowProc( hWnd, wMsg, wParam, lParam )


  END SELECT
END FUNCTION

' -----------------------------------------------------------------------------
' -- Window whose event is being processed by a callback
FUNCTION tbgl_CallingWindow ALIAS "tbgl_CallingWindow" ( ) EXPORT AS LONG
  FUNCTION = CallingWindow
END FUNCTION

' -----------------------------------------------------------------------------

' -- Sets existing window to windowed mode
' -- winStyle = %TBGL_UNDEFINED keeps current window style, xPos = %TBGL_UNDEFINED keeps current position
SUB tbgl_SetWindowed ALIAS "tbgl_SetWindowed" ( _
    BYVAL wHandle AS DWORD _
  , BYVAL xRes AS LONG _
  , BYVAL yRes AS LONG _
  , BYVAL Bpp AS BYTE _
  , BYVAL winStyle AS LONG _
  , BYVAL xPos AS LONG _
  , BYVAL yPos AS LONG _
  ) EXPORT
  LOCAL flagsWindowed, flags AS LONG
  LOCAL hMenu, wFlags AS DWORD
  LOCAL wr AS RECT, cr AS RECT, x AS LONG, y AS LONG, xx AS LONG, yy AS LONG
  LOCAL sx, sy AS LONG

  gOpenGLStarted = 0
  g_Win.xRes = xRes
  g_Win.yRes = yRes
  g_Win.aspectRat# = g_Win.xRes / g_Win.yRes
  g_Win.BitDepth = Bpp
  m_Validate_gWin_Bitdepth
  flagsWindowed = g_Win.flagsWindowed

  ' -- [!] information is stored on bit positions according to %TBGL_WS_* flags
  IF winStyle <> %TBGL_UNDEFINED THEN
    flags = winStyle
    IF BIT( flags, 1 ) OR BIT( flags, 2 ) OR BIT( flags, 3 ) THEN flagsWindowed = flagsWindowed OR %WS_SYSMENU

    IF BIT( flags, 1 ) THEN flagsWindowed = flagsWindowed OR %WS_MINIMIZEBOX
    IF BIT( flags, 2 ) THEN flagsWindowed = flagsWindowed OR %WS_MAXIMIZEBOX
    IF BIT( flags, 4 ) THEN g_Win.ignrAspect = 1
    IF BIT( flags, 5 ) = 0 THEN
      flagsWindowed = flagsWindowed OR %WS_THICKFRAME
    ELSE
      flagsWindowed = flagsWindowed AND NOT %WS_THICKFRAME
    END IF
    g_Win.flagsWindowed = flagsWindowed

  END IF

  SetWindowLong( whandle, %GWL_EXSTYLE, %WS_EX_LEFT )
  SetWindowLong( whandle, %GWL_STYLE, flagsWindowed )

  hMenu = GetSystemMenu( whandle, 0 )
  wFlags = %MF_BYCOMMAND OR IIF( BIT( flags, 3 ) = 0, %MF_GRAYED, %MF_ENABLED )
  EnableMenuItem( hMenu, %SC_CLOSE, wFlags )

  IF g_Win.fullScreen = 1 THEN
    ChangeDisplaySettings( BYVAL 0, 0 )
    WHILE ShowCursor(%TRUE) < 0
    WEND

    SLEEP( 0 )
  END IF
  sx = GetSystemMetrics( %SM_CXSCREEN )
  sy = GetSystemMetrics( %SM_CYSCREEN )

  DIM i AS LONG

  ' -- When changing from full screen, we have no decorations, which makes it harder :")
  ' -- Need to go twice to get correct dimensions
  FOR i = 0 TO g_Win.fullScreen
    GetWindowRect whandle, wr
    GetClientRect whandle, cr
    x = wr.nLeft
    y = wr.nTop
    xx = ( wr.nRight - wr.nLeft ) + ( g_Win.Xres - ( cr.nRight - cr.nLeft ))    '-1
    yy = ( wr.nBottom - wr.nTop ) + ( g_Win.Yres - ( cr.nBottom - cr.nTop ))    '-1


    IF i = 0 THEN
      IF xPos = %TBGL_UNDEFINED THEN
        xPos = g_Win.xPos
        yPos = g_Win.yPos
      END IF

      IF g_Win.xPos = 0 THEN
        x = ( sx - xRes ) / 2
        y = ( sy - yRes ) / 2
      ELSE
        x = xPos
        y = yPos
      END IF

      g_Win.xPos = x
      g_Win.yPos = y

    END IF

    MoveWindow( whandle, x, y, xx, yy, %TRUE )
  NEXT


  ShowWindow wHandle, %SW_SHOWNORMAL
  SetForegroundWindow wHandle
  SetFocus wHandle

  g_Win.fullScreen = 0
  gOpenGLStarted = 1
  internal_RefreshViewport( )

END SUB

' -- Sets existing window to fullscreen
SUB tbgl_SetFullscreen ALIAS "tbgl_SetFullscreen" ( BYVAL wHandle AS DWORD, BYVAL xRes AS LONG, BYVAL yRes AS LONG, BYVAL Bpp AS BYTE ) EXPORT

  g_Win.fullScreen = 1

  g_Win.xRes = xRes
  g_Win.yRes = yRes
  g_Win.aspectRat# = g_Win.xRes / g_Win.yRes
  g_Win.BitDepth = Bpp
  m_Validate_gWin_Bitdepth

  ' --

  SetWindowLong( whandle, %GWL_EXSTYLE, %WS_EX_LEFT )
  SetWindowLong( whandle, %GWL_STYLE, %WS_POPUP OR %WS_VISIBLE )

  internal_SetFullscreenMode( "TBGL_SetFullscreen" )

  MoveWindow whandle, 0&, 0&, g_Win.xRes, g_Win.yRes, %TRUE

  ShowWindow wHandle, %SW_SHOWNORMAL
  SetForegroundWindow wHandle
  SetFocus wHandle

  internal_RenderFrame
  internal_RenderFrame
  gOpenGLStarted = 1&
  internal_RefreshViewport( )
  internal_RefreshViewport( )

END SUB


' -- Enumerates available fullscreen modes
FUNCTION tbgl_GetFullscreenModes ALIAS "tbgl_GetFullscreenModes" ( ) EXPORT AS STRING

  LOCAL i, r AS LONG
  LOCAL ScrnModeID, ValidScrnModeID AS LONG
  LOCAL ScrnMode AS DEVMODE
  LOCAL MODE AS STRING
  LOCAL modes AS STRING
  MODE = ""
  modes = ""
  DIM DModes( 1 TO 128 ) AS STRING

  DO
    INCR ScrnModeID
    i = EnumDisplaySettings( "\\.\Display1", ScrnModeID, ScrnMode )
    IF i = 0 THEN EXIT DO

    IF ScrnMode.dmBitsPerPel >= 16 AND ScrnMode.dmPelsWidth >= 640 THEN
      MODE = USING$( "#_x#_x#", ScrnMode.dmPelsWidth, ScrnMode.dmPelsHeight, ScrnMode.dmBitsPerPel )
      IF ChangeDisplaySettings( ScrnMode, %CDS_TEST ) <> %DISP_CHANGE_SUCCESSFUL THEN EXIT IF
      ARRAY SCAN DModes( ) FOR ValidScrnModeID, = MODE, TO r
      IF r = 0 THEN
        INCR ValidScrnModeID
        IF ValidScrnModeID > UBOUND( DModes ) THEN REDIM PRESERVE DModes( 1 TO ValidScrnModeID + 8 )
        DModes( ValidScrnModeID ) = MODE
      END IF
      '
    END IF
  LOOP

  modes = JOIN$( DModes( ), "," )
  modes = RTRIM$( modes, "," )
  FUNCTION = modes

END FUNCTION

' -- Shows the created window
SUB tbgl_ShowWindow ALIAS "tbgl_ShowWindow" ( ) EXPORT
  LOCAL d AS RECT, sx, sy AS LONG
  LOCAL iar AS LONG

  ShowWindow (g_Win.handle, %SW_SHOWNORMAL)

  ' -- Make this the topmost window
  SetForegroundWindow (g_Win.handle)

  ' -- Sets UI focus
  SetFocus g_Win.handle

  iar = g_Win.ignrAspect
  g_Win.ignrAspect = 0

  GetWindowRect( g_Win.handle, d )
  sx = GetSystemMetrics( %SM_CXSCREEN )
  sy = GetSystemMetrics( %SM_CYSCREEN )

  ' -- If window gets bigger than desktop
  IF d.nRight - d.nLeft > sx OR d.nBottom - d.nTop > sy THEN internal_MaximizeKeepAspectRatio
  g_Win.ignrAspect = iar
  internal_RefreshViewport( )

  ' -- 2 dummy frames to clear buffers
  internal_RenderFrame
  internal_RenderFrame

END SUB

' -- Retrieves client area size of given window
SUB tbgl_GetWindowClient ALIAS "tbgl_GetWindowClient" ( BYVAL winHandle AS DWORD, BYREF clientWidth AS LONG, BYREF clientHeight AS LONG ) EXPORT
  LOCAL cSize AS RECT

  GetClientRect( winHandle, cSize )
  clientWidth  = cSize.nRight
  clientHeight = cSize.nBottom

END SUB

GLOBAL g_DisplayCount, g_DisplayIndex, g_DisplayWidth, g_DisplayHeight, g_DisplayLeft, g_DisplayTop, g_DisplayRight, g_DisplayBottom AS LONG
FUNCTION DisplayEnumProc(BYVAL hMonitor AS DWORD, BYVAL hDc AS DWORD, BYREF r AS RECT, BYVAL lParam AS DWORD) AS LONG
  g_DisplayCount += 1

  IF g_DisplayCount = 1 THEN
    g_DisplayLeft   = 0
    g_DisplayTop    = 0
    g_DisplayWidth  = 0
    g_DisplayHeight = 0
  END IF

  ' -- Works ok even if Index is not defined
  IF g_DisplayCount = g_DisplayIndex THEN
    g_DisplayLeft   = r.nLeft
    g_DisplayTop    = r.nTop
    g_DisplayRight  = r.nRight
    g_DisplayBottom = r.nBottom

    g_DisplayWidth  = g_DisplayRight - g_DisplayLeft
    g_DisplayHeight = g_DisplayBottom - g_DisplayTop
    FUNCTION = 0
  ELSE
    FUNCTION = 1
  END IF

END FUNCTION

GLOBAL g_WindowLeft, g_WindowTop AS LONG
FUNCTION DisplayWindowFinderProc(BYVAL hMonitor AS DWORD, BYVAL hDc AS DWORD, BYREF r AS RECT, BYVAL lParam AS DWORD) AS LONG
  g_DisplayCount += 1

  IF g_DisplayCount = 1 THEN
    g_DisplayLeft   = 0
    g_DisplayTop    = 0
    g_DisplayWidth  = 0
    g_DisplayHeight = 0
  END IF

  IF r.nRight > g_WindowLeft THEN
    g_DisplayIndex = g_DisplayCount

    g_DisplayLeft   = r.nLeft
    g_DisplayTop    = r.nTop
    g_DisplayRight  = r.nRight
    g_DisplayBottom = r.nBottom

    g_DisplayWidth  = g_DisplayRight - g_DisplayLeft
    g_DisplayHeight = g_DisplayBottom - g_DisplayTop
    FUNCTION = 0
  ELSE
    FUNCTION = 1
  END IF

END FUNCTION

FUNCTION tbgl_DisplayGetCount ALIAS "tbgl_DisplayGetCount" ( ) EXPORT AS LONG

  g_DisplayCount = 0
  g_DisplayIndex = 0

  IF EnumDisplayMonitors(0, BYVAL 0, CODEPTR(DisplayEnumProc), 0) <> 0 THEN
    FUNCTION = g_DisplayCount
  ELSE
    FUNCTION = 1
  END IF
END FUNCTION

' -- Retrieves resolution of given display and desktop color depth
SUB tbgl_DisplayGetInfo ALIAS "tbgl_DisplayGetInfo" ( BYVAL displayNumber AS LONG, BYREF displayWidth AS LONG, BYREF displayHeight AS LONG, BYREF displayBitDepth AS LONG ) EXPORT
  LOCAL dHdc AS DWORD

  g_DisplayCount = 0
  g_DisplayIndex = displayNumber

  EnumDisplayMonitors(0, BYVAL 0, CODEPTR(DisplayEnumProc), 0)

  displayWidth  = g_DisplayWidth
  displayHeight = g_DisplayHeight
  dHdc = GetDC( %HWND_DESKTOP )
  displayBitDepth = GetDeviceCaps( dHDc, %BITSPIXEL )
  ReleaseDC( %HWND_DESKTOP, dHdc )

END SUB

' -- Moves window to given display, newX/newY are relative to the display, %TBGL_UNDEFINED keeps current offset
FUNCTION tbgl_SendWindowToDisplay ALIAS "tbgl_SendWindowToDisplay" ( BYVAL wHandle AS DWORD, BYVAL eDisplay AS LONG, BYVAL newX AS LONG, BYVAL newY AS LONG ) EXPORT AS LONG

  LOCAL currentDisplay AS LONG
  LOCAL r AS RECT

  currentDisplay = internal_GetWindowDisplay(wHandle)
  GetWindowRect(wHandle, r)

  IF newX = %TBGL_UNDEFINED THEN newX = r.nLeft-g_DisplayLeft
  IF newY = %TBGL_UNDEFINED THEN newY = r.nTop-g_DisplayTop

  g_DisplayCount = 0
  g_DisplayIndex = eDisplay

  EnumDisplayMonitors(0, BYVAL 0, CODEPTR(DisplayEnumProc), 0)

  newX += g_DisplayLeft
  newY += g_DisplayTop
  IF newX > g_DisplayRight THEN
    newX = g_DisplayLeft
  END IF

  IF newY > g_DisplayBottom THEN
    newY = g_DisplayTop
  END IF

  GetWindowRect(wHandle, r)
  MoveWindow(wHandle, newX, newY, r.nRight-r.nLeft, r.nBottom-r.nTop, %TRUE)

END FUNCTION

FUNCTION internal_GetWindowDisplay(BYVAL wHandle AS DWORD) AS LONG
  LOCAL wr AS RECT
  LOCAL cr AS RECT

  g_DisplayCount = 0
  g_DisplayIndex = 0

  GetWindowRect(wHandle, wr)
  GetClientRect(wHandle, cr)
  g_WindowLeft = wr.nLeft - (((wr.nLeft-wr.nRight)-(cr.nLeft-cr.nRight))/2)
  g_WindowTop = wr.nTop
  EnumDisplayMonitors(0, BYVAL 0, CODEPTR(DisplayWindowFinderProc), 0)

  FUNCTION = g_DisplayIndex
  g_DisplayIndex = 0

END FUNCTION


FUNCTION tbgl_GetWindowDisplay ALIAS "tbgl_GetWindowDisplay" ( BYVAL wHandle AS DWORD ) EXPORT AS LONG
  FUNCTION = internal_GetWindowDisplay(wHandle)
END FUNCTION


' -- Retrieves desktop resolution and color depth
SUB tbgl_GetDesktopInfo ALIAS "tbgl_GetDesktopInfo" ( BYREF desktopWidth AS LONG, BYREF desktopHeight AS LONG, BYREF desktopBitDepth AS LONG ) EXPORT
  LOCAL dHdc AS DWORD

  desktopWidth  = GetSystemMetrics( %SM_CXSCREEN )
  desktopHeight = GetSystemMetrics( %SM_CYSCREEN )
  dHdc = GetDC( %HWND_DESKTOP )
  desktopBitDepth = GetDeviceCaps( dHDc, %BITSPIXEL )
  ReleaseDC( %HWND_DESKTOP, dHdc )

END SUB

SUB tbgl_SetWindowTitle ALIAS "tbgl_SetWindowTitle" ( BYVAL whandle AS DWORD, BYVAL wtext AS STRING ) EXPORT

  SetWindowText( whandle, BYCOPY wtext )

END SUB

FUNCTION tbgl_IsFullscreen ALIAS "tbgl_IsFullscreen" ( BYVAL whandle AS DWORD ) EXPORT AS LONG

  FUNCTION = IIF( g_Win.FullScreen <> 0 AND g_Win.handle = whandle, %TRUE, %FALSE )

END FUNCTION

' -- Determines whether window still exists
FUNCTION tbgl_IsWindow ALIAS "tbgl_IsWindow" ( BYVAL whandle AS DWORD ) EXPORT AS LONG

  FUNCTION = IsWindow( whandle )

END FUNCTION

' -- Sets icon from disk to TBGL window
SUB tbgl_SetWindowIcon ALIAS "tbgl_SetWindowIcon" ( BYVAL whandle AS DWORD, BYVAL wtext AS STRING ) EXPORT
  LOCAL hIcon AS DWORD

  hIcon = LoadImage( %NULL, BYCOPY wtext, %IMAGE_ICON, 32, 32, %LR_LOADFROMFILE )
  SENDMESSAGE( whandle, %WM_SETICON, %ICON_BIG, hIcon )
  SENDMESSAGE( whandle, %WM_SETICON, %ICON_SMALL, hIcon )
  DeleteObject( hIcon )

END SUB

' -- Kills window and releases resources
SUB tbgl_DestroyWindow ALIAS "tbgl_DestroyWindow" ( ) EXPORT

  Internal_TBGL_Free

END SUB

' --

' -- Calls funcPointer (see internal_InvokeCallback) every funcInterval milliseconds
SUB tbgl_BindPeriodicFunction ALIAS "tbgl_BindPeriodicFunction" ( BYVAL winHandle AS DWORD, BYVAL funcPointer AS LONG, BYVAL funcInterval AS LONG ) EXPORT

  g_Win.periodicFuncPointer  = funcPointer
  g_Win.periodicFuncInterval = funcInterval

  KillTimer winHandle, %PeriodicFuncTimer
  SetTimer winHandle, %PeriodicFuncTimer, g_Win.periodicFuncInterval, %NULL

END SUB

SUB tbgl_UnBindPeriodicFunction ALIAS "tbgl_UnBindPeriodicFunction" ( BYVAL winHandle AS DWORD ) EXPORT

  g_Win.periodicFuncPointer  = 0
  g_Win.periodicFuncInterval = 0
  KillTimer winHandle, %PeriodicFuncTimer

END SUB

' -- Runs message loop until periodic function is unbound
SUB tbgl_ProcessPeriodicFunction ALIAS "tbgl_ProcessPeriodicFunction" ( BYVAL winHandle AS DWORD ) EXPORT
  LOCAL wMsg AS TAGMSG

  WHILE GetMessage(wMsg, winHandle, 0, 0)
    CALL TranslateMessage(wMsg)
    CALL DispatchMessage(wMsg)
    IF g_Win.periodicFuncPointer = 0 THEN EXIT LOOP
  LOOP

END SUB

SUB tbgl_PeriodicChangeFunction ALIAS "tbgl_PeriodicChangeFunction" ( BYVAL winHandle AS DWORD, BYVAL funcPointer AS LONG ) EXPORT

  g_Win.periodicFuncPointer = funcPointer

  KillTimer winHandle, %PeriodicFuncTimer
  SetTimer winHandle, %PeriodicFuncTimer, g_Win.periodicFuncInterval, %NULL

END SUB

SUB tbgl_PeriodicChangeInterval ALIAS "tbgl_PeriodicChangeInterval" ( BYVAL winHandle AS DWORD, BYVAL funcInterval AS LONG ) EXPORT

  g_Win.periodicFuncInterval = funcInterval

  KillTimer winHandle, %PeriodicFuncTimer
  SetTimer winHandle, %PeriodicFuncTimer, g_Win.periodicFuncInterval, %NULL

END SUB

' ---------------------------------------------------------------------------------------------
' -- eventCode is %TBGL.OnDropFiles, eventProcPointer is callback (see internal_InvokeCallback)
FUNCTION tbgl_BindWindowEvent ALIAS "tbgl_BindWindowEvent" ( BYVAL hWindow AS DWORD, BYVAL eventCode AS LONG, BYVAL eventProcPointer AS LONG ) EXPORT AS LONG
  FUNCTION = 1

  IF eventProcPointer = %NULL THEN
    TBGLERROR "TBGL_BindWindowEvent: The specified event handler routine does not exist"
    FUNCTION = 0
    EXIT FUNCTION
  END IF

  SELECT CASE eventCode
    CASE %TBGL.OnDropFiles
'      OleInitialize(%NULL)
'      ' Create a new instance of our implemented IDropTarget interface
'      g_pDropTarget = CLASS "CDropTarget"
'      IF ISOBJECT(g_pDropTarget) THEN
'         ' Sets the handle of the label
'         g_pDropTarget.SetHwnd hWindow
'         ' Locks the object to ensure that it stays in memory
'         CoLockObjectExternal(g_pDropTarget, %TRUE, %FALSE)
'         ' Registers the specified window as one that can be the target
'         ' of an OLE drag-and-drop operation and specifies the IDropTarget
'         ' instance to use for drop operations.
'         RegisterDragDrop(hWindow, g_pDropTarget)

          DragAcceptFiles(hWindow, %true)

         g_OnEventPointers.OnDropFiles = eventProcPointer
'      else
'        function = 0
'      END IF

    CASE ELSE
      TBGLERROR "TBGL_BindWindowEvent: Unknown event type"
      FUNCTION = 0

  END SELECT

END FUNCTION

FUNCTION tbgl_UnBindWindowEvent ALIAS "tbgl_UnBindWindowEvent" ( BYVAL hWindow AS DWORD, BYVAL eventCode AS LONG ) EXPORT AS LONG

  FUNCTION = 1

  SELECT CASE eventCode
    CASE %TBGL.OnDropFiles
      IF g_OnEventPointers.OnDropFiles THEN
'        ' // Revokes the registration of the specified application window as a
'        ' // potential target for OLE drag-and-drop operations.
'        IF g_Win.handle THEN RevokeDragDrop(g_Win.handle)
'
'        IF ISOBJECT(g_pDropTarget) THEN
'          ' // Unlocks our IDropTarget interface
'          CoLockObjectExternal(g_pDropTarget, %FALSE, %FALSE)
'          ' // Frees the memory used by our IDropTarget interface
'          g_pDropTarget = NOTHING
'        END IF
'
'        OleUninitialize()

          DragAcceptFiles(hWindow, %false)
          g_OnEventPointers.OnDropFiles = %NULL
      END IF

    CASE ELSE
      TBGLERROR "TBGL_UnBindWindowEvent: Unknown event type"
      FUNCTION = 0

  END SELECT

END FUNCTION

' -- File name of dropped file (1-based index), valid inside OnDropFiles callback
FUNCTION tbgl_OnDropFiles_GetFileName ALIAS "tbgl_OnDropFiles_GetFileName" ( BYVAL fileNo AS LONG ) EXPORT AS STRING
  FUNCTION = PARSE$(g_OnDropFiles_Files, "|", fileNo)
END FUNCTION

FUNCTION tbgl_OnDropFiles_GetFileCount ALIAS "tbgl_OnDropFiles_GetFileCount" ( ) EXPORT AS LONG
  FUNCTION = PARSECOUNT(g_OnDropFiles_Files, "|")
END FUNCTION

' -- 1.9.11.0

' -- Retrieves window position, returns GetWindowRect result
FUNCTION tbgl_GetWindowPos ALIAS "tbgl_GetWindowPos" ( BYVAL hWnd AS DWORD, BYREF outX AS LONG, BYREF outY AS LONG ) EXPORT AS LONG
  DIM r AS RECT

  FUNCTION = GetWindowRect(hWnd, r)

  outX = r.nLeft
  outY = r.nTop

END FUNCTION

FUNCTION tbgl_SetWindowMinClient ALIAS "tbgl_SetWindowMinClient" ( BYVAL hWnd AS DWORD, BYVAL minWidth AS LONG, BYVAL minHeight AS LONG ) EXPORT AS LONG

  IF hWnd <> g_Win.handle THEN
    FUNCTION = 0
    EXIT FUNCTION
  ELSE

    IF minWidth < 1 OR minHeight < 1 THEN
      tbglerror "Invalid parameter for TBGL_SetWindowMinClient"+$CRLF+$CRLF+"Minimum client width AND height must be positive integers, function received minWidth="+FORMAT$(minWidth)+", minHeight="+FORMAT$(minHeight)
      FUNCTION = 0
      EXIT FUNCTION
    END IF

    ' -- If aspect ratio maintaining is enabled
    IF g_Win.ignrAspect = 0 THEN
      IF minWidth <> ROUND(minHeight * g_Win.aspectRat,0) THEN
        tbglerror "Invalid parameter for TBGL_SetWindowMinClient"+$CRLF+$CRLF+"When TBGL window is in mode maintaining aspect ratio, you need to specify minimum client size with the same ratio"+$CRLF+ _
                  "Hint for this case: minWidth="+FORMAT$(ROUND(minHeight * g_Win.aspectRat, 0))+", minHeight="+FORMAT$(minHeight)
        FUNCTION = 0
        EXIT FUNCTION
      END IF
    END IF

    g_Win.minWidth  = minWidth
    g_Win.minHeight = minHeight
    FUNCTION = 1
  END IF

END FUNCTION

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

' -- ( caption, xRes, yRes, bitDepth [, flags [, xPos, yPos ]] )
FUNCTION Exec_TBGL_CreateWindowEx( ) AS EXT
  LOCAL CAPTION AS STRING
  LOCAL xRes, yRes, fsFlag, xPos, yPos, Bpp AS EXT

  thinBASIC_Parse1StringXNumbers( 4, 6, CAPTION, xRes, yRes, Bpp, fsFlag, xPos, yPos )
  FUNCTION = tbgl_CreateWindowEx( CAPTION, xRes, yRes, Bpp, fsFlag, xPos, yPos )

END FUNCTION

FUNCTION Exec_TBGL_CallingWindow() AS EXT
  FUNCTION = tbgl_CallingWindow( )
END FUNCTION

' -- ( hWnd, xRes, yRes [, bitDepth [, style [, xPos, yPos ]]] ), omitted style / position keep current ones
SUB Exec_TBGL_SetWindowed( )
  LOCAL wHandle, xRes, yRes, Bpp, winStyle, xPos, yPos AS EXT
  LOCAL parsed AS LONG

  Bpp = g_Win.BitDepth
  parsed = thinBASIC_ParseXNumbers( 3, 7, wHandle, xRes, yRes, Bpp, winStyle, xPos, yPos )

  IF parsed < 5 THEN winStyle = %TBGL_UNDEFINED
  IF parsed < 6 THEN xPos = %TBGL_UNDEFINED

  tbgl_SetWindowed( wHandle, xRes, yRes, Bpp, winStyle, xPos, yPos )

END SUB

' -- Bit depth defaults to current one
SUB Exec_TBGL_SetFullscreen( )
  LOCAL wHandle, xRes, yRes, Bpp AS EXT

  Bpp = g_Win.BitDepth
  thinBASIC_ParseXNumbers( 3, 4, whandle, xRes, yRes, Bpp )
  tbgl_SetFullscreen( wHandle, xRes, yRes, Bpp )

END SUB

FUNCTION Exec_TBGL_GetFullscreenModes( ) AS STRING
  FUNCTION = tbgl_GetFullscreenModes( )
END FUNCTION

SUB Exec_TBGL_ShowWindow( )
  tbgl_ShowWindow( )
END SUB

SUB Exec_TBGL_GetWindowClient( )
  LOCAL winHandle AS EXT
  LOCAL clientWidth, clientHeight AS LONG

  '---Declare needed variables. To change a variable we need
  '---a reference pointer and an aboslute position (in case of arrays)
  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL ParensPresent AS LONG


  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE
  thinBasic_ParseNumber winHandle
  IF thinBasic_CheckComma( ) THEN
    '---Check if any error
    IF thinBasic_ErrorFree THEN
      '---Get reference of first variable
      thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )
      IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )

        IF thinBasic_ErrorFree THEN

          tbgl_GetWindowClient( winHandle, clientWidth, clientHeight )

          '---Now assign values to referenced variables
          thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, clientWidth )
          thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, clientHeight )

        END IF
      END IF
    END IF
  END IF

  IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory

END SUB

FUNCTION Exec_TBGL_DisplayGetCount() AS LONG
  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN thinBasic_CheckCloseParens_Mandatory
  FUNCTION = tbgl_DisplayGetCount( )
END FUNCTION

' -- ( displayNumber, widthVariable, heightVariable [, bitDepthVariable ] )
FUNCTION Exec_TBGL_DisplayGetInfo() AS LONG
  '---Declare needed variables. To change a variable we need
  '---a reference pointer and an aboslute position (in case of arrays)
  LOCAL eDisplayNumber AS EXT
  LOCAL displayWidth, displayHeight, displayBitDepth AS LONG

  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL lVariablePtr3 AS LONG
  LOCAL lVariableAbsPos3 AS LONG

  IF thinBasic_CheckOpenParens_Mandatory THEN
    '---Check if any error
    IF thinBasic_ErrorFree THEN

      thinBasic_ParseNumber eDisplayNumber
      IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN

        '---Get reference of first variable
        thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )

        IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )

          IF thinBasic_CheckComma_Optional( ) AND thinBasic_ErrorFree THEN
            thinBasic_VariableParse( lVariablePtr3, lVariableAbsPos3 )
          END IF

          IF thinBasic_ErrorFree THEN

            tbgl_DisplayGetInfo( eDisplayNumber, displayWidth, displayHeight, displayBitDepth )

            '---Now assign values to referenced variables
            thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, displayWidth )
            thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, displayHeight )
            IF lVariablePtr3 THEN
              thinBasic_ChangeVariableNumberDirect( lVariablePtr3, lVariableAbsPos3, displayBitDepth )
            END IF
          END IF
        END IF
      END IF
    END IF
  END IF

  thinBasic_CheckCloseParens_Mandatory

END FUNCTION

' -- ( hWnd, display [, x [, y ]] ), omitted coordinates keep current offset within display
FUNCTION Exec_TBGL_SendWindowToDisplay() AS LONG

  LOCAL wHandle AS EXT
  LOCAL eDisplay AS EXT
  LOCAL newX, newY AS EXT

  newX = %TBGL_UNDEFINED
  newY = %TBGL_UNDEFINED

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber(wHandle)
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber(eDisplay)

      IF thinBasic_CheckComma_Optional THEN
        thinBasic_ParseNumber(NewX)
      END IF
      IF thinBasic_CheckComma_Optional THEN
        thinBasic_ParseNumber(NewY)
      END IF
      IF thinBasic_CheckCloseParens_Mandatory THEN
        FUNCTION = tbgl_SendWindowToDisplay( wHandle, eDisplay, newX, newY )
      END IF
    END IF
  END IF

END FUNCTION

FUNCTION Exec_TBGL_GetWindowDisplay() AS LONG
  LOCAL wHandle AS EXT

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber(wHandle)
    IF thinBasic_CheckCloseParens_Mandatory THEN

      FUNCTION = tbgl_GetWindowDisplay(wHandle)

    END IF
  END IF

END FUNCTION

SUB Exec_TBGL_GetDesktopInfo( )

  '---Declare needed variables. To change a variable we need
  '---a reference pointer and an aboslute position (in case of arrays)
  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL lVariablePtr3 AS LONG
  LOCAL lVariableAbsPos3 AS LONG

  LOCAL desktopWidth, desktopHeight, desktopBitDepth AS LONG

  LOCAL ParensPresent AS LONG

  IF thinBasic_CheckOpenParens_Optional = %TRUE THEN ParensPresent = %TRUE

  '---Check if any error
  IF thinBasic_ErrorFree THEN
    '---Get reference of first variable
    thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )

    IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
      thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )

      IF thinBasic_CheckComma( ) AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse( lVariablePtr3, lVariableAbsPos3 )


        IF thinBasic_ErrorFree THEN

          tbgl_GetDesktopInfo( desktopWidth, desktopHeight, desktopBitDepth )

          '---Now assign values to referenced variables
          thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, desktopWidth )
          thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, desktopHeight )
          thinBasic_ChangeVariableNumberDirect( lVariablePtr3, lVariableAbsPos3, desktopBitDepth )

        END IF
      END IF
    END IF
  END IF

  IF ParensPresent = %TRUE THEN thinBasic_CheckCloseParens_Mandatory
END SUB

SUB Exec_TBGL_SetWindowTitle( )
  LOCAL whandle AS EXT
  LOCAL wtext AS STRING

  IF thinBasic_CheckOpenParens THEN
    thinBasic_ParseNumber whandle
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseString wtext
      IF thinBasic_CheckCloseParens THEN
        tbgl_SetWindowTitle( whandle, wtext )
      END IF
    END IF
  END IF

END SUB

FUNCTION Exec_TBGL_IsFullscreen( ) AS EXT
  FUNCTION = tbgl_IsFullscreen( thinBASIC_Parse1Number )
END FUNCTION

FUNCTION Exec_TBGL_IsWindow( ) AS EXT
  FUNCTION = tbgl_IsWindow( thinBasic_Parse1Number )
END FUNCTION

SUB Exec_TBGL_SetWindowIcon( )
  LOCAL whandle AS EXT
  LOCAL wtext AS STRING

  IF thinBasic_CheckOpenParens THEN
    thinBasic_ParseNumber whandle
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseString wtext
      IF thinBasic_CheckCloseParens THEN
        tbgl_SetWindowIcon( whandle, wtext )
      END IF
    END IF
  END IF

END SUB

SUB Exec_TBGL_DestroyWindow()
  tbgl_DestroyWindow( )
END SUB

' -- [(] hWnd, function, interval [)]
FUNCTION Exec_TBGL_BindPeriodicFunction() AS EXT
  LOCAL winHandle AS EXT, FuncInterval AS EXT
  LOCAL funcPointer AS LONG
  LOCAL p AS LONG

  p = thinBasic_CheckOpenParens_Optional

    thinBasic_ParseNumber winHandle
    IF thinBasic_CheckComma_Mandatory THEN
      funcPointer = thinBasic_FunctionParseAndGetPtr(%FALSE)
      g_Win.periodicFuncPointer = funcPointer
      IF thinBasic_CheckComma_Mandatory THEN
        thinBasic_ParseNumber FuncInterval
        tbgl_BindPeriodicFunction( winHandle, funcPointer, FuncInterval )
      END IF
    END IF

  IF p THEN thinBasic_CheckCloseParens_Mandatory

END FUNCTION

FUNCTION Exec_TBGL_UnBindPeriodicFunction() AS EXT
  tbgl_UnBindPeriodicFunction( thinBasic_Parse1Number( ))
END FUNCTION

FUNCTION Exec_TBGL_ProcessPeriodicFunction() AS EXT
  tbgl_ProcessPeriodicFunction( thinBasic_Parse1Number( ))
END FUNCTION

FUNCTION Exec_TBGL_PeriodicChangeFunction() AS EXT
  LOCAL winHandle AS EXT
  LOCAL p AS LONG

  p = thinBasic_CheckOpenParens_Optional
    thinBasic_ParseNumber winHandle
    IF thinBasic_CheckComma_Mandatory THEN
      tbgl_PeriodicChangeFunction( winHandle, thinBasic_FunctionParseAndGetPtr(%FALSE) )
    END IF

  IF p THEN thinBasic_CheckCloseParens_Mandatory

END FUNCTION

FUNCTION Exec_TBGL_PeriodicChangeInterval() AS EXT
  LOCAL winHandle AS EXT, FuncInterval AS EXT
  LOCAL p AS LONG

  p = thinBasic_CheckOpenParens_Optional

    thinBasic_ParseNumber winHandle
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber FuncInterval
      tbgl_PeriodicChangeInterval( winHandle, FuncInterval )
    END IF

  IF p THEN thinBasic_CheckCloseParens_Mandatory

END FUNCTION


' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

' [O] Function to create window, really obsolete, do not touch, will be removed
' -- Kept as thinBasic keyword only, it has no counterpart in general purpose DLL
FUNCTION Exec_TBGL_CreateWindow( ) AS EXT

  LOCAL CAPTION AS STRING
  LOCAL wce AS WndClassEx
  LOCAL lpDevMode AS DEVMODE
  LOCAL xRes, yRes AS EXT

  'Define and register OpenGL-Application class:
  g_Win.ClassName = "OpenGL"
  wce.hInstance = g_ModuleInstance
  wce.cbSize = SIZEOF( wce )
  wce.lpszClassName = VARPTR( g_Win.ClassName )
  wce.lpfnWndProc = CODEPTR( WndProc )
  wce.Style = %CS_HREDRAW OR %CS_VREDRAW OR %CS_OWNDC
  wce.hCursor = LoadCursor ( %Null, BYVAL %IDC_ARROW )
  wce.hIcon = LoadIcon( g_ModuleInstance, BYVAL %IDI_APPLICATION )
  wce.hIconSm = LoadIcon( g_ModuleInstance, BYVAL %IDI_APPLICATION )

  IF ISFALSE( RegisterClassEx( wce )) THEN
    TBGLERROR "TBGL_CreateWindow : Cannot register window class"
    EXIT FUNCTION
  END IF

  IF thinBasic_CheckOpenParens( %TRUE, %TRUE ) THEN
    thinBasic_ParseString CAPTION
  ELSE
    CAPTION = "TBGL Console"
  END IF

  LOCAL Bpp AS EXT
  IF thinBasic_CheckComma( %TRUE, %FALSE ) THEN
    thinBasic_ParseNumber xRes
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber yRes
      IF thinBasic_CheckComma_Mandatory THEN
        thinBasic_ParseNumber Bpp
        IF thinBasic_CheckCloseParens( %TRUE, %TRUE ) THEN

          g_Win.FullScreen = 1
          g_Win.xRes = xRes
          g_Win.yRes = yRes
          g_Win.BitDepth = Bpp
          g_Win.handle = CreateWindowEx( 0, g_Win.ClassName, BYCOPY CAPTION, %WS_POPUP OR %WS_VISIBLE, 0, 0, xRes, yRes, 0, 0, g_ModuleInstance, BYVAL 0 )

          lpDevMode.dmSize = LEN( DEVMODE )
          lpDevMode.dmPelsWidth = xRes
          lpDevMode.dmPelsHeight = yRes
          lpDevMode.dmFields = %DM_PELSWIDTH OR %DM_PELSHEIGHT

          m_Validate_gWin_Bitdepth
          lpDevMode.dmBitsPerPel = g_Win.BitDepth
          lpDevMode.dmFields = lpDevMode.dmFields OR %DM_BITSPERPEL


          ChangeDisplaySettings( lpDevMode, %CDS_FULLSCREEN )
          WHILE ShowCursor(%FALSE) >= 0
          WEND

        END IF
      END IF
    END IF
  ELSE

    g_Win.FullScreen = 0
    g_Win.BitDepth = 32
    g_Win.xRes = 640
    g_Win.yRes = 480
    g_Win.handle = CreateWindowEx( %WS_EX_CLIENTEDGE, g_Win.ClassName, BYCOPY CAPTION, _
      %WS_VISIBLE OR %WS_CAPTION OR %WS_THICKFRAME, _
      100, 100, 640, 480, %Null, %Null, g_ModuleInstance, BYVAL %Null )

  END IF

  g_Win.aspectRat# = g_Win.xRes / g_Win.yRes
  g_Win.controlParent = 0

  IF ISFALSE( g_Win.handle ) THEN    'DebugT "Cannot create window"
    TBGLERROR "TBGL_CreateWindow : Cannot create window"
    EXIT FUNCTION
  END IF

  FUNCTION = g_Win.handle

END FUNCTION

FUNCTION Exec_TBGL_BindWindowEvent() AS LONG
  LOCAL hWindow, eventCode AS EXT
  LOCAL funcPointer AS LONG

  FUNCTION = 0

  IF thinBasic_CheckOpenParens_Mandatory() THEN
    thinBasic_ParseNumber hWindow

    IF thinBasic_CheckComma_Mandatory() THEN
      thinBasic_ParseNumber eventCode

      IF thinBasic_CheckComma_Mandatory() THEN
        funcPointer = thinBasic_FunctionParseAndGetPtr(%FALSE)

        IF thinBasic_CheckCloseParens_Mandatory() THEN
          FUNCTION = tbgl_BindWindowEvent(hWindow, eventCode, funcPointer)
        END IF
      END IF
    END IF
  END IF

END FUNCTION

FUNCTION Exec_TBGL_UnBindWindowEvent() AS LONG
  LOCAL hWindow, eventCode AS EXT

  FUNCTION = 0

  thinBasic_Parse2Numbers(hWindow, eventCode)

  IF thinBasic_ErrorFree() THEN
    FUNCTION = tbgl_UnBindWindowEvent(hWindow, eventCode)
  END IF

END FUNCTION

FUNCTION Exec_TBGL_OnDropFiles_GetFileName() AS STRING
  FUNCTION = tbgl_OnDropFiles_GetFileName( thinBasic_Parse1Number() )
END FUNCTION

FUNCTION Exec_TBGL_OnDropFiles_GetFileCount() AS LONG

  IF thinBasic_CheckOpenParens_Mandatory() THEN
    IF thinBasic_CheckCloseParens_Mandatory() THEN
      FUNCTION = tbgl_OnDropFiles_GetFileCount( )
    END IF
  END IF

END FUNCTION

FUNCTION Exec_TBGL_GetWindowPos() AS EXT
  LOCAL outX, outY AS LONG
  LOCAL hWnd AS EXT
  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber hWnd
    IF thinBasic_Checkcomma_Mandatory THEN
    thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )
      IF thinBasic_Checkcomma_Mandatory THEN
        thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )
        IF thinBasic_CheckCloseParens_Mandatory() THEN

          FUNCTION = tbgl_GetWindowPos( hWnd, outX, outY )

          thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, outX )
          thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, outY )
        END IF
      END IF
    END IF
  END IF

END FUNCTION

FUNCTION Exec_TBGL_SetWindowMinClient() AS EXT
  LOCAL hWnd, minWidth, minHeight AS EXT

  thinBasic_Parse3Numbers(hWnd, minWidth, minHeight)
  FUNCTION = tbgl_SetWindowMinClient( hWnd, minWidth, minHeight )

END FUNCTION

#ENDIF

' -----------------------------------------------------------------------------
#INCLUDE "tbgl_Window.Internal.inc"

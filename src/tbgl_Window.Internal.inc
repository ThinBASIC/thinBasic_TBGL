'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles internals of TBGL window
' =============================================================================

' =============================================================================================
' -- Flag holding info whether sources were released already
GLOBAL CleanUpPerformedAlready AS BYTE
DECLARE FUNCTION wglGetExtensionStringARB_Prototype(BYVAL hDc AS DWORD) AS DWORD
' -- OpenGL initialization, called in %WM_CREATE
SUB internal_InitOpenGL( )

  ' -- Means we have all recources online, not released
  CleanUpPerformedAlready = 0

  ' -- Local pixel-format descriptor
  LOCAL pfd AS PIXELFORMATDESCRIPTOR
  LOCAL zBufferDepth AS LONG

  zBufferDepth = 32
  DO
    ' -- Fill relevent members of pfd
    pfd.nSize       = SIZEOF( PIXELFORMATDESCRIPTOR )
    pfd.iPixelType  = %PFD_TYPE_RGBA
    pfd.nVersion    = 1
    pfd.dwFlags     = %PFD_DRAW_TO_WINDOW OR %PFD_SUPPORT_OPENGL OR %PFD_DOUBLEBUFFER
    pfd.cColorBits  = g_Win.BitDepth
    pfd.cDepthBits  = zBufferDepth
    pfd.iLayerType  = %PFD_MAIN_PLANE

    ' -- Set pixel format to device context.
    IF SetPixelFormat( g_Win.hDC, ChoosePixelFormat( g_Win.hDC, pfd ), pfd ) THEN EXIT DO

    zBufferDepth -= 8
  LOOP UNTIL zBufferDepth < 16


  ' -- Create OpenGL context.
  g_Win.hRC = wglCreateContext( g_Win.hDC )
  wglMakeCurrent g_Win.hDC, g_Win.hRC

  ' -- Set Initial OpenGL attributes

  ' -- Depth
  glEnable %GL_DEPTH_TEST
  glDepthMask %GL_TRUE
  glDepthFunc %GL_LESS
  glClearDepth 1.0

  ' -- Color
  glClearColor 0.0, 0.0, 0.0, 0.5
  glEnable %GL_COLOR_MATERIAL
  glColorMaterial %GL_FRONT_AND_BACK, %GL_AMBIENT_AND_DIFFUSE

  ' -- Lighting
  glEnable %GL_NORMALIZE
  glEnable %GL_AUTO_NORMAL
  glShadeModel %GL_SMOOTH
  glLightModelI %GL_LIGHT_MODEL_TWO_SIDE, 1

  ' -- Texture related
  glPixelStorei(%GL_PACK_ALIGNMENT, 1)

  ' -- Allocating resources
  resource_Texturing_Alloc()
  resource_Fonts_Alloc()
  resource_DisplayLists_Alloc()
  resource_Materials_Alloc()
  resource_GBuffers_Alloc()

  ' -- Initializing data
  resource_Frame_Init()

  ' -- Blending
  glBlendFunc %GL_ONE, %GL_ONE

  ' -- Beauty - I suspect driver ignores this :)
  glHint %GL_PERSPECTIVE_CORRECTION_HINT, %GL_NICEST

  ' -- Setup of view
  DIM mvmatrix( 0 TO 15 )   AS GLOBAL DOUBLE
  DIM projmatrix( 0 TO 15 ) AS GLOBAL DOUBLE
  DIM g_FontProjMatrix( 0 TO 15 ) AS GLOBAL DOUBLE
  DIM g_FontModeMatrix( 0 TO 15 ) AS GLOBAL DOUBLE
  DIM Viewport( 0 TO 3 )    AS GLOBAL LONG

  glViewport 0, 0, g_Win.xRes, g_Win.yRes    ' -- Set viewport region.
  glMatrixMode %GL_PROJECTION                ' -- Switch to Projection-Matrix mode.
  glLoadIdentity                             ' -- Reset the Projection-Matrix.

  gluPerspective( g_defaultFOV, g_Win.xRes / g_Win.yRes, 0.09, g_Win.DrawDistance )

  glMatrixMode %GL_MODELVIEW                 ' -- Select the Modelview-Matrix

  ' [!] Initialization of extensions
  LOCAL OpenGLInfoPTR AS ASCIIZ PTR
  OpenGLInfoPTR = glGetString( %GL_EXTENSIONS )
  gOpenGLExtensions = UCASE$( @OpenGLInfoPTR )

  LOCAL WGLInfoPTR AS ASCIIZ PTR
  DIM wglGetExtensionStringARB_Address AS DWORD
  wglGetExtensionStringARB_Address = wglGetProcAddress("wglGetExtensionsStringARB")

  IF wglGetExtensionStringARB_Address THEN

    CALL DWORD wglGetExtensionStringARB_Address USING wglGetExtensionStringARB_Prototype(g_Win.hDC) TO WGLInfoPTR
    gOpenGLExtensions = TRIM$(gOpenGLExtensions, $SPC)
    gOpenGLExtensions += " " + UCASE$(@WGLInfoPTR)

  END IF
  gOpenGLExtensions = TRIM$(gOpenGLExtensions, $SPC)

  ' -- Anisotropic filtering
  IF internal_IsExtensionSupported( "GL_EXT_TEXTURE_FILTER_ANISOTROPIC" ) THEN
    texture_AnisotropyAllowed = 1
    glGetFloatV( %GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, texture_AnisotropyMax )
  ELSE
    texture_AnisotropyAllowed = 0
    texture_AnisotropyMax = 0
  END IF

  ' -- OpenGL version
  OpenGLInfoPTR = glGetString( %GL_VERSION )
  LOCAL tempS AS STRING
  temps = UCASE$( @OpenGLInfoPTR )

  gvOpenGL = VAL( PARSE$( TempS, ".", 1 )) * 1000 + VAL( PARSE$( TempS, ".", 2 )) * 100 + VAL( PARSE$( TempS, ".", 3 )) * 10 + VAL( PARSE$( TempS, ".", 4 ))

  ' -- V-Sync availability
  ptr_wglSwapIntervalEXT = wglGetProcAddress( "wglSwapIntervalEXT" )

  ' -- No we are ready to render
  g_Win.RenderMatrixMode = %TBGL_3D
  gOpenGLStarted    = 1

END SUB

' -- Synchronizes window proportions and viewport, useful when sizing
SUB internal_RefreshViewport( )
  IF gOpenGLStarted THEN
    LOCAL cr AS RECT
    GetClientRect g_Win.handle, cr
    g_Win.XRes = cr.nRight
    g_Win.YRes = cr.nBottom
    glViewport 0, 0, g_Win.xRes, g_Win.yRes    ' -- Set viewport region.
    glMatrixMode %GL_PROJECTION                ' -- Switch to Projection-Matrix mode.
    glLoadIdentity                             ' -- Reset the Projection-Matrix.

    IF g_customViewportRatioActive THEN
      gluPerspective( g_defaultFOV, g_customViewportRatio, 0.09, g_Win.DrawDistance )
    ELSE
      gluPerspective( g_defaultFOV, g_Win.xRes / g_Win.yRes, 0.09, g_Win.DrawDistance )
    END IF

    glMatrixMode %GL_MODELVIEW                 ' -- Select the Modelview-Matrix
  END IF
END SUB

' -- Synchronizes window proportions and viewport, useful when sizing
SUB internal_RefreshViewportSoft( )
  IF gOpenGLStarted THEN
    glMatrixMode %GL_PROJECTION                ' -- Switch to Projection-Matrix mode.
    glLoadIdentity                             ' -- Reset the Projection-Matrix.

    IF g_customViewportRatioActive THEN
      gluPerspective( g_defaultFOV, g_customViewportRatio, 0.09, g_Win.DrawDistance )
    ELSE
      glGetIntegerV( %GL_VIEWPORT, VIEWPORT( 0 ))
      gluPerspective( g_defaultFOV, VIEWPORT( 2 ) / VIEWPORT( 3 ), 0.09, g_Win.DrawDistance )
    END IF

    glMatrixMode %GL_MODELVIEW                 ' -- Select the Modelview-Matrix
  END IF
END SUB

' -- Assures keeping ratio when maximizing
SUB internal_MaximizeKeepAspectRatio( )
  LOCAL wr AS RECT, cr AS RECT
  LOCAL x, y, xx, yy, sx, sy AS LONG
  LOCAL currentDisplay AS LONG

  IF gOpenGLStarted = 1 AND g_Win.Fullscreen = 0 THEN
    GetClientRect g_Win.handle, cr

    IF cr.nRight = 0 THEN EXIT SUB

    GetWindowRect g_Win.handle, wr

    currentDisplay = internal_GetWindowDisplay(g_Win.handle)

    sx = g_DisplayWidth
    sy = g_DisplayHeight

    IF g_Win.FullScreen = 0 THEN    ' -- Windowed
      IF g_Win.ignrAspect = 0 THEN
        xx = sx
        yy = ( sx - ( wr.nRight - wr.nLeft - cr.nRight )) / g_Win.aspectRat
        yy+= (( wr.nBottom - wr.nTop ) - ( cr.nBottom ))
        IF yy > sy THEN
          yy = sy
          xx = ( sy - ( wr.nBottom - wr.nTop - cr.nBottom )) * g_Win.aspectRat
          xx+= ( wr.nRight - wr.nLeft - cr.nRight )
        END IF
        x = ( sx - xx ) / 2 + g_DisplayLeft
        y = ( sy - yy ) / 2 + g_DisplayTop
      ELSE

        x = g_DisplayLeft
        y = g_DisplayTop
        xx = sx - 1    ' oh well :")
        yy = sy - 1
      END IF
      MoveWindow g_Win.handle, x, y, xx, yy, %TRUE

    END IF

    ' -- Synces window size and viewport size
    internal_RefreshViewport( )

  END IF
END SUB

' -- General function to keep aspect ratio
SUB internal_KeepAspectRatio( BYVAL KeepBasedOnY AS LONG )

  LOCAL wr          , cr            AS RECT
  LOCAL posX        , posY          AS LONG
  LOCAL wWidth      , wHeight       AS LONG
  LOCAL cWidth      , cHeight       AS LONG
  LOCAL wcWidthDelta, wcHeightDelta AS LONG
  LOCAL updateSize                  AS LONG
  LOCAL minClientCasted             AS LONG

  ' -- Early occlusion tests to avoid overprocessing
  ' -- or processing of nonsense cases
  IF gOpenGLStarted <> 1 OR g_Win.Fullscreen <> 0 THEN
    EXIT SUB
  END IF

  ' -- Retrieve client area
  GetClientRect g_Win.handle, cr

  ' -- Invalid? Let's cut it right here...
  IF cr.nRight = 0 THEN
    EXIT SUB
  END IF

  ' -- Retrieve window area
  GetWindowRect g_Win.handle, wr

  ' -- Initialize window states for easier handling
  posX = wr.nLeft
  posY = wr.nTop

  wWidth        = ( wr.nRight  - wr.nLeft )
  wHeight       = ( wr.nBottom - wr.nTop  )

  cWidth        = ( cr.nRight  - cr.nLeft )
  cHeight       = ( cr.nBottom - cr.nTop  )

  wcWidthDelta  = wWidth  - cWidth
  wcHeightDelta = wHeight - cHeight

  ' -- Perform aspect ratio correction, if necessary
  IF g_Win.ignrAspect = 0 AND cWidth/cHeight <> g_Win.aspectRat THEN

    IF keepBasedOnY = 0 THEN
      cHeight = cWidth / g_Win.aspectRat
    ELSE
      cWidth  = cHeight * g_Win.aspectRat
    END IF

    updateSize  = %true
  END IF

  ' -- Perform minimum client area check
  IF g_Win.minWidth <> -1 AND g_Win.minHeight <> -1 THEN

    IF cWidth < g_Win.minWidth THEN
      cWidth = g_Win.minWidth

      updateSize  = %true
    END IF

    IF cHeight < g_Win.minHeight THEN
      cHeight = g_Win.minHeight

      updateSize  = %true
    END IF

  END IF

  ' -- If update needed, do it
  IF updateSize = %true THEN
    wWidth  = cWidth + wcWidthDelta
    wHeight = cHeight + wcHeightDelta

    MoveWindow g_Win.handle, posX, posY, wWidth, wHeight, %TRUE
  END IF

  internal_RefreshViewport( )

'  LOCAL wr AS RECT, cr AS RECT, x AS LONG, y AS LONG, xx AS LONG, yy AS LONG
'  local minClientApplied as long
'  local widthDelta, heightDelta, cxx, cyy as long
'  IF gOpenGLStarted = 1 AND g_Win.Fullscreen = 0 THEN
'
'    GetClientRect g_Win.handle, cr
'    IF cr.nRight = 0 THEN EXIT SUB
'
'    GetWindowRect g_Win.handle, wr
'
'    ' -- Postprocessing of size only for windowed mode
'    IF g_Win.FullScreen = 0 THEN
'      x = wr.nLeft
'      y = wr.nTop
'
'      ' -- Calculate client size, and window size, when we need it for min client constraint
'      IF g_Win.minWidth <> -1 OR g_Win.minHeight <> -1 THEN
'        cxx = ( cr.nRight - cr.nLeft )
'        cyy = ( cr.nBottom - cr.nTop )
'
'        widthDelta  = ( wr.nRight  - wr.nLeft ) - cxx
'        heightDelta = ( wr.nBottom - wr.nTop  ) - cyy
'
'        ' -- Is width client constraint reached?
'        IF g_Win.minWidth <> -1 THEN
'
'          ' -- For case of locked aspect ratio
'          IF g_Win.ignrAspect = 0 THEN
'
'
'          else
'            IF cxx < g_Win.minWidth THEN
'              minClientApplied = 1
'              cxx = g_Win.minWidth
'            END IF
'          end if
'
'        END IF
'
'        ' -- Is height client constraint reached?
'        IF g_Win.minHeight <> -1 THEN
'
'          IF cyy < g_Win.minHeight THEN
'            minClientApplied = 1
'            cyy = g_Win.minHeight
'          END IF
'
'        END IF
'
'        ' -- If any of the constraints applied, let's tune the client
'        IF minClientApplied THEN
'
'          xx = cxx + widthDelta
'          yy = cyy + heightDelta
'          IF g_Win.ignrAspect <> 0 THEN
'            MoveWindow g_Win.handle, x, y, xx, yy, %TRUE
'          end if
'        END IF
'
'      END IF
'
'      ' -- For case of locked aspect ratio
'      IF g_Win.ignrAspect = 0 THEN
'
'        ' -- Check aspect ratio
'        IF KeepBasedOnY = 0 THEN
'
'          if minClientApplied = 1 then
'            yy = cyy + heightDelta
'            xx = (cxx * g_Win.aspectRat) + widthDelta            '' ------------------------------------ BLBOST, osetrit ruzna aspect ratia
'
'''            xx = cxx + widthDelta
'''            yy = (cyy * g_Win.aspectRat) + heightDelta
'          else
'            xx = ( wr.nRight - wr.nLeft )
'            yy = ( wr.nBottom - wr.nTop ) + (( cr.nRight / g_Win.aspectRat ) - cr.nBottom )
'          end if
'        ELSE
'          yy = ( wr.nBottom - wr.nTop )
'          xx = ( wr.nRight - wr.nLeft ) + (( cr.nBottom * g_Win.aspectRat ) - cr.nRight )
'        END IF
'
'        MoveWindow g_Win.handle, x, y, xx, yy, %TRUE
'
'      END IF
'    END IF
'
'    ' -- Synces window size and viewport size
'    internal_RefreshViewport( )
'
'  END IF
END SUB

' -- Sets fullscreen mode safely
SUB internal_SetFullscreenMode( commandUsed AS STRING )
  LOCAL lpDevMode AS DEVMODE
  LOCAL ScrnMode AS DEVMODE
  lpDevMode.dmSize = LEN( DEVMODE )
  lpDevMode.dmPelsWidth = g_Win.xRes
  lpDevMode.dmPelsHeight = g_Win.yRes
  lpDevMode.dmFields = %DM_PELSWIDTH OR %DM_PELSHEIGHT
  lpDevMode.dmBitsPerPel = g_Win.BitDepth
  lpDevMode.dmFields = lpDevMode.dmFields OR %DM_BITSPERPEL

  LOCAL i, r, maxHZ AS LONG

  ' -- Enumerating available display modes
  r = 1
  maxHZ = 0
  WHILE r > 0
    INCR i
    r = EnumDisplaySettings( "\\.\Display1", i, ScrnMode )

    IF ScrnMode.dmPelsWidth = lpDevMode.dmPelsWidth AND _
      ScrnMode.dmPelsHeight = lpDevMode.dmPelsHeight AND _
      ScrnMode.dmBitsPerPel = lpDevMode.dmBitsPerPel THEN
      maxHZ = MAX( maxHZ, ScrnMode.dmDisplayFrequency ) ' -- We want maximum refresh rate to save our eyes from bleeding
    END IF
  WEND

  IF maxHZ > 0 THEN
    lpDevMode.dmDisplayFrequency = maxHZ
    lpDevMode.dmFields = lpDevMode.dmFields OR %DM_DISPLAYFREQUENCY
  END IF

  LOCAL ret AS LONG
  ret = ChangeDisplaySettings( lpDevMode, %CDS_FULLSCREEN )

  LOCAL tempRes AS STRING
  tempres = STR$( g_Win.xRes ) + "x" + FORMAT$( g_Win.yRes ) + "x" + FORMAT$( g_Win.bitDepth ) + $CRLF + "Use TBGL_GetFullscreenModes to retrieve valid resolutions"
  IF ret <> 0 THEN tbglError ( commandUsed + ": " + SWITCH$( ret = %DISP_CHANGE_RESTART, "Video mode change would need restart:" + tempres, _
                                                             ret = %DISP_CHANGE_FAILED, "Video mode change failed:" + tempRes, _
                                                             ret = %DISP_CHANGE_BADMODE, "Video mode not supported:" + tempRes, _
                                                             ret = %DISP_CHANGE_NOTUPDATED, "Unable to write settings to the registry", _
                                                             ret = %DISP_CHANGE_BADFLAGS, "Invalid flags", _
                                                             ret = %DISP_CHANGE_BADPARAM, "Bad parameters", _
                                                             ret = %DISP_CHANGE_BADDUALVIEW, "The settings change was unsuccessful because system is DualView capable" ))

  ShowCursor 0

END SUB

' -- Renders empty frame, used when dragging
SUB internal_RenderFrame( )

  IF g_Win.hRC THEN
    glClear %GL_COLOR_BUFFER_BIT OR %GL_DEPTH_BUFFER_BIT    ' -- Clear OpenGL Buffers
    glLoadIdentity                                          ' -- Set Modelview Matrix to IDN

    wglSwapBuffers g_Win.hDC                                ' -- WGL will flush and swap buffers
  END IF

END SUB

' [!] -- VERY IMPORTANT FUNCTION assuring releasing resources
SUB Internal_TBGL_Free( )

  ' -- OpenGL not launched? Then what do you want to release, honey? :)
  IF gOpenGLStarted = 0 THEN EXIT SUB

  REGISTER i AS LONG

  INCR CleanUpPerformedAlready

  IF CleanUpPerformedAlready = 1 THEN

    ' -- No more actions :)
    gOpenGLStarted = 0

    ' -- Release textures
    resource_Texturing_Release()
    resource_Fonts_Release()
    resource_DisplayLists_Release()
    resource_Materials_Release()
    resource_GBuffers_Release()

    ' -- Reinit for further use
    resource_Window_Release()
    resource_Window_Alloc()

    ' -- Release keybuffers
    RESET keyPressed( )

    ' -- Release OpenGL
    wglMakeCurrent( 0, %Null )
    wglDeleteContext( g_Win.hRC )
    ReleaseDC g_Win.handle, g_Win.hDC
    g_Win.HDC = 0

    IF g_Win.controlParent = 0 THEN

      IF g_Win.periodicFuncPointer THEN
        g_Win.periodicFuncPointer = 0
        KillTimer g_Win.handle, %PeriodicFuncTimer
      END IF

      IF DestroyWindow( g_Win.handle ) = 0 THEN
        g_Win.handle = 0
      END IF

      IF UnregisterClass( g_Win.ClassName, g_ModuleInstance ) = 0 THEN
        i = 1
      END IF
      IF g_Win.FullScreen = 1 THEN
        ChangeDisplaySettings( BYVAL 0, 0 )
        ShowCursor 1
      END IF
    END IF
    ' -- Prepare for possible new start
    g_Win.handle = 0
    CALL internal_InitValues( )
  END IF

END SUB

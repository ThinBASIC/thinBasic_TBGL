'-------------------------------------------------------------------------
'  GLWGL.INC -- Part of the OpenGL32 API Includes for PB/DLL 6.0 Release 4
'  [gl.h, glu.h, glaux.h + wgl API]
'  Copyright 1999 sublevel6.
'  Portions copyright Microsoft (and Silicon Graphics?).
'
'  Release date: Saturday, December 11, 1999
'  Homepage:     http://ryme.future.easyspace.com/
'  E-Mail:       dinky@pmail.net (to Dinky)
'                angus_law@hotmail.com (to rYme)
'-------------------------------------------------------------------------

%GLWGL_INC                 = 1

%PFD_DRAW_TO_WINDOW        = &H00000004
%PFD_TYPE_RGBA             = 0
%PFD_SUPPORT_OPENGL        = &H00000020
%PFD_DOUBLEBUFFER          = &H00000001

#IF %DEF(%COMPLETEWGL)
%PFD_TYPE_COLORINDEX       = 1

%PFD_MAIN_PLANE            = 0
%PFD_OVERLAY_PLANE         = 1
%PFD_UNDERLAY_PLANE        = -1


%PFD_STEREO                = &H00000002

%PFD_DRAW_TO_BITMAP        = &H00000008
%PFD_SUPPORT_GDI           = &H00000010

%PFD_GENERIC_FORMAT        = &H00000040
%PFD_NEED_PALETTE          = &H00000080
%PFD_NEED_SYSTEM_PALETTE   = &H00000100
%PFD_SWAP_EXCHANGE         = &H00000200
%PFD_SWAP_COPY             = &H00000400
%PFD_SWAP_LAYER_BUFFERS    = &H00000800
%PFD_GENERIC_ACCELERATED   = &H00001000
%PFD_SUPPORT_DIRECTDRAW    = &H00002000

%PFD_DEPTH_DONTCARE        = &H20000000
%PFD_DOUBLEBUFFER_DONTCARE = &H40000000
%PFD_STEREO_DONTCARE       = &H80000000

%WGL_FONT_LINES            = 0
%WGL_FONT_POLYGONS         = 1

%LPD_DOUBLEBUFFER          = &H00000001
%LPD_STEREO                = &H00000002
%LPD_SUPPORT_GDI           = &H00000010
%LPD_SUPPORT_OPENGL        = &H00000020
%LPD_SHARE_DEPTH           = &H00000040
%LPD_SHARE_STENCIL         = &H00000080
%LPD_SHARE_ACCUM           = &H00000100
%LPD_SWAP_EXCHANGE         = &H00000200
%LPD_SWAP_COPY             = &H00000400
%LPD_TRANSPARENT           = &H00001000

%LPD_TYPE_RGBA             = 0
%LPD_TYPE_COLORINDEX       = 1

%WGL_SWAP_MAIN_PLANE       = &H00000001
%WGL_SWAP_OVERLAY1         = &H00000002
%WGL_SWAP_OVERLAY2         = &H00000004
%WGL_SWAP_OVERLAY3         = &H00000008
%WGL_SWAP_OVERLAY4         = &H00000010
%WGL_SWAP_OVERLAY5         = &H00000020
%WGL_SWAP_OVERLAY6         = &H00000040
%WGL_SWAP_OVERLAY7         = &H00000080
%WGL_SWAP_OVERLAY8         = &H00000100
%WGL_SWAP_OVERLAY9         = &H00000200
%WGL_SWAP_OVERLAY10        = &H00000400
%WGL_SWAP_OVERLAY11        = &H00000800
%WGL_SWAP_OVERLAY12        = &H00001000
%WGL_SWAP_OVERLAY13        = &H00002000
%WGL_SWAP_OVERLAY14        = &H00004000
%WGL_SWAP_OVERLAY15        = &H00008000
%WGL_SWAP_UNDERLAY1        = &H00010000
%WGL_SWAP_UNDERLAY2        = &H00020000
%WGL_SWAP_UNDERLAY3        = &H00040000
%WGL_SWAP_UNDERLAY4        = &H00080000
%WGL_SWAP_UNDERLAY5        = &H00100000
%WGL_SWAP_UNDERLAY6        = &H00200000
%WGL_SWAP_UNDERLAY7        = &H00400000
%WGL_SWAP_UNDERLAY8        = &H00800000
%WGL_SWAP_UNDERLAY9        = &H01000000
%WGL_SWAP_UNDERLAY10       = &H02000000
%WGL_SWAP_UNDERLAY11       = &H04000000
%WGL_SWAP_UNDERLAY12       = &H08000000
%WGL_SWAP_UNDERLAY13       = &H10000000
%WGL_SWAP_UNDERLAY14       = &H20000000
%WGL_SWAP_UNDERLAY15       = &H40000000

%WGL_SWAPMULTIPLE_MAX      = 16
#ENDIF
'TYPE POINTFLOAT
'  x AS SINGLE
'  y AS SINGLE
'END TYPE

'TYPE GLYPHMETRICSFLOAT
'  gmfBlackBoxX AS SINGLE
'  gmfBlackBoxY AS SINGLE
'  gmfptGlyphOrigin AS POINTFLOAT
'  gmfCellIncX AS SINGLE
'  gmfCellIncY AS SINGLE
'END TYPE

#IF %DEF(%COMPLETEWGL)
TYPE tagLAYERPLANEDESCRIPTOR
  nSize AS WORD
  nVersion AS WORD
  dwFlags AS DWORD
  iPixelType AS BYTE
  cColorBits AS BYTE
  cRedBits AS BYTE
  cRedShift AS BYTE
  cGreenBits AS BYTE
  cGreenShift AS BYTE
  cBlueBits AS BYTE
  cBlueShift AS BYTE
  cAlphaBits AS BYTE
  cAlphaShift AS BYTE
  cAccumBits AS BYTE
  cAccumRedBits AS BYTE
  cAccumGreenBits AS BYTE
  cAccumBlueBits AS BYTE
  cAccumAlphaBits AS BYTE
  cDepthBits AS BYTE
  cStencilBits AS BYTE
  cAuxBuffers AS BYTE
  iLayerPlane AS BYTE
  bReserved AS BYTE
 crTransparent AS LONG
END TYPE

#ENDIF
'TYPE WGLSWAP
'  hdc AS LONG
'  uiFlags AS DWORD
'END TYPE

'DECLARE FUNCTION wglCreateContext          LIB "opengl32.dll" ALIAS "wglCreateContext"          (BYVAL hDC AS LONG) AS LONG
DECLARE FUNCTION wglDeleteContext          LIB "opengl32.dll" ALIAS "wglDeleteContext"          (BYVAL hGLRC AS LONG) AS LONG
DECLARE FUNCTION wglMakeCurrent            LIB "opengl32.dll" ALIAS "wglMakeCurrent"            (BYVAL hDC AS LONG, BYVAL HGLRC AS LONG) AS LONG
DECLARE FUNCTION wglSwapBuffers            LIB "opengl32.dll" ALIAS "wglSwapBuffers"            (BYVAL hDC AS LONG) AS LONG

#IF %DEF(%COMPLETEWGL)
DECLARE FUNCTION wglCopyContext            LIB "opengl32.dll" ALIAS "wglCopyContext"            (BYVAL hGLRC AS LONG, BYVAL hGLRC AS LONG, BYVAL UINT AS DWORD) AS LONG
DECLARE FUNCTION wglCreateLayerContext     LIB "opengl32.dll" ALIAS "wglCreateLayerContext"     (BYVAL hDC AS LONG, BYVAL OGLint AS LONG) AS LONG
DECLARE FUNCTION wglGetCurrentContext      LIB "opengl32.dll" ALIAS "wglGetCurrentContext"      () AS LONG
DECLARE FUNCTION wglGetProcAddress         LIB "opengl32.dll" ALIAS "wglGetProcAddress"         (BYVAL lpStr AS ASCIIZ) AS LONG
DECLARE FUNCTION wglShareLists             LIB "opengl32.dll" ALIAS "wglShareLists"             (BYVAL hDC AS LONG, BYVAL HGLRC AS LONG) AS LONG
DECLARE FUNCTION wglUseFontBitmaps         LIB "opengl32.dll" ALIAS "wglUseFontBitmapsA"        (BYVAL hDC AS LONG, BYVAL var1 AS DWORD, BYVAL var2 AS DWORD, BYVAL var3 AS DWORD) AS LONG
'DECLARE FUNCTION wglUseFontOutlines        LIB "opengl32.dll" ALIAS "wglUseFontOutlinesA"       (BYVAL hDC AS LONG, BYVAL var1 AS DWORD, BYVAL var2 AS DWORD, BYVAL var3 AS DWORD, BYVAL var3 AS SINGLE, BYVAL var4 AS SINGLE, BYVAL var5 AS LONG,  _
'lpGLYPHMETRICSFLOAT AS ANY) AS LONG
'DECLARE FUNCTION wglUseFontOutlines LIB "OPENGL32.DLL" ALIAS "wglUseFontOutlinesA" (BYVAL hdc AS DWORD, BYVAL dFirst AS DWORD, BYVAL dCount AS DWORD, BYVAL dListBase AS DWORD, BYVAL deviation AS SINGLE, BYVAL extrusion AS SINGLE, _
'                                                                                    BYVAL iFormat AS LONG, BYVAL lpgmf AS DWORD) AS LONG
DECLARE FUNCTION wglDescribeLayerPlane     LIB "opengl32.dll" ALIAS "wglDescribeLayerPlane"     (BYVAL hDC AS LONG, BYVAL var1 AS LONG, BYVAL var2 AS LONG, BYVAL var3 AS DWORD, lpLAYERPLANEDESCRIPTOR AS tagLAYERPLANEDESCRIPTOR) AS LONG
DECLARE FUNCTION wglSetLayerPaletteEntries LIB "opengl32.dll" ALIAS "wglSetLayerPaletteEntries" (BYVAL hDC AS LONG, BYVAL var1 AS LONG, BYVAL var2 AS LONG, BYVAL var3 AS LONG, iCOLORREF AS LONG) AS LONG
DECLARE FUNCTION wglGetLayerPaletteEntries LIB "opengl32.dll" ALIAS "wglGetLayerPaletteEntries" (BYVAL hDC AS LONG, BYVAL var1 AS LONG, BYVAL var2 AS LONG, BYVAL var3 AS LONG, iCOLORREF AS LONG) AS LONG
DECLARE FUNCTION wglRealizeLayerPalette    LIB "opengl32.dll" ALIAS "wglRealizeLayerPalette"    (BYVAL hDC AS LONG, BYVAL var1 AS LONG, BYVAL var2 AS LONG) AS LONG
DECLARE FUNCTION wglSwapLayerBuffers       LIB "opengl32.dll" ALIAS "wglSwapLayerBuffers"       (BYVAL hDC AS LONG, BYVAL var1 AS DWORD) AS LONG
'DECLARE FUNCTION wglSwapMultipleBuffers    LIB "opengl32.dll" ALIAS "wglSwapMultipleBuffers"    (BYVAL var1 AS DWORD, var2 AS WGLSWAP) AS DWORD
#ENDIF

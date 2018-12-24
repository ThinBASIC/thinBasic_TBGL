' =============================================================================
'  Program name: thinBASIC TBGL
'
'  Description : thinBasic module for 3D graphics
' =============================================================================

' -----------------------------------------------------------------------------
' -- RESOURCES
' -----------------------------------------------------------------------------
#RESOURCE ICON, PROGRAM, "TBGL.ICO"
#RESOURCE VERSIONINFO
#RESOURCE FILEVERSION 1, 10, 16, 0
#RESOURCE PRODUCTVERSION 1, 10, 16, 0

#RESOURCE STRINGINFO "0409", "04B0"

#RESOURCE VERSION$ "CompanyName",      "thinBasic"
#RESOURCE VERSION$ "FileDescription",  "thinBASIC module for 2D/3D graphics"
#RESOURCE VERSION$ "FileVersion",      "1.10.16.0"
#RESOURCE VERSION$ "InternalName",     "TBGL"
#RESOURCE VERSION$ "OriginalFilename", "ThinBASIC_TBGL.dll"
#RESOURCE VERSION$ "LegalCopyright",   "Copyright © thinBasic 2018 (Dec 24 2018 Build)"
#RESOURCE VERSION$ "ProductName",      "TBGL"
#RESOURCE VERSION$ "ProductVersion",   "1.10.16.0"
#RESOURCE VERSION$ "Comments",         "Support site: http://www.thinbasic.com/"

' -----------------------------------------------------------------------------
' -- DIRECTIVES
' -----------------------------------------------------------------------------
#OPTIMIZE SPEED   ' -- Speed optimization via aligning
#TOOLS OFF
'#debug display on

#COMPILE DLL "thinbasic_TBGL.dll"
#DIM ALL

' -----------------------------------------------------------------------------
' -- INCLUDES /ELEMENTAL
' -----------------------------------------------------------------------------
%USEMACROS = 1
#INCLUDE "Windows.INC"

' -- Every used defined thinBasic module must include this file
#INCLUDE "thinCore.inc"

' -- Includes from José Roca
#INCLUDE "wgl/glext.inc" ' -- Josés headers, include gl.inc and glu.inc
#INCLUDE "wgl/glwgl.inc" ' -- Mod of Sublevel 6 headers

' -----------------------------------------------------------------------------
' -- GENERAL PURPOSE EQUATES AND CODE PIECES
' -----------------------------------------------------------------------------
%TBGL_2D              = 2
%TBGL_3D              = 3

%TBGL_PARAM_RELATIVE  = 0
%TBGL_PARAM_PIXELS    = 1

%TBGL_CLIENTAREA      = 0
%TBGL_VIEWPORT        = 1
%TBGL_WINDOWAREA      = 2
%TBGL_CUSTOM          = -1

%TBGL_X               = 1
%TBGL_Y               = 2
%TBGL_Z               = 3
%TBGL_UNDEFINED       = -2147483648

%TBGL_RED             = 1
%TBGL_GREEN           = 2
%TBGL_BLUE            = 4
%TBGL_ALPHA           = 8

%TBGL_FOG             = 1
%TBGL_TEXTURING       = 2
%TBGL_BLEND           = 4
%TBGL_DEPTH           = 16
%TBGL_DEPTHMASK       = 32
%TBGL_LIGHTING        = 64
%TBGL_LINESTIPPLE     = 128
%TBGL_FRAGOP_COPY_INVERTED = 256

%TBGL_FILE            = 0
%TBGL_RETURN          = 1
' %TBGL_MEMORY          = 2

%TBGL_DEFAULT         = -1
%TBGL_RESET           = 1
%TBGL_PRESERVE        = 2

MACRO TBGL_HANDLE_SIZE= 4      ' -- In bytes
MACRO TBGL_HANDLE     = LONG


GLOBAL g_ModuleInstance AS LONG
GLOBAL g_defaultFOV AS DOUBLE
GLOBAL g_customViewportRatio AS DOUBLE, g_customViewportRatioActive AS LONG

#IF NOT %DEF(%WINAPI)
'  DECLARE FUNCTION GetProcessHeap LIB "KERNEL32.DLL" ALIAS "GetProcessHeap" () AS LONG
  DECLARE FUNCTION HeapAlloc LIB "KERNEL32.DLL" ALIAS "HeapAlloc" (BYVAL hHeap AS DWORD, BYVAL dwFlags AS DWORD, BYVAL dwBytes AS DWORD) AS DWORD
  DECLARE FUNCTION HeapFree LIB "KERNEL32.DLL" ALIAS "HeapFree" (BYVAL hHeap AS DWORD, BYVAL dwFlags AS DWORD, BYVAL lpMem AS DWORD) AS LONG
  DECLARE FUNCTION HeapReAlloc LIB "KERNEL32.DLL" ALIAS "HeapReAlloc" (BYVAL hHeap AS DWORD, BYVAL dwFlags AS DWORD, BYVAL lpMem AS DWORD, BYVAL dwBytes AS DWORD) AS DWORD
  %HEAP_NO_SERIALIZE             = &H00000001
  %HEAP_GENERATE_EXCEPTIONS      = &H00000004
  %HEAP_ZERO_MEMORY              = &H00000008
#ENDIF

#IF NOT %DEF(%HEAP_ALLOC_FLAGS)
' See MSDN for options for flags.
%HEAP_ALLOC_FLAGS    = %HEAP_ZERO_MEMORY OR %HEAP_GENERATE_EXCEPTIONS
%HEAP_FREE_FLAGS     = 0&
#ENDIF


FUNCTION Mem_Alloc  (BYVAL NumberOfBytes AS LONG) AS DWORD
  ' returns: address of block of size NumberOfBytes
   FUNCTION = HeapAlloc (GetProcessHeap(), %HEAP_ALLOC_FLAGS, NumberOfBytes)
END FUNCTION

FUNCTION Mem_Free (BYVAL BlockAddress AS DWORD) AS DWORD
  'returns: null on failure (probably was passed an invalid pointer)
  FUNCTION = HeapFree (getProcessHeap(), %HEAP_FREE_FLAGS, blockAddress)
END FUNCTION

FUNCTION Mem_Copy (BYVAL fromAddress AS DWORD, BYVAL toAddress AS DWORD, BYVAL howMuch AS DWORD ) AS DWORD
  LOCAL sBuffer AS STRING

  sBuffer = PEEK$(fromAddress, howMuch)
  POKE$ toAddress, sBuffer
END FUNCTION


' -- General purpose macro, which will help to handle TBGL_Use* single parameter commands
MACRO MACRO_Use1Param(glParameter)
  LOCAL flag AS EXT
  flag = thinBasic_Parse1Number

  IF thinBasic_ErrorFree THEN
    IF flag THEN glEnable glParameter ELSE glDisable glParameter
  END IF
END MACRO

' -- General purpose macro, which will help to handle TBGL_Use* dual parameter commands
' -- *dual parameter in final keyword, such as TBGL_UseLightSource <Light>, <State>
MACRO MACRO_Use2Param()
  LOCAL glstate, flag AS EXT

  thinBasic_Parse2Numbers( glstate, flag )

  IF thinBasic_ErrorFree THEN
    IF flag THEN glEnable glstate ELSE glDisable glstate
  END IF
END MACRO

' -- Used in functions parsing 3 params byref
SUB internal_Parse3Byref( BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE )

  LOCAL lVariablePtr1 AS LONG
  LOCAL lVariableAbsPos1 AS LONG

  LOCAL lVariablePtr2 AS LONG
  LOCAL lVariableAbsPos2 AS LONG

  LOCAL lVariablePtr3 AS LONG
  LOCAL lVariableAbsPos3 AS LONG

  IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
    thinBasic_VariableParse( lVariablePtr1, lVariableAbsPos1 )
    IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
      thinBasic_VariableParse( lVariablePtr2, lVariableAbsPos2 )
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse( lVariablePtr3, lVariableAbsPos3 )
        IF thinBasic_CheckCloseParens_Mandatory THEN

          '---Now assign values to referenced variables
          thinBasic_ChangeVariableNumberDirect( lVariablePtr1, lVariableAbsPos1, x )
          thinBasic_ChangeVariableNumberDirect( lVariablePtr2, lVariableAbsPos2, y )
          thinBasic_ChangeVariableNumberDirect( lVariablePtr3, lVariableAbsPos3, z )

        END IF
      END IF
    END IF
  END IF

END SUB

FUNCTION File_Exists(BYVAL FullFileName AS STRING) AS LONG
  FUNCTION = %FALSE
  IF DIR$(FullFileName, %NORMAL OR %READONLY OR %HIDDEN OR %SYSTEM) = "" THEN EXIT FUNCTION
  FUNCTION = %TRUE
END FUNCTION

' -----------------------------------------------------------------------------
' -- TBGL TYPES and GLOBALs - placed here to avoid circular reference
' -----------------------------------------------------------------------------

' -- UDT describing window properties
TYPE tWinProperties
  ClassName AS ASCIIZ * 80    ' -- Name of class
  HANDLE AS DWORD             ' -- Window handle
  flagsWindowed AS LONG       ' -- Window flags = whether to allow maximize, minimize ...
  DrawDistance AS LONG        ' -- Draw distance
  hDC AS DWORD                ' -- Handle of device
  hRC AS DWORD                ' -- Context basedon on hDC
  fullScreen AS BYTE          ' -- Determines whether window is fullscreen or windowed
  XRes AS LONG                ' -- Actual X resolution
  YRes AS LONG                ' -- Actual Y resolution
  ignrAspect AS LONG          ' -- Do we allow to ignore original aspect ratio?
  aspectRat AS DOUBLE         ' -- Actual aspect ratio
  BitDepth AS BYTE            ' -- Bit depth of colors
  xPos AS LONG                ' -- x position of window
  yPos AS LONG                ' -- y position of window

  RenderMatrixMode AS LONG    ' -- 2D or 3D

  controlParent AS LONG       ' -- Parent dialog

  periodicFuncPointer  AS LONG
  periodicFuncInterval AS LONG

  minWidth  AS LONG
  minHeight AS LONG
END TYPE

GLOBAL g_Win AS tWinProperties
' -----------------------------------------------------------------------------
' -- TBGL INCLUDES
' -----------------------------------------------------------------------------

#INCLUDE "tbgl_ErrorHandling.inc"
#INCLUDE "tbgl_LowLevelOpenGL.inc"

#INCLUDE "tbgl_garbageCollection.inc"
#INCLUDE "tbgl_Texturing.inc"
#INCLUDE "tbgl_Entities.inc"

#INCLUDE "tbgl_Window.inc"
#INCLUDE "tbgl_Canvas.inc"

#INCLUDE "tbgl_Frame.inc"
#INCLUDE "tbgl_Transformations.inc"
#INCLUDE "tbgl_ColorAndAlpha.inc"
#INCLUDE "tbgl_Materials.inc"
#INCLUDE "tbgl_Blending.inc"
#INCLUDE "tbgl_Lighting.inc"
#INCLUDE "tbgl_Depth.inc"
#INCLUDE "tbgl_Fog.inc"
#INCLUDE "tbgl_Clipping.inc"
#INCLUDE "tbgl_Fonts.inc"

#INCLUDE "tbgl_Input.inc"
#INCLUDE "tbgl_Geometry.inc"

#INCLUDE "tbgl_CollisionAndTests.inc"
#INCLUDE "tbgl_Primitives2D.inc"
#INCLUDE "tbgl_Sprites2D.inc"


SUB internal_InitValues( )

  LastBindedTexture = - 1
  g_Win.DrawDistance = 150
  internal_SetPrimitiveQuality( 24 )

  error_display = %TBGL_ERROR_MSGBOX
  texture_DefaultModelFilter = %TBGL_TEX_MIPMAP
  texture_DefaultModelFilterMultiplier = 1

  g_TBGL_SetActiveBMPFont = 1
  g_TBGL_SetActiveFont    = 1

  g_Win.flagsWindowed     = %WS_VISIBLE OR %WS_CAPTION
  g_Win.ignrAspect        = 0

  g_Win.minWidth           = -1
  g_Win.minHeight          = -1

  g_defaultFOV            = 45
  g_customViewportRatio       = 1
  g_customViewportRatioActive = %FALSE

  REDIM StateProtectStack(1 TO 4) AS GLOBAL LONG
  StateProtectStackDepth = 0

  REDIM StateStack(1 TO 4) AS GLOBAL LONG
  StateStackDepth = 0

  REDIM LineWidthStack(1 TO 4) AS GLOBAL LONG
  LineWidthStackDepth = 0

  REDIM LineStippleStack(1 TO 4) AS GLOBAL LineStipple_Item
  LineStippleStackDepth = 0

  REDIM PointSizeStack(1 TO 4) AS GLOBAL LONG
  PointSizeStackDepth = 0

  REDIM ColorStack(1 TO 4) AS GLOBAL ColorStack_Item
  ColorStackDepth = 0

  REDIM PolygonLookStack(1 TO 4) AS GLOBAL LONG
  PolygonLookStackDepth = 0

  REDIM BindTextureStack(1 TO 4) AS GLOBAL LONG
  BindTextureStackDepth = 0

  REDIM BlendFuncStack(1 TO 4) AS GLOBAL BlendFunc_Item
  BlendFuncStackDepth = 0

  REDIM AlphaFuncStack(1 TO 4) AS GLOBAL AlphaFunc_Item
  AlphaFuncStackDepth = 0

  REDIM DepthFuncStack(1 TO 4) AS GLOBAL LONG
  DepthFuncStackDepth = 0

  internal_SceneSubsystemInit( )

  resource_Window_Alloc()

END SUB

'----------------------------------------------------------------------------

FUNCTION LoadLocalSymbols ALIAS "LoadLocalSymbols" ( OPTIONAL BYVAL sPath AS STRING ) EXPORT AS LONG
  ' This function is automatically called by thinCore whenever this DLL is loaded.
  ' This function MUST be present in every external DLL you want to use
  ' with thinBasic
  ' Use this function to initialize every variable you need and for loading the
  ' new symbol (read Keyword) you have created.
  '----------------------------------------------------------------------------

  ' -----------------------------------------------------------------------------
  ' -- INITIALIZATION
  ' -----------------------------------------------------------------------------

  QueryPerformanceFrequency( freq )               ' -- If succeeds, we can use high precision FrameRate measurement

  DIM lpTexture( 0 TO %LIMIT_MAX_TEXTUREINDEX )
  DIM lpTextureFont( 0 TO %LIMIT_MAX_FONTINDEX )
  DIM TextureList( 0 TO %LIMIT_MAX_TEXTUREINDEX )

  DIM g_WinFont( 1 TO 4 ) AS GLOBAL tWinFont
  DIM g_BMPFont( 1 TO 4 ) AS GLOBAL tBMPFont

  internal_InitValues( )

  GetAsyncKeyState( %VK_LBUTTON )
  GetAsyncKeyState( %VK_RBUTTON )
  GetAsyncKeyState( %VK_MBUTTON )

  DIM keyPressed( 0 TO 256 ) AS BYTE


  ' -----------------------------------------------------------------------------
  ' -- UDT
  ' -----------------------------------------------------------------------------
  thinBasic_AddUDT( _
    "TYPE TBGL_tTexturingInfo" + $CRLF + _
    "  maxWidth     AS LONG" + $CRLF + _
    "  maxHeight    AS LONG" + $CRLF + _
    "  NPOTSupport  AS LONG" + $CRLF + _
    "END TYPE" _
    )

  thinBasic_AddUDT( _
    "  TYPE TBGL_tUseInfo" + $CRLF + _
    "    Fog           AS LONG" + $CRLF + _
    "    Texturing     AS LONG" + $CRLF + _
    "    Blend         AS LONG" + $CRLF + _
    "    Depth         AS LONG" + $CRLF + _
    "    DepthMask     AS LONG" + $CRLF + _
    "    Lighting      AS LONG" + $CRLF + _
    "    Alpha         AS LONG" + $CRLF + _
    "    LineStipple   AS LONG" + $CRLF + _
    "    RenderMatrixMode  AS LONG" + $CRLF + _
    "  END TYPE" _
    )

    thinBasic_AddUDT(_
    "TYPE TBGL_tEntityIdentifier" + $CRLF + _
    "  scene   AS LONG" + $CRLF + _
    "  entity  AS LONG" + $CRLF + _
    "END TYPE" _
    )

    thinBasic_AddUDT(_
    "TYPE TBGL_tFaceParameters" + $CRLF + _
    "  R   AS BYTE" + $CRLF + _
    "  G   AS BYTE" + $CRLF + _
    "  B   AS BYTE" + $CRLF + _
    "  Texture   AS LONG" + $CRLF + _
    "END TYPE" _
    )

    thinBasic_AddUDT(_
    "TYPE TBGL_tVector3D" + $CRLF + _
    "  x AS DOUBLE" + $CRLF + _
    "  y AS DOUBLE" + $CRLF + _
    "  z AS DOUBLE" + $CRLF + _
    "END TYPE" _
    )

  ' -----------------------------------------------------------------------------
  ' -- KEYWORDS
  ' -----------------------------------------------------------------------------

  thinBasic_LoadSymbolEx  "TBGL_AlphaFunc"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_AlphaFunc )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_AlphaFunc(BYVAL function AS NUMBER, BYVAL value AS NUMBER)", "Specifies the way alpha testing will be performed"

  thinBasic_LoadSymbolEx  "TBGL_BackColor"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_tbgl_BackColor )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_BackColor( R, G, B [, Alpha] )", "Sets implicit color for background of the scene"

  thinBasic_LoadSymbolEx  "TBGL_BeginPoly"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_BeginPoly )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_BeginPoly( Type )", "Starts the definition of verticesDepending on specified equate constant you can create points, lines and polys this way"

  thinBasic_LoadSymbolEx  "TBGL_BindCanvas"                       , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_BindCanvas )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_BindCanvas( hCtrl )", "This command allows to render 3D graphics over existing GUI control, it should be first TBGL command you use"

  thinBasic_LoadSymbolEx  "TBGL_BindTexture"                      , %thinBasic_ReturnCodeLong               , CODEPTR( Exec_TBGL_BindTexture )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_BindTexture( TextureIndex )", "Sets specified texture as current.This texture will be mapped automatically to following objects, if texturing is enabled"

  thinBasic_LoadSymbolEx  "TBGL_BlendFunc"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_BlendFunc )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_BlendFunc( sFactor, dFactor )", "Sets the default blending function for use when blending is enabled."

  thinBasic_LoadSymbolEx  "TBGL_Box"                              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Box )                                    , %thinBasic_ForceOverWrite, _
                          "TBGL_Box( sX, sY, sZ )", "Creates box with predefined normal vectors and texture coordinates"

  thinBasic_LoadSymbolEx  "TBGL_BuildFont"                        , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_BuildFont )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_BuildFont( fontHandle [, fontSlot] )", "Creates OpenGL font from any Windows font, returns font slot"

  thinBasic_LoadSymbolEx  "TBGL_CallList"                         , %thinBasic_ReturnCodeLong               , CODEPTR( Exec_TBGL_CallList )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_CallList( ListNumber )", "Calls a predefined display list."

  thinBasic_LoadSymbolEx  "TBGL_Camera"                           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Camera )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_Camera( x, y, z, x2, y2, z2 )", "Sets camera to determine how scene will be viewed"

  thinBasic_LoadSymbolEx  "TBGL_CanvasBound"                      , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_CanvasBound )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_CanvasBound( hCtrl )", "Function to determine whether it is possible to render to canvas control or not"

  thinBasic_LoadSymbolEx  "TBGL_CenterCursor"                     , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_CenterCursor )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_CenterCursor", "Places cursor in middle of client area"

  thinBasic_LoadSymbolEx  "TBGL_ClearFrame"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_ClearFrame )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_ClearFrame [( mode ) ]", "Prepares window for drawing."

  thinBasic_LoadSymbolEx  "TBGL_Color"                            , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Color )                                  , %thinBasic_ForceOverWrite, _
                          "TBGL_Color( R, G, B[, A] )", "Sets default color for vertexes, objects ..."

  thinBasic_LoadSymbolEx  "TBGL_ColorAlpha"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_ColorAlpha )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_ColorAlpha( R, G, B, Alpha )", "Sets default color and alpha value for vertexes, objects ..."

  thinBasic_LoadSymbolEx  "TBGL_CreateWindow"                     , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_CreateWindow )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_CreateWindow|TBGL_CreateWindow( Caption )|TBGL_CreateWindow( Caption, XResolution, YResolution, BitDepth )", "Creates TBGL window, OBSOLETE"

  thinBasic_LoadSymbolEx  "TBGL_CreateWindowEx"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_CreateWindowEx )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_CreateWindowEx( Caption, XResolution, YResolution, BitDepth, windowFlags [, xPos , yPos ] )", "Creates TBGL window, it should be first TBGL command you use"

  thinBasic_LoadSymbolEx  "TBGL_Cylinder"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Cylinder )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_Cylinder( BottomRadius, TopRadius, Height )", "Creates cylinder with predefined normal vectors and texture coordinates"

  thinBasic_LoadSymbolEx  "TBGL_DeleteList"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_DeleteList )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_DeleteList( ListNumber )", "Deletes already defined display list"

  thinBasic_LoadSymbolEx  "TBGL_DepthFunc"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_DepthFunc )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_DepthFunc( function )", "Specifies the way depth testing will be performed"


  thinBasic_LoadSymbolEx  "TBGL_DestroyWindow"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_DestroyWindow )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_DestroyWindow", "Destroys TBGL window"

  thinBasic_LoadSymbolEx  "TBGL_DrawFrame"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_DrawFrame )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_DrawFrame", "Swaps screen buffersUse it to perform rendering of the scene"

  thinBasic_LoadSymbolEx  "TBGL_EndList"                          , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EndList )                                , %thinBasic_ForceOverWrite, _
                          "TBGL_EndList", "Ends the TBGL_NewList section."

  thinBasic_LoadSymbolEx  "TBGL_EndPoly"                          , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EndPoly )                                , %thinBasic_ForceOverWrite, _
                          "TBGL_EndPoly", "Ends the definition of vertices"

  thinBasic_LoadSymbolEx  "TBGL_EntityCopyTo"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCopyTo )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCopyTo( SceneID, EntityID, DestinationEntityID )", "Copies entity to empty slot"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateBox"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateBox )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateBox( SceneID, EntityID [, ParentEntityID [, sX, sY, sZ [, Texture [, R, G, B ]]]] )", "Creates new box entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateCamera"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateCamera )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateCamera( SceneID, EntityID [, ParentEntityID ])", "Creates new camera entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateCylinder"             , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateCylinder )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateCylinder( SceneID, EntityID [, ParentEntityID [, RadiusLower [, RadiusHigher [, Height [, Tex [, R, G, B ]]]]]] )", "Creates new cylinder entity without capped bases"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateCylinderCapped"       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateCylinderCapped )             , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateCylinderCapped( SceneID, EntityID [, ParentEntityID [, RadiusLower [, RadiusHigher [, Height [, Tex [, R, G, B ]]]]]] )", "Creates new cylinder entity with capped bases"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateDLSlot"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateDLSlot )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateDLSlot( SceneID, EntityID, ParentEntityID, DisplayList )", "Binds display list to entity system"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateFuncSlot"             , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateFuncSlot )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateFuncSlot( SceneID, EntityID, ParentEntityID, UserFunction )", "Creates new function slot entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateLight"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateLight )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateLight( SceneID, EntityID [, ParentEntityID [, Type ]] )", "Creates new light entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateModelSlot"            , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateModelSlot )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateModelSlot( SceneID, EntityID, ParentEntityID, ModelID )", "Binds model loaded using TBGL_m15LoadModel to entity system"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreatePivot"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreatePivot )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreatePivot( SceneID, EntityID [, ParentEntityID [, X, Y, Z ]] )", "Creates new pivot entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateSphere"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateSphere )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateSphere( SceneID, EntityID [, ParentEntityID [, Radius [, Tex [, R, G, B ]]]] )", "Creates new sphere entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityCreateTorus"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityCreateTorus )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityCreateTorus( SceneID, EntityID [, ParentEntityID [, RadiusMinor [, RadiusMajor [, Tex [, R, G, B ]]]]] )", "Creates new torus entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityDestroy"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityDestroy )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityDestroy( SceneID, EntityID )", "Destroys entity and makes entity slot empty"

  thinBasic_LoadSymbolEx  "TBGL_EntityFindByPos"                  , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityFindByPos )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityFindByPos( SceneID, EntityType, x, y, z [, GlobalCoordinates ] )", "Returns ID of first entity of given type which has same x, y, z as passed parameters"

  thinBasic_LoadSymbolEx  "TBGL_EntityFindNearest"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityFindNearest )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityFindNearest( SceneID, EntityType, ReferenceEntity, rangeX, rangeY, rangeZ [, GlobalCoordinates ] )", "Returns ID of nearest entity of given type which fits in specified range"

  thinBasic_LoadSymbolEx  "TBGL_EntityFindNearestByPos"           , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityFindNearestByPos )                 , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityFindNearestByPos( SceneID, EntityType, x, y, z, rangeX, rangeY, rangeZ [, GlobalCoordinates ] )", "Returns ID of nearest entity of given type which fits in specified range"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetAngleXY"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetAngleXY )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetAngleXY( SceneID, EntityID, EntityTargetID, ForwardDirection )", "Returns angle between specified entity and reference entity, based on comparsion of positions in XY plane"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetAngleXZ"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetAngleXZ )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetAngleXZ( SceneID, EntityID, EntityTargetID, ForwardDirection )", "Returns angle between specified entity and reference entity, based on comparsion of positions in XZ plane"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetAngleYZ"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetAngleYZ )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetAngleYZ( SceneID, EntityID, EntityTargetID, ForwardDirection )", "Returns angle between specified entity and reference entity, based on comparsion of positions in YZ plane"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetDistance"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetDistance )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetDistance( SceneID, EntityID1, EntityID2 )", "Gets distance between two given entities"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetDistancePos"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetDistancePos )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetDistancePos( SceneID, EntityID, x, y, z )", "Gets distance between given entity and position"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetFreeID"                  , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetFreeID )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetFreeID( SceneID [, StartingEntityID] )", "Returns number of next free entityID slot"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetName"                    , %thinBasic_ReturnString                 , CODEPTR( Exec_TBGL_EntityGetName )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetName( SceneID, EntityID )", "Retrieves name of an entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetPos"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityGetPos )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetPos( SceneID, EntityID, X, Y, Z [, GlobalCoordinates ] )", "Gets position of entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetUse"                     , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetUse )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetUse( SceneID, EntityID )", "Determines whether entity is enabled or disabled"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetUserDataPointer"         , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetUserDataPointer )               , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetUserDataPointer( SceneID, EntityID )", "Returns pointer to data stored using TBGL_EntitySetUserData."

  thinBasic_LoadSymbol  "TBGL_EntityGetUserPointer"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetUserPointer )                   , %thinBasic_ForceOverWrite

  thinBasic_LoadSymbolEx  "TBGL_EntityGetXAxis"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityGetXAxis )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetXAxis( SceneID, EntityID, X, Y, Z )", "Returns entity X axis in global coordinates"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetYAxis"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityGetYAxis )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetYAxis( SceneID, EntityID, X, Y, Z )", "Returns entity Y axis in global coordinates"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetZAxis"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityGetZAxis )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetZAxis( SceneID, EntityID, X, Y, Z )", "Returns entity Z axis in global coordinates"

  thinBasic_LoadSymbolEx  "TBGL_EntityMove"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityMove )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityMove( SceneID, EntityID, X, Y, Z )", "Adds specified values to x, y, z global position of entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityPush"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityPush )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityPush( SceneID, EntityID, X, Y, Z )", "Adds specified values to x, y, z in local coordinates of entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetAmbient"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetAmbient )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetAmbient( SceneID, EntityID, R, G, B )", "Sets ambient color for light entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetBorderFade"              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetBorderFade )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetBorderFade( SceneID, EntityID, BorderFade )", "Sets property to determine light distribution for spotlight"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetColor"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetColor )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetColor( SceneID, EntityID, R, G, B )", "Sets RGB color for whole entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetColorMask"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetColorMask )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetColorMask( SceneID, EntityID, Mask )", "Defines the way camera provides image of geometry from scene via color component filtering."

  thinBasic_LoadSymbolEx  "TBGL_EntitySetCutoff"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetCutoff )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetCutoff( SceneID, EntityID, CutoffAngle )", "Sets cutoff angle for spotlight"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetFOV"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetFOV )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetFOV( SceneID, EntityID, Angle2 )", "Sets field of view for camera"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetName"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetName )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetName( SceneID, EntityID, Name )", "Assigns name to an entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetPos"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetPos )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetPos( SceneID, EntityID, X, Y, Z )", "Sets position to entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetRot"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetRot )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetRot( SceneID, EntityID, X, Y, Z )", "Resets entity axes and sets angles of rotation"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetScale"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetScale )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetScale( SceneID, EntityID, X, Y, Z )", "Scales entity by specified factors"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetScale"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityGetScale )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetScale( SceneID, EntityID, varX, varY, varZ )", "Retrieves entity scale to passed variables"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetTarget"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetTarget )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetTarget( SceneID, EntityID, EntityToLookAt )|TBGL_EntitySetTarget( SceneID, EntityIDFrom, EntityIDTo, EntityToLookAt )", "Makes one or more entities to look at other entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetTargetPos"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetTargetPos )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetTargetPos( SceneID, EntityID, X, Y, Z )", "Makes entity look at specific position"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetTexture"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetTexture )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetTexture( SceneID, EntityID, textureIndex )", "Sets texture for whole entity"

  thinBasic_LoadSymbolEx  "TBGL_EntityGetTexture"                 , %thinBasic_ReturnCodeLong               , CODEPTR( Exec_TBGL_EntityGetTexture )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetTexture( SceneID, EntityID ) AS LONG", "Gets texture assgigned to entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetUse"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetUse )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetUse( SceneID, EntityID, UseFlag )", "Sets whether entity will be considered in scene composition process"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetUserData"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetUserData )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetUserData( SceneID, EntityID, UserData )", "Assigns any user defined data to the entity."

  thinBasic_LoadSymbol  "TBGL_EntitySetUserPointer"             , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetUserPointer )                   , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_EntitySetXYAxis"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetXYAxis )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetXYAxis( SceneID, EntityID, xX, xY, xZ, yX, yY, yZ )", "Sets local X and Y axis to the entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetXZAxis"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetXZAxis )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetXZAxis( SceneID, EntityID, xX, xY, xZ, zX, zY, zZ )", "Sets local X and Z axis to the entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySetYZAxis"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetYZAxis )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetYZAxis( SceneID, EntityID, yX, yY, yZ, zX, zY, zZ )", "Sets local Y and Z axis to the entity"

  thinBasic_LoadSymbolEx  "TBGL_EntitySyncAxes"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySyncAxes )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySyncAxes( SceneID, EntityID, EntityID2, GlobalCoordinates )", "Allows to synchronize rotation of two entites"

  thinBasic_LoadSymbolEx  "TBGL_EntityTrackPos"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityTrackPos )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityTrackPos( SceneID, EntityID, localX, localY, localZ, globalX, globalY, globalZ   )", "Converts entity local coordinates to global coordinates"

  thinBasic_LoadSymbolEx  "TBGL_EntityTurn"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityTurn )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityTurn( SceneID, EntityID, X, Y, Z )", "Turns entity around its local axes by specified angles"

  thinBasic_LoadSymbolEx  "TBGL_ErrorMessages"                    , %thinBasic_ReturnCodeLong               , CODEPTR( Exec_TBGL_ErrorMessages )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_ErrorMessages Type", "Statement to specify way of TBGL run-time error notification"

  thinBasic_LoadSymbolEx  "TBGL_EvaluatePOTMatch"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EvaluatePOTMatch )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EvaluatePOTMatch( resolutionX, resolutionY, varFitX, varFitY )", "Finds nearest safe match for non power of two resolution."

  thinBasic_LoadSymbolEx  "TBGL_FontHandle"                       , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_FontHandle )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_FontHandle( fontName, fontSize [, style ] )", "Returns handle to Windows font, this can be used with TBGL_BuildFont"

  thinBasic_LoadSymbolEx  "TBGL_GetAsyncKeyState"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetAsyncKeyState )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_GetAsyncKeyState( keyCode )", "Useful function for checking key status independently on actual window"

  thinBasic_LoadSymbolEx  "TBGL_GetDesktopInfo"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_GetDesktopInfo )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetDesktopInfo( variableX, variableY, variableDepth )", "Fills passed variables with desktop size and color depth"

  thinBasic_LoadSymbolEx  "TBGL_DisplayGetCount"                   , %thinBasic_ReturnCodeLong              , CODEPTR( Exec_TBGL_DisplayGetCount )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_DisplayGetCount()"                 , "Returns the count of displays currently attached"

  thinBasic_LoadSymbolEx  "TBGL_DisplayGetInfo"                   , %thinBasic_ReturnCodeLong              , CODEPTR( Exec_TBGL_DisplayGetInfo )                , %thinBasic_ForceOverWrite, _
                          "TBGL_DisplayGetInfo( displayNumber, variableWidth, variableHeight, OPTIONAL variableDepth)", "Returns the resolution and bit depth of specified display"

  thinBasic_LoadSymbolEx  "TBGL_SendWindowToDisplay"                   , %thinBasic_ReturnCodeLong              , CODEPTR( Exec_TBGL_SendWindowToDisplay )                , %thinBasic_ForceOverWrite, _
                          "TBGL_SendWindowToDisplay( windowHandle, displayNumber, OPTIONAL newX, newY)", "Sends window to specified display"

  thinBasic_LoadSymbolEx  "TBGL_GetWindowDisplay"                   , %thinBasic_ReturnCodeLong              , CODEPTR( Exec_TBGL_GetWindowDisplay )                , %thinBasic_ForceOverWrite, _
                          "TBGL_GetWindowDisplay( windowHandle)", "Retrieves number of display the window is present on"

  thinBasic_LoadSymbolEx  "TBGL_GetWindowPos"                   , %thinBasic_ReturnNumber              , CODEPTR( Exec_TBGL_GetWindowPos )                , %thinBasic_ForceOverWrite, _
                          "TBGL_GetWindowPos( BYVAL windowHandle AS DWORD, BYREF X AS LONG, BYREF Y AS LONG)", "Retrieves position of the upper left corner of specified window"


  thinBasic_LoadSymbolEx  "TBGL_GetFullscreenModes"               , %thinBasic_ReturnString                 , CODEPTR( Exec_TBGL_GetFullscreenModes )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_GetFullscreenModes", "Returns list of available fullscreen modes"


  IF freq <> 0 THEN
    thinBasic_LoadSymbolEx "TBGL_GetFrameRate", %thinBasic_ReturnNumber, CODEPTR( EXEC_TBGL_GetFrameRate_PERFCOUNTER ), %thinBasic_ForceOverWrite, _
                          "TBGL_GetFramerate", "Returns current framerate based on time gap beetween last call and this"

  ELSE
    thinBasic_LoadSymbolEx "TBGL_GetFrameRate", %thinBasic_ReturnNumber, CODEPTR( EXEC_TBGL_GetFrameRate_GETTICKCOUNT ), %thinBasic_ForceOverWrite, _
                          "TBGL_GetFramerate", "Returns current framerate based on time gap beetween last call and this"

  END IF

  thinBasic_LoadSymbolEx  "TBGL_GetLastGLError"                   , %thinBasic_ReturnString                 , CODEPTR( Exec_TBGL_GetLastGLError )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetLastGLError", "Returns text description of last OpenGL error"

  thinBasic_LoadSymbolEx  "TBGL_GetMultiAsyncKeyState"            , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetMultiAsyncKeyState )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_GetMultiAsyncKeyState( keyCode1 [, keyCode2[, keyCode3]] )", "Useful function for checking if multiple keys are pressed independently on actual window"

  thinBasic_LoadSymbolEx  "TBGL_GetPixelInfo"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_GetPixelInfo )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_GetPixelInfo( x, y, type, variable1, variable2, variable3 )", "Fills passed variables with requested values to obtain x,y,z position or RGB color from 3D world"

  thinBasic_LoadSymbolEx  "TBGL_GetProcAddress"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetProcAddress )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetProcAddress( sExtensionFunction )", "Returns the address of an OpenGL extension function"

  thinBasic_LoadSymbolEx  "TBGL_GetRenderMatrixMode"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetRenderMatrixMode )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_GetRenderMatrixMode", "Command to determine whether TBGL is set to 2D or 3D drawing mode"

  thinBasic_LoadSymbolEx  "TBGL_GetTextureData"                   , %thinBasic_ReturnString                 , CODEPTR( Exec_TBGL_GetTextureData )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetTextureData( TextureIndex AS LONG, TypeOfData AS LONG  )", "Returns raw texture data in specified format"

  thinBasic_LoadSymbolEx  "TBGL_GetFreeTextureIndex"                   , %thinBasic_ReturnNumber               , CODEPTR( Exec_TBGL_GetFreeTextureIndex )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetFreeTextureIndex()", "Returns number of texture index, to which no texture is loaded at the moment"

  thinBasic_LoadSymbolEx  "TBGL_GetTextureHandle"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetTextureHandle )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetTextureHandle( TextureIndex AS LONG )", "Returns OpenGL handle of specified TBGL texture slot"

  thinBasic_LoadSymbolEx  "TBGL_GetTextureList"                   , %thinBasic_ReturnString                 , CODEPTR( Exec_TBGL_GetTextureList )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetTextureList", "Returns comma delimited list of textures loaded in memory"

  thinBasic_LoadSymbolEx  "TBGL_GetTextureName"                   , %thinBasic_ReturnString                 , CODEPTR( Exec_TBGL_GetTextureName )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetTextureName( index )", "Returns name of texture at specified index"

  thinBasic_LoadSymbolEx  "TBGL_GetTextureResolution"             , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_GetTextureResolution )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_GetTextureResolution( TextureIndex, nWidth, nHeight )", "Retrieves resolution of texture in pixels"

  thinBasic_LoadSymbolEx  "TBGL_GetUseBlend"                      , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetUseBlend )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_GetUseBlend", "Determines whether blending is enabled"

  thinBasic_LoadSymbolEx  "TBGL_GetUseDepth"                      , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetUseDepth )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_GetUseDepth", "Determines whether depth testing is enabled"

  thinBasic_LoadSymbolEx  "TBGL_GetUseLighting"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetUseLighting )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_GetUseLighting", "Determines whether lighting is enabled"

  thinBasic_LoadSymbolEx  "TBGL_GetUseTexturing"                  , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetUseTexturing )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_GetUseTexturing", "Determines whether texturing is enabled"

  thinBasic_LoadSymbolEx  "TBGL_GetWindowAnyKeyState"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetWindowAnyKeyState )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_GetWindowAnyKeyState( hWnd, cKey1 [, vKey2[, vKey3[,vKey4]]] )", "Function to determine whether any of the passed keys has been pressed."

  thinBasic_LoadSymbolEx  "TBGL_GetWindowBMP"                     , %thinBasic_ReturnString                 , CODEPTR( Exec_TBGL_GetWindowBMP )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_GetWindowBMP( wHandle, [CaptureType [, Width[, Height]]] )", "It returns screenshot of specified window in BMP string"

  thinBasic_LoadSymbolEx  "TBGL_GetWindowClient"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_GetWindowClient )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_GetWindowClient( hWnd, variableX, variableY )", "Fills passed variables with window client width and height"

  thinBasic_LoadSymbolEx  "TBGL_GetWindowKeyOnce"                 , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_GetWindowKeyOnce )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_GetWindowKeyOnce( hWnd, keyCode )", "Useful function for checking if button was pressed down"

  thinBasic_LoadSymbolEx  "TBGL_GetWindowKeyState"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetWindowKeyState )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_GetWindowKeyState( hWnd, keyCode )", "Useful function for checking key status in specified window"

  thinBasic_LoadSymbolEx  "TBGL_GetWindowMultiKeyState"           , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetWindowMultiKeyState )                 , %thinBasic_ForceOverWrite, _
                          "TBGL_GetWindowMultiKeyState( hWnd, keyCode1 [, keyCode2[, keyCode3]] )", "Useful function for checking if keys are pressed"

  thinBasic_LoadSymbolEx  "TBGL_IsFullscreen"                     , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_IsFullscreen )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_IsFullscreen( hWnd )", "Serves to determine whether TBGL window is fullscreen"

  thinBasic_LoadSymbolEx  "TBGL_IsPointBehindView"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_IsPointBehindView )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_IsPointBehindView( x, y, z )", "Returns non-zero value in case point is behind actual camera ( not visible )"

  thinBasic_LoadSymbolEx  "TBGL_IsPointVisible"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_IsPointVisible )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_IsPointVisible( x, y, z )", "Returns non-zero value in case point is directly visible from current camera"

  thinBasic_LoadSymbolEx  "TBGL_IsWindow"                         , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_IsWindow )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_IsWindow(BYVAL hWnd AS DWORD)", "Checks for existence of window specified by handle"

  thinBasic_LoadSymbolEx  "TBGL_KillFont"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_KillFont )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_KillFont[( fontSlot ) ]", "Kills font created using TBGL_BuildFont"

  thinBasic_LoadSymbolEx  "TBGL_LineStipple"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_LineStipple )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_LineStipple factor, pattern", "Specifies the line stipple pattern"

  thinBasic_LoadSymbolEx  "TBGL_LineWidth"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_LineWidth )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_LineWidth pSize", "Sets width of lines created using %GL_LINES, %GL_LINE_LOOP and %GL_LINE_STRIP style"

  thinBasic_LoadSymbolEx  "TBGL_LoadBMPFont"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_LoadBMPFont )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_LoadBMPFont BitMapName [, Lines [, FontSlot ]]", "Creates ""font"" from bitmap, optimized for rendering in 3D mode"

  thinBasic_LoadSymbolEx  "TBGL_LoadBMPFont2D"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_LoadBMPFont2D )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_LoadBMPFont2D BitMapName [, Lines [, Columns[, FontSlot ]]]", "Creates ""font"" from bitmap, optimized for rendering in 2D mode"

  thinBasic_LoadSymbolEx  "TBGL_BeginPrintBMP"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_BeginPrintBMP )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_BeginPrintBMP", "Statement to prepare printing of font loaded by TBGL_LoadBMPFont or TBGL_LoadBMPFont2D"

  thinBasic_LoadSymbolEx  "TBGL_EndPrintBMP"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EndPrintBMP )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_EndPrintBMP", "Statement to finish printing of font loaded by TBGL_LoadBMPFont or TBGL_LoadBMPFont2D"


  thinBasic_LoadSymbol  "TBGL_LoadFont"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_tbgl_LoadBMPFont )                               , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_LoadTexture"                      , %thinBasic_ReturnCodeLong               , CODEPTR( Exec_TBGL_LoadTexture )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_LoadTexture TextureFile, TextureIndex, TextureFiltering [, AnisotropicLevel [, TransparentColor ]", _
                          "Loads texture from following uncompressed file formats:BMP file with 8, 24 or 32 bit colorsTGA files - 32bit ( 24bit RGB + 8bit alpha )"


  thinBasic_LoadSymbolEx  "TBGL_LoadTexturesFromTiles"            , %thinBasic_ReturnCodeLong               , CODEPTR( Exec_TBGL_LoadTexturesFromTiles )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_LoadTexturesFromTiles fileName, startIndex, numberOfColumns, numberOfRows, TextureFiltering [, AnisotropicLevel [, TransparentColor ]", "Function creates multiple textures from one"

  thinBasic_LoadSymbolEx  "TBGL_MakeTexture"                      , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_MakeTexture )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_MakeTexture StringBuffer, TypeOfData, Width, Height, TextureIndex, TextureFiltering [, AnisotropicLevel [, TransparentColor ]", "Makes texture from data in string in specified way"

  thinBasic_LoadSymbolEx  "TBGL_DeleteTexture"                    , %thinBasic_ReturnNone                     , CODEPTR( Exec_TBGL_DeleteTexture )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_DeleteTexture(TextureIndex)", "Deletes texture from memory."


  thinBasic_LoadSymbolEx  "TBGL_MouseGetLButton"                  , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_MouseGetLButton )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_MouseGetLButton", "Function which returns whether left mouse button is down or not"

  thinBasic_LoadSymbolEx  "TBGL_MouseGetMButton"                  , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_MouseGetMButton )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_MouseGetMButton", "Function which returns whether middle mouse button is down or not"

  thinBasic_LoadSymbolEx  "TBGL_MouseGetPosX"                     , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_MouseGetPosX )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_MouseGetPosX", "Function which returns X position of mouse cursor"

  thinBasic_LoadSymbolEx  "TBGL_MouseGetPosY"                     , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_MouseGetPosY )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_MouseGetPosY", "Function which returns Y position of mouse cursor"

  thinBasic_LoadSymbolEx  "TBGL_MouseGetRButton"                  , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_MouseGetRButton )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_MouseGetRButton", "Function which returns whether right mouse button is down or not"

  thinBasic_LoadSymbolEx  "TBGL_MouseGetWheelDelta"               , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_MouseGetWheelDelta )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_MouseGetWheelDelta", "Function which returns whether mouse wheel was scrolled down or up"

  thinBasic_LoadSymbolEx  "TBGL_NewList"                          , %thinBasic_ReturnCodeLong               , CODEPTR( Exec_TBGL_NewList )                                , %thinBasic_ForceOverWrite, _
                          "TBGL_NewList ListNumber", "Creates new display listPolygons defined beetween TBGL_NewList and TBGL_EndList can be accessed by TBGL_CallList function."

  thinBasic_LoadSymbolEx  "TBGL_Normal"                           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Normal )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_Normal x, y, z", "Specifies normal vector for vertex."

  thinBasic_LoadSymbolEx  "TBGL_PointInside3D"                    , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_PointInside3D )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_PointInside3D( x, y, z, objectType, baseX, baseY, baseZ, parameter1 [, parameter2 [, parameter3 ] ] )", "Returns whether point is in specified shape"

  thinBasic_LoadSymbolEx  "TBGL_PointSize"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PointSize )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_PointSize pSize", "Set the size of vertex-point in pixels."

  thinBasic_LoadSymbolEx  "TBGL_PolygonLook"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PolygonLook )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_PolygonLook style", "Converts graphic output of whole scene to polygons, lines or points"

  thinBasic_LoadSymbolEx  "TBGL_PopMatrix"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopMatrix )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_PopMatrix", "Pop the current matrix stack."

  thinBasic_LoadSymbolEx  "TBGL_Pos3DtoPos2D"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Pos3DtoPos2D )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_Pos3DtoPos2D( x, y, z, px, py )", "Converts 3D x,y,z coordinates to 2D coordinates in pixels"

  thinBasic_LoadSymbol  "TBGL_Print"                            , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PrintBMP )                               , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_PrintBMP"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PrintBMP )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_PrintBMP Text, xPos, yPos [, alignment]", "Prints text using bitmap font loaded by TBGL_LoadBMPFont or TBGL_LoadBMPFont2D"

  thinBasic_LoadSymbolEx  "TBGL_PrintFont"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PrintFont )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_PrintFont(text, x, y [, z])", "Prints font created using TBGL_BuildFont to specified position"

  thinBasic_LoadSymbolEx  "TBGL_PrintFont2D"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PrintFont2D )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_PrintFont2D( sText, x, y [, screenAlignment, anchorAlignment, maximumWidth ])", "Prints font created using TBGL_BuildFont to specified 2D position with possible anchor setup and length truncation"

  thinBasic_LoadSymbolEx  "TBGL_GetFontTextSize"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_GetFontTextSize )             , %thinBasic_ForceOverWrite, _
                          "TBGL_GetFontTextSize( text, varWidth, varHeight )", "Returns size of the specified text for currently active font in pixels"

  thinBasic_LoadSymbolEx  "TBGL_PushMatrix"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushMatrix )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_PushMatrix", "Push the current matrix stack."

  thinBasic_LoadSymbolEx  "TBGL_ReleaseCanvas"                    , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_ReleaseCanvas )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_ReleaseCanvas( hCtrl )", "This command releases canvas control from rendering and destroys all created TBGL resources"

  thinBasic_LoadSymbolEx  "TBGL_RenderMatrix2D"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_RenderMatrix2D )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_RenderMatrix2D[( leftX, bottomY, rightX, topY )]", "Sets rendering mode to 2D mode"

  thinBasic_LoadSymbolEx  "TBGL_RenderMatrix3D"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_RenderMatrix3D )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_RenderMatrix3D[( aspectRatioType [, aspectRatio )]]", "Sets rendering mode to default 3D"

  thinBasic_LoadSymbolEx  "TBGL_RenderToTexture"                  , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_RenderToTexture )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_RenderToTexture( TextureIndex, pxX, pxY, pxWidth, pxHeight )", "Copies part of screen to texture"

  thinBasic_LoadSymbolEx  "TBGL_ResetKeyState"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_ResetKeyState )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_ResetKeyState()", "Resets key status for keyboard handling functions"

  thinBasic_LoadSymbolEx  "TBGL_ResetMatrix"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_tbgl_ResetMatrix )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_ResetMatrix", "Cancels all translations and rotations."

  thinBasic_LoadSymbolEx  "TBGL_Rotate"                           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Rotate )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_Rotate AngleInDegrees [, x, y, z ]", "Rotates by defined angle around defined vector x, y, z"

  thinBasic_LoadSymbolEx  "TBGL_RotateXYZ"                        , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_RotateXYZ )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_RotateXYZ xAngle, yAngle, zAngle", "Performs rotation around all requested axes"

  thinBasic_LoadSymbolEx  "TBGL_SaveScreenShot"                   , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_SaveScreenShot )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SaveScreenshot FileName [, CaptureType ]", "Saves current contents of TBGL window to BMP file"

  thinBasic_LoadSymbolEx  "TBGL_Scale"                            , %thinBasic_ReturnNone                   , CODEPTR( Exec_tbgl_Scale )                                  , %thinBasic_ForceOverWrite, _
                          "TBGL_Scale sx, [sy, [sz ]]", "Multiplies all x, y, z by sx, sy, sz in current matrix ."

  thinBasic_LoadSymbolEx  "TBGL_SceneCreate"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SceneCreate )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_SceneCreate( SceneID )", "Creates new scene, place to which you can put entities"

  thinBasic_LoadSymbolEx  "TBGL_SceneDestroy"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SceneCreate )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_SceneDestroy( SceneID )", "Erases scene including nested entities"

  thinBasic_LoadSymbolEx  "TBGL_SceneRender"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SceneRender )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_SceneRender( SceneID )", "Performs excution of all entites scene contains"

  thinBasic_LoadSymbol  "TBGL_SceneCollide"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SceneCollide )                           , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_SetActiveBMPFont"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetActiveBMPFont )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SetActiveBMPFont FontSlot", "Sets actual font slot for bitmap fonts loaded via TBGL_LoadBMPFont."

  thinBasic_LoadSymbolEx  "TBGL_SetActiveFont"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetActiveFont )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_SetActiveFont FontSlot", "Sets actual font slot for Windows fonts created using TBGL_BuildFont."

  thinBasic_LoadSymbolEx  "TBGL_SetDrawDistance"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetDrawDistance )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_SetDrawDistance Meters", "Sets maximum depth of visible objects."

  thinBasic_LoadSymbolEx  "TBGL_SetFullscreen"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetFullscreen )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_SetFullscreen( hWnd, XResolution, YResolution [, BitDepth ] )", "Sets TBGL window to fullscreen mode"

  thinBasic_LoadSymbolEx  "TBGL_SetLightParameter"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetLightParameter )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SetLightParameter LightEquate, ParameterType, a, b, c, d", "Used to setup various parameters of declared light source"

  thinBasic_LoadSymbolEx  "TBGL_SetPrimitiveQuality"              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetPrimitiveQuality )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SetPrimitiveQuality NumberOfEdges", "Sets general quality level for primitives"

  thinBasic_LoadSymbolEx  "TBGL_SetTextureRepeatMode"              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetTextureRepeatMode )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SetTextureRepeatMode( textureIndex, param1 [, param2 ] )", "Sets texture mapping repeat mode for specified texture"

  thinBasic_LoadSymbolEx  "TBGL_SetWindowIcon"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetWindowIcon )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_SetWindowIcon( hWnd, fileName )", "Sets icon to specified TBGL window"

  thinBasic_LoadSymbolEx  "TBGL_SetWindowTitle"                   , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_SetWindowTitle )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SetWindowTitle( hWnd, text )", "Sets text to title of specified window"

  thinBasic_LoadSymbolEx  "TBGL_SetWindowMinClient"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SetWindowMinClient )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SetWindowMinClient( BYVAL hWnd AS DWORD, BYVAL minClientWidth AS LONG, BYVAL minClientHeight AS LONG)", "Sets minmal size for TBGL window client area"


  thinBasic_LoadSymbolEx  "TBGL_SetWindowed"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetWindowed )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_SetWindowed( hWnd, XResolution, YResolution [, BitDepth [, windowFlags[, xPos , yPos ] ] ] )", "Sets TBGL window to windowed mode"

  thinBasic_LoadSymbolEx  "TBGL_SetupClipPlane"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetupClipPlane )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SetupClipPlane cpEquate, x, y, z, w", "Setups clip plane for current matrix"

  thinBasic_LoadSymbolEx  "TBGL_SetupFog"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetupFog )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_SetupFog R, G, B, Alpha, Density", "Sets attributes of fog effect"

  thinBasic_LoadSymbolEx  "TBGL_SetupLightSource"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetupLightSource )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SetupLightSource LightEquate, x, y, z, R, G, B, DiffuseValue", "Used to setup parameters of declared light source"

  thinBasic_LoadSymbolEx  "TBGL_ShowCursor"                       , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_ShowCursor )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_ShowCursor( flag )", "Serves to enable or disable cursor"

  thinBasic_LoadSymbolEx  "TBGL_ShowWindow"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_ShowWindow )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_ShowWindow", "Shows TBGL window created by TBGL_CreateWindow."

  thinBasic_LoadSymbolEx  "TBGL_Sphere"                           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Sphere )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_Sphere Radius", "Creates sphere with predefined normal vectors and texture coordinates"

  thinBasic_LoadSymbolEx  "TBGL_TexCoord2D"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_TexCoord2D )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_TexCoord2D x, y", "Assigns texture coordinate to following vertex"

  thinBasic_LoadSymbolEx  "TBGL_TexturingQuery"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_TexturingQuery )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_TexturingQuery( udtVariable )", "Allows to retrieve detailed information on texturing subsystem."

  thinBasic_LoadSymbolEx  "TBGL_Torus"                            , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Torus )                                  , %thinBasic_ForceOverWrite, _
                          "TBGL_Torus MinorRadius, MajorRadius", "Creates torus primitive object with predefined normal vectors and texture coordinates"

  thinBasic_LoadSymbolEx  "TBGL_Translate"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Translate )                              , %thinBasic_ForceOverWrite, _
                          "TBGL_Translate x, y [, z]", "Moves the coordinate system origin to the point specified by (x, y, z)"

  thinBasic_LoadSymbolEx  "TBGL_UpdateCanvasProportions"          , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_UpdateCanvasProportions )                , %thinBasic_ForceOverWrite, _
                          "TBGL_UpdateCanvasProportions( hCtrl )", "This command allows to maintain correct image proportions after resizing the canvas control"

  thinBasic_LoadSymbolEx  "TBGL_UseAlphaTest"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseAlphaTest )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_UseAlphaTest Flag", "Disable or enable alpha test"

  thinBasic_LoadSymbolEx  "TBGL_UseBlend"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseBlend )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_UseBlend Flag", "Disable or enable blending"

  thinBasic_LoadSymbol  "TBGL_UseBlendFlag"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseBlend )                               , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_UseClipPlane"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseClipPlane )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_UseClipPlane cpEquate, flag", "Disable or enable the usage of specified clip plane"

  thinBasic_LoadSymbol  "TBGL_UseClipPlaneFlag"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseClipPlane )                           , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_UseDepth"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseDepth )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_UseDepth Flag", "Disable or enable the depth testing"

  thinBasic_LoadSymbol  "TBGL_UseDepthFlag"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseDepth )                               , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_UseDepthMask"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseDepthMask )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_UseDepthMask Flag", "Disable or enable the depth mask"

  thinBasic_LoadSymbolEx  "TBGL_UseFog"                           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseFog )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_UseFog Flag", "Disable or enable usage of fog"

  thinBasic_LoadSymbol  "TBGL_UseFogFlag"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseFog )                                 , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_UseLightSource"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseLightSource )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_UseLightSource LightEquate, flag", "Disable or enable the usage of specified light source"

  thinBasic_LoadSymbol  "TBGL_UseLightSourceFlag"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseLightSource )                         , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_UseLighting"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseLighting )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_UseLighting Flag", "Disable or enable the use of lighting"

  thinBasic_LoadSymbol  "TBGL_UseLightingFlag"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseLighting )                            , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_UseLineStipple"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseLineStipple )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_UseLineStipple Flag", "Disable or enable the use of line stipple"

  thinBasic_LoadSymbol  "TBGL_UseLineStippleFlag"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseLineStipple )                         , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_UseQuery"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseQuery )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_UseQuery( udtVariable )", "Allows to retrieve detailed information on states affecting rendering."

  thinBasic_LoadSymbolEx  "TBGL_UseTexture"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseTexturing )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_UseTexture Flag", "Disable or enable texture mapping"

  thinBasic_LoadSymbol  "TBGL_UseTextureFlag"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseTexturing )                           , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_UseTexturing"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_UseTexturing )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_UseTexturing Flag", "Disable or enable texture mapping"

  thinBasic_LoadSymbolEx  "TBGL_UseVSync"                         , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_UseVSync )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_UseVSync Factor", "Statement to control vertical synchronisationAffects image quality and framerate"

  thinBasic_LoadSymbolEx  "TBGL_Vertex"                           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Vertex )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_Vertex x, y [, z]", "Adds vertex at x, y, z"

  thinBasic_LoadSymbolEx  "TBGL_Viewport"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Viewport )                               , %thinBasic_ForceOverWrite, _
                          "TBGL_Viewport( x, y, width, height [, ParameterType] )", "Allows to setup viewports in the current window."

  thinBasic_LoadSymbolEx  "TBGL_m15AddBoneTreeItem"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15AddBoneTreeItem )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_m15AddBoneTreeItem ModelID, BoneID, Level, ChildLevel", "Statement to define hierarchy between bones. You can define parent-child relation for the bones using this."

  thinBasic_LoadSymbolEx  "TBGL_m15ApplyBones"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15ApplyBones )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15ApplyBones ModelID", "This statement performs the rotations defined by tbgl_m15RotBone"

  thinBasic_LoadSymbolEx  "TBGL_m15ClearModel"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15ClearModel )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15ClearModel ModelID", "This statement will clear specified model slot"

  thinBasic_LoadSymbolEx  "TBGL_m15DefBoneAddVertex"              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15DefBoneAddVertex )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_m15DefBoneAddVertex( ModelID, BoneID, VertexID )", "Adds vertex to specified bone"

  thinBasic_LoadSymbolEx  "TBGL_m15DefBoneAnchor"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15DefBoneAnchor )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_m15DefBoneAnchor( ModelID, BoneID, x, y, z )", "Specifies anchor point for bone"

  thinBasic_LoadSymbolEx  "TBGL_m15DefBoneBox"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15DefBoneBox )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15DefBoneBox ModelID, BoneID, minX, maxX, minY, maxY, minZ, maxZ, anchorX, anchorY, anchorZ", _
                          "Statement to define area limited by sides of block. The vertices found in this area will be indexed for later use with this bone."

  thinBasic_LoadSymbolEx  "TBGL_m15DefBoneColor"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15DefBoneColor )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15DefBoneColor( ModelID, BoneID, R, G, B, anchorX, anchorY, anchorZ )", "Statement to define bone according to vertex color, defined in M15 fileThe vertices found in this area will be indexed for later use with this bone."

  thinBasic_LoadSymbolEx  "TBGL_m15DefBoneEmpty"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15DefBoneEmpty )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15DefBoneEmpty( ModelID, BoneID )", "Erases all links to vertices the bone had and also erases links to all child bones"

  thinBasic_LoadSymbolEx  "TBGL_m15DefBoneLayer"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15DefBoneLayer )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15DefBoneLayer ModelID, BoneID, LayerName, anchorX, anchorY, anchorZ", "Statement to define bone from layer, defined in M15 fileThe vertices found in this area will be indexed for later use with this bone."

  thinBasic_LoadSymbolEx  "TBGL_m15DefBoneReset"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15DefBoneReset )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15DefBoneReset( ModelID, BoneID )", "Erases all links to vertices the bone had"

  thinBasic_LoadSymbolEx  "TBGL_m15DrawModel"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15DrawModel )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_m15DrawModel ModelID", "This statement will render specified model"

  thinBasic_LoadSymbolEx  "TBGL_m15EraseChildbones"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15EraseChildbones )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_m15EraseChildbones( ModelID, BoneID )", "Erases links to all child bones"

  thinBasic_LoadSymbolEx  "TBGL_m15GetBoneChild"                  , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_m15GetBoneChild )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetBoneChild( ModelID, BoneID, N )", "Returns ID of N-th child bone"

  thinBasic_LoadSymbolEx  "TBGL_m15GetBoneChildCount"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_m15GetBoneChildCount )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetBoneChildCount( ModelID, BoneID )", "Returns number child bones"

  thinBasic_LoadSymbolEx  "TBGL_m15GetModelDimensions"            , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15GetModelDimensions )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetModelDimensions( ModelID, varX, varY, varZ )", "Returns width(x), height(y) and length(z) of model in specified slot"

  thinBasic_LoadSymbolEx  "TBGL_m15GetModelPolycount"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_m15GetModelPolycount )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetModelPolycount( ModelID )", "Statement to get number of polygons of specified model slot"

  thinBasic_LoadSymbolEx  "TBGL_m15GetModelVertexcount"           , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_m15GetModelVertexcount )                 , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetModelVertexcount( ModelID )", "Function which returns number of vertices of specified model"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexB"                    , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexB )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexB( ModelID, VertexIndex )", "Statement to get Blue component of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexG"                    , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexG )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexG( ModelID, VertexIndex )", "Statement to get Green component of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexLayer"                , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexLayer )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexLayer( ModelID, VertexIndex )", "Statement to get layer number of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexPStop"                , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexPStop )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexPStop( ModelID, VertexIndex )", "Statement to get polygon end flag of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexParam"                , %thinBasic_ReturnCodeSingle             , CODEPTR( EXEC_TBGL_m15GetVertexParam )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexParam( ModelID, VertexIndex, Parameter )", "This statement allows you to read model parameters on the fly"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexR"                    , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexR )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexR( ModelID, VertexIndex )", "Statement to get Red component of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexRGB"                  , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15GetVertexRGB )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexRGB( ModelID, VertexIndex, varR, varG, varB )", "Statement to get R, G and B color components of specified vertex to passed variables"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexTexN"                 , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexTexN )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexTexN( ModelID, VertexIndex )", "Statement to get texture number of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexTexX"                 , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexTexX )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexTexX( ModelID, VertexIndex )", "Statement to get X texture coordinate ( also known as U ) of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexTexXY"                , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15GetVertexTexXY )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexTexXY( ModelID, VertexIndex, varTexX, varTexY )", "Statement to get X, Y texture coordinates( also known as U, V ) of specified vertex to passed variables"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexTexY"                 , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexTexY )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexTexY( ModelID, VertexIndex )", "Statement to get Y texture coordinate ( also known as V ) of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexX"                    , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexX )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexX( ModelID, VertexIndex )", "Statement to get X coordinate of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexXYZ"                  , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15GetVertexXYZ )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexXYZ( ModelID, VertexIndex, varX, varY, varZ )", "Statement to get X, Y and Z coordinates of specified vertex to passed variables"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexY"                    , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexY )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexY( ModelID, VertexIndex )", "Statement to get Y coordinate of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15GetVertexZ"                    , %thinBasic_ReturnNumber                 , CODEPTR( EXEC_TBGL_m15GetVertexZ )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15GetVertexZ( ModelID, VertexIndex )", "Statement to get Z coordinate of specified vertex"

  thinBasic_LoadSymbolEx  "TBGL_m15InitModelBuffers"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_m15InitModelBuffers )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_m15InitModelBuffers NumModels, NumVertices", "It serves to allocate memory for models"

  thinBasic_LoadSymbolEx  "TBGL_m15LoadModel"                     , %thinBasic_ReturnCodeLong               , CODEPTR( EXEC_TBGL_m15LoadModel )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_m15LoadModel ModelFile, TextureSubdirectory, ModelID, TargetedDisplayList, NormalVectorMethod", "This statement loads the M15 model from file for later use"

  thinBasic_LoadSymbolEx  "TBGL_m15RecalcNormals"                 , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15RecalcNormals )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_m15RecalcNormals ModelID, NormalVectorMethod [, layer]", "This statement will force recalculation of model normals"

  thinBasic_LoadSymbolEx  "TBGL_m15ResetBones"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15ResetBones )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15ResetBones ModelID", "This statement resets all bone transformations"

  thinBasic_LoadSymbolEx  "TBGL_m15RotBone"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15RotBone )                             , %thinBasic_ForceOverWrite, _
                          "TBGL_m15RotBone ModelID, BoneID, angleX, angleY, angleZ", "This statement sets bone angle"

  thinBasic_LoadSymbolEx  "TBGL_m15RotBoneX"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15RotBoneX )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_m15RotBoneX ModelID, BoneID, angleX", "This statement sets bone rotation around X axis"

  thinBasic_LoadSymbolEx  "TBGL_m15RotBoneY"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15RotBoneY )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_m15RotBoneY ModelID, BoneID, angleX", "This statement sets bone rotation around Y axis"

  thinBasic_LoadSymbolEx  "TBGL_m15RotBoneZ"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15RotBoneZ )                            , %thinBasic_ForceOverWrite, _
                          "TBGL_m15RotBoneZ ModelID, BoneID, angleX", "This statement sets bone rotation around Z axis"

  thinBasic_LoadSymbolEx  "TBGL_m15SetBoneChild"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15SetBoneChild )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetBoneChild( ModelID, BoneID, ChildBoneID )", "Adds child bone for specified bone"

  thinBasic_LoadSymbolEx  "TBGL_m15SetDefaultTextureFilter"       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_m15SetDefaultTextureFilter )             , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetDefaultTextureFilter( TextureFiltering [, AnisotropicLevel ] )", "Sets default texture filtering for models loaded after this command"

  thinBasic_LoadSymbolEx  "TBGL_m15SetModelVertexcount"           , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetModelVertexcount )                 , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetModelVertexcount( ModelID, Value )", "Overrides information about number of vertices"

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexB"                    , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexB )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexB( ModelID, VertexIndex, b)|TBGL_m15SetVertexB( ModelID, IndexFrom, IndexTo, b)", "Statement to set Blue color component of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexG"                    , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexG )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexG( ModelID, VertexIndex, g)|TBGL_m15SetVertexG( ModelID, IndexFrom, IndexTo, g)", "Statement to set Green color component of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexLayer"                , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexLayer )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexLayer( ModelID, VertexIndex, layer)|TBGL_m15SetVertexLayer( ModelID, IndexFrom, IndexTo, layer)", "Statement to set layer number to vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexPStop"                , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexPStop )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexPStop( ModelID, VertexIndex, pStop)|TBGL_m15SetVertexPStop( ModelID, IndexFrom, IndexTo, pStop)", "Statement to set flag for end of model polygon."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexParam"                , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexParam )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexParam( ModelID, VertexIndex, Parameter, NewValue )", "This statement allows you to modify model parameters on the fly"

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexR"                    , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexR )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexR( ModelID, VertexIndex, r)|TBGL_m15SetVertexR( ModelID, IndexFrom, IndexTo, r)", "Statement to set Red color component of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexRGB"                  , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexRGB )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexRGB( ModelID, VertexIndex, R, G, B )|TBGL_m15SetVertexRGB( ModelID, IndexFrom, IndexTo, R, G, B )", "Statement to set R, G and B color component to specified vertex ( vertices )"

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexRGB"                  , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexRGB )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexRGB( ModelID, VertexIndex, R, G, B )|TBGL_m15SetVertexRGB( ModelID, IndexFrom, IndexTo, R, G, B )", "Statement to set R, G and B color component to specified vertex ( vertices )"

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexTexN"                 , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexTexN )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexTexN( ModelID, VertexIndex, texN)|TBGL_m15SetVertexTexN( ModelID, IndexFrom, IndexTo, texN)", "Statement to set texture index of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexTexX"                 , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexTexX )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexTexX( ModelID, VertexIndex, texX)|TBGL_m15SetVertexTexX( ModelID, IndexFrom, IndexTo, texX)", "Statement to set texture X cooridinate ( also known as U ) of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexTexXY"                , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexTexXY )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexTexXY( ModelID, VertexIndex, x, y )|TBGL_m15SetVertexTexXY( ModelID, IndexFrom, IndexTo, x, y )", "Statement to set X, Y ( also known as U, V ) coordinates for texturing."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexTexY"                 , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexTexY )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexTexY( ModelID, VertexIndex, texY)|TBGL_m15SetVertexTexY( ModelID, IndexFrom, IndexTo, texY)", "Statement to set texture Y cooridinate ( also known as V ) of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexX"                    , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexX )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexX( ModelID, VertexIndex, x)|TBGL_m15SetVertexX( ModelID, IndexFrom, IndexTo, x)", "Statement to set X coordinate of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexXYZ"                  , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexXYZ )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexXYZ( ModelID, VertexIndex, x, y, z )|TBGL_m15SetVertexXYZ( ModelID, IndexFrom, IndexTo, x, y, z )", "Statement to set X, Y and Z coordinates to specified vertex ( vertices )"

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexXYZ"                  , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexXYZ )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexXYZ( ModelID, VertexIndex, x, y, z )|TBGL_m15SetVertexXYZ( ModelID, IndexFrom, IndexTo, x, y, z )", "Statement to set X, Y and Z coordinates to specified vertex ( vertices )"

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexY"                    , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexY )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexY( ModelID, VertexIndex, y)|TBGL_m15SetVertexY( ModelID, IndexFrom, IndexTo, y)", "Statement to set Y coordinate of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_m15SetVertexZ"                    , %thinBasic_ReturnNone                   , CODEPTR( EXEC_TBGL_m15SetVertexZ )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_m15SetVertexZ( ModelID, VertexIndex, z)|TBGL_m15SetVertexZ( ModelID, IndexFrom, IndexTo, z)", "Statement to set Z coordinate of model vertex."

  thinBasic_LoadSymbolEx  "TBGL_oglDeviceContext"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_oglDeviceContext )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_oglDeviceContext()", "Function to return device context used by TBGL"

  thinBasic_LoadSymbolEx  "TBGL_oglExtensionSupport"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_oglExtensionSupport )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_oglExtensionSupport( sExtension )", "Function to determine whether specified OpenGL extension has support in driver"

  thinBasic_LoadSymbolEx  "TBGL_oglIsAccelerated"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_oglIsAccelerated )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_oglIsAccelerated()", "Function to determine whether OpenGL runs in accelerated mode"

  thinBasic_LoadSymbolEx  "TBGL_oglRenderContext"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_oglRenderContext )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_oglRenderContext()", "Function to return rendering context used by TBGL"

  thinBasic_LoadSymbolEx  "TBGL_oglRenderer"              , %thinBasic_ReturnString                      , CODEPTR( Exec_TBGL_oglRenderer )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_oglRenderer()", "Function to retrieve OpenGL renderer"

  thinBasic_LoadSymbolEx  "TBGL_oglVersionSupport"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_oglVersionSupport )                , %thinBasic_ForceOverWrite, _
                          "TBGL_oglVersionSupport( Major, Minor )", "Function to determine whether specified OpenGL version is supported in driver"

  thinBasic_LoadSymbolEx  "TBGL_oglVersion"                    , %thinBasic_ReturnString                 , CODEPTR( Exec_TBGL_oglVersion )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_oglVersion( Major, Minor )", "Function to determine OpenGL version supported in driver"

  thinBasic_LoadSymbolEx  "TBGL_Rect"                             , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Rect )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_Rect( x1, y1, x2, y2 )|TBGL_Rect( x1, y1, z1, x2, y2, z2 )", "Draw a rectangle defined by 2 points."

  thinBasic_LoadSymbolEx  "TBGL_Line"                             , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Line )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_Line( x1, y1, x2, y2 )|TBGL_Line( x1, y1, z1, x2, y2, z2 )", "Draws a line defined by 2 points."

  thinBasic_LoadSymbolEx  "TBGL_Point"                            , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_Point )                                , %thinBasic_ForceOverWrite, _
                          "TBGL_Point( xPos, yPos [, zPos ] )", "Draws a point."

  thinBasic_LoadSymbolEx  "TBGL_NGon"                             , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_NGon )                                 , %thinBasic_ForceOverWrite, _
                          "TBGL_NGon( xPos, yPos, radius, vertexCount )", "Draws a n-gon shape."


  thinBasic_LoadSymbolEx  "TBGL_SpriteAddSpeed"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteAddSpeed )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteAddSpeed( spriteID , speed [ , angle ] )", "Set the sprites X and Y speed through a push."

  thinBasic_LoadSymbolEx  "TBGL_SpriteAddSpeedXY"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteAddSpeedXY )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteAddSpeedXY( spriteID , xSpeed , ySpeed [ , pushMode ] )", "Set the sprites X and Y speed through a push."

  thinBasic_LoadSymbolEx  "TBGL_SpriteAddSpin"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteAddSpin )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteAddSpin( spriteID , spinFactor )", "Set the sprites spinning speed."

  thinBasic_LoadSymbolEx  "TBGL_SpriteAnimate"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteAnimate )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteAnimate( spriteID [, speed ] )", "Animate a sprite"


  thinBasic_LoadSymbolEx  "TBGL_SpriteCollided"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteCollided )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteCollided( sourceSprite [ , targetSprite ] )", "Check if a sprite collides with another one"

  thinBasic_LoadSymbolEx  "TBGL_SpriteCheckMouse"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteCheckMouse )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteCheckMouse( spriteID [, mouseButton ] )", "Check if the mouse cursor is over a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteCopy"                       , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteCopy )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteCopy( spriteID )", "Copy a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteCreate"                     , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteCreate )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteCreate( textureIndex )", "Create a single sprite from an existing texture"

  thinBasic_LoadSymbolEx  "TBGL_SpriteCreateAnim"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteCreateAnim )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteCreateAnim( textureIndex , tileWidth , tileHeight )", "Create an animated sprite from an existing texture"

  thinBasic_LoadSymbolEx  "TBGL_SpriteCreateSheet"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteCreateSheet )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteCreateSheet( textureIndex , tileWidth , tileHeight, spriteArray )", "Creates sprites from an existing texture (sprite sheet)"


  thinBasic_LoadSymbolEx  "TBGL_SpriteDelete"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteDelete )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteDelete( spriteID )", "Delete an existing sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteDraw"                       , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteDraw )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteDraw( spriteID )", "Draw a sprite at its internal stored position"

  thinBasic_LoadSymbolEx  "TBGL_SpriteDrawAt"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteDrawAt )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteDrawAt( spriteID , xPos , yPos [ , layerNo ] )", "Draw a sprite at its internal stored position"

  thinBasic_LoadSymbolEx  "TBGL_SpriteExists"                     , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteExists )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteExists( spriteID )", "Returns true in case given sprite exists"


  thinBasic_LoadSymbolEx  "TBGL_SpriteGetActive"                  , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetActive )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetActive( spriteID )", "Read the active flag of sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetAlpha"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetAlpha )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetAlpha( spriteID )", "Retrieve the sprites current alpha value"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetAngle"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetAngle )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetAngle( spriteID )", "Retrieve the sprites current angle in degrees"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetAnimGroup"               , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetAnimGroup )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetAnimGroup( spriteID )", "Return the current animation group a sprite has been set on."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetAnimLength"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetAnimLength )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetAnimLength( spriteID [, animGroup ] )", "Return the current animation length of an animgroup from a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetAnimTime"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetAnimTime )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetAnimTime( spriteID [, animGroup ] )", "Return the current animation time of an animgroup from a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetBaseSize"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteGetBaseSize )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetBaseSize( spriteID , width, height )", "Return the base size of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetChildCount"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetChildCount )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetChildCount( spriteID )", "Retrieve the amount of sprite childs"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetChildID"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetChildID )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetChildID( spriteID, index )", "Retrieve the ID of child sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetCollisionGroup"          , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetCollisionGroup )              , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetCollisionGroup( spriteID )", "Return the collision group of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetCollisionHeight"         , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetCollisionHeight )             , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetCollisionHeight( spriteID )", "Return the collision height of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetCollisionMode"           , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetCollisionMode )               , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetCollisionMode( spriteID )", "Return the collision mode of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetCollisionRadius"         , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetCollisionRadius )             , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetCollisionRadius( spriteID )", "Return the collision radius of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetCollisionType"           , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetCollisionType )               , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetCollisionType( spriteID )", "Return the collision type of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetCollisionWith"           , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetCollisionWith )               , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetCollisionWith( spriteID , colgroup )", "Retrieve if a sprite will collide with a specific collision group"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetCollisionWidth"          , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetCollisionWidth )              , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetCollisionWidth( spriteID )", "Return the collision width of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetFriction"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetFriction )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetFriction( spriteID )", "Return the friction of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetGroup"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetGroup )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetGroup( spriteID )", "Return the group a sprite has been set to."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetID"                      , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetID )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetID( index [, group ] )", "Retrieve the ID of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetLayer"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetLayer )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetLayer( spriteID )", "Return the layer number of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetOldPos"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteGetOldPos )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetOldPos( spriteID , XPos , YPos )", "Return the old position of a sprite before the last change of coordinates"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetOrder"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetOrder )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetOrder( spriteID )", "Return the order of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetParent"                  , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetParent )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetParent( spriteID )", "Retrieve the ID of the sprites parent."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetPos"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteGetPos )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetPos( spriteID , XPos , YPos [, ZPos ] )", "Return the position of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetScale"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetScale )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetScale( spriteID )", "Return the scale factor of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetSpeed"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetSpeed )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetSpeed( spriteID )", "Retrieve the current speed factor of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetSpeedAngle"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetSpeedAngle )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetSpeedAngle( spriteID )", "Retrieve the current speed angle of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetSpeedXY"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteGetSpeedXY )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetSpeedXY( spriteID , XSpeed , YSpeed [, spinSpeed ] )", "Retrieve the current speed factors of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetSpin"                    , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetSpin )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetSpin( spriteID )", "Retrieve the sprites current spin speed"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetTag"                     , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetTag )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetTag( spriteID )", "Return the tag of a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetTargetAngle"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetTargetAngle )                 , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetTargetAngle( spriteID , targetSpriteID [, relativeFlag ] )", "Return the angle of a targets position related to the position of your sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetTargetDist"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetTargetDist )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetTargetDist( spriteID ,  targetSpriteID )", "Return the distance of a target sprite related to the  position of your sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetTexFrame"                , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetTexFrame )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetTexFrame( spriteID )", "Retrieve the sprites current texture frame number"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetTexture"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetTexture )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetTexture( spriteID )", "Retrieve the sprites current texture ID"

  thinBasic_LoadSymbol  "TBGL_SpriteGetUserDataID"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetUserDataID )                  , %thinBasic_ForceOverWrite
  thinBasic_LoadSymbolEx  "TBGL_SpriteGetUserDataPointer"         , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetUserDataPointer )             , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetUserDataPointer( spriteID )", "Retrieve a pointer to the sprites user data"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetVector"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteGetVector )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetVector( spriteID , distance, angle, XPos , YPos )", "Return a new vector depending of the position of a sprite, and given distance and angle factors"

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetVectorAngle"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetVectorAngle )                 , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetVectorAngle( spriteID ,  XPos , YPos [, relativeFlag ] )", "Return the angle of a vector related to the  position of a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetVectorDist"              , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetVectorDist )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetVectorDist( spriteID ,  XPos , YPos )", "Return the distance of a vector related to the  position of a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteGetVisible"                 , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteGetVisible )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteGetVisible( spriteID )", "Determine the visible flag of a sprite"


  thinBasic_LoadSymbolEx  "TBGL_SpriteLoad"                       , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteLoad )                           , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteLoad( textureFile [ , transparentColor ] )", "Load a single sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteLoadAnim"                   , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteLoadAnim )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteLoadAnim( textureFile , tileWidth , tileHeight [ , transparentColor ] )", "Load an animated sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteLoadSheet"                  , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpriteLoadSheet )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteLoadSheet( texturefilename , tileWidth , tileHeight, spriteArray, [ transparentColor ] )", "Load  sprites from an existing texture file (sprite sheet)"


  thinBasic_LoadSymbolEx  "TBGL_SpritesAnimateAll"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpritesAnimateAll )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpritesAnimateAll( [ speed ] )", "Animate all sprites that are active"

  thinBasic_LoadSymbolEx  "TBGL_SpritesCheckMouseAll"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpritesCheckMouseAll)                  , %thinBasic_ForceOverWrite, _
                          "TBGL_SpritesCheckMouseAll( [ mouseButton ] )", "Check if the mouse cursor is over some sprites."

  thinBasic_LoadSymbolEx  "TBGL_SpritesCount"                     , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SpritesCount)                          , %thinBasic_ForceOverWrite, _
                          "TBGL_SpritesCount( [ group ] )", "Retrieve the total number of sprites (within a group)"

  thinBasic_LoadSymbolEx  "TBGL_SpritesDeleteAll"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpritesDeleteAll )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpritesDeleteAll( [ group ] )", "Delete all existing sprites or just sprites of a given group."

  thinBasic_LoadSymbolEx  "TBGL_SpritesDrawAll"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpritesDrawAll )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpritesDrawAll( [ spritegroup ] )", "Draw all existing, active and visible sprites"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetActive"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetActive )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetActive( spriteID , activeFlag )", "Set the active flag of sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetAlpha"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetAlpha )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetAlpha( spriteID , alpha )", "Set the alpha value of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetAngle"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetAngle )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetAngle( spriteID , angle [, relativeFlag ] )", "Rotate a sprite to an absolute angle"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetAnim"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetAnim )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetAnim( spriteID , startFrame , endFrame [, animGroup ] )", "Set the animation frames for a animation group of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetAnimGroup"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetAnimGroup )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetAnimGroup( spriteID , animGroup )", "Set the active animation group of sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetAnimSpeed"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetAnimSpeed )                   , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetAnimSpeed( spriteID , animSpeed [, animGroup ] )", "Set the animation speed of an animation group of sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetAnimTime"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetAnimTime )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetAnimTime( spriteID , animtime [, animGroup ] )", "Set the current animation time of an animgroup from a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetBaseSize"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetBaseSize )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetBaseSize( spriteID , width , height )", "Set a new size of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetCollisionGroup"          , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetCollisionGroup )              , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetCollisionGroup( spriteID , colgroup )", "Set the collision group of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetCollisionHeight"         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetCollisionHeight )             , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetCollisionHeight( spriteID , colHeight )", "Return the collision height of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetCollisionMode"           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetCollisionMode )               , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetCollisionMode( spriteID , mode )", "Set the collision mode of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetCollisionRadius"         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetCollisionRadius )             , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetCollisionRadius( spriteID , colradius )", "Return the collision radius of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetCollisionType"           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetCollisionType )               , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetCollisionType( spriteID , collisionType )", "Set the collision type of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetCollisionWith"           , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetCollisionWith )               , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetCollisionWith( spriteID , colgroup, activeFlag )", "Set which collision groups a sprite will collide with"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetCollisionWidth"          , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetCollisionWidth )              , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetCollisionWidth( spriteID , colWidth )", "Return the collision width of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetColor"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetColor )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetColor( spriteID , red, green, blue )", "Set the color of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetEvent"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetEvent )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetEvent( spriteID , spriteEvent , functionName )", "Set the event handler routine for a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetFriction"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetFriction )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetFriction( spriteID , friction )", "Set the friction of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetGroup"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetGroup )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetGroup( spriteID, spriteGroup )", "Set the group of a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetHFlip"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetHFlip )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetHFlip( spriteID , flag )", "Set the horizontal flip flag of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetLayer"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetLayer )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetLayer( spriteID , layerNo )", "Set the layer number of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetOrder"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetOrder )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetOrder( spriteID , order )", "Set the order of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetParent"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetParent )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetParent( spriteID , parentID )", "Set the parent of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetParentMode"              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetParentMode )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetParentMode( spriteID , parentMode )", "Set the parent mode of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetPos"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetPos )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetPos( spriteID , xPos , yPos [ , relativeFlag ] )", "Move a sprite to a new position"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetScale"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetScale )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetScale( spriteID , scaleFactor [, relativeFlag ] )", "Scale a sprite to an absolute scale factor"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetShadow"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetShadow )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetShadow( spriteID , shadowAlpha [, XOffset, YOffset, size ] )", "Set the shadow parameters of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetSpeed"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetSpeed )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetSpeed( spriteID [, speed ] )", "Set the sprites speed factor"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetSpeedAngle"              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetSpeedAngle )                  , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetSpeedAngle( spriteID [, angle ] )", "Set the sprites speed factor"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetSpeedXY"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetSpeedXY )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetSpeedXY( spriteID , xSpeed , ySpeed [ , pushMode ] )", "Set the sprites X and Y speed through a push."

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetSpin"                    , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetSpin )                        , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetSpin( spriteID , angleSpeed )", "Set the sprites current spin speed"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetTag"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetTag )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetTag( spriteID, tag )", "Set the tag of a sprite."

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetTexCoord"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetTexCoord )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetTexCoord( spriteID , frameNo, x1, y1, x2, y2 )", "Set the sprites texture coordinates of a frame."

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetTexFrame"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetTexFrame )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetTexFrame( spriteID , frameNo )", "Set the sprites current texture frame number"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetTexture"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetTexture )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetTexture( spriteID , textureIndex )", "Set the sprites current texture ID"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetUserData"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetUserData )                    , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetUserData( spriteID ,  userDataVar )", "Set the sprite's user data"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetVFlip"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetVFlip )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetVFlip( spriteID , flag )", "Set the vertical flip flag of a sprite"

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetVisible"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetVisible )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetVisible( spriteID , visibleFlag )", "Set the visible flag of a sprite"


  thinBasic_LoadSymbolEx  "TBGL_SpriteSlide"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSlide )                          , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSlide( spriteID , xFacor , yFactor )", "Move a sprite relatively to its current position foward/backward or sideways"

  thinBasic_LoadSymbolEx  "TBGL_SpritesUpdateAll"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpritesUpdateAll )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_SpritesUpdateAll( [ speed ] )", "Update the position of all sprites by their speed factors and also the animation of animated sprites."

  thinBasic_LoadSymbolEx  "TBGL_SpriteUpdate"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteUpdate )                         , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteUpdate( spriteID [, speed ] )", "Update the position of a sprite by its speed factors and also the animation of an animated sprite."



  thinBasic_LoadSymbolEx  "TBGL_PushStateProtect"                 , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushStateProtect )                     , %thinBasic_ForceOverWrite, _
                          "TBGL_PushStateProtect mask", "Starts block of protection from potentially enabled states"

  thinBasic_LoadSymbolEx  "TBGL_PopStateProtect"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopStateProtect )                      , %thinBasic_ForceOverWrite, _
                          "TBGL_PopStateProtect", "Ends block of protection from potentially enabled states"

  thinBasic_LoadSymbolEx  "TBGL_PushState"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushState )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushState mask", "Starts block with enforced states"

  thinBasic_LoadSymbolEx  "TBGL_PopState"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopState )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopState", "Ends block of forced states and disables them"

  thinBasic_LoadSymbolEx  "TBGL_PushLineWidth"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushLineWidth )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushLineWidth( width )", "Starts isolated block with given line width set up"

  thinBasic_LoadSymbolEx  "TBGL_PopLineWidth"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopLineWidth )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopLineWidth", "Ends block of isolated line width and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_PushPointSize"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushPointSize )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushPointSize( pointSize )", "Starts isolated block with given point size set up"

  thinBasic_LoadSymbolEx  "TBGL_PopPointSize"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopPointSize )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopPointSize", "Ends block of isolated point size and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_PushColor"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushColor )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushColor( r, g, b [, a] )", "Starts isolated block with given color set up"

  thinBasic_LoadSymbolEx  "TBGL_PopColor"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopColor )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopColor", "Ends block of isolated color and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_PushLineStipple"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushLineStipple )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushLineStipple( factor, pattern )", "Starts isolated block with given factor and pattern set up"

  thinBasic_LoadSymbolEx  "TBGL_PopLineStipple"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopLineStipple )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopLineStipple", "Ends block of isolated factor and pattern and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_PushPolygonLook"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushPolygonLook )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushPolygonLook( look )", "Starts isolated block with given look set up"

  thinBasic_LoadSymbolEx  "TBGL_PopPolygonLook"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopPolygonLook )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopPolygonLook", "Ends block of isolated look and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_PushTexture"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushTexture )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushTexture( index )", "Starts isolated block with given texture set up"

  thinBasic_LoadSymbolEx  "TBGL_PopTexture"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopTexture )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopTexture", "Ends block of isolated texture and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_PushBlendFunc"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushBlendFunc )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushBlendFunc( sFactor, dFactor )", "Starts isolated block with given blending function set up"

  thinBasic_LoadSymbolEx  "TBGL_PopBlendFunc"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopBlendFunc )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopBlendFunc", "Ends block of isolated blending function and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_PushAlphaFunc"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushAlphaFunc )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushAlphaFunc( function, value )", "Starts isolated block with given alpha test function set up"

  thinBasic_LoadSymbolEx  "TBGL_PopAlphaFunc"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopAlphaFunc )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopAlphaFunc", "Ends block of isolated alpha test function and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_PushDepthFunc"                        , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushDepthFunc )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushDepthFunc( function, value )", "Starts isolated block with given depth test function set up"

  thinBasic_LoadSymbolEx  "TBGL_PopDepthFunc"                         , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopDepthFunc )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopDepthFunc", "Ends block of isolated depth test function and returns back previous state"

  thinBasic_LoadSymbolEx  "TBGL_CallingEntity"                    , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_CallingEntity )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_CallingEntity", "Retrieves information about entity calling function"

  ''
  thinBasic_LoadSymbolEx  "TBGL_BindPeriodicFunction"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_BindPeriodicFunction )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_BindPeriodicFunction( hWnd, functionName, interval )", "Selects script function to be called periodically once TBGL_ProcessPeriodicFunction is called."

  thinBasic_LoadSymbolEx  "TBGL_UnBindPeriodicFunction"           , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_UnBindPeriodicFunction )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_UnBindPeriodicFunction( hWnd )", "Releases function bound by TBGL_BindPeriodicFunction and executed by TBGL_ProcessPeriodicFunction from being called periodically."

  thinBasic_LoadSymbolEx  "TBGL_ProcessPeriodicFunction"          , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_ProcessPeriodicFunction )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_ProcessPeriodicFunction( hWnd )", "Halts further script execution and calls function bound by TBGL_BindPeriodicFunction in discrete time intervals."

  '' NEW
  thinBasic_LoadSymbolEx  "TBGL_PeriodicBindFunction"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_BindPeriodicFunction )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PeriodicBindFunction( hWnd, functionName, interval )", "Selects script function to be called periodically once TBGL_PeriodicProcessFunction is called."

  thinBasic_LoadSymbolEx  "TBGL_PeriodicUnBindFunction"           , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_UnBindPeriodicFunction )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PeriodicUnBindFunction( hWnd )", "Releases function bound by TBGL_PeriodicBindFunction and executed by TBGL_PeriodicProcessFunction from being called periodically."

  thinBasic_LoadSymbolEx  "TBGL_PeriodicProcessFunction"          , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_ProcessPeriodicFunction )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PeriodicProcessFunction( hWnd )", "Halts further script execution and calls function bound by TBGL_PeriodicBindFunction in discrete time intervals."

  thinBasic_LoadSymbolEx  "TBGL_PeriodicChangeFunction"          , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_PeriodicChangeFunction )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PeriodicChangeFunction( hWnd, functionName )", "This changes the function called once TBGL_PeriodicProcessFunction is processed."

  thinBasic_LoadSymbolEx  "TBGL_PeriodicChangeInterval"          , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_PeriodicChangeInterval )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PeriodicChangeInterval( hWnd, interval )", "This changes the interval of function calls once TBGL_PeriodicProcessFunction is processed."

  ''

  thinBasic_LoadSymbolEx  "TBGL_CallingWindow"                    , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_CallingWindow )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_CallingWindow", "Returns handle of window in function invoked by TBGL_ProcessPeriodicFunction."


  thinBasic_LoadSymbolEx  "TBGL_EntityAttach"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityAttach )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityAttach( SceneID, EntityID, ParentEntityID, Mode )", "Attaches entity to another entity."

  thinBasic_LoadSymbolEx  "TBGL_EntityDetach"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityDetach )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityDetach( SceneID, EntityID, Mode )", "Detaches given entity from parent entity."


  thinBasic_LoadSymbolEx  "TBGL_EntitySetParent"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetParent )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetParent( SceneID, EntityID, ParentEntityID )", "Sets new parent entity for given entity."

  thinBasic_LoadSymbolEx  "TBGL_EntityGetParent"                  , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetParent )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetParent( SceneID, EntityID )", "Returns parent entity of given entity."


  thinBasic_LoadSymbol  "TBGL_EntityGetCollisionInfo"           , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityGetCollisionInfo )                       , %thinBasic_ForceOverWrite

  thinBasic_LoadSymbolEx  "TBGL_NewListSpace"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_NewListSpace )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_NewListSpace( firstArrayItem [, howManyLists] )", "Creates new display lists slots, while passing their IDs to given array."

  thinBasic_LoadSymbolEx  "TBGL_DeleteListSpace"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_DeleteListSpace )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_DeleteListSpace( firstArrayItem [, howManyLists] )", "Deletes all display lists slots generated by TBGL_NewListSpace to given array."


  thinBasic_LoadSymbolEx  "TBGL_EntitySetSpecular"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetSpecular )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetSpecular( SceneID, EntityID, R, G, B )", "Sets specular color for light entity"


  thinBasic_LoadSymbolEx  "TBGL_NewMaterial"                      , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_NewMaterial )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_NewMaterial", "Creates new material you can set properties to."

  thinBasic_LoadSymbolEx  "TBGL_DeleteMaterial"                   , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_DeleteMaterial )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_DeleteMaterial( hMaterial )", "Deletes existing material."

  thinBasic_LoadSymbolEx  "TBGL_PushMaterial"                     , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushMaterial )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushMaterial( hMaterial )", "Enables existing material for use."

  thinBasic_LoadSymbolEx  "TBGL_PopMaterial"                      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopMaterial )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopMaterial", "Disables use of material chosen by TBGL_PushMaterial."

  thinBasic_LoadSymbolEx  "TBGL_SetMaterialDiffuse"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetMaterialDiffuse )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SetMaterialDiffuse( hMaterial, R, G, B )", "Defines material diffuse property."

  thinBasic_LoadSymbolEx  "TBGL_SetMaterialAmbient"               , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetMaterialAmbient )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SetMaterialAmbient( hMaterial, R, G, B )", "Defines ambient material property."

  thinBasic_LoadSymbolEx  "TBGL_SetMaterialSpecular"              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetMaterialSpecular )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SetMaterialSpecular( hMaterial, R, G, B )", "Defines specular material property."

  thinBasic_LoadSymbolEx  "TBGL_SetMaterialSpecularExponent"      , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetMaterialSpecularExponent )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SetMaterialSpecularExponent( hMaterial, exponent )", "Defines specular exponent for specular material property."

  thinBasic_LoadSymbolEx  "TBGL_SetMaterialEmission"              , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SetMaterialEmission )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SetMaterialEmission( hMaterial, R, G, B )", "Defines emission material property."

  thinBasic_LoadSymbolEx  "TBGL_SpriteSetAnimType"                , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_SpriteSetAnimType )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SpriteSetAnimType( spriteID , animType [, animGroup ] )", "Set the animation type of an animation group of sprite"

  thinBasic_LoadSymbolEx  "TBGL_GetVSyncMaxFramerate"             , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_GetVSyncMaxFramerate )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_GetVSyncMaxFrameRate()", "Function to retrieve the maximum framerate achievable after call to TBGL_UseVSync(1)."

  thinBasic_LoadSymbolEx  "TBGL_SetMaterialTexture"               , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_SetMaterialTexture )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_SetMaterialTexture(BYVAL material AS NUMBER, BYVAL textureIndex AS NUMBER)", "Function to assign texture ID to the material."

  thinBasic_LoadSymbolEx  "TBGL_EntityGetMatrix"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntityGetMatrix )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityGetMatrix(BYVAL SceneID AS LONG, BYVAL EntityID AS LONG, BYREF Matrix() AS SINGLE)", "Function to retrieve transformation or just rotation matrix from entity."

  thinBasic_LoadSymbolEx  "TBGL_EntitySetMatrix"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_EntitySetMatrix )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntitySetMatrix(BYVAL SceneID AS LONG, BYVAL EntityID AS LONG, BYREF Matrix() AS SINGLE)", "Function to set transformation or just rotation matrix to entity."

  thinBasic_LoadSymbolEx  "TBGL_EntityEnumByDataSignature"        , %thinBasic_ReturnNumber                 , CODEPTR( Exec_TBGL_EntityEnumByDataSignature )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_EntityEnumByDataSignature(BYVAL SceneID AS LONG, BYREF EntityIDs() AS LONG, BYVAL Signature AS LONG) AS LONG", "Function to pass IDs of all entities with matching data signature to array, returning their count."

  thinBasic_LoadSymbolEx  "TBGL_BindWindowEvent"        , %thinBasic_ReturnCodeLong                 , CODEPTR( Exec_TBGL_BindWindowEvent )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_BindWindowEvent(BYVAL hWindow AS DWORD, BYVAL eventCode AS LONG, BYVAL eventHandler AS FUNCTION) AS LONG", "Function to enable custom handling of the event"

  thinBasic_LoadSymbolEx  "TBGL_UnBindWindowEvent"        , %thinBasic_ReturnCodeLong                 , CODEPTR( Exec_TBGL_UnBindWindowEvent )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_BindWindowEvent(BYVAL hWindow AS DWORD, BYVAL eventCode AS LONG) AS LONG", "Function to disable custom handling of the event"

  thinBasic_LoadSymbolEx  "TBGL_OnDropFiles_GetFileCount"        , %thinBasic_ReturnCodeLong             , CODEPTR( Exec_TBGL_OnDropFiles_GetFileCount )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_OnDropFiles_GetFileCount() AS LONG", "Function to return number of files passed to window"

  thinBasic_LoadSymbolEx  "TBGL_OnDropFiles_GetFileName"        , %thinBasic_ReturnString                , CODEPTR( Exec_TBGL_OnDropFiles_GetFileName )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_OnDropFiles_GetFileName(BYVAL fileNumber AS DWORD) AS STRING", "Function to return n-th dragged file name in OnDropFiles event handler"

  thinBasic_LoadSymbolEx  "TBGL_PushLogicOp"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PushLogicOp )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PushLogicOp operation", "Begins region of specified fragment operation"

  thinBasic_LoadSymbolEx  "TBGL_PopLogicOp"                  , %thinBasic_ReturnNone                   , CODEPTR( Exec_TBGL_PopLogicOp )                       , %thinBasic_ForceOverWrite, _
                          "TBGL_PopLogicOp", "Ends region of specified fragment operation"


  declare_GBuffers()


  ' -----------------------------------------------------------------------------
  ' -- EQUATES
  ' -----------------------------------------------------------------------------
  thinBasic_AddEquate   "%GL_CLIP_PLANE0"                       , "" , %GL_CLIP_PLANE0
  thinBasic_AddEquate   "%GL_CLIP_PLANE1"                       , "" , %GL_CLIP_PLANE1
  thinBasic_AddEquate   "%GL_CLIP_PLANE2"                       , "" , %GL_CLIP_PLANE2
  thinBasic_AddEquate   "%GL_CLIP_PLANE3"                       , "" , %GL_CLIP_PLANE3
  thinBasic_AddEquate   "%GL_CLIP_PLANE4"                       , "" , %GL_CLIP_PLANE4
  thinBasic_AddEquate   "%GL_CLIP_PLANE5"                       , "" , %GL_CLIP_PLANE5
  thinBasic_AddEquate   "%GL_DST_ALPHA"                         , "" , %GL_DST_ALPHA
  thinBasic_AddEquate   "%GL_DST_COLOR"                         , "" , %GL_DST_COLOR
  thinBasic_AddEquate   "%GL_FILL"                              , "" , %GL_FILL
  thinBasic_AddEquate   "%GL_LIGHT0"                            , "" , %GL_LIGHT0
  thinBasic_AddEquate   "%GL_LIGHT1"                            , "" , %GL_LIGHT1
  thinBasic_AddEquate   "%GL_LIGHT2"                            , "" , %GL_LIGHT2
  thinBasic_AddEquate   "%GL_LIGHT3"                            , "" , %GL_LIGHT3
  thinBasic_AddEquate   "%GL_LIGHT4"                            , "" , %GL_LIGHT4
  thinBasic_AddEquate   "%GL_LIGHT5"                            , "" , %GL_LIGHT5
  thinBasic_AddEquate   "%GL_LIGHT6"                            , "" , %GL_LIGHT6
  thinBasic_AddEquate   "%GL_LIGHT7"                            , "" , %GL_LIGHT7
  thinBasic_AddEquate   "%GL_LINE"                              , "" , %GL_LINE
  thinBasic_AddEquate   "%GL_LINES"                             , "" , %GL_LINES
  thinBasic_AddEquate   "%GL_LINE_LOOP"                         , "" , %GL_LINE_LOOP
  thinBasic_AddEquate   "%GL_LINE_STRIP"                        , "" , %GL_LINE_STRIP
  thinBasic_AddEquate   "%GL_ONE"                               , "" , %GL_ONE
  thinBasic_AddEquate   "%GL_ONE_MINUS_DST_ALPHA"               , "" , %GL_ONE_MINUS_DST_ALPHA
  thinBasic_AddEquate   "%GL_ONE_MINUS_DST_COLOR"               , "" , %GL_ONE_MINUS_DST_COLOR
  thinBasic_AddEquate   "%GL_ONE_MINUS_SRC_ALPHA"               , "" , %GL_ONE_MINUS_SRC_ALPHA
  thinBasic_AddEquate   "%GL_ONE_MINUS_SRC_COLOR"               , "" , %GL_ONE_MINUS_SRC_COLOR
  thinBasic_AddEquate   "%GL_POINT"                             , "" , %GL_POINT
  thinBasic_AddEquate   "%GL_POINTS"                            , "" , %GL_POINTS
  thinBasic_AddEquate   "%GL_POLYGON"                           , "" , %GL_POLYGON
  thinBasic_AddEquate   "%GL_QUADS"                             , "" , %GL_QUADS
  thinBasic_AddEquate   "%GL_QUAD_STRIP"                        , "" , %GL_QUAD_STRIP
  thinBasic_AddEquate   "%GL_SRC_ALPHA"                         , "" , %GL_SRC_ALPHA
  thinBasic_AddEquate   "%GL_SRC_ALPHA_SATURATE"                , "" , %GL_SRC_ALPHA_SATURATE
  thinBasic_AddEquate   "%GL_SRC_COLOR"                         , "" , %GL_SRC_COLOR
  thinBasic_AddEquate   "%GL_TRIANGLES"                         , "" , %GL_TRIANGLES
  thinBasic_AddEquate   "%GL_TRIANGLE_FAN"                      , "" , %GL_TRIANGLE_FAN
  thinBasic_AddEquate   "%GL_TRIANGLE_STRIP"                    , "" , %GL_TRIANGLE_STRIP
  thinBasic_AddEquate   "%GL_ZERO"                              , "" , %GL_ZERO
  thinBasic_AddEquate   "%TBGL_2D"                              , "" , %TBGL_2D
  thinBasic_AddEquate   "%TBGL_3D"                              , "" , %TBGL_3D
  thinBasic_AddEquate   "%TBGL_ALIGN_NONE"                      , "" , %TBGL_ALIGN_NONE
  thinBasic_AddEquate   "%TBGL_ALIGN_CENTER"                    , "" , %TBGL_ALIGN_CENTER
  thinBasic_AddEquate   "%TBGL_ALIGN_CENTER_CENTER"             , "" , %TBGL_ALIGN_CENTER_CENTER
  thinBasic_AddEquate   "%TBGL_ALIGN_CENTER_DOWN"               , "" , %TBGL_ALIGN_CENTER_DOWN
  thinBasic_AddEquate   "%TBGL_ALIGN_CENTER_UP"                 , "" , %TBGL_ALIGN_CENTER_UP
  thinBasic_AddEquate   "%TBGL_ALIGN_LEFT"                      , "" , %TBGL_ALIGN_LEFT
  thinBasic_AddEquate   "%TBGL_ALIGN_LEFT_CENTER"               , "" , %TBGL_ALIGN_LEFT_CENTER
  thinBasic_AddEquate   "%TBGL_ALIGN_LEFT_DOWN"                 , "" , %TBGL_ALIGN_LEFT_DOWN
  thinBasic_AddEquate   "%TBGL_ALIGN_LEFT_UP"                   , "" , %TBGL_ALIGN_LEFT_UP
  thinBasic_AddEquate   "%TBGL_ALIGN_RIGHT"                     , "" , %TBGL_ALIGN_RIGHT
  thinBasic_AddEquate   "%TBGL_ALIGN_RIGHT_CENTER"              , "" , %TBGL_ALIGN_RIGHT_CENTER
  thinBasic_AddEquate   "%TBGL_ALIGN_RIGHT_DOWN"                , "" , %TBGL_ALIGN_RIGHT_DOWN
  thinBasic_AddEquate   "%TBGL_ALIGN_RIGHT_UP"                  , "" , %TBGL_ALIGN_RIGHT_UP
  thinBasic_AddEquate   "%TBGL_ALPHA"                           , "" , %TBGL_ALPHA
  thinBasic_AddEquate   "%TBGL_ALWAYS"                          , "" , %GL_ALWAYS
  thinBasic_AddEquate   "%TBGL_BLEND"                           , "" , %TBGL_BLEND
  thinBasic_AddEquate   "%TBGL_BLUE"                            , "" , %TBGL_BLUE
  thinBasic_AddEquate   "%TBGL_BOLD"                            , "" , %TBGL_BOLD
  thinBasic_AddEquate   "%TBGL_BOX"                             , "" , %TBGL_ENTITY_BOX
  thinBasic_AddEquate   "%TBGL_CAMERA"                          , "" , %TBGL_ENTITY_CAMERA
  thinBasic_AddEquate   "%TBGL_CAMERATYPE_DEFAULT"              , "" , %TBGL_CAMERATYPE_DEFAULT
  thinBasic_AddEquate   "%TBGL_CLEAR_COLOR"                     , "" , %TBGL_CLEAR_COLOR
  thinBasic_AddEquate   "%TBGL_CLEAR_DEPTH"                     , "" , %TBGL_CLEAR_DEPTH
  thinBasic_AddEquate   "%TBGL_CLIENTAREA"                      , "" , %TBGL_CLIENTAREA
  thinBasic_AddEquate   "%TBGL_CUSTOM"                          , "" , %TBGL_CUSTOM
  thinBasic_AddEquate   "%TBGL_CYLINDER"                        , "" , %TBGL_ENTITY_CYLINDER
  thinBasic_AddEquate   "%TBGL_DATA_RGB"                        , "" , %TBGL_DATA_RGB
  thinBasic_AddEquate   "%TBGL_DATA_RGBA"                       , "" , %TBGL_DATA_RGBA

  thinBasic_AddEquate   "%TBGL_DATA_BGR"                        , "" , %TBGL_DATA_BGR
  thinBasic_AddEquate   "%TBGL_DATA_BGRA"                       , "" , %TBGL_DATA_BGRA

  thinBasic_AddEquate   "%TBGL_DEFAULT"                         , "" , %TBGL_DEFAULT
  thinBasic_AddEquate   "%TBGL_DEPTH"                           , "" , %TBGL_DEPTH
  thinBasic_AddEquate   "%TBGL_DEPTHMASK"                       , "" , %TBGL_DEPTHMASK
  thinBasic_AddEquate   "%TBGL_DISPLAYLIST"                     , "" , %TBGL_ENTITY_DISPLAYLIST
  thinBasic_AddEquate   "%TBGL_ENTITY_CAMERA"                   , "" , %TBGL_ENTITY_CAMERA
  thinBasic_AddEquate   "%TBGL_ENTITY_DISPLAYLIST"              , "" , %TBGL_ENTITY_DISPLAYLIST
  thinBasic_AddEquate   "%TBGL_ENTITY_EMPTY"                    , "" , %TBGL_ENTITY_EMPTY
  thinBasic_AddEquate   "%TBGL_ENTITY_LIGHT"                    , "" , %TBGL_ENTITY_LIGHT
  thinBasic_AddEquate   "%TBGL_ENTITY_MODEL"                    , "" , %TBGL_ENTITY_MODEL
  thinBasic_AddEquate   "%TBGL_ENTITY_PIVOT"                    , "" , %TBGL_ENTITY_PIVOT
  thinBasic_AddEquate   "%TBGL_EQUAL"                           , "" , %GL_EQUAL
  thinBasic_AddEquate   "%TBGL_ERROR_FILE"                      , "" , %TBGL_ERROR_FILE
  thinBasic_AddEquate   "%TBGL_ERROR_MSGBOX"                    , "" , %TBGL_ERROR_MSGBOX
  thinBasic_AddEquate   "%TBGL_ERROR_NONE"                      , "" , %TBGL_ERROR_NONE
  thinBasic_AddEquate   "%TBGL_FILE_BMP"                        , "" , %TBGL_FILE_BMP
  thinBasic_AddEquate   "%TBGL_FILE_TGA"                        , "" , %TBGL_FILE_TGA
  thinBasic_AddEquate   "%TBGL_FOG"                             , "" , %TBGL_FOG
  thinBasic_AddEquate   "%TBGL_GEQUAL"                          , "" , %GL_GEQUAL
  thinBasic_AddEquate   "%TBGL_GREATER"                         , "" , %GL_GREATER
  thinBasic_AddEquate   "%TBGL_GREEN"                           , "" , %TBGL_GREEN
  thinBasic_AddEquate   "%TBGL_ITALICS"                         , "" , %TBGL_ITALICS
  thinBasic_AddEquate   "%TBGL_LEQUAL"                          , "" , %GL_LEQUAL
  thinBasic_AddEquate   "%TBGL_LESS"                            , "" , %GL_LESS
  thinBasic_AddEquate   "%TBGL_LIGHT"                           , "" , %TBGL_ENTITY_LIGHT
  thinBasic_AddEquate   "%TBGL_LIGHTING"                        , "" , %TBGL_LIGHTING
  thinBasic_AddEquate   "%TBGL_LIGHTTYPE_DIRECTIONAL"           , "" , %TBGL_LIGHTTYPE_DIRECTIONAL
  thinBasic_AddEquate   "%TBGL_LIGHTTYPE_POINT"                 , "" , %TBGL_LIGHTTYPE_POINT
  thinBasic_AddEquate   "%TBGL_LIGHTTYPE_SPOT"                  , "" , %TBGL_LIGHTTYPE_SPOT
  thinBasic_AddEquate   "%TBGL_LIGHT_AMBIENT"                   , "" , %GL_AMBIENT
  thinBasic_AddEquate   "%TBGL_LIGHT_CONSTANT_ATTENUATION"      , "" , %GL_CONSTANT_ATTENUATION
  thinBasic_AddEquate   "%TBGL_LIGHT_DIFFUSE"                   , "" , %GL_DIFFUSE
  thinBasic_AddEquate   "%TBGL_LIGHT_LINEAR_ATTENUATION"        , "" , %GL_LINEAR_ATTENUATION
  thinBasic_AddEquate   "%TBGL_LIGHT_POSITION"                  , "" , %GL_POSITION
  thinBasic_AddEquate   "%TBGL_LIGHT_QUADRATIC_ATTENUATION"     , "" , %GL_QUADRATIC_ATTENUATION
  thinBasic_AddEquate   "%TBGL_LIGHT_SPECULAR"                  , "" , %GL_SPECULAR
  thinBasic_AddEquate   "%TBGL_LIGHT_SPOT_CUTOFF"               , "" , %GL_SPOT_CUTOFF
  thinBasic_AddEquate   "%TBGL_LIGHT_SPOT_DIRECTION"            , "" , %GL_SPOT_DIRECTION
  thinBasic_AddEquate   "%TBGL_LIGHT_SPOT_EXPONENT"             , "" , %GL_SPOT_EXPONENT
  thinBasic_AddEquate   "%TBGL_LINESTIPPLE"                     , "" , %TBGL_LINESTIPPLE
  thinBasic_AddEquate   "%TBGL_M15B"                            , "" , %TBGL_M15B
  thinBasic_AddEquate   "%TBGL_M15G"                            , "" , %TBGL_M15G
  thinBasic_AddEquate   "%TBGL_M15LAYER"                        , "" , %TBGL_M15LAYER
  thinBasic_AddEquate   "%TBGL_M15PSTOP"                        , "" , %TBGL_M15PSTOP
  thinBasic_AddEquate   "%TBGL_M15R"                            , "" , %TBGL_M15R
  thinBasic_AddEquate   "%TBGL_M15TEXN"                         , "" , %TBGL_M15TEXN
  thinBasic_AddEquate   "%TBGL_M15TEXX"                         , "" , %TBGL_M15TEXX
  thinBasic_AddEquate   "%TBGL_M15TEXY"                         , "" , %TBGL_M15TEXY
  thinBasic_AddEquate   "%TBGL_M15X"                            , "" , %TBGL_M15X
  thinBasic_AddEquate   "%TBGL_M15Y"                            , "" , %TBGL_M15Y
  thinBasic_AddEquate   "%TBGL_M15Z"                            , "" , %TBGL_M15Z
  thinBasic_AddEquate   "%TBGL_MODEL"                           , "" , %TBGL_ENTITY_MODEL
  thinBasic_AddEquate   "%TBGL_NEVER"                           , "" , %GL_NEVER
  thinBasic_AddEquate   "%TBGL_NORMAL_NONE"                     , "" , %TBGL_NORMAL_NONE
  thinBasic_AddEquate   "%TBGL_NORMAL_PRECISE"                  , "" , %TBGL_NORMAL_PRECISE
  thinBasic_AddEquate   "%TBGL_NORMAL_SMOOTH"                   , "" , %TBGL_NORMAL_SMOOTH
  thinBasic_AddEquate   "%TBGL_NORMAL_FROMFILE"                 , "" , %TBGL_NORMAL_FROMFILE
  thinBasic_AddEquate   "%TBGL_NOTEQUAL"                        , "" , %GL_NOTEQUAL
  thinBasic_AddEquate   "%TBGL_OBJ_CUBE"                        , "" , 2
  thinBasic_AddEquate   "%TBGL_OBJ_CUBE3"                       , "" , 3
  thinBasic_AddEquate   "%TBGL_OBJ_CYLINDER"                    , "" , 4
  thinBasic_AddEquate   "%TBGL_OBJ_SPHERE"                      , "" , 1
  thinBasic_AddEquate   "%TBGL_PARAM_PIXELS"                    , "" , %TBGL_PARAM_PIXELS
  thinBasic_AddEquate   "%TBGL_PARAM_RELATIVE"                  , "" , %TBGL_PARAM_RELATIVE
  thinBasic_AddEquate   "%TBGL_PINFO_RGB"                       , "" , 123
  thinBasic_AddEquate   "%TBGL_PINFO_XYZ"                       , "" , 456
  thinBasic_AddEquate   "%TBGL_PIVOT"                           , "" , %TBGL_ENTITY_PIVOT
  thinBasic_AddEquate   "%TBGL_PRESERVE"                        , "" , %TBGL_PRESERVE
  thinBasic_AddEquate   "%TBGL_RED"                             , "" , %TBGL_RED
  thinBasic_AddEquate   "%TBGL_RESET"                           , "" , %TBGL_RESET
  thinBasic_AddEquate   "%TBGL_RESET_MATRIX"                    , "" , %TBGL_RESET_MATRIX
  thinBasic_AddEquate   "%TBGL_SPHERE"                          , "" , %TBGL_ENTITY_SPHERE
  thinBasic_AddEquate   "%TBGL_TEX_CRISPANISO"                  , "" , %TBGL_TEX_CRISPANISO
  thinBasic_AddEquate   "%TBGL_TEX_ANISO"                       , "" , %TBGL_TEX_ANISO
  thinBasic_AddEquate   "%TBGL_TEX_LINEAR"                      , "" , %TBGL_TEX_LINEAR
  thinBasic_AddEquate   "%TBGL_TEX_MIPMAP"                      , "" , %TBGL_TEX_MIPMAP
  thinBasic_AddEquate   "%TBGL_TEX_NEAREST"                     , "" , %TBGL_TEX_NEAREST
  thinBasic_AddEquate   "%TBGL_TEXTURING"                       , "" , %TBGL_TEXTURING
  thinBasic_AddEquate   "%TBGL_TORUS"                           , "" , %TBGL_ENTITY_TORUS
  thinBasic_AddEquate   "%TBGL_UNDEFINED"                       , "" , %TBGL_UNDEFINED
  thinBasic_AddEquate   "%TBGL_VIEWPORT"                        , "" , %TBGL_VIEWPORT
  thinBasic_AddEquate   "%TBGL_WINDOWAREA"                      , "" , %TBGL_WINDOWAREA
  thinBasic_AddEquate   "%TBGL_WS_CLOSEBOX"                     , "" , %TBGL_WS_CLOSEBOX
  thinBasic_AddEquate   "%TBGL_WS_DONTKEEPASPECTRATIO"          , "" , %TBGL_WS_DONTKEEPASPECTRATIO
  thinBasic_AddEquate   "%TBGL_WS_DONTSIZE"                     , "" , %TBGL_WS_DONTSIZE
  thinBasic_AddEquate   "%TBGL_WS_FULLSCREEN"                   , "" , %TBGL_WS_FULLSCREEN
  thinBasic_AddEquate   "%TBGL_WS_MAXIMIZEBOX"                  , "" , %TBGL_WS_MAXIMIZEBOX
  thinBasic_AddEquate   "%TBGL_WS_MINIMIZEBOX"                  , "" , %TBGL_WS_MINIMIZEBOX
  thinBasic_AddEquate   "%TBGL_WS_WINDOWED"                     , "" , %TBGL_WS_WINDOWED
  thinBasic_AddEquate   "%TBGL_X"                               , "" , %TBGL_X
  thinBasic_AddEquate   "%TBGL_Y"                               , "" , %TBGL_Y
  thinBasic_AddEquate   "%TBGL_Z"                               , "" , %TBGL_Z
  thinBasic_AddEquate   "%VK_0"                                 , "" , %VK_0
  thinBasic_AddEquate   "%VK_1"                                 , "" , %VK_1
  thinBasic_AddEquate   "%VK_2"                                 , "" , %VK_2
  thinBasic_AddEquate   "%VK_3"                                 , "" , %VK_3
  thinBasic_AddEquate   "%VK_4"                                 , "" , %VK_4
  thinBasic_AddEquate   "%VK_5"                                 , "" , %VK_5
  thinBasic_AddEquate   "%VK_6"                                 , "" , %VK_6
  thinBasic_AddEquate   "%VK_7"                                 , "" , %VK_7
  thinBasic_AddEquate   "%VK_8"                                 , "" , %VK_8
  thinBasic_AddEquate   "%VK_9"                                 , "" , %VK_9
  thinBasic_AddEquate   "%VK_A"                                 , "" , %VK_A
  thinBasic_AddEquate   "%VK_ACCEPT"                            , "" , %VK_ACCEPT
  thinBasic_AddEquate   "%VK_ADD"                               , "" , %VK_ADD
  thinBasic_AddEquate   "%VK_APPS"                              , "" , %VK_APPS
  thinBasic_AddEquate   "%VK_B"                                 , "" , %VK_B
  thinBasic_AddEquate   "%VK_BACK"                              , "" , %VK_BACK
  thinBasic_AddEquate   "%VK_C"                                 , "" , %VK_C
  thinBasic_AddEquate   "%VK_CANCEL"                            , "" , %VK_CANCEL
  thinBasic_AddEquate   "%VK_CAPITAL"                           , "" , %VK_CAPITAL
  thinBasic_AddEquate   "%VK_CLEAR"                             , "" , %VK_CLEAR
  thinBasic_AddEquate   "%VK_CONTROL"                           , "" , %VK_CONTROL
  thinBasic_AddEquate   "%VK_CONVERT"                           , "" , %VK_CONVERT
  thinBasic_AddEquate   "%VK_D"                                 , "" , %VK_D
  thinBasic_AddEquate   "%VK_DECIMAL"                           , "" , %VK_DECIMAL
  thinBasic_AddEquate   "%VK_DELETE"                            , "" , %VK_DELETE
  thinBasic_AddEquate   "%VK_DIVIDE"                            , "" , %VK_DIVIDE
  thinBasic_AddEquate   "%VK_DOWN"                              , "" , %VK_DOWN
  thinBasic_AddEquate   "%VK_E"                                 , "" , %VK_E
  thinBasic_AddEquate   "%VK_END"                               , "" , %VK_END
  thinBasic_AddEquate   "%VK_ESCAPE"                            , "" , %VK_ESCAPE
  thinBasic_AddEquate   "%VK_EXECUTE"                           , "" , %VK_EXECUTE
  thinBasic_AddEquate   "%VK_F"                                 , "" , %VK_F
  thinBasic_AddEquate   "%VK_F1"                                , "" , %VK_F1
  thinBasic_AddEquate   "%VK_F10"                               , "" , %VK_F10
  thinBasic_AddEquate   "%VK_F11"                               , "" , %VK_F11
  thinBasic_AddEquate   "%VK_F12"                               , "" , %VK_F12
  thinBasic_AddEquate   "%VK_F13"                               , "" , %VK_F13
  thinBasic_AddEquate   "%VK_F14"                               , "" , %VK_F14
  thinBasic_AddEquate   "%VK_F15"                               , "" , %VK_F15
  thinBasic_AddEquate   "%VK_F16"                               , "" , %VK_F16
  thinBasic_AddEquate   "%VK_F17"                               , "" , %VK_F17
  thinBasic_AddEquate   "%VK_F18"                               , "" , %VK_F18
  thinBasic_AddEquate   "%VK_F19"                               , "" , %VK_F19
  thinBasic_AddEquate   "%VK_F2"                                , "" , %VK_F2
  thinBasic_AddEquate   "%VK_F20"                               , "" , %VK_F20
  thinBasic_AddEquate   "%VK_F21"                               , "" , %VK_F21
  thinBasic_AddEquate   "%VK_F22"                               , "" , %VK_F22
  thinBasic_AddEquate   "%VK_F23"                               , "" , %VK_F23
  thinBasic_AddEquate   "%VK_F24"                               , "" , %VK_F24
  thinBasic_AddEquate   "%VK_F3"                                , "" , %VK_F3
  thinBasic_AddEquate   "%VK_F4"                                , "" , %VK_F4
  thinBasic_AddEquate   "%VK_F5"                                , "" , %VK_F5
  thinBasic_AddEquate   "%VK_F6"                                , "" , %VK_F6
  thinBasic_AddEquate   "%VK_F7"                                , "" , %VK_F7
  thinBasic_AddEquate   "%VK_F8"                                , "" , %VK_F8
  thinBasic_AddEquate   "%VK_F9"                                , "" , %VK_F9
  thinBasic_AddEquate   "%VK_FINAL"                             , "" , %VK_FINAL
  thinBasic_AddEquate   "%VK_G"                                 , "" , %VK_G
  thinBasic_AddEquate   "%VK_H"                                 , "" , %VK_H
  thinBasic_AddEquate   "%VK_HANGEUL"                           , "" , %VK_HANGEUL
  thinBasic_AddEquate   "%VK_HANGUL"                            , "" , %VK_HANGUL
  thinBasic_AddEquate   "%VK_HANJA"                             , "" , %VK_HANJA
  thinBasic_AddEquate   "%VK_HELP"                              , "" , %VK_HELP
  thinBasic_AddEquate   "%VK_HOME"                              , "" , %VK_HOME
  thinBasic_AddEquate   "%VK_I"                                 , "" , %VK_I
  thinBasic_AddEquate   "%VK_INSERT"                            , "" , %VK_INSERT
  thinBasic_AddEquate   "%VK_J"                                 , "" , %VK_J
  thinBasic_AddEquate   "%VK_JUNJA"                             , "" , %VK_JUNJA
  thinBasic_AddEquate   "%VK_K"                                 , "" , %VK_K
  thinBasic_AddEquate   "%VK_KANA"                              , "" , %VK_KANA
  thinBasic_AddEquate   "%VK_KANJI"                             , "" , %VK_KANJI
  thinBasic_AddEquate   "%VK_L"                                 , "" , %VK_L
  thinBasic_AddEquate   "%VK_LBUTTON"                           , "" , %VK_LBUTTON
  thinBasic_AddEquate   "%VK_LCONTROL"                          , "" , %VK_LCONTROL
  thinBasic_AddEquate   "%VK_LEFT"                              , "" , %VK_LEFT
  thinBasic_AddEquate   "%VK_LINEFEED"                          , "" , &H0A
  thinBasic_AddEquate   "%VK_LMENU"                             , "" , %VK_LMENU
  thinBasic_AddEquate   "%VK_LSHIFT"                            , "" , %VK_LSHIFT
  thinBasic_AddEquate   "%VK_LWIN"                              , "" , %VK_LWIN
  thinBasic_AddEquate   "%VK_M"                                 , "" , %VK_M
  thinBasic_AddEquate   "%VK_MBUTTON"                           , "" , %VK_MBUTTON
  thinBasic_AddEquate   "%VK_MENU"                              , "" , %VK_MENU
  thinBasic_AddEquate   "%VK_MODECHANGE"                        , "" , %VK_MODECHANGE
  thinBasic_AddEquate   "%VK_MULTIPLY"                          , "" , %VK_MULTIPLY
  thinBasic_AddEquate   "%VK_N"                                 , "" , %VK_N
  thinBasic_AddEquate   "%VK_NEXT"                              , "" , %VK_NEXT
  thinBasic_AddEquate   "%VK_NONCONVERT"                        , "" , %VK_NONCONVERT
  thinBasic_AddEquate   "%VK_NUMLOCK"                           , "" , %VK_NUMLOCK
  thinBasic_AddEquate   "%VK_NUMPAD0"                           , "" , %VK_NUMPAD0
  thinBasic_AddEquate   "%VK_NUMPAD1"                           , "" , %VK_NUMPAD1
  thinBasic_AddEquate   "%VK_NUMPAD2"                           , "" , %VK_NUMPAD2
  thinBasic_AddEquate   "%VK_NUMPAD3"                           , "" , %VK_NUMPAD3
  thinBasic_AddEquate   "%VK_NUMPAD4"                           , "" , %VK_NUMPAD4
  thinBasic_AddEquate   "%VK_NUMPAD5"                           , "" , %VK_NUMPAD5
  thinBasic_AddEquate   "%VK_NUMPAD6"                           , "" , %VK_NUMPAD6
  thinBasic_AddEquate   "%VK_NUMPAD7"                           , "" , %VK_NUMPAD7
  thinBasic_AddEquate   "%VK_NUMPAD8"                           , "" , %VK_NUMPAD8
  thinBasic_AddEquate   "%VK_NUMPAD9"                           , "" , %VK_NUMPAD9
  thinBasic_AddEquate   "%VK_O"                                 , "" , %VK_O
  thinBasic_AddEquate   "%VK_P"                                 , "" , %VK_P
  thinBasic_AddEquate   "%VK_PAUSE"                             , "" , %VK_PAUSE
  thinBasic_AddEquate   "%VK_PGDN"                              , "" , %VK_PGDN
  thinBasic_AddEquate   "%VK_PGUP"                              , "" , %VK_PGUP
  thinBasic_AddEquate   "%VK_PRINT"                             , "" , %VK_PRINT
  thinBasic_AddEquate   "%VK_PRIOR"                             , "" , %VK_PRIOR
  thinBasic_AddEquate   "%VK_Q"                                 , "" , %VK_Q
  thinBasic_AddEquate   "%VK_R"                                 , "" , %VK_R
  thinBasic_AddEquate   "%VK_RBUTTON"                           , "" , %VK_RBUTTON
  thinBasic_AddEquate   "%VK_RCONTROL"                          , "" , %VK_RCONTROL
  thinBasic_AddEquate   "%VK_RETURN"                            , "" , %VK_RETURN
  thinBasic_AddEquate   "%VK_RIGHT"                             , "" , %VK_RIGHT
  thinBasic_AddEquate   "%VK_RMENU"                             , "" , %VK_RMENU
  thinBasic_AddEquate   "%VK_RSHIFT"                            , "" , %VK_RSHIFT
  thinBasic_AddEquate   "%VK_RWIN"                              , "" , %VK_RWIN
  thinBasic_AddEquate   "%VK_S"                                 , "" , %VK_S
  thinBasic_AddEquate   "%VK_SCROLL"                            , "" , %VK_SCROLL
  thinBasic_AddEquate   "%VK_SELECT"                            , "" , %VK_SELECT
  thinBasic_AddEquate   "%VK_SEPARATOR"                         , "" , %VK_SEPARATOR
  thinBasic_AddEquate   "%VK_SHIFT"                             , "" , %VK_SHIFT
  thinBasic_AddEquate   "%VK_SLEEP"                             , "" , %VK_SLEEP
  thinBasic_AddEquate   "%VK_SNAPSHOT"                          , "" , %VK_SNAPSHOT
  thinBasic_AddEquate   "%VK_SPACE"                             , "" , %VK_SPACE
  thinBasic_AddEquate   "%VK_SUBTRACT"                          , "" , %VK_SUBTRACT
  thinBasic_AddEquate   "%VK_T"                                 , "" , %VK_T
  thinBasic_AddEquate   "%VK_TAB"                               , "" , %VK_TAB
  thinBasic_AddEquate   "%VK_U"                                 , "" , %VK_U
  thinBasic_AddEquate   "%VK_UP"                                , "" , %VK_UP
  thinBasic_AddEquate   "%VK_V"                                 , "" , %VK_V
  thinBasic_AddEquate   "%VK_W"                                 , "" , %VK_W
  thinBasic_AddEquate   "%VK_X"                                 , "" , %VK_X
  thinBasic_AddEquate   "%VK_XBUTTON1"                          , "" , %VK_XBUTTON1
  thinBasic_AddEquate   "%VK_XBUTTON2"                          , "" , %VK_XBUTTON2
  thinBasic_AddEquate   "%VK_Y"                                 , "" , %VK_Y
  thinBasic_AddEquate   "%VK_Z"                                 , "" , %VK_Z

  thinBasic_AddEquate   "%TBGL_OnBeforeDraw"                    , "" , %TBGL_OnBeforeDraw
  thinBasic_AddEquate   "%TBGL_OnAfterDraw"                     , "" , %TBGL_OnAfterDraw
  thinBasic_AddEquate   "%TBGL_OnBeforeUpdate"                  , "" , %TBGL_OnBeforeUpdate
  thinBasic_AddEquate   "%TBGL_OnAfterUpdate"                   , "" , %TBGL_OnAfterUpdate
  thinBasic_AddEquate   "%TBGL_OnCollision"                     , "" , %TBGL_OnCollision
  thinBasic_AddEquate   "%TBGL_OnBeforeAnimate"                 , "" , %TBGL_OnBeforeAnimate
  thinBasic_AddEquate   "%TBGL_OnAfterAnimate"                  , "" , %TBGL_OnAfterAnimate
  thinBasic_AddEquate   "%TBGL_OnDelete"                        , "" , %TBGL_OnDelete
  thinBasic_AddEquate   "%TBGL_OnMouseOver"                     , "" , %TBGL_OnMouseOver

  thinBasic_AddEquate   "%TBGL_OnDropFiles"                     , "" , %TBGL.OnDropFiles

  thinBasic_AddEquate   "%TBGL_Pos"                             , "" , %TBGL_Pos
  thinBasic_AddEquate   "%TBGL_Rotate"                          , "" , %TBGL_Rotate
  thinBasic_AddEquate   "%TBGL_Scale"                           , "" , %TBGL_Scale
  thinBasic_AddEquate   "%TBGL_Delete"                          , "" , %TBGL_Delete
  thinBasic_AddEquate   "%TBGL_Speed"                           , "" , %TBGL_Speed
  thinBasic_AddEquate   "%TBGL_Spin"                            , "" , %TBGL_Spin
  thinBasic_AddEquate   "%TBGL_Orbit"                           , "" , %TBGL_Orbit
  thinBasic_AddEquate   "%TBGL_ChildRel"                        , "" , %TBGL_ChildRel

  thinBasic_AddEquate   "%TBGL_FILE"                            , "" , %TBGL_FILE
  thinBasic_AddEquate   "%TBGL_RETURN"                          , "" , %TBGL_RETURN

  thinBasic_AddVariable "TBGL_MeSprite"                         , "" , 0 ,  %VarSubType_Long, VARPTR(meSprite)
  thinBasic_AddVariable "TBGL_YouSprite"                        , "" , 0 ,  %VarSubType_Long, VARPTR(youSprite)

  thinBasic_AddEquate   "%TBGL_Rectangle"                       , "" , %TBGL_Rectangle
  thinBasic_AddEquate   "%TBGL_Circle"                          , "" , %TBGL_Circle

  thinBasic_AddEquate   "%TBGL_Loop"                            , "" , %TBGL_Loop
  thinBasic_AddEquate   "%TBGL_Interval"                        , "" , %TBGL_Interval
  thinBasic_AddEquate   "%TBGL_Bounce"                          , "" , %TBGL_Bounce

  thinBasic_AddEquate   "%TBGL_ColumnRow"                       , "" , %TBGL_ColumnRow
  thinBasic_AddEquate   "%TBGL_RowColumn"                       , "" , %TBGL_RowColumn

  thinBasic_AddEquate   "%TBGL_Repeat"                          , "" , %TBGL_REPEAT
  thinBasic_AddEquate   "%TBGL_ClampToEdge"                     , "" , %TBGL_CLAMPTOEDGE

  thinBasic_AddEquate   "%TBGL_BLACK"              , "" , %GL_CLEAR
  thinBasic_AddEquate   "%TBGL_WHITE"              , "" , %GL_SET
  thinBasic_AddEquate   "%TBGL_SRC"                , "" , %GL_COPY
  thinBasic_AddEquate   "%TBGL_SRC_INVERTED"       , "" , %GL_COPY_INVERTED
  thinBasic_AddEquate   "%TBGL_NOOP"               , "" , %GL_NOOP
  thinBasic_AddEquate   "%TBGL_DST_INVERTED"       , "" , %GL_INVERT
  thinBasic_AddEquate   "%TBGL_SRC_AND_DST"        , "" , %GL_AND
  thinBasic_AddEquate   "%TBGL_SRC_NAND_DST"       , "" , %GL_NAND
  thinBasic_AddEquate   "%TBGL_SRC_OR_DST"         , "" , %GL_OR
  thinBasic_AddEquate   "%TBGL_SRC_NOR_DST"        , "" , %GL_NOR
  thinBasic_AddEquate   "%TBGL_SRC_XOR_DST"        , "" , %GL_XOR
  thinBasic_AddEquate   "%TBGL_SRC_EQV_DST"        , "" , %GL_EQUIV
  thinBasic_AddEquate   "%TBGL_SRC_AND_NOT_DST"    , "" , %GL_AND_REVERSE
  thinBasic_AddEquate   "%TBGL_NOT_SRC_AND_DST"    , "" , %GL_AND_INVERTED
  thinBasic_AddEquate   "%TBGL_SRC_OR_NOT_DST"     , "" , %GL_OR_REVERSE
  thinBasic_AddEquate   "%TBGL_NOT_SRC_OR_DST"     , "" , %GL_OR_INVERTED

END FUNCTION

'----------------------------------------------------------------------------

FUNCTION UnLoadLocalSymbols ALIAS "UnLoadLocalSymbols" ( ) EXPORT AS LONG
  ' This function is automatically called by thinCore whenever this DLL is unloaded.
  ' This function CAN be present but it is not necessary. If present, this function
  ' will be executed by thinBasic core when module will be released.
  ' Use this function to perform uninitialize process, if needed.
  '----------------------------------------------------------------------------

  ' -----------------------------------------------------------------------------
  ' -- INITIALIZATION
  ' -----------------------------------------------------------------------------
  internal_DeleteAllSprites()
  Internal_TBGL_Free( )  ' -- In case programmer forgot to destroy window / canvas

  FUNCTION = 0&
END FUNCTION

'----------------------------------------------------------------------------

FUNCTION LIBMAIN ALIAS "LibMain" ( BYVAL hInstance AS LONG, BYVAL fwdReason AS LONG, BYVAL lpvReserved AS LONG ) EXPORT AS LONG

  g_ModuleInstance = hInstance
  SELECT CASE fwdReason

    CASE %DLL_PROCESS_ATTACH
      FUNCTION = 1&
      EXIT FUNCTION

    CASE %DLL_PROCESS_DETACH
      FUNCTION = 1&
      EXIT FUNCTION

    CASE %DLL_THREAD_ATTACH
      FUNCTION = 1&
      EXIT FUNCTION

    CASE %DLL_THREAD_DETACH
      FUNCTION = 1&
      EXIT FUNCTION

  END SELECT

END FUNCTION

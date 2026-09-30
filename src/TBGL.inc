' =============================================================================
'  Include file for TBGL
'
'  You can use it to include TBGL statically in your code, or to interface DLL
'
'  Define %TBGL_EMBEDDED = 1 before including this header should you wish to
'  not use the DLL, but compile TBGL into your application.
' =============================================================================

#IF NOT %DEF(%TBGL_THINBASIC_INTERPRETER_INTERFACE)
  %TBGL_THINBASIC_INTERPRETER_INTERFACE = 0
#ENDIF

#IF NOT %DEF(%TBGL_EMBEDDED)
  %TBGL_EMBEDDED = 0
#ENDIF

#IF %TBGL_EMBEDDED = 1
  #INCLUDE "thinbasic_TBGL.bas"
#ELSE
  TYPE TBGL_tVector3D
    x AS DOUBLE
    y AS DOUBLE
    z AS DOUBLE
  END TYPE

  TYPE TBGL_tCollisionInfo
    position AS TBGL_tVector3D
    normal   AS TBGL_tVector3D
    depth    AS DOUBLE
    scene    AS LONG
    entity1  AS LONG
    entity2  AS LONG
  END TYPE

  TYPE TBGL_tEntityIdentifier
    scene  AS LONG
    entity AS LONG
  END TYPE

  TYPE TBGL_tFaceParameters
    R       AS BYTE
    G       AS BYTE
    B       AS BYTE
    Texture AS LONG
  END TYPE

  TYPE TBGL_tTexturingInfo
    maxWidth    AS LONG
    maxHeight   AS LONG
    NPOTSupport AS LONG
  END TYPE

  TYPE TBGL_tUseInfo
    Fog              AS LONG
    Texturing        AS LONG
    Blend            AS LONG
    Depth            AS LONG
    DepthMask        AS LONG
    Lighting         AS LONG
    Alpha            AS LONG
    LineStipple      AS LONG
    RenderMatrixMode AS LONG
  END TYPE

  TYPE TBGL_tVector2F
    x AS SINGLE
    y AS SINGLE
  END TYPE

  TYPE TBGL_tVector3F
    x AS SINGLE
    y AS SINGLE
    z AS SINGLE
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

  %GL_CLIP_PLANE0                   = 12288
  %GL_CLIP_PLANE1                   = 12289
  %GL_CLIP_PLANE2                   = 12290
  %GL_CLIP_PLANE3                   = 12291
  %GL_CLIP_PLANE4                   = 12292
  %GL_CLIP_PLANE5                   = 12293
  %GL_DST_ALPHA                     = 772
  %GL_DST_COLOR                     = 774
  %GL_FILL                          = 6914
  %GL_LIGHT0                        = 16384
  %GL_LIGHT1                        = 16385
  %GL_LIGHT2                        = 16386
  %GL_LIGHT3                        = 16387
  %GL_LIGHT4                        = 16388
  %GL_LIGHT5                        = 16389
  %GL_LIGHT6                        = 16390
  %GL_LIGHT7                        = 16391
  %GL_LINE                          = 6913
  %GL_LINES                         = 1
  %GL_LINE_LOOP                     = 2
  %GL_LINE_STRIP                    = 3
  %GL_ONE                           = 1
  %GL_ONE_MINUS_DST_ALPHA           = 773
  %GL_ONE_MINUS_DST_COLOR           = 775
  %GL_ONE_MINUS_SRC_ALPHA           = 771
  %GL_ONE_MINUS_SRC_COLOR           = 769
  %GL_POINT                         = 6912
  %GL_POINTS                        = 0
  %GL_POLYGON                       = 9
  %GL_QUADS                         = 7
  %GL_QUAD_STRIP                    = 8
  %GL_SRC_ALPHA                     = 770
  %GL_SRC_ALPHA_SATURATE            = 776
  %GL_SRC_COLOR                     = 768
  %GL_TRIANGLES                     = 4
  %GL_TRIANGLE_FAN                  = 6
  %GL_TRIANGLE_STRIP                = 5
  %GL_ZERO                          = 0
  %TBGL_2D                          = 2
  %TBGL_3D                          = 3
  %TBGL_ALIGN_CENTER                = 100
  %TBGL_ALIGN_CENTER_CENTER         = 103
  %TBGL_ALIGN_CENTER_DOWN           = 102
  %TBGL_ALIGN_CENTER_UP             = 101
  %TBGL_ALIGN_LEFT                  = -10
  %TBGL_ALIGN_LEFT_CENTER           = -13
  %TBGL_ALIGN_LEFT_DOWN             = -12
  %TBGL_ALIGN_LEFT_UP               = -11
  %TBGL_ALIGN_NONE                  = -1
  %TBGL_ALIGN_RIGHT                 = 10
  %TBGL_ALIGN_RIGHT_CENTER          = 13
  %TBGL_ALIGN_RIGHT_DOWN            = 12
  %TBGL_ALIGN_RIGHT_UP              = 11
  %TBGL_ALPHA                       = 8
  %TBGL_ALWAYS                      = 519
  %TBGL_BLACK                       = 5376
  %TBGL_BLEND                       = 4
  %TBGL_BLUE                        = 4
  %TBGL_BOLD                        = 1
  %TBGL_Bounce                      = 2
  %TBGL_BOX                         = 7
  %TBGL_CAMERA                      = 2
  %TBGL_CAMERATYPE_DEFAULT          = 0
  %TBGL_ChildRel                    = 256
  %TBGL_Circle                      = 2
  %TBGL_ClampToEdge                 = 2
  %TBGL_CLEAR_COLOR                 = 1
  %TBGL_CLEAR_DEPTH                 = 2
  %TBGL_CLIENTAREA                  = 0
  %TBGL_ColumnRow                   = 0
  %TBGL_CUSTOM                      = -1
  %TBGL_CYLINDER                    = 10
  %TBGL_DATA_BGR                    = 4
  %TBGL_DATA_BGRA                   = 5
  %TBGL_DATA_RGB                    = 0
  %TBGL_DATA_RGBA                   = 1
  %TBGL_DEFAULT                     = -1
  %TBGL_Delete                      = 8
  %TBGL_DEPTH                       = 16
  %TBGL_DEPTHMASK                   = 32
  %TBGL_DISPLAYLIST                 = 6
  %TBGL_DST_INVERTED                = 5386
  %TBGL_ENTITY_CAMERA               = 2
  %TBGL_ENTITY_DISPLAYLIST          = 6
  %TBGL_ENTITY_EMPTY                = 0
  %TBGL_ENTITY_LIGHT                = 1
  %TBGL_ENTITY_MODEL                = 5
  %TBGL_ENTITY_PIVOT                = 3
  %TBGL_EQUAL                       = 514
  %TBGL_ERROR_FILE                  = 2
  %TBGL_ERROR_MSGBOX                = 1
  %TBGL_ERROR_NONE                  = 0
  %TBGL_FILE                        = 0
  %TBGL_FILE_BMP                    = 2
  %TBGL_FILE_TGA                    = 3
  %TBGL_FOG                         = 1
  %TBGL_GEQUAL                      = 518
  %TBGL_GREATER                     = 516
  %TBGL_GREEN                       = 2
  %TBGL_Interval                    = 1
  %TBGL_ITALICS                     = 2
  %TBGL_LEQUAL                      = 515
  %TBGL_LESS                        = 513
  %TBGL_LIGHT                       = 1
  %TBGL_LIGHTING                    = 64
  %TBGL_LIGHTTYPE_DIRECTIONAL       = 1
  %TBGL_LIGHTTYPE_POINT             = 0
  %TBGL_LIGHTTYPE_SPOT              = 2
  %TBGL_LIGHT_AMBIENT               = 4608
  %TBGL_LIGHT_CONSTANT_ATTENUATION  = 4615
  %TBGL_LIGHT_DIFFUSE               = 4609
  %TBGL_LIGHT_LINEAR_ATTENUATION    = 4616
  %TBGL_LIGHT_POSITION              = 4611
  %TBGL_LIGHT_QUADRATIC_ATTENUATION = 4617
  %TBGL_LIGHT_SPECULAR              = 4610
  %TBGL_LIGHT_SPOT_CUTOFF           = 4614
  %TBGL_LIGHT_SPOT_DIRECTION        = 4612
  %TBGL_LIGHT_SPOT_EXPONENT         = 4613
  %TBGL_LINESTIPPLE                 = 128
  %TBGL_Loop                        = 0
  %TBGL_M15B                        = 10
  %TBGL_M15G                        = 9
  %TBGL_M15LAYER                    = 11
  %TBGL_M15PSTOP                    = 4
  %TBGL_M15R                        = 8
  %TBGL_M15TEXN                     = 5
  %TBGL_M15TEXX                     = 6
  %TBGL_M15TEXY                     = 7
  %TBGL_M15X                        = 1
  %TBGL_M15Y                        = 2
  %TBGL_M15Z                        = 3
  %TBGL_MODEL                       = 5
  %TBGL_NEVER                       = 512
  %TBGL_NOOP                        = 5381
  %TBGL_NORMAL_FROMFILE             = 3
  %TBGL_NORMAL_NONE                 = 0
  %TBGL_NORMAL_PRECISE              = 1
  %TBGL_NORMAL_SMOOTH               = 2
  %TBGL_NOTEQUAL                    = 517
  %TBGL_NOT_SRC_AND_DST             = 5380
  %TBGL_NOT_SRC_OR_DST              = 5389
  %TBGL_OBJ_CUBE                    = 2
  %TBGL_OBJ_CUBE3                   = 3
  %TBGL_OBJ_CYLINDER                = 4
  %TBGL_OBJ_SPHERE                  = 1
  %TBGL_OnAfterAnimate              = 7
  %TBGL_OnAfterDraw                 = 5
  %TBGL_OnAfterUpdate               = 2
  %TBGL_OnBeforeAnimate             = 6
  %TBGL_OnBeforeDraw                = 4
  %TBGL_OnBeforeUpdate              = 1
  %TBGL_OnCollision                 = 3
  %TBGL_OnDelete                    = 8
  %TBGL_OnDropFiles                 = 0
  %TBGL_OnMouseOver                 = 9
  %TBGL_OPENGL_ORTHO_2D             = 0
  %TBGL_Orbit                       = 64
  %TBGL_PARAM_PIXELS                = 1
  %TBGL_PARAM_RELATIVE              = 0
  %TBGL_PINFO_RGB                   = 123
  %TBGL_PINFO_XYZ                   = 456
  %TBGL_PIVOT                       = 3
  %TBGL_PIXEL_PERFECT_2D            = 1
  %TBGL_Pos                         = 1
  %TBGL_PRESERVE                    = 2
  %TBGL_Rectangle                   = 1
  %TBGL_RED                         = 1
  %TBGL_Repeat                      = 1
  %TBGL_RESET                       = 1
  %TBGL_RESET_MATRIX                = 4
  %TBGL_RETURN                      = 1
  %TBGL_Rotate                      = 2
  %TBGL_RowColumn                   = 1
  %TBGL_Scale                       = 4
  %TBGL_Speed                       = 16
  %TBGL_SPHERE                      = 8
  %TBGL_Spin                        = 32
  %TBGL_SRC                         = 5379
  %TBGL_SRC_AND_DST                 = 5377
  %TBGL_SRC_AND_NOT_DST             = 5378
  %TBGL_SRC_EQV_DST                 = 5385
  %TBGL_SRC_INVERTED                = 5388
  %TBGL_SRC_NAND_DST                = 5390
  %TBGL_SRC_NOR_DST                 = 5384
  %TBGL_SRC_OR_DST                  = 5383
  %TBGL_SRC_OR_NOT_DST              = 5387
  %TBGL_SRC_XOR_DST                 = 5382
  %TBGL_TEXTURING                   = 2
  %TBGL_TEX_ANISO                   = 3
  %TBGL_TEX_CRISPANISO              = 4
  %TBGL_TEX_LINEAR                  = 1
  %TBGL_TEX_MIPMAP                  = 2
  %TBGL_TEX_NEAREST                 = 0
  %TBGL_TORUS                       = 9
  %TBGL_UNDEFINED                   = -2147483648
  %TBGL_VIEWPORT                    = 1
  %TBGL_WHITE                       = 5391
  %TBGL_WINDOWAREA                  = 2
  %TBGL_WS_CLOSEBOX                 = 8
  %TBGL_WS_DONTKEEPASPECTRATIO      = 16
  %TBGL_WS_DONTSIZE                 = 32
  %TBGL_WS_FULLSCREEN               = 1
  %TBGL_WS_MAXIMIZEBOX              = 4
  %TBGL_WS_MINIMIZEBOX              = 2
  %TBGL_WS_WINDOWED                 = 0
  %TBGL_X                           = 1
  %TBGL_Y                           = 2
  %TBGL_Z                           = 3
  %VK_0                             = 48
  %VK_1                             = 49
  %VK_2                             = 50
  %VK_3                             = 51
  %VK_4                             = 52
  %VK_5                             = 53
  %VK_6                             = 54
  %VK_7                             = 55
  %VK_8                             = 56
  %VK_9                             = 57
  %VK_A                             = 65
  %VK_ACCEPT                        = 30
  %VK_ADD                           = 107
  %VK_APPS                          = 93
  %VK_B                             = 66
  %VK_BACK                          = 8
  %VK_C                             = 67
  %VK_CANCEL                        = 3
  %VK_CAPITAL                       = 20
  %VK_CLEAR                         = 12
  %VK_CONTROL                       = 17
  %VK_CONVERT                       = 28
  %VK_D                             = 68
  %VK_DECIMAL                       = 110
  %VK_DELETE                        = 46
  %VK_DIVIDE                        = 111
  %VK_DOWN                          = 40
  %VK_E                             = 69
  %VK_END                           = 35
  %VK_ESCAPE                        = 27
  %VK_EXECUTE                       = 43
  %VK_F                             = 70
  %VK_F1                            = 112
  %VK_F10                           = 121
  %VK_F11                           = 122
  %VK_F12                           = 123
  %VK_F13                           = 124
  %VK_F14                           = 125
  %VK_F15                           = 126
  %VK_F16                           = 127
  %VK_F17                           = 128
  %VK_F18                           = 129
  %VK_F19                           = 130
  %VK_F2                            = 113
  %VK_F20                           = 131
  %VK_F21                           = 132
  %VK_F22                           = 133
  %VK_F23                           = 134
  %VK_F24                           = 135
  %VK_F3                            = 114
  %VK_F4                            = 115
  %VK_F5                            = 116
  %VK_F6                            = 117
  %VK_F7                            = 118
  %VK_F8                            = 119
  %VK_F9                            = 120
  %VK_FINAL                         = 24
  %VK_G                             = 71
  %VK_H                             = 72
  %VK_HANGEUL                       = 21
  %VK_HANGUL                        = 21
  %VK_HANJA                         = 25
  %VK_HELP                          = 47
  %VK_HOME                          = 36
  %VK_I                             = 73
  %VK_INSERT                        = 45
  %VK_J                             = 74
  %VK_JUNJA                         = 23
  %VK_K                             = 75
  %VK_KANA                          = 21
  %VK_KANJI                         = 25
  %VK_L                             = 76
  %VK_LBUTTON                       = 1
  %VK_LCONTROL                      = 162
  %VK_LEFT                          = 37
  %VK_LINEFEED                      = 10
  %VK_LMENU                         = 164
  %VK_LSHIFT                        = 160
  %VK_LWIN                          = 91
  %VK_M                             = 77
  %VK_MBUTTON                       = 4
  %VK_MENU                          = 18
  %VK_MODECHANGE                    = 31
  %VK_MULTIPLY                      = 106
  %VK_N                             = 78
  %VK_NEXT                          = 34
  %VK_NONCONVERT                    = 29
  %VK_NUMLOCK                       = 144
  %VK_NUMPAD0                       = 96
  %VK_NUMPAD1                       = 97
  %VK_NUMPAD2                       = 98
  %VK_NUMPAD3                       = 99
  %VK_NUMPAD4                       = 100
  %VK_NUMPAD5                       = 101
  %VK_NUMPAD6                       = 102
  %VK_NUMPAD7                       = 103
  %VK_NUMPAD8                       = 104
  %VK_NUMPAD9                       = 105
  %VK_O                             = 79
  %VK_P                             = 80
  %VK_PAUSE                         = 19
  %VK_PGDN                          = 34
  %VK_PGUP                          = 33
  %VK_PRINT                         = 42
  %VK_PRIOR                         = 33
  %VK_Q                             = 81
  %VK_R                             = 82
  %VK_RBUTTON                       = 2
  %VK_RCONTROL                      = 163
  %VK_RETURN                        = 13
  %VK_RIGHT                         = 39
  %VK_RMENU                         = 165
  %VK_RSHIFT                        = 161
  %VK_RWIN                          = 92
  %VK_S                             = 83
  %VK_SCROLL                        = 145
  %VK_SELECT                        = 41
  %VK_SEPARATOR                     = 108
  %VK_SHIFT                         = 16
  %VK_SLEEP                         = 95
  %VK_SNAPSHOT                      = 44
  %VK_SPACE                         = 32
  %VK_SUBTRACT                      = 109
  %VK_T                             = 84
  %VK_TAB                           = 9
  %VK_U                             = 85
  %VK_UP                            = 38
  %VK_V                             = 86
  %VK_W                             = 87
  %VK_X                             = 88
  %VK_XBUTTON1                      = 5
  %VK_XBUTTON2                      = 6
  %VK_Y                             = 89
  %VK_Z                             = 90

  DECLARE SUB tbgl_EntityCreateBoundingSphere LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateBoundingSphere" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL PARENT AS LONG, BYVAL eRadius AS SINGLE, BYVAL FuncPTR AS LONG)
  DECLARE FUNCTION tbgl_EntityGetCollisionInfo LIB "TBGL.DLL" ALIAS "tbgl_EntityGetCollisionInfo" () AS DWORD
  DECLARE SUB tbgl_EntityCreateBox LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateBox" ( _
      BYVAL ScriptSceneID AS LONG _
    , BYVAL EntityID AS LONG _
    , BYVAL parentEntity AS LONG _
    , BYVAL sizeA AS SINGLE _
    , BYVAL sizeB AS SINGLE _
    , BYVAL sizeC AS SINGLE _
    , BYVAL tex AS LONG _
    , BYVAL r AS BYTE _
    , BYVAL g AS BYTE _
    , BYVAL b AS BYTE _
    )
  DECLARE SUB tbgl_EntityCreateCamera LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateCamera" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL PARENT AS LONG)
  DECLARE SUB tbgl_EntitySetFOV LIB "TBGL.DLL" ALIAS "tbgl_EntitySetFOV" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL fov AS DOUBLE)
  DECLARE SUB tbgl_EntitySetColorMask LIB "TBGL.DLL" ALIAS "tbgl_EntitySetColorMask" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL lcm AS LONG)
  DECLARE SUB tbgl_EntityCreateCylinder LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateCylinder" ( _
      BYVAL ScriptSceneID AS LONG _
    , BYVAL EntityID AS LONG _
    , BYVAL PARENT AS LONG _
    , BYVAL radius AS SINGLE _
    , BYVAL radius2 AS SINGLE _
    , BYVAL height AS SINGLE _
    , BYVAL tex AS LONG _
    , BYVAL r AS BYTE _
    , BYVAL g AS BYTE _
    , BYVAL b AS BYTE _
    )
  DECLARE SUB tbgl_EntityCreateCylinderCapped LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateCylinderCapped" ( _
      BYVAL ScriptSceneID AS LONG _
    , BYVAL EntityID AS LONG _
    , BYVAL PARENT AS LONG _
    , BYVAL radius AS SINGLE _
    , BYVAL radius2 AS SINGLE _
    , BYVAL height AS SINGLE _
    , BYVAL tex AS LONG _
    , BYVAL r AS BYTE _
    , BYVAL g AS BYTE _
    , BYVAL b AS BYTE _
    )
  DECLARE SUB tbgl_EntityCreateDLSlot LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateDLSlot" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL PARENT AS LONG, BYVAL displayListSlot AS LONG)
  DECLARE SUB tbgl_EntityCreateFuncSlot LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateFuncSlot" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL PARENT AS LONG, BYVAL FuncPTR AS LONG)
  DECLARE FUNCTION tbgl_CallingEntity LIB "TBGL.DLL" ALIAS "tbgl_CallingEntity" () AS DWORD
  DECLARE SUB tbgl_EntityCreateLight LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateLight" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL PARENT AS LONG, BYVAL lightType AS LONG)
  DECLARE SUB tbgl_EntitySetCutoff LIB "TBGL.DLL" ALIAS "tbgl_EntitySetCutoff" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL cutoff AS SINGLE)
  DECLARE SUB tbgl_EntitySetBorderFade LIB "TBGL.DLL" ALIAS "tbgl_EntitySetBorderFade" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL exponent AS SINGLE)
  DECLARE SUB tbgl_EntitySetAmbient LIB "TBGL.DLL" ALIAS "tbgl_EntitySetAmbient" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE)
  DECLARE SUB tbgl_EntitySetSpecular LIB "TBGL.DLL" ALIAS "tbgl_EntitySetSpecular" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE)
  DECLARE SUB tbgl_EntityCreateModelSlot LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateModelSlot" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL PARENT AS LONG, BYVAL modelSlot AS LONG)
  DECLARE SUB tbgl_EntityCreatePivot LIB "TBGL.DLL" ALIAS "tbgl_EntityCreatePivot" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL PARENT AS LONG, BYVAL px AS DOUBLE, BYVAL py AS DOUBLE, BYVAL pz AS DOUBLE)
  DECLARE SUB tbgl_EntityCreateSphere LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateSphere" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL PARENT AS LONG, BYVAL radius AS SINGLE, BYVAL tex AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE)
  DECLARE SUB tbgl_EntityCreateTorus LIB "TBGL.DLL" ALIAS "tbgl_EntityCreateTorus" ( _
      BYVAL ScriptSceneID AS LONG _
    , BYVAL EntityID AS LONG _
    , BYVAL PARENT AS LONG _
    , BYVAL radius AS SINGLE _
    , BYVAL radius2 AS SINGLE _
    , BYVAL tex AS LONG _
    , BYVAL r AS BYTE _
    , BYVAL g AS BYTE _
    , BYVAL b AS BYTE _
    )
  DECLARE SUB tbgl_UseBlend LIB "TBGL.DLL" ALIAS "tbgl_UseBlend" (BYVAL flag AS LONG)
  DECLARE SUB tbgl_BlendFunc LIB "TBGL.DLL" ALIAS "tbgl_BlendFunc" (BYVAL srcFactor AS DWORD, BYVAL dstFactor AS DWORD)
  DECLARE FUNCTION tbgl_GetUseBlend LIB "TBGL.DLL" ALIAS "tbgl_GetUseBlend" () AS LONG
  DECLARE SUB tbgl_PushBlendFunc LIB "TBGL.DLL" ALIAS "tbgl_PushBlendFunc" (BYVAL srcFactor AS DWORD, BYVAL dstFactor AS DWORD)
  DECLARE SUB tbgl_PopBlendFunc LIB "TBGL.DLL" ALIAS "tbgl_PopBlendFunc" ()
  DECLARE FUNCTION tbgl_BindCanvas LIB "TBGL.DLL" ALIAS "tbgl_BindCanvas" (BYVAL hCtrl AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_UpdateCanvasProportions LIB "TBGL.DLL" ALIAS "tbgl_UpdateCanvasProportions" (BYVAL hCtrl AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_CanvasBound LIB "TBGL.DLL" ALIAS "tbgl_CanvasBound" (BYVAL hCtrl AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_ReleaseCanvas LIB "TBGL.DLL" ALIAS "tbgl_ReleaseCanvas" (BYVAL hCtrl AS DWORD) AS LONG
  DECLARE SUB tbgl_UseClipPlane LIB "TBGL.DLL" ALIAS "tbgl_UseClipPlane" (BYVAL whichPlane AS DWORD, BYVAL flag AS LONG)
  DECLARE SUB tbgl_SetupClipPlane LIB "TBGL.DLL" ALIAS "tbgl_SetupClipPlane" (BYVAL whichPlane AS DWORD, BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE, BYVAL w AS DOUBLE)
  DECLARE FUNCTION tbgl_PointInside3D LIB "TBGL.DLL" ALIAS "tbgl_PointInside3D" ( _
      BYVAL x AS DOUBLE _
    , BYVAL y AS DOUBLE _
    , BYVAL z AS DOUBLE _
    , BYVAL lType AS LONG _
    , BYVAL cx AS DOUBLE _
    , BYVAL cy AS DOUBLE _
    , BYVAL cz AS DOUBLE _
    , BYVAL par1 AS DOUBLE _
    , BYVAL par2 AS DOUBLE _
    , BYVAL par3 AS DOUBLE _
    ) AS LONG
  DECLARE FUNCTION tbgl_IsPointBehindView LIB "TBGL.DLL" ALIAS "tbgl_IsPointBehindView" (BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE) AS LONG
  DECLARE FUNCTION tbgl_IsPointVisible LIB "TBGL.DLL" ALIAS "tbgl_IsPointVisible" (BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE) AS LONG
  DECLARE SUB tbgl_Pos3DtoPos2D LIB "TBGL.DLL" ALIAS "tbgl_Pos3DtoPos2D" (BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE, BYREF x2D AS DOUBLE, BYREF y2D AS DOUBLE)
  DECLARE SUB tbgl_UseQuery LIB "TBGL.DLL" ALIAS "tbgl_UseQuery" (BYREF useInfo AS TBGL_tUseInfo)
  DECLARE SUB tbgl_Color LIB "TBGL.DLL" ALIAS "tbgl_Color" (BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE)
  DECLARE SUB tbgl_ColorAlpha LIB "TBGL.DLL" ALIAS "tbgl_ColorAlpha" (BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE)
  DECLARE SUB tbgl_BackColor LIB "TBGL.DLL" ALIAS "tbgl_BackColor" (BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE)
  DECLARE SUB tbgl_UseAlphaTest LIB "TBGL.DLL" ALIAS "tbgl_UseAlphaTest" (BYVAL flag AS LONG)
  DECLARE SUB tbgl_AlphaFunc LIB "TBGL.DLL" ALIAS "tbgl_AlphaFunc" (BYVAL func AS DWORD, BYVAL value AS SINGLE)
  DECLARE SUB tbgl_PushAlphaFunc LIB "TBGL.DLL" ALIAS "tbgl_PushAlphaFunc" (BYVAL func AS DWORD, BYVAL value AS SINGLE)
  DECLARE SUB tbgl_PopAlphaFunc LIB "TBGL.DLL" ALIAS "tbgl_PopAlphaFunc" ()
  DECLARE SUB tbgl_PushLogicOp LIB "TBGL.DLL" ALIAS "tbgl_PushLogicOp" (BYVAL logicOp AS DWORD)
  DECLARE SUB tbgl_PopLogicOp LIB "TBGL.DLL" ALIAS "tbgl_PopLogicOp" ()
  DECLARE SUB tbgl_UseDepth LIB "TBGL.DLL" ALIAS "tbgl_UseDepth" (BYVAL flag AS LONG)
  DECLARE SUB tbgl_UseDepthMask LIB "TBGL.DLL" ALIAS "tbgl_UseDepthMask" (BYVAL flag AS LONG)
  DECLARE FUNCTION tbgl_GetUseDepth LIB "TBGL.DLL" ALIAS "tbgl_GetUseDepth" () AS LONG
  DECLARE SUB tbgl_DepthFunc LIB "TBGL.DLL" ALIAS "tbgl_DepthFunc" (BYVAL func AS DWORD)
  DECLARE SUB tbgl_PushDepthFunc LIB "TBGL.DLL" ALIAS "tbgl_PushDepthFunc" (BYVAL func AS DWORD)
  DECLARE SUB tbgl_PopDepthFunc LIB "TBGL.DLL" ALIAS "tbgl_PopDepthFunc" ()
  DECLARE SUB tbgl_NewListSpace LIB "TBGL.DLL" ALIAS "tbgl_NewListSpace" (BYVAL listIDs AS LONG PTR, BYVAL count AS LONG)
  DECLARE SUB tbgl_DeleteListSpace LIB "TBGL.DLL" ALIAS "tbgl_DeleteListSpace" (BYVAL listIDs AS LONG PTR, BYVAL count AS LONG)
  DECLARE FUNCTION tbgl_NewList LIB "TBGL.DLL" ALIAS "tbgl_NewList" (BYVAL list AS LONG) AS LONG
  DECLARE SUB tbgl_EndList LIB "TBGL.DLL" ALIAS "tbgl_EndList" ()
  DECLARE FUNCTION tbgl_CallList LIB "TBGL.DLL" ALIAS "tbgl_CallList" (BYVAL list AS LONG) AS LONG
  DECLARE SUB tbgl_DeleteList LIB "TBGL.DLL" ALIAS "tbgl_DeleteList" (BYVAL list AS LONG)
  DECLARE SUB tbgl_SceneCreate LIB "TBGL.DLL" ALIAS "tbgl_SceneCreate" (BYVAL ScriptSceneID AS LONG)
  DECLARE SUB tbgl_SceneRender LIB "TBGL.DLL" ALIAS "tbgl_SceneRender" (BYVAL ScriptSceneID AS LONG)
  DECLARE SUB tbgl_SceneCollide LIB "TBGL.DLL" ALIAS "tbgl_SceneCollide" (BYVAL ScriptSceneID AS LONG)
  DECLARE FUNCTION tbgl_EntityEnumByDataSignature LIB "TBGL.DLL" ALIAS "tbgl_EntityEnumByDataSignature" (BYVAL ScriptSceneID AS LONG, BYVAL ElementSignature AS LONG, BYVAL entityIDs AS LONG PTR, BYVAL maxCount AS LONG) AS LONG
  DECLARE SUB tbgl_EntityDestroy LIB "TBGL.DLL" ALIAS "tbgl_EntityDestroy" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG)
  DECLARE SUB tbgl_EntityAttach LIB "TBGL.DLL" ALIAS "tbgl_EntityAttach" (BYVAL sceneID AS LONG, BYVAL entityID AS LONG, BYVAL parentEntityID AS LONG, BYVAL MODE AS LONG)
  DECLARE SUB tbgl_EntityDetach LIB "TBGL.DLL" ALIAS "tbgl_EntityDetach" (BYVAL sceneID AS LONG, BYVAL entityID AS LONG, BYVAL MODE AS LONG)
  DECLARE SUB tbgl_EntitySetParent LIB "TBGL.DLL" ALIAS "tbgl_EntitySetParent" (BYVAL sceneID AS LONG, BYVAL entityID AS LONG, BYVAL parentEntityID AS LONG)
  DECLARE FUNCTION tbgl_EntityGetParent LIB "TBGL.DLL" ALIAS "tbgl_EntityGetParent" (BYVAL sceneID AS LONG, BYVAL entityID AS LONG) AS LONG
  DECLARE SUB tbgl_EntitySetUserPointer LIB "TBGL.DLL" ALIAS "tbgl_EntitySetUserPointer" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL uPointer AS LONG)
  DECLARE FUNCTION tbgl_EntityGetUserPointer LIB "TBGL.DLL" ALIAS "tbgl_EntityGetUserPointer" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG) AS LONG
  DECLARE SUB tbgl_EntitySetUserData LIB "TBGL.DLL" ALIAS "tbgl_EntitySetUserData" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL pData AS DWORD, BYVAL dataSize AS LONG)
  DECLARE SUB tbgl_EntitySetUserDataString LIB "TBGL.DLL" ALIAS "tbgl_EntitySetUserDataString" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL sData AS STRING)
  DECLARE FUNCTION tbgl_EntityGetUserDataPointer LIB "TBGL.DLL" ALIAS "tbgl_EntityGetUserDataPointer" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG) AS DWORD
  DECLARE SUB tbgl_EntitySetName LIB "TBGL.DLL" ALIAS "tbgl_EntitySetName" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL sName AS STRING)
  DECLARE FUNCTION tbgl_EntityGetName LIB "TBGL.DLL" ALIAS "tbgl_EntityGetName" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG) AS STRING
  DECLARE FUNCTION tbgl_EntityGetFreeID LIB "TBGL.DLL" ALIAS "tbgl_EntityGetFreeID" (BYVAL ScriptSceneID AS LONG, BYVAL EntityFrom AS LONG) AS LONG
  DECLARE SUB tbgl_EntitySetUse LIB "TBGL.DLL" ALIAS "tbgl_EntitySetUse" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL Use AS LONG)
  DECLARE FUNCTION tbgl_EntityGetUse LIB "TBGL.DLL" ALIAS "tbgl_EntityGetUse" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG) AS LONG
  DECLARE SUB tbgl_EntityCopyTo LIB "TBGL.DLL" ALIAS "tbgl_EntityCopyTo" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL EntityID2 AS LONG, BYVAL PARENT AS LONG)
  DECLARE SUB tbgl_EntitySetPos LIB "TBGL.DLL" ALIAS "tbgl_EntitySetPos" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE)
  DECLARE SUB tbgl_EntityGetPos LIB "TBGL.DLL" ALIAS "tbgl_EntityGetPos" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYREF x AS DOUBLE, BYREF y AS DOUBLE, BYREF z AS DOUBLE, BYVAL GlobalPosition AS LONG)
  DECLARE SUB tbgl_EntityMove LIB "TBGL.DLL" ALIAS "tbgl_EntityMove" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL dx AS DOUBLE, BYVAL dy AS DOUBLE, BYVAL dz AS DOUBLE)
  DECLARE SUB tbgl_EntityPush LIB "TBGL.DLL" ALIAS "tbgl_EntityPush" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL dx AS DOUBLE, BYVAL dy AS DOUBLE, BYVAL dz AS DOUBLE)
  DECLARE SUB tbgl_EntityTrackPos LIB "TBGL.DLL" ALIAS "tbgl_EntityTrackPos" ( _
      BYVAL ScriptSceneID AS LONG _
    , BYVAL EntityID AS LONG _
    , BYVAL x AS DOUBLE _
    , BYVAL y AS DOUBLE _
    , BYVAL z AS DOUBLE _
    , BYREF trackedX AS DOUBLE _
    , BYREF trackedY AS DOUBLE _
    , BYREF trackedZ AS DOUBLE _
    )
  DECLARE FUNCTION tbgl_EntityFindNearestByPos LIB "TBGL.DLL" ALIAS "tbgl_EntityFindNearestByPos" ( _
      BYVAL ScriptSceneID AS LONG _
    , BYVAL EntityType AS LONG _
    , BYVAL x AS DOUBLE _
    , BYVAL y AS DOUBLE _
    , BYVAL z AS DOUBLE _
    , BYVAL rangeX AS DOUBLE _
    , BYVAL rangeY AS DOUBLE _
    , BYVAL rangeZ AS DOUBLE _
    , BYVAL GlobalCoordinates AS LONG _
    ) AS LONG
  DECLARE FUNCTION tbgl_EntityFindByPos LIB "TBGL.DLL" ALIAS "tbgl_EntityFindByPos" (BYVAL ScriptSceneID AS LONG, BYVAL EntityType AS LONG, BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE, BYVAL GlobalCoordinates AS LONG) AS LONG
  DECLARE FUNCTION tbgl_EntityFindNearest LIB "TBGL.DLL" ALIAS "tbgl_EntityFindNearest" ( _
      BYVAL ScriptSceneID AS LONG _
    , BYVAL EntityType AS LONG _
    , BYVAL EntityFrom AS LONG _
    , BYVAL rangeX AS DOUBLE _
    , BYVAL rangeY AS DOUBLE _
    , BYVAL rangeZ AS DOUBLE _
    , BYVAL GlobalCoordinates AS LONG _
    ) AS LONG
  DECLARE FUNCTION tbgl_EntityGetDistance LIB "TBGL.DLL" ALIAS "tbgl_EntityGetDistance" (BYVAL ScriptSceneID AS LONG, BYVAL EntityFrom AS LONG, BYVAL EntityTo AS LONG) AS DOUBLE
  DECLARE FUNCTION tbgl_EntityGetDistancePos LIB "TBGL.DLL" ALIAS "tbgl_EntityGetDistancePos" (BYVAL ScriptSceneID AS LONG, BYVAL EntityFrom AS LONG, BYVAL x2 AS DOUBLE, BYVAL y2 AS DOUBLE, BYVAL z2 AS DOUBLE) AS DOUBLE
  DECLARE SUB tbgl_EntitySetScale LIB "TBGL.DLL" ALIAS "tbgl_EntitySetScale" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL sx AS DOUBLE, BYVAL sy AS DOUBLE, BYVAL sz AS DOUBLE)
  DECLARE SUB tbgl_EntityGetScale LIB "TBGL.DLL" ALIAS "tbgl_EntityGetScale" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYREF sx AS DOUBLE, BYREF sy AS DOUBLE, BYREF sz AS DOUBLE)
  DECLARE SUB tbgl_EntitySetRot LIB "TBGL.DLL" ALIAS "tbgl_EntitySetRot" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE)
  DECLARE SUB tbgl_EntityTurn LIB "TBGL.DLL" ALIAS "tbgl_EntityTurn" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL dx AS DOUBLE, BYVAL dy AS DOUBLE, BYVAL dz AS DOUBLE)
  DECLARE SUB tbgl_EntitySetTarget LIB "TBGL.DLL" ALIAS "tbgl_EntitySetTarget" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL TargetEntityID AS LONG)
  DECLARE SUB tbgl_EntitySetTargetRange LIB "TBGL.DLL" ALIAS "tbgl_EntitySetTargetRange" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL LastParam AS LONG, BYVAL EntityID2 AS LONG)
  DECLARE SUB tbgl_EntitySetTargetPos LIB "TBGL.DLL" ALIAS "tbgl_EntitySetTargetPos" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE)
  DECLARE SUB tbgl_EntityGetZAxis LIB "TBGL.DLL" ALIAS "tbgl_EntityGetZAxis" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYREF x AS DOUBLE, BYREF y AS DOUBLE, BYREF z AS DOUBLE)
  DECLARE SUB tbgl_EntityGetYAxis LIB "TBGL.DLL" ALIAS "tbgl_EntityGetYAxis" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYREF x AS DOUBLE, BYREF y AS DOUBLE, BYREF z AS DOUBLE)
  DECLARE SUB tbgl_EntityGetXAxis LIB "TBGL.DLL" ALIAS "tbgl_EntityGetXAxis" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYREF x AS DOUBLE, BYREF y AS DOUBLE, BYREF z AS DOUBLE)
  DECLARE SUB tbgl_EntityGetMatrix LIB "TBGL.DLL" ALIAS "tbgl_EntityGetMatrix" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL matrix AS DOUBLE PTR, BYVAL elementCount AS LONG, BYVAL matrixType AS LONG)
  DECLARE SUB tbgl_EntitySetMatrix LIB "TBGL.DLL" ALIAS "tbgl_EntitySetMatrix" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL matrix AS DOUBLE PTR, BYVAL elementCount AS LONG, BYVAL matrixType AS LONG)
  DECLARE SUB tbgl_EntitySetXYAxis LIB "TBGL.DLL" ALIAS "tbgl_EntitySetXYAxis" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL xX AS DOUBLE, BYVAL xY AS DOUBLE, BYVAL xZ AS DOUBLE, BYVAL yX AS DOUBLE, BYVAL yY AS DOUBLE, BYVAL yZ AS DOUBLE)
  DECLARE SUB tbgl_EntitySetXZAxis LIB "TBGL.DLL" ALIAS "tbgl_EntitySetXZAxis" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL xX AS DOUBLE, BYVAL xY AS DOUBLE, BYVAL xZ AS DOUBLE, BYVAL zX AS DOUBLE, BYVAL zY AS DOUBLE, BYVAL zZ AS DOUBLE)
  DECLARE SUB tbgl_EntitySetYZAxis LIB "TBGL.DLL" ALIAS "tbgl_EntitySetYZAxis" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL yX AS DOUBLE, BYVAL yY AS DOUBLE, BYVAL yZ AS DOUBLE, BYVAL zX AS DOUBLE, BYVAL zY AS DOUBLE, BYVAL zZ AS DOUBLE)
  DECLARE SUB tbgl_EntitySyncAxes LIB "TBGL.DLL" ALIAS "tbgl_EntitySyncAxes" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL EntityID2 AS LONG, BYVAL GlobalCoordinates AS LONG)
  DECLARE FUNCTION tbgl_EntityGetAngleXZ LIB "TBGL.DLL" ALIAS "tbgl_EntityGetAngleXZ" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL EntityTargetID AS LONG, BYVAL EntityAxis AS LONG) AS DOUBLE
  DECLARE FUNCTION tbgl_EntityGetAngleXY LIB "TBGL.DLL" ALIAS "tbgl_EntityGetAngleXY" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL EntityTargetID AS LONG, BYVAL EntityAxis AS LONG) AS DOUBLE
  DECLARE FUNCTION tbgl_EntityGetAngleYZ LIB "TBGL.DLL" ALIAS "tbgl_EntityGetAngleYZ" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL EntityTargetID AS LONG, BYVAL EntityAxis AS LONG) AS DOUBLE
  DECLARE SUB tbgl_EntitySetTexture LIB "TBGL.DLL" ALIAS "tbgl_EntitySetTexture" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL slot AS LONG)
  DECLARE FUNCTION tbgl_EntityGetTexture LIB "TBGL.DLL" ALIAS "tbgl_EntityGetTexture" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG) AS LONG
  DECLARE SUB tbgl_EntityResetColor LIB "TBGL.DLL" ALIAS "tbgl_EntityResetColor" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG)
  DECLARE SUB tbgl_EntitySetColor LIB "TBGL.DLL" ALIAS "tbgl_EntitySetColor" (BYVAL ScriptSceneID AS LONG, BYVAL EntityID AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE)
  DECLARE FUNCTION tbgl_ErrorMessages LIB "TBGL.DLL" ALIAS "tbgl_ErrorMessages" (BYVAL displayMode AS LONG) AS LONG
  DECLARE FUNCTION tbgl_GetLastError LIB "TBGL.DLL" ALIAS "tbgl_GetLastError" () AS STRING
  DECLARE FUNCTION tbgl_GetLastGLError LIB "TBGL.DLL" ALIAS "tbgl_GetLastGLError" () AS STRING
  DECLARE SUB tbgl_SetupFog LIB "TBGL.DLL" ALIAS "tbgl_SetupFog" (BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE, BYVAL density AS SINGLE)
  DECLARE SUB tbgl_UseFog LIB "TBGL.DLL" ALIAS "tbgl_UseFog" (BYVAL flag AS LONG)
  DECLARE FUNCTION tbgl_FontHandle LIB "TBGL.DLL" ALIAS "tbgl_FontHandle" (BYVAL sFntName AS STRING, BYVAL fFntSize AS SINGLE, BYVAL lStyle AS LONG) AS DWORD
  DECLARE FUNCTION tbgl_BuildFont LIB "TBGL.DLL" ALIAS "tbgl_BuildFont" (BYVAL x AS DWORD, BYVAL SlotNumber AS LONG) AS LONG
  DECLARE SUB tbgl_SetActiveFont LIB "TBGL.DLL" ALIAS "tbgl_SetActiveFont" (BYVAL SlotNumber AS LONG)
  DECLARE SUB tbgl_PrintFont LIB "TBGL.DLL" ALIAS "tbgl_PrintFont" (BYVAL stext AS STRING, BYVAL x AS SINGLE, BYVAL y AS SINGLE)
  DECLARE SUB tbgl_PrintFont3D LIB "TBGL.DLL" ALIAS "tbgl_PrintFont3D" (BYVAL stext AS STRING, BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_PrintFont2D LIB "TBGL.DLL" ALIAS "tbgl_PrintFont2D" (BYVAL stext AS STRING, BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL screenAlignment AS LONG, BYVAL anchorAlignment AS LONG, BYVAL maximumWidth AS LONG)
  DECLARE SUB tbgl_GetFontTextSize LIB "TBGL.DLL" ALIAS "tbgl_GetFontTextSize" (BYVAL sText AS STRING, BYREF outX AS LONG, BYREF outY AS LONG)
  DECLARE SUB tbgl_KillFont LIB "TBGL.DLL" ALIAS "tbgl_KillFont" (BYVAL SlotNumber AS LONG)
  DECLARE SUB tbgl_LoadBMPFont LIB "TBGL.DLL" ALIAS "tbgl_LoadBMPFont" (BYVAL TextureName AS STRING, BYVAL LinesSuggested AS SINGLE, BYVAL SlotNumber AS LONG)
  DECLARE SUB tbgl_LoadBMPFont2D LIB "TBGL.DLL" ALIAS "tbgl_LoadBMPFont2D" (BYVAL TextureName AS STRING, BYVAL LinesSuggested AS SINGLE, BYVAL ColumnsSuggested AS SINGLE, BYVAL SlotNumber AS LONG)
  DECLARE SUB tbgl_BeginPrintBMP LIB "TBGL.DLL" ALIAS "tbgl_BeginPrintBMP" ()
  DECLARE SUB tbgl_EndPrintBMP LIB "TBGL.DLL" ALIAS "tbgl_EndPrintBMP" ()
  DECLARE SUB tbgl_PrintBMP LIB "TBGL.DLL" ALIAS "tbgl_PrintBMP" (BYVAL TXT AS STRING, BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL tAlign AS LONG)
  DECLARE SUB tbgl_SetActiveBMPFont LIB "TBGL.DLL" ALIAS "tbgl_SetActiveBMPFont" (BYVAL SlotNumber AS LONG)
  DECLARE SUB tbgl_ClearFrame LIB "TBGL.DLL" ALIAS "tbgl_ClearFrame" (BYVAL LFlags AS LONG)
  DECLARE SUB tbgl_DrawFrame LIB "TBGL.DLL" ALIAS "tbgl_DrawFrame" ()
  DECLARE SUB tbgl_RenderMatrix3D LIB "TBGL.DLL" ALIAS "tbgl_RenderMatrix3D" (BYVAL ratioType AS LONG, BYVAL customRatio AS DOUBLE)
  DECLARE SUB tbgl_RenderMatrix2D LIB "TBGL.DLL" ALIAS "tbgl_RenderMatrix2D" ()
  DECLARE SUB tbgl_RenderMatrix2DEx LIB "TBGL.DLL" ALIAS "tbgl_RenderMatrix2DEx" (byval minx as single, byval miny as single, byval maxx as single, byval maxy as single, byval mode2d as long)
  DECLARE FUNCTION tbgl_GetRenderMatrixMode LIB "TBGL.DLL" ALIAS "tbgl_GetRenderMatrixMode" () AS LONG
  DECLARE FUNCTION tbgl_UseVSync LIB "TBGL.DLL" ALIAS "tbgl_UseVSync" (BYVAL interval AS LONG) AS LONG
  DECLARE FUNCTION tbgl_GetVSyncMaxFramerate LIB "TBGL.DLL" ALIAS "tbgl_GetVSyncMaxFramerate" () AS LONG
  DECLARE FUNCTION tbgl_SaveScreenShot LIB "TBGL.DLL" ALIAS "tbgl_SaveScreenShot" (BYVAL sBmpFile AS STRING, BYVAL capturemode AS LONG) AS LONG
  DECLARE FUNCTION tbgl_GetWindowBMP LIB "TBGL.DLL" ALIAS "tbgl_GetWindowBMP" (BYVAL handleW AS DWORD, BYVAL captureMode AS LONG, BYREF bmpWidth AS LONG, BYREF bmpHeight AS LONG) AS STRING
  DECLARE SUB tbgl_SetDrawDistance LIB "TBGL.DLL" ALIAS "tbgl_SetDrawDistance" (BYVAL drawDistance AS LONG)
  DECLARE SUB tbgl_Viewport LIB "TBGL.DLL" ALIAS "tbgl_Viewport" (BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL xx AS SINGLE, BYVAL yy AS SINGLE, BYVAL vunits AS LONG)
  DECLARE FUNCTION tbgl_GetFrameRate LIB "TBGL.DLL" ALIAS "tbgl_GetFrameRate" () AS DOUBLE
  DECLARE FUNCTION tbgl_GetPixelInfoRGB LIB "TBGL.DLL" ALIAS "tbgl_GetPixelInfoRGB" (BYVAL x AS LONG, BYVAL y AS LONG, BYREF r AS BYTE, BYREF g AS BYTE, BYREF b AS BYTE) AS LONG
  DECLARE FUNCTION tbgl_GetPixelInfoXYZ LIB "TBGL.DLL" ALIAS "tbgl_GetPixelInfoXYZ" (BYVAL x AS LONG, BYVAL y AS LONG, BYREF dX AS DOUBLE, BYREF dY AS DOUBLE, BYREF dZ AS DOUBLE) AS LONG
  DECLARE FUNCTION tbgl_GBufferCreate LIB "TBGL.DLL" ALIAS "tbgl_GBufferCreate" (BYVAL gType AS LONG, BYVAL gDims AS LONG) AS LONG
  DECLARE FUNCTION tbgl_GBufferDestroy LIB "TBGL.DLL" ALIAS "tbgl_GBufferDestroy" (BYVAL gbHandle AS LONG) AS LONG
  DECLARE FUNCTION tbgl_GBufferDefineFromArray LIB "TBGL.DLL" ALIAS "tbgl_GBufferDefineFromArray" ( _
      BYVAL gbHandle AS LONG _
    , BYVAL gKind AS LONG _
    , BYVAL items AS LONG _
    , BYVAL pVertices AS DWORD _
    , BYVAL pColors AS DWORD _
    , BYVAL colorElementSize AS LONG _
    , BYVAL pNormals AS DWORD _
    , BYVAL pTexCoords1 AS DWORD _
    , BYVAL pTexCoords2 AS DWORD _
    , BYVAL pTexCoords3 AS DWORD _
    , BYVAL pTexCoords4 AS DWORD _
    ) AS LONG
  DECLARE FUNCTION tbgl_GBufferRender LIB "TBGL.DLL" ALIAS "tbgl_GBufferRender" (BYVAL gbHandle AS LONG) AS LONG
  DECLARE SUB tbgl_PointSize LIB "TBGL.DLL" ALIAS "tbgl_PointSize" (BYVAL pointSize AS SINGLE)
  DECLARE SUB tbgl_LineWidth LIB "TBGL.DLL" ALIAS "tbgl_LineWidth" (BYVAL lineWidth AS SINGLE)
  DECLARE SUB tbgl_LineStipple LIB "TBGL.DLL" ALIAS "tbgl_LineStipple" (BYVAL factor AS LONG, BYVAL pattern AS WORD)
  DECLARE SUB tbgl_UseLineStipple LIB "TBGL.DLL" ALIAS "tbgl_UseLineStipple" (BYVAL flag AS LONG)
  DECLARE SUB tbgl_PolygonLook LIB "TBGL.DLL" ALIAS "tbgl_PolygonLook" (BYVAL polygonMode AS DWORD)
  DECLARE SUB tbgl_BeginPoly LIB "TBGL.DLL" ALIAS "tbgl_BeginPoly" (BYVAL polyType AS DWORD)
  DECLARE SUB tbgl_EndPoly LIB "TBGL.DLL" ALIAS "tbgl_EndPoly" ()
  DECLARE SUB tbgl_Vertex LIB "TBGL.DLL" ALIAS "tbgl_Vertex" (BYVAL x AS SINGLE, BYVAL y AS SINGLE)
  DECLARE SUB tbgl_Vertex3D LIB "TBGL.DLL" ALIAS "tbgl_Vertex3D" (BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_ShowCursor LIB "TBGL.DLL" ALIAS "tbgl_ShowCursor" (BYVAL flag AS LONG)
  DECLARE FUNCTION tbgl_MouseInClient LIB "TBGL.DLL" ALIAS "tbgl_MouseInClient" () AS LONG
  DECLARE FUNCTION tbgl_MouseGetPosX LIB "TBGL.DLL" ALIAS "tbgl_MouseGetPosX" (byval coordinateMode as long) AS SINGLE
  DECLARE FUNCTION tbgl_MouseGetPosY LIB "TBGL.DLL" ALIAS "tbgl_MouseGetPosY" (byval coordinateMode as long) AS SINGLE
  DECLARE FUNCTION tbgl_MouseGetLButton LIB "TBGL.DLL" ALIAS "tbgl_MouseGetLButton" () AS LONG
  DECLARE FUNCTION tbgl_MouseGetRButton LIB "TBGL.DLL" ALIAS "tbgl_MouseGetRButton" () AS LONG
  DECLARE FUNCTION tbgl_MouseGetMButton LIB "TBGL.DLL" ALIAS "tbgl_MouseGetMButton" () AS LONG
  DECLARE FUNCTION tbgl_MouseGetWheelDelta LIB "TBGL.DLL" ALIAS "tbgl_MouseGetWheelDelta" () AS LONG
  DECLARE FUNCTION tbgl_CenterCursor LIB "TBGL.DLL" ALIAS "tbgl_CenterCursor" () AS LONG
  DECLARE SUB tbgl_ResetKeyState LIB "TBGL.DLL" ALIAS "tbgl_ResetKeyState" ()
  DECLARE FUNCTION tbgl_GetAsyncKeyState LIB "TBGL.DLL" ALIAS "tbgl_GetAsyncKeyState" (byval eKey as long) AS LONG
  DECLARE FUNCTION tbgl_GetWindowKeyState LIB "TBGL.DLL" ALIAS "tbgl_GetWindowKeyState" (byval ehWnd as dword, byval eKey as long) AS LONG
  DECLARE FUNCTION tbgl_GetWindowKeyOnce LIB "TBGL.DLL" ALIAS "tbgl_GetWindowKeyOnce" (byval ehWnd as dword, byval eKey as long) AS LONG
  DECLARE FUNCTION tbgl_GetMultiAsyncKeyState LIB "TBGL.DLL" ALIAS "tbgl_GetMultiAsyncKeyState" (byval keys as long ptr, byval keyCount as long) AS LONG
  DECLARE FUNCTION tbgl_GetWindowMultiKeyState LIB "TBGL.DLL" ALIAS "tbgl_GetWindowMultiKeyState" (byval ehWnd as dword, byval keys as long ptr, byval keyCount as long) AS LONG
  DECLARE FUNCTION tbgl_GetWindowAnyKeyState LIB "TBGL.DLL" ALIAS "tbgl_GetWindowAnyKeyState" (byval ehWnd as dword, byval keys as long ptr, byval keyCount as long) AS LONG
  DECLARE FUNCTION tbgl_GetKeysPressed LIB "TBGL.DLL" ALIAS "tbgl_GetKeysPressed" (byval hWnd as dword, byval pressedKeysPtr as long ptr, byval totalElements as long) AS LONG
  DECLARE SUB tbgl_Normal LIB "TBGL.DLL" ALIAS "tbgl_Normal" (BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_SetLightParameter LIB "TBGL.DLL" ALIAS "tbgl_SetLightParameter" (BYVAL lightId AS DWORD, BYVAL whatComponent AS DWORD, BYVAL x1 AS SINGLE, BYVAL x2 AS SINGLE, BYVAL x3 AS SINGLE, BYVAL x4 AS SINGLE)
  DECLARE SUB tbgl_UseLighting LIB "TBGL.DLL" ALIAS "tbgl_UseLighting" (BYVAL flag AS LONG)
  DECLARE SUB tbgl_UseLightSource LIB "TBGL.DLL" ALIAS "tbgl_UseLightSource" (BYVAL lightId AS DWORD, BYVAL flag AS LONG)
  DECLARE FUNCTION tbgl_GetUseLighting LIB "TBGL.DLL" ALIAS "tbgl_GetUseLighting" () AS LONG
  DECLARE SUB tbgl_SetupLightSource LIB "TBGL.DLL" ALIAS "tbgl_SetupLightSource" (BYVAL lightId AS DWORD, BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL diff AS BYTE)
  DECLARE FUNCTION tbgl_oglExtensionSupport LIB "TBGL.DLL" ALIAS "tbgl_oglExtensionSupport" (BYVAL Extensions AS STRING) AS LONG
  DECLARE FUNCTION tbgl_oglVersionSupport LIB "TBGL.DLL" ALIAS "tbgl_oglVersionSupport" (BYVAL Major AS LONG, BYVAL Minor AS LONG) AS LONG
  DECLARE FUNCTION tbgl_oglVersion LIB "TBGL.DLL" ALIAS "tbgl_oglVersion" (BYREF Major AS LONG, BYREF Minor AS LONG) AS STRING
  DECLARE FUNCTION tbgl_oglRenderer LIB "TBGL.DLL" ALIAS "tbgl_oglRenderer" () AS STRING
  DECLARE FUNCTION tbgl_oglRenderContext LIB "TBGL.DLL" ALIAS "tbgl_oglRenderContext" () AS DWORD
  DECLARE FUNCTION tbgl_oglDeviceContext LIB "TBGL.DLL" ALIAS "tbgl_oglDeviceContext" () AS DWORD
  DECLARE FUNCTION tbgl_oglIsAccelerated LIB "TBGL.DLL" ALIAS "tbgl_oglIsAccelerated" () AS LONG
  DECLARE FUNCTION tbgl_GetProcAddress LIB "TBGL.DLL" ALIAS "tbgl_GetProcAddress" (BYVAL Extension AS STRING) AS DWORD
  DECLARE SUB tbgl_PushStateProtect LIB "TBGL.DLL" ALIAS "tbgl_PushStateProtect" (BYVAL bitMask AS LONG)
  DECLARE SUB tbgl_PopStateProtect LIB "TBGL.DLL" ALIAS "tbgl_PopStateProtect" ()
  DECLARE SUB tbgl_PushState LIB "TBGL.DLL" ALIAS "tbgl_PushState" (BYVAL bitMask AS LONG)
  DECLARE SUB tbgl_PopState LIB "TBGL.DLL" ALIAS "tbgl_PopState" ()
  DECLARE SUB tbgl_PushLineWidth LIB "TBGL.DLL" ALIAS "tbgl_PushLineWidth" (BYVAL lineWidth AS SINGLE)
  DECLARE SUB tbgl_PopLineWidth LIB "TBGL.DLL" ALIAS "tbgl_PopLineWidth" ()
  DECLARE SUB tbgl_PushLineStipple LIB "TBGL.DLL" ALIAS "tbgl_PushLineStipple" (BYVAL factor AS LONG, BYVAL pattern AS WORD)
  DECLARE SUB tbgl_PopLineStipple LIB "TBGL.DLL" ALIAS "tbgl_PopLineStipple" ()
  DECLARE SUB tbgl_PushPointSize LIB "TBGL.DLL" ALIAS "tbgl_PushPointSize" (BYVAL pointSize AS SINGLE)
  DECLARE SUB tbgl_PopPointSize LIB "TBGL.DLL" ALIAS "tbgl_PopPointSize" ()
  DECLARE SUB tbgl_PushColor LIB "TBGL.DLL" ALIAS "tbgl_PushColor" (BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE)
  DECLARE SUB tbgl_PopColor LIB "TBGL.DLL" ALIAS "tbgl_PopColor" ()
  DECLARE SUB tbgl_PushPolygonLook LIB "TBGL.DLL" ALIAS "tbgl_PushPolygonLook" (BYVAL polygonMode AS DWORD)
  DECLARE SUB tbgl_PopPolygonLook LIB "TBGL.DLL" ALIAS "tbgl_PopPolygonLook" ()
  DECLARE FUNCTION tbgl_NewMaterial LIB "TBGL.DLL" ALIAS "tbgl_NewMaterial" () AS LONG
  DECLARE SUB tbgl_DeleteMaterial LIB "TBGL.DLL" ALIAS "tbgl_DeleteMaterial" (BYVAL mHandle AS LONG)
  DECLARE SUB tbgl_PushMaterial LIB "TBGL.DLL" ALIAS "tbgl_PushMaterial" (BYVAL mHandle AS LONG)
  DECLARE SUB tbgl_PopMaterial LIB "TBGL.DLL" ALIAS "tbgl_PopMaterial" (BYVAL mHandle AS LONG)
  DECLARE SUB tbgl_SetMaterialDiffuse LIB "TBGL.DLL" ALIAS "tbgl_SetMaterialDiffuse" (BYVAL mHandle AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE)
  DECLARE SUB tbgl_SetMaterialAmbient LIB "TBGL.DLL" ALIAS "tbgl_SetMaterialAmbient" (BYVAL mHandle AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE)
  DECLARE SUB tbgl_SetMaterialSpecular LIB "TBGL.DLL" ALIAS "tbgl_SetMaterialSpecular" (BYVAL mHandle AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE)
  DECLARE SUB tbgl_SetMaterialSpecularExponent LIB "TBGL.DLL" ALIAS "tbgl_SetMaterialSpecularExponent" (BYVAL mHandle AS LONG, BYVAL e AS SINGLE)
  DECLARE SUB tbgl_SetMaterialEmission LIB "TBGL.DLL" ALIAS "tbgl_SetMaterialEmission" (BYVAL mHandle AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE)
  DECLARE SUB tbgl_SetMaterialTexture LIB "TBGL.DLL" ALIAS "tbgl_SetMaterialTexture" (BYVAL mHandle AS LONG, BYVAL textureID AS LONG)
  DECLARE FUNCTION tbgl_m15InitModelBuffers LIB "TBGL.DLL" ALIAS "tbgl_m15InitModelBuffers" (BYVAL n AS LONG, BYVAL n2 AS LONG) AS LONG
  DECLARE SUB tbgl_m15SetDefaultTextureFilter LIB "TBGL.DLL" ALIAS "tbgl_m15SetDefaultTextureFilter" (BYVAL eFilter AS LONG, BYVAL eAniso AS SINGLE)
  DECLARE FUNCTION tbgl_m15LoadModel LIB "TBGL.DLL" ALIAS "tbgl_m15LoadModel" (BYVAL FileName AS STRING, BYVAL TextureDirectory AS STRING, BYVAL Model AS LONG, BYVAL MakeList AS LONG, BYVAL NormalMethod AS LONG) AS LONG
  DECLARE SUB tbgl_m15DrawModel LIB "TBGL.DLL" ALIAS "tbgl_m15DrawModel" (BYVAL ModelID AS LONG)
  DECLARE SUB tbgl_m15RecalcNormals LIB "TBGL.DLL" ALIAS "tbgl_m15RecalcNormals" (BYVAL Model AS LONG, BYVAL nType AS LONG, BYVAL Layer AS STRING)
  DECLARE SUB tbgl_m15ClearModel LIB "TBGL.DLL" ALIAS "tbgl_m15ClearModel" (BYVAL Model AS LONG)
  DECLARE FUNCTION tbgl_m15GetModelVertexcount LIB "TBGL.DLL" ALIAS "tbgl_m15GetModelVertexcount" (BYVAL Model AS LONG) AS LONG
  DECLARE SUB tbgl_m15SetModelVertexcount LIB "TBGL.DLL" ALIAS "tbgl_m15SetModelVertexcount" (BYVAL Model AS LONG, BYVAL vertexCount AS LONG)
  DECLARE FUNCTION tbgl_m15GetModelPolycount LIB "TBGL.DLL" ALIAS "tbgl_m15GetModelPolycount" (BYVAL Model AS LONG) AS LONG
  DECLARE SUB tbgl_m15GetModelDimensions LIB "TBGL.DLL" ALIAS "tbgl_m15GetModelDimensions" (BYVAL Model AS LONG, BYREF sizeX AS SINGLE, BYREF sizeY AS SINGLE, BYREF sizeZ AS SINGLE)
  DECLARE SUB tbgl_m15SetVertexParam LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexParam" (BYVAL Model AS LONG, BYVAL i AS LONG, BYVAL lngParam AS LONG, BYVAL value AS SINGLE)
  DECLARE FUNCTION tbgl_m15GetVertexParam LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexParam" (BYVAL Model AS LONG, BYVAL i AS LONG, BYVAL lngParam AS LONG) AS SINGLE
  DECLARE SUB tbgl_m15SetVertexX LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexX" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS SINGLE)
  DECLARE FUNCTION tbgl_m15GetVertexX LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexX" (BYVAL Model AS LONG, BYVAL index AS LONG) AS SINGLE
  DECLARE SUB tbgl_m15SetVertexY LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexY" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS SINGLE)
  DECLARE FUNCTION tbgl_m15GetVertexY LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexY" (BYVAL Model AS LONG, BYVAL index AS LONG) AS SINGLE
  DECLARE SUB tbgl_m15SetVertexZ LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexZ" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS SINGLE)
  DECLARE FUNCTION tbgl_m15GetVertexZ LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexZ" (BYVAL Model AS LONG, BYVAL index AS LONG) AS SINGLE
  DECLARE SUB tbgl_m15SetVertexPStop LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexPStop" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS BYTE)
  DECLARE FUNCTION tbgl_m15GetVertexPStop LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexPStop" (BYVAL Model AS LONG, BYVAL index AS LONG) AS BYTE
  DECLARE SUB tbgl_m15SetVertexR LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexR" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS BYTE)
  DECLARE FUNCTION tbgl_m15GetVertexR LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexR" (BYVAL Model AS LONG, BYVAL index AS LONG) AS BYTE
  DECLARE SUB tbgl_m15SetVertexG LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexG" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS BYTE)
  DECLARE FUNCTION tbgl_m15GetVertexG LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexG" (BYVAL Model AS LONG, BYVAL index AS LONG) AS BYTE
  DECLARE SUB tbgl_m15SetVertexB LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexB" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS BYTE)
  DECLARE FUNCTION tbgl_m15GetVertexB LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexB" (BYVAL Model AS LONG, BYVAL index AS LONG) AS BYTE
  DECLARE SUB tbgl_m15SetVertexTexN LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexTexN" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS LONG)
  DECLARE FUNCTION tbgl_m15GetVertexTexN LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexTexN" (BYVAL Model AS LONG, BYVAL index AS LONG) AS LONG
  DECLARE SUB tbgl_m15SetVertexTexX LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexTexX" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS SINGLE)
  DECLARE FUNCTION tbgl_m15GetVertexTexX LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexTexX" (BYVAL Model AS LONG, BYVAL index AS LONG) AS SINGLE
  DECLARE SUB tbgl_m15SetVertexTexY LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexTexY" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS SINGLE)
  DECLARE FUNCTION tbgl_m15GetVertexTexY LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexTexY" (BYVAL Model AS LONG, BYVAL index AS LONG) AS SINGLE
  DECLARE SUB tbgl_m15SetVertexLayer LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexLayer" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL value AS LONG)
  DECLARE FUNCTION tbgl_m15GetVertexLayer LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexLayer" (BYVAL Model AS LONG, BYVAL index AS LONG) AS LONG
  DECLARE SUB tbgl_m15SetVertexXYZ LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexXYZ" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_m15GetVertexXYZ LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexXYZ" (BYVAL Model AS LONG, BYVAL index AS LONG, BYREF x AS SINGLE, BYREF y AS SINGLE, BYREF z AS SINGLE)
  DECLARE SUB tbgl_m15SetVertexRGB LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexRGB" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE)
  DECLARE SUB tbgl_m15GetVertexRGB LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexRGB" (BYVAL Model AS LONG, BYVAL index AS LONG, BYREF r AS BYTE, BYREF g AS BYTE, BYREF b AS BYTE)
  DECLARE SUB tbgl_m15SetVertexTexXY LIB "TBGL.DLL" ALIAS "tbgl_m15SetVertexTexXY" (BYVAL Model AS LONG, BYVAL fromIndex AS LONG, BYVAL toIndex AS LONG, BYVAL u AS SINGLE, BYVAL v AS SINGLE)
  DECLARE SUB tbgl_m15GetVertexTexXY LIB "TBGL.DLL" ALIAS "tbgl_m15GetVertexTexXY" (BYVAL Model AS LONG, BYVAL index AS LONG, BYREF u AS SINGLE, BYREF v AS SINGLE)
  DECLARE SUB tbgl_m15DefBoneBox LIB "TBGL.DLL" ALIAS "tbgl_m15DefBoneBox" ( _
      BYVAL Model AS LONG _
    , BYVAL ID AS LONG _
    , BYVAL minx AS SINGLE _
    , BYVAL maxx AS SINGLE _
    , BYVAL miny AS SINGLE _
    , BYVAL maxy AS SINGLE _
    , BYVAL minz AS SINGLE _
    , BYVAL maxz AS SINGLE _
    , BYVAL anchorX AS SINGLE _
    , BYVAL anchorY AS SINGLE _
    , BYVAL anchorZ AS SINGLE _
    )
  DECLARE SUB tbgl_m15DefBoneLayer LIB "TBGL.DLL" ALIAS "tbgl_m15DefBoneLayer" (BYVAL Model AS LONG, BYVAL ID AS LONG, BYVAL layer AS STRING, BYVAL anchorX AS SINGLE, BYVAL anchorY AS SINGLE, BYVAL anchorZ AS SINGLE)
  DECLARE SUB tbgl_m15DefBoneColor LIB "TBGL.DLL" ALIAS "tbgl_m15DefBoneColor" (BYVAL Model AS LONG, BYVAL ID AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL anchorX AS SINGLE, BYVAL anchorY AS SINGLE, BYVAL anchorZ AS SINGLE)
  DECLARE SUB tbgl_m15DefBoneAddVertex LIB "TBGL.DLL" ALIAS "tbgl_m15DefBoneAddVertex" (BYVAL Model AS LONG, BYVAL ID AS LONG, BYVAL vertexID AS LONG)
  DECLARE SUB tbgl_m15DefBoneAnchor LIB "TBGL.DLL" ALIAS "tbgl_m15DefBoneAnchor" (BYVAL Model AS LONG, BYVAL ID AS LONG, BYVAL anchorX AS SINGLE, BYVAL anchorY AS SINGLE, BYVAL anchorZ AS SINGLE)
  DECLARE SUB tbgl_m15DefBoneReset LIB "TBGL.DLL" ALIAS "tbgl_m15DefBoneReset" (BYVAL Model AS LONG, BYVAL Bone AS LONG)
  DECLARE SUB tbgl_m15DefBoneEmpty LIB "TBGL.DLL" ALIAS "tbgl_m15DefBoneEmpty" (BYVAL Model AS LONG, BYVAL Bone AS LONG)
  DECLARE SUB tbgl_m15EraseChildbones LIB "TBGL.DLL" ALIAS "tbgl_m15EraseChildbones" (BYVAL Model AS LONG, BYVAL Bone AS LONG)
  DECLARE SUB tbgl_m15SetBoneChild LIB "TBGL.DLL" ALIAS "tbgl_m15SetBoneChild" (BYVAL Model AS LONG, BYVAL Bone AS LONG, BYVAL ChildBone AS BYTE)
  DECLARE FUNCTION tbgl_m15GetBoneChild LIB "TBGL.DLL" ALIAS "tbgl_m15GetBoneChild" (BYVAL Model AS LONG, BYVAL Bone AS LONG, BYVAL ChildNumber AS LONG) AS BYTE
  DECLARE FUNCTION tbgl_m15GetBoneChildCount LIB "TBGL.DLL" ALIAS "tbgl_m15GetBoneChildCount" (BYVAL Model AS LONG, BYVAL Bone AS LONG) AS BYTE
  DECLARE SUB tbgl_m15ResetBones LIB "TBGL.DLL" ALIAS "tbgl_m15ResetBones" (BYVAL Model AS LONG)
  DECLARE SUB tbgl_m15RotBone LIB "TBGL.DLL" ALIAS "tbgl_m15RotBone" (BYVAL Model AS LONG, BYVAL Bone AS LONG, BYVAL nanglex AS SINGLE, BYVAL nangley AS SINGLE, BYVAL nanglez AS SINGLE)
  DECLARE SUB tbgl_m15RotBoneX LIB "TBGL.DLL" ALIAS "tbgl_m15RotBoneX" (BYVAL Model AS LONG, BYVAL Bone AS LONG, BYVAL nangle AS SINGLE)
  DECLARE SUB tbgl_m15RotBoneY LIB "TBGL.DLL" ALIAS "tbgl_m15RotBoneY" (BYVAL Model AS LONG, BYVAL Bone AS LONG, BYVAL nangle AS SINGLE)
  DECLARE SUB tbgl_m15RotBoneZ LIB "TBGL.DLL" ALIAS "tbgl_m15RotBoneZ" (BYVAL Model AS LONG, BYVAL Bone AS LONG, BYVAL nangle AS SINGLE)
  DECLARE SUB tbgl_m15ApplyBones LIB "TBGL.DLL" ALIAS "tbgl_m15ApplyBones" (BYVAL Model AS LONG)
  DECLARE SUB tbgl_m15AddBoneTreeItem LIB "TBGL.DLL" ALIAS "tbgl_m15AddBoneTreeItem" (BYVAL Model AS LONG, BYVAL Bone AS BYTE, BYVAL Level AS BYTE, BYVAL ChildLevel AS BYTE)
  DECLARE SUB tbgl_SetPrimitiveQuality LIB "TBGL.DLL" ALIAS "tbgl_SetPrimitiveQuality" (BYVAL quality AS LONG)
  DECLARE SUB tbgl_Box LIB "TBGL.DLL" ALIAS "tbgl_Box" (BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_Cylinder LIB "TBGL.DLL" ALIAS "tbgl_Cylinder" (BYVAL lowR AS SINGLE, BYVAL hiR AS SINGLE, BYVAL height AS SINGLE)
  DECLARE SUB tbgl_Sphere LIB "TBGL.DLL" ALIAS "tbgl_Sphere" (BYVAL radius AS SINGLE)
  DECLARE SUB tbgl_Torus LIB "TBGL.DLL" ALIAS "tbgl_Torus" (BYVAL minR AS SINGLE, BYVAL maxR AS SINGLE)
  DECLARE SUB tbgl_Rect LIB "TBGL.DLL" ALIAS "tbgl_Rect" (BYVAL a AS SINGLE, BYVAL b AS SINGLE, BYVAL c AS SINGLE, BYVAL d AS SINGLE)
  DECLARE SUB tbgl_Rect3D LIB "TBGL.DLL" ALIAS "tbgl_Rect3D" (BYVAL a AS SINGLE, BYVAL b AS SINGLE, BYVAL c AS SINGLE, BYVAL d AS SINGLE, BYVAL e AS SINGLE, BYVAL f AS SINGLE, BYVAL planeAligned AS LONG)
  DECLARE SUB tbgl_Line LIB "TBGL.DLL" ALIAS "tbgl_Line" (BYVAL a AS SINGLE, BYVAL b AS SINGLE, BYVAL c AS SINGLE, BYVAL d AS SINGLE)
  DECLARE SUB tbgl_Line3D LIB "TBGL.DLL" ALIAS "tbgl_Line3D" (BYVAL a AS SINGLE, BYVAL b AS SINGLE, BYVAL c AS SINGLE, BYVAL d AS SINGLE, BYVAL e AS SINGLE, BYVAL f AS SINGLE)
  DECLARE SUB tbgl_Point LIB "TBGL.DLL" ALIAS "tbgl_Point" (BYVAL x AS SINGLE, BYVAL y AS SINGLE)
  DECLARE SUB tbgl_Point3D LIB "TBGL.DLL" ALIAS "tbgl_Point3D" (BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_NGon LIB "TBGL.DLL" ALIAS "tbgl_NGon" (BYVAL ex AS SINGLE, BYVAL ey AS SINGLE, BYVAL radius AS SINGLE, BYVAL sides AS LONG)
  DECLARE SUB tbgl_NGon3D LIB "TBGL.DLL" ALIAS "tbgl_NGon3D" (BYVAL ex AS SINGLE, BYVAL ey AS SINGLE, BYVAL z AS SINGLE, BYVAL radius AS SINGLE, BYVAL sides AS LONG, BYVAL angleDeg AS SINGLE)
  DECLARE FUNCTION tbgl_SpriteGetSpeedAngle LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetSpeedAngle" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetSpeed LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetSpeed" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteLoad LIB "TBGL.DLL" ALIAS "tbgl_SpriteLoad" (BYVAL TexName AS STRING, BYVAL lRGB AS LONG) AS DWORD
  DECLARE FUNCTION tbgl_SpriteCreate LIB "TBGL.DLL" ALIAS "tbgl_SpriteCreate" (BYVAL etex AS LONG) AS DWORD
  DECLARE FUNCTION tbgl_SpriteCreateAnim LIB "TBGL.DLL" ALIAS "tbgl_SpriteCreateAnim" (BYVAL etex AS LONG, BYVAL ecw AS LONG, BYVAL ech AS LONG) AS DWORD
  DECLARE FUNCTION tbgl_SpriteLoadAnim LIB "TBGL.DLL" ALIAS "tbgl_SpriteLoadAnim" (BYVAL TexName AS STRING, BYVAL ecw AS LONG, BYVAL ech AS LONG, BYVAL lRGB AS LONG) AS DWORD
  DECLARE FUNCTION tbgl_SpriteLoadSheet LIB "TBGL.DLL" ALIAS "tbgl_SpriteLoadSheet" (BYVAL TexName AS STRING, BYVAL cw AS LONG, BYVAL ch AS LONG, BYVAL lRGB AS LONG, BYVAL spriteIDs AS DWORD PTR, BYVAL maxCount AS LONG) AS LONG
  DECLARE FUNCTION tbgl_SpriteCreateSheet LIB "TBGL.DLL" ALIAS "tbgl_SpriteCreateSheet" (BYVAL etex AS LONG, BYVAL cw AS LONG, BYVAL ch AS LONG, BYVAL spriteIDs AS DWORD PTR, BYVAL maxCount AS LONG) AS LONG
  DECLARE FUNCTION tbgl_SpritesCount LIB "TBGL.DLL" ALIAS "tbgl_SpritesCount" () AS LONG
  DECLARE FUNCTION tbgl_SpritesCountGroup LIB "TBGL.DLL" ALIAS "tbgl_SpritesCountGroup" (BYVAL group AS LONG) AS LONG
  DECLARE FUNCTION tbgl_SpriteCopy LIB "TBGL.DLL" ALIAS "tbgl_SpriteCopy" (BYVAL pSprite AS DWORD) AS DWORD
  DECLARE SUB tbgl_SpriteDelete LIB "TBGL.DLL" ALIAS "tbgl_SpriteDelete" (BYVAL pSprite AS DWORD)
  DECLARE SUB tbgl_SpritesDeleteAll LIB "TBGL.DLL" ALIAS "tbgl_SpritesDeleteAll" ()
  DECLARE SUB tbgl_SpritesDeleteGroup LIB "TBGL.DLL" ALIAS "tbgl_SpritesDeleteGroup" (BYVAL igroup AS LONG)
  DECLARE SUB tbgl_SpriteAddSpin LIB "TBGL.DLL" ALIAS "tbgl_SpriteAddSpin" (BYVAL pSprite AS DWORD, BYVAL spinSpeed AS SINGLE)
  DECLARE SUB tbgl_SpriteSetShadow LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetShadow" (BYVAL pSprite AS DWORD, BYVAL eAlpha AS SINGLE, BYVAL eXOff AS SINGLE, BYVAL eYOff AS SINGLE, BYVAL eSize AS SINGLE)
  DECLARE SUB tbgl_SpriteSetAlpha LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetAlpha" (BYVAL pSprite AS DWORD, BYVAL eAlpha AS SINGLE)
  DECLARE SUB tbgl_SpriteSetColor LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetColor" (BYVAL pSprite AS DWORD, BYVAL eRed AS BYTE, BYVAL eGreen AS BYTE, BYVAL eBlue AS BYTE)
  DECLARE SUB tbgl_SpriteSetSpin LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetSpin" (BYVAL pSprite AS DWORD, BYVAL spinSpeed AS SINGLE)
  DECLARE SUB tbgl_SpriteSetAngle LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetAngle" (BYVAL pSprite AS DWORD, BYVAL eangle AS SINGLE, BYVAL erelative AS LONG)
  DECLARE SUB tbgl_SpriteSetTexCoord LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetTexCoord" (BYVAL pSprite AS DWORD, BYVAL eframe AS LONG, BYVAL ex1 AS SINGLE, BYVAL ey1 AS SINGLE, BYVAL ex2 AS SINGLE, BYVAL ey2 AS SINGLE)
  DECLARE SUB tbgl_SpriteSetBaseSize LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetBaseSize" (BYVAL pSprite AS DWORD, BYVAL ewidth AS LONG, BYVAL eheight AS LONG)
  DECLARE SUB tbgl_SpriteSetTexFrame LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetTexFrame" (BYVAL pSprite AS DWORD, BYVAL eframe AS LONG)
  DECLARE SUB tbgl_SpriteSetTexture LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetTexture" (BYVAL pSprite AS DWORD, BYVAL etex AS LONG)
  DECLARE FUNCTION tbgl_SpriteGetTexture LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetTexture" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetTexFrame LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetTexFrame" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE SUB tbgl_SpriteUpdate LIB "TBGL.DLL" ALIAS "tbgl_SpriteUpdate" (BYVAL pSprite AS DWORD, BYVAL SPEED AS SINGLE)
  DECLARE SUB tbgl_SpritesAnimateAll LIB "TBGL.DLL" ALIAS "tbgl_SpritesAnimateAll" (BYVAL SPEED AS SINGLE)
  DECLARE FUNCTION tbgl_SpriteGetID LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetID" (BYVAL index AS LONG) AS DWORD
  DECLARE FUNCTION tbgl_SpriteGetIDInGroup LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetIDInGroup" (BYVAL index AS LONG, BYVAL igroup AS LONG) AS DWORD
  DECLARE SUB tbgl_SpritesUpdateAll LIB "TBGL.DLL" ALIAS "tbgl_SpritesUpdateAll" (BYVAL SPEED AS SINGLE)
  DECLARE SUB tbgl_SpritesDrawAll LIB "TBGL.DLL" ALIAS "tbgl_SpritesDrawAll" ()
  DECLARE SUB tbgl_SpritesDrawGroup LIB "TBGL.DLL" ALIAS "tbgl_SpritesDrawGroup" (BYVAL igroup AS LONG)
  DECLARE SUB tbgl_SpriteSlide LIB "TBGL.DLL" ALIAS "tbgl_SpriteSlide" (BYVAL pSprite AS DWORD, BYVAL ex AS SINGLE, BYVAL ey AS SINGLE)
  DECLARE SUB tbgl_SpriteAddSpeedXY LIB "TBGL.DLL" ALIAS "tbgl_SpriteAddSpeedXY" (BYVAL pSprite AS DWORD, BYVAL ex AS SINGLE, BYVAL ey AS SINGLE, BYVAL eflag AS LONG)
  DECLARE SUB tbgl_SpriteAddSpeed LIB "TBGL.DLL" ALIAS "tbgl_SpriteAddSpeed" (BYVAL pSprite AS DWORD, BYVAL espeed AS SINGLE, BYVAL eangle AS SINGLE)
  DECLARE SUB tbgl_SpriteSetSpeed LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetSpeed" (BYVAL pSprite AS DWORD, BYVAL espeed AS SINGLE, BYVAL eangle AS SINGLE)
  DECLARE FUNCTION tbgl_SpriteGetTargetAngle LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetTargetAngle" (BYVAL pSprite AS DWORD, BYVAL pSpriteTarget AS DWORD, BYVAL eflag AS LONG) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetVectorAngle LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetVectorAngle" (BYVAL pSprite AS DWORD, BYVAL xpos AS SINGLE, BYVAL ypos AS SINGLE, BYVAL eflag AS LONG) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetTargetDist LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetTargetDist" (BYVAL pSprite AS DWORD, BYVAL pSpriteTarget AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetVectorDist LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetVectorDist" (BYVAL pSprite AS DWORD, BYVAL xpos AS SINGLE, BYVAL ypos AS SINGLE) AS SINGLE
  DECLARE SUB tbgl_SpriteSetSpeedAngle LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetSpeedAngle" (BYVAL pSprite AS DWORD, BYVAL eangle AS SINGLE)
  DECLARE SUB tbgl_SpriteSetSpeedXY LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetSpeedXY" (BYVAL pSprite AS DWORD, BYVAL ex AS SINGLE, BYVAL ey AS SINGLE, BYVAL eflag AS LONG)
  DECLARE SUB tbgl_SpriteSetPos LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetPos" (BYVAL pSprite AS DWORD, BYVAL ex AS SINGLE, BYVAL ey AS SINGLE, BYVAL erelative AS LONG)
  DECLARE SUB tbgl_SpriteSetTag LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetTag" (BYVAL pSprite AS DWORD, BYVAL eTag AS LONG)
  DECLARE SUB tbgl_SpriteSetGroup LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetGroup" (BYVAL pSprite AS DWORD, BYVAL egroup AS LONG)
  DECLARE FUNCTION tbgl_SpriteGetTag LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetTag" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetGroup LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetGroup" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE SUB tbgl_SpriteSetParent LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetParent" (BYVAL pSprite AS DWORD, BYVAL eparent AS LONG)
  DECLARE SUB tbgl_SpriteSetParentMode LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetParentMode" (BYVAL pSprite AS DWORD, BYVAL emode AS LONG)
  DECLARE FUNCTION tbgl_SpriteGetParent LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetParent" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetChildCount LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetChildCount" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetChildID LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetChildID" (BYVAL pSprite AS DWORD, BYVAL index AS LONG) AS DWORD
  DECLARE SUB tbgl_SpriteSetScale LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetScale" (BYVAL pSprite AS DWORD, BYVAL eScale AS SINGLE, BYVAL erelative AS LONG)
  DECLARE SUB tbgl_SpriteSetHFlip LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetHFlip" (BYVAL pSprite AS DWORD, BYVAL eflag AS LONG)
  DECLARE SUB tbgl_SpriteSetVFlip LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetVFlip" (BYVAL pSprite AS DWORD, BYVAL eflag AS LONG)
  DECLARE SUB tbgl_SpriteSetLayer LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetLayer" (BYVAL pSprite AS DWORD, BYVAL elayer AS SINGLE)
  DECLARE FUNCTION tbgl_SpriteGetLayer LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetLayer" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE SUB tbgl_SpriteAnimate LIB "TBGL.DLL" ALIAS "tbgl_SpriteAnimate" (BYVAL pSprite AS DWORD, BYVAL SPEED AS SINGLE)
  DECLARE SUB tbgl_SpriteSetAnim LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetAnim" (BYVAL pSprite AS DWORD, BYVAL eStart AS LONG, BYVAL eEnd AS LONG, BYVAL group AS LONG)
  DECLARE FUNCTION tbgl_SpriteGetScale LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetScale" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetSpin LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetSpin" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE SUB tbgl_SpriteSetAnimtime LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetAnimtime" (BYVAL pSprite AS DWORD, BYVAL etime AS SINGLE, BYVAL group AS LONG)
  DECLARE FUNCTION tbgl_SpriteGetAnimtime LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetAnimtime" (BYVAL pSprite AS DWORD, BYVAL group AS LONG) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetAnimGroup LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetAnimGroup" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetAnimLength LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetAnimLength" (BYVAL pSprite AS DWORD, BYVAL group AS LONG) AS LONG
  DECLARE SUB tbgl_SpriteSetAnimSpeed LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetAnimSpeed" (BYVAL pSprite AS DWORD, BYVAL espeed AS SINGLE, BYVAL group AS LONG)
  DECLARE SUB tbgl_SpriteSetAnimGroup LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetAnimGroup" (BYVAL pSprite AS DWORD, BYVAL egroup AS LONG)
  DECLARE SUB tbgl_SpriteSetAnimType LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetAnimType" (BYVAL pSprite AS DWORD, BYVAL eType AS BYTE, BYVAL eGroup AS LONG)
  DECLARE SUB tbgl_SpriteSetCollisionType LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetCollisionType" (BYVAL pSprite AS DWORD, BYVAL ect AS LONG)
  DECLARE FUNCTION tbgl_SpriteGetCollisionWith LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetCollisionWith" (BYVAL pSprite AS DWORD, BYVAL w AS LONG) AS LONG
  DECLARE SUB tbgl_SpriteSetCollisionWith LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetCollisionWith" (BYVAL pSprite AS DWORD, BYVAL w AS LONG, BYVAL eflag AS LONG)
  DECLARE SUB tbgl_SpriteSetCollisionMode LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetCollisionMode" (BYVAL pSprite AS DWORD, BYVAL emode AS LONG)
  DECLARE SUB tbgl_SpriteSetCollisionGroup LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetCollisionGroup" (BYVAL pSprite AS DWORD, BYVAL ecg AS LONG)
  DECLARE SUB tbgl_SpriteSetCollisionRadius LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetCollisionRadius" (BYVAL pSprite AS DWORD, BYVAL ecr AS SINGLE)
  DECLARE SUB tbgl_SpriteSetCollisionWidth LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetCollisionWidth" (BYVAL pSprite AS DWORD, BYVAL ew AS SINGLE)
  DECLARE SUB tbgl_SpriteSetCollisionHeight LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetCollisionHeight" (BYVAL pSprite AS DWORD, BYVAL eh AS SINGLE)
  DECLARE FUNCTION tbgl_SpriteGetCollisionType LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetCollisionType" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetCollisionMode LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetCollisionMode" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetCollisionRadius LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetCollisionRadius" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetCollisionWidth LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetCollisionWidth" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetCollisionHeight LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetCollisionHeight" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetCollisionGroup LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetCollisionGroup" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE SUB tbgl_SpriteSetActive LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetActive" (BYVAL pSprite AS DWORD, BYVAL ea AS LONG)
  DECLARE SUB tbgl_SpriteSetVisible LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetVisible" (BYVAL pSprite AS DWORD, BYVAL ev AS LONG)
  DECLARE SUB tbgl_SpriteSetFriction LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetFriction" (BYVAL pSprite AS DWORD, BYVAL eFriction AS SINGLE)
  DECLARE FUNCTION tbgl_SpriteGetActive LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetActive" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetVisible LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetVisible" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteGetAngle LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetAngle" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetAlpha LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetAlpha" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE FUNCTION tbgl_SpriteGetFriction LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetFriction" (BYVAL pSprite AS DWORD) AS SINGLE
  DECLARE SUB tbgl_SpriteGetBaseSize LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetBaseSize" (BYVAL pSprite AS DWORD, BYREF w AS LONG, BYREF h AS LONG)
  DECLARE SUB tbgl_SpriteGetVector LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetVector" (BYVAL pSprite AS DWORD, BYVAL SPEED AS SINGLE, BYVAL angle AS SINGLE, BYREF x AS SINGLE, BYREF y AS SINGLE)
  DECLARE SUB tbgl_SpriteGetOldPos LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetOldPos" (BYVAL pSprite AS DWORD, BYREF x AS SINGLE, BYREF y AS SINGLE)
  DECLARE SUB tbgl_SpriteGetPos LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetPos" (BYVAL pSprite AS DWORD, BYREF x AS SINGLE, BYREF y AS SINGLE, BYREF z AS SINGLE)
  DECLARE SUB tbgl_SpriteGetSpeedXY LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetSpeedXY" (BYVAL pSprite AS DWORD, BYREF x AS SINGLE, BYREF y AS SINGLE, BYREF spin AS SINGLE)
  DECLARE SUB tbgl_SpriteSetOrder LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetOrder" (BYVAL pSprite AS DWORD, BYVAL lOrder AS LONG)
  DECLARE FUNCTION tbgl_SpriteGetOrder LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetOrder" (BYVAL sp AS DWORD) AS LONG
  DECLARE SUB tbgl_SpriteDraw LIB "TBGL.DLL" ALIAS "tbgl_SpriteDraw" (BYVAL pSprite AS DWORD)
  DECLARE SUB tbgl_SpriteDrawAt LIB "TBGL.DLL" ALIAS "tbgl_SpriteDrawAt" (BYVAL pSprite AS DWORD, BYVAL ex AS SINGLE, BYVAL ey AS SINGLE)
  DECLARE SUB tbgl_SpriteDrawAtLayer LIB "TBGL.DLL" ALIAS "tbgl_SpriteDrawAtLayer" (BYVAL pSprite AS DWORD, BYVAL ex AS SINGLE, BYVAL ey AS SINGLE, BYVAL elayer AS SINGLE)
  DECLARE SUB tbgl_SpriteSetEvent LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetEvent" (BYVAL pSprite AS DWORD, BYVAL eventID AS LONG, BYVAL ScriptFuncPtr AS LONG)
  DECLARE FUNCTION tbgl_SpriteCheckMouse LIB "TBGL.DLL" ALIAS "tbgl_SpriteCheckMouse" (BYVAL pSpriteA AS DWORD, BYVAL ebutton AS LONG) AS LONG
  DECLARE FUNCTION tbgl_SpritesCheckMouseAll LIB "TBGL.DLL" ALIAS "tbgl_SpritesCheckMouseAll" (BYVAL ebutton AS LONG) AS LONG
  DECLARE FUNCTION tbgl_SpriteCollided LIB "TBGL.DLL" ALIAS "tbgl_SpriteCollided" (BYVAL pSpriteA AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteCollidedWith LIB "TBGL.DLL" ALIAS "tbgl_SpriteCollidedWith" (BYVAL pSpriteA AS DWORD, BYVAL pSpriteB AS DWORD) AS LONG
  DECLARE SUB tbgl_SpriteSetUserData LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetUserData" (BYVAL pSprite AS DWORD, BYVAL pData AS DWORD, BYVAL dataSize AS LONG)
  DECLARE SUB tbgl_SpriteSetUserDataString LIB "TBGL.DLL" ALIAS "tbgl_SpriteSetUserDataString" (BYVAL pSprite AS DWORD, BYVAL sData AS STRING)
  DECLARE FUNCTION tbgl_SpriteGetUserDataPointer LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetUserDataPointer" (BYVAL pSprite AS DWORD) AS DWORD
  DECLARE FUNCTION tbgl_SpriteGetUserDataID LIB "TBGL.DLL" ALIAS "tbgl_SpriteGetUserDataID" (BYVAL pSprite AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_SpriteExists LIB "TBGL.DLL" ALIAS "tbgl_SpriteExists" (BYVAL sp AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_LoadTexture LIB "TBGL.DLL" ALIAS "tbgl_LoadTexture" (BYVAL TexName AS STRING, BYVAL TexNum AS LONG, BYVAL TexFilter AS LONG, BYVAL anisoLevel AS SINGLE, BYVAL lRGB AS LONG) AS LONG
  DECLARE FUNCTION tbgl_LoadTexturesFromTiles LIB "TBGL.DLL" ALIAS "tbgl_LoadTexturesFromTiles" ( _
      BYVAL TexName AS STRING _
    , BYVAL TexNum AS LONG _
    , BYVAL numCols AS LONG _
    , BYVAL numRows AS LONG _
    , BYVAL TexFilter AS LONG _
    , BYVAL anisoLevel AS SINGLE _
    , BYVAL lRGB AS LONG _
    ) AS LONG
  DECLARE FUNCTION tbgl_MakeTexture LIB "TBGL.DLL" ALIAS "tbgl_MakeTexture" ( _
      BYVAL TexBuffer AS STRING _
    , BYVAL texType AS LONG _
    , BYVAL x AS LONG _
    , BYVAL y AS LONG _
    , BYVAL TexNum AS LONG _
    , BYVAL TexFilter AS LONG _
    , BYVAL AnisoX AS SINGLE _
    , BYVAL lRGB AS LONG _
    ) AS LONG
  DECLARE SUB tbgl_UseTexturing LIB "TBGL.DLL" ALIAS "tbgl_UseTexturing" (BYVAL flag AS LONG)
  DECLARE FUNCTION tbgl_GetUseTexturing LIB "TBGL.DLL" ALIAS "tbgl_GetUseTexturing" () AS LONG
  DECLARE FUNCTION tbgl_BindTexture LIB "TBGL.DLL" ALIAS "tbgl_BindTexture" (BYVAL TexNum AS LONG) AS LONG
  DECLARE SUB tbgl_PushTexture LIB "TBGL.DLL" ALIAS "tbgl_PushTexture" (BYVAL parameter AS LONG)
  DECLARE SUB tbgl_PopTexture LIB "TBGL.DLL" ALIAS "tbgl_PopTexture" ()
  DECLARE SUB tbgl_TexCoord2D LIB "TBGL.DLL" ALIAS "tbgl_TexCoord2D" (BYVAL u AS SINGLE, BYVAL v AS SINGLE)
  DECLARE SUB tbgl_RenderToTexture LIB "TBGL.DLL" ALIAS "tbgl_RenderToTexture" (BYVAL destTexture AS LONG, BYVAL x AS LONG, BYVAL y AS LONG, BYVAL twidth AS LONG, BYVAL theight AS LONG)
  DECLARE SUB tbgl_EvaluatePOTMatch LIB "TBGL.DLL" ALIAS "tbgl_EvaluatePOTMatch" (BYVAL resX AS LONG, BYVAL resY AS LONG, BYREF fitX AS LONG, BYREF fitY AS LONG)
  DECLARE FUNCTION tbgl_GetTextureList LIB "TBGL.DLL" ALIAS "tbgl_GetTextureList" () AS STRING
  DECLARE FUNCTION tbgl_GetTextureName LIB "TBGL.DLL" ALIAS "tbgl_GetTextureName" (BYVAL i AS LONG) AS STRING
  DECLARE SUB tbgl_GetTextureResolution LIB "TBGL.DLL" ALIAS "tbgl_GetTextureResolution" (BYVAL nTex AS LONG, BYREF nWidth AS LONG, BYREF nHeight AS LONG)
  DECLARE FUNCTION tbgl_GetTextureHandle LIB "TBGL.DLL" ALIAS "tbgl_GetTextureHandle" (BYVAL TexNum AS LONG) AS DWORD
  DECLARE FUNCTION tbgl_GetFreeTextureIndex LIB "TBGL.DLL" ALIAS "tbgl_GetFreeTextureIndex" () AS LONG
  DECLARE FUNCTION tbgl_GetTextureData LIB "TBGL.DLL" ALIAS "tbgl_GetTextureData" (BYVAL nTex AS LONG, BYVAL nFormat AS LONG) AS STRING
  DECLARE SUB tbgl_TexturingQuery LIB "TBGL.DLL" ALIAS "tbgl_TexturingQuery" (BYREF texturingInfo AS TBGL_tTexturingInfo)
  DECLARE SUB tbgl_DeleteTexture LIB "TBGL.DLL" ALIAS "tbgl_DeleteTexture" (BYVAL texID AS LONG)
  DECLARE SUB tbgl_SetTextureRepeatMode LIB "TBGL.DLL" ALIAS "tbgl_SetTextureRepeatMode" (BYVAL textureIndex AS LONG, BYVAL param1 AS LONG, BYVAL param2 AS LONG)
  DECLARE SUB tbgl_Camera LIB "TBGL.DLL" ALIAS "tbgl_Camera" (BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, BYVAL z AS DOUBLE, BYVAL x2 AS DOUBLE, BYVAL y2 AS DOUBLE, BYVAL z2 AS DOUBLE)
  DECLARE SUB tbgl_Scale LIB "TBGL.DLL" ALIAS "tbgl_Scale" (BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_Translate LIB "TBGL.DLL" ALIAS "tbgl_Translate" (BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_Rotate LIB "TBGL.DLL" ALIAS "tbgl_Rotate" (BYVAL angle AS SINGLE, BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE SUB tbgl_PushMatrix LIB "TBGL.DLL" ALIAS "tbgl_PushMatrix" ()
  DECLARE SUB tbgl_PopMatrix LIB "TBGL.DLL" ALIAS "tbgl_PopMatrix" ()
  DECLARE SUB tbgl_ResetMatrix LIB "TBGL.DLL" ALIAS "tbgl_ResetMatrix" ()
  DECLARE SUB tbgl_RotateXYZ LIB "TBGL.DLL" ALIAS "tbgl_RotateXYZ" (BYVAL x AS SINGLE, BYVAL y AS SINGLE, BYVAL z AS SINGLE)
  DECLARE FUNCTION tbgl_CreateWindowEx LIB "TBGL.DLL" ALIAS "tbgl_CreateWindowEx" (BYVAL CAPTION AS STRING, BYVAL xRes AS LONG, BYVAL yRes AS LONG, BYVAL Bpp AS BYTE, BYVAL flags AS LONG, BYVAL xPos AS LONG, BYVAL yPos AS LONG) AS DWORD
  DECLARE FUNCTION tbgl_CallingWindow LIB "TBGL.DLL" ALIAS "tbgl_CallingWindow" () AS LONG
  DECLARE SUB tbgl_SetWindowed LIB "TBGL.DLL" ALIAS "tbgl_SetWindowed" (BYVAL wHandle AS DWORD, BYVAL xRes AS LONG, BYVAL yRes AS LONG, BYVAL Bpp AS BYTE, BYVAL winStyle AS LONG, BYVAL xPos AS LONG, BYVAL yPos AS LONG)
  DECLARE SUB tbgl_SetFullscreen LIB "TBGL.DLL" ALIAS "tbgl_SetFullscreen" (BYVAL wHandle AS DWORD, BYVAL xRes AS LONG, BYVAL yRes AS LONG, BYVAL Bpp AS BYTE)
  DECLARE FUNCTION tbgl_GetFullscreenModes LIB "TBGL.DLL" ALIAS "tbgl_GetFullscreenModes" () AS STRING
  DECLARE SUB tbgl_ShowWindow LIB "TBGL.DLL" ALIAS "tbgl_ShowWindow" ()
  DECLARE SUB tbgl_GetWindowClient LIB "TBGL.DLL" ALIAS "tbgl_GetWindowClient" (BYVAL winHandle AS DWORD, BYREF clientWidth AS LONG, BYREF clientHeight AS LONG)
  DECLARE FUNCTION tbgl_DisplayGetCount LIB "TBGL.DLL" ALIAS "tbgl_DisplayGetCount" () AS LONG
  DECLARE SUB tbgl_DisplayGetInfo LIB "TBGL.DLL" ALIAS "tbgl_DisplayGetInfo" (BYVAL displayNumber AS LONG, BYREF displayWidth AS LONG, BYREF displayHeight AS LONG, BYREF displayBitDepth AS LONG)
  DECLARE FUNCTION tbgl_SendWindowToDisplay LIB "TBGL.DLL" ALIAS "tbgl_SendWindowToDisplay" (BYVAL wHandle AS DWORD, BYVAL eDisplay AS LONG, BYVAL newX AS LONG, BYVAL newY AS LONG) AS LONG
  DECLARE FUNCTION tbgl_GetWindowDisplay LIB "TBGL.DLL" ALIAS "tbgl_GetWindowDisplay" (BYVAL wHandle AS DWORD) AS LONG
  DECLARE SUB tbgl_GetDesktopInfo LIB "TBGL.DLL" ALIAS "tbgl_GetDesktopInfo" (BYREF desktopWidth AS LONG, BYREF desktopHeight AS LONG, BYREF desktopBitDepth AS LONG)
  DECLARE SUB tbgl_SetWindowTitle LIB "TBGL.DLL" ALIAS "tbgl_SetWindowTitle" (BYVAL whandle AS DWORD, BYVAL wtext AS STRING)
  DECLARE FUNCTION tbgl_IsFullscreen LIB "TBGL.DLL" ALIAS "tbgl_IsFullscreen" (BYVAL whandle AS DWORD) AS LONG
  DECLARE FUNCTION tbgl_IsWindow LIB "TBGL.DLL" ALIAS "tbgl_IsWindow" (BYVAL whandle AS DWORD) AS LONG
  DECLARE SUB tbgl_SetWindowIcon LIB "TBGL.DLL" ALIAS "tbgl_SetWindowIcon" (BYVAL whandle AS DWORD, BYVAL wtext AS STRING)
  DECLARE SUB tbgl_DestroyWindow LIB "TBGL.DLL" ALIAS "tbgl_DestroyWindow" ()
  DECLARE SUB tbgl_BindPeriodicFunction LIB "TBGL.DLL" ALIAS "tbgl_BindPeriodicFunction" (BYVAL winHandle AS DWORD, BYVAL funcPointer AS LONG, BYVAL funcInterval AS LONG)
  DECLARE SUB tbgl_UnBindPeriodicFunction LIB "TBGL.DLL" ALIAS "tbgl_UnBindPeriodicFunction" (BYVAL winHandle AS DWORD)
  DECLARE SUB tbgl_ProcessPeriodicFunction LIB "TBGL.DLL" ALIAS "tbgl_ProcessPeriodicFunction" (BYVAL winHandle AS DWORD)
  DECLARE SUB tbgl_PeriodicChangeFunction LIB "TBGL.DLL" ALIAS "tbgl_PeriodicChangeFunction" (BYVAL winHandle AS DWORD, BYVAL funcPointer AS LONG)
  DECLARE SUB tbgl_PeriodicChangeInterval LIB "TBGL.DLL" ALIAS "tbgl_PeriodicChangeInterval" (BYVAL winHandle AS DWORD, BYVAL funcInterval AS LONG)
  DECLARE FUNCTION tbgl_BindWindowEvent LIB "TBGL.DLL" ALIAS "tbgl_BindWindowEvent" (BYVAL hWindow AS DWORD, BYVAL eventCode AS LONG, BYVAL eventProcPointer AS LONG) AS LONG
  DECLARE FUNCTION tbgl_UnBindWindowEvent LIB "TBGL.DLL" ALIAS "tbgl_UnBindWindowEvent" (BYVAL hWindow AS DWORD, BYVAL eventCode AS LONG) AS LONG
  DECLARE FUNCTION tbgl_OnDropFiles_GetFileName LIB "TBGL.DLL" ALIAS "tbgl_OnDropFiles_GetFileName" (BYVAL fileNo AS LONG) AS STRING
  DECLARE FUNCTION tbgl_OnDropFiles_GetFileCount LIB "TBGL.DLL" ALIAS "tbgl_OnDropFiles_GetFileCount" () AS LONG
  DECLARE FUNCTION tbgl_GetWindowPos LIB "TBGL.DLL" ALIAS "tbgl_GetWindowPos" (BYVAL hWnd AS DWORD, BYREF outX AS LONG, BYREF outY AS LONG) AS LONG
  DECLARE FUNCTION tbgl_SetWindowMinClient LIB "TBGL.DLL" ALIAS "tbgl_SetWindowMinClient" (BYVAL hWnd AS DWORD, BYVAL minWidth AS LONG, BYVAL minHeight AS LONG) AS LONG
  DECLARE FUNCTION tbgl_InitializeLibrary LIB "TBGL.DLL" ALIAS "tbgl_InitializeLibrary" () AS LONG
  DECLARE FUNCTION tbgl_ReleaseLibrary LIB "TBGL.DLL" ALIAS "tbgl_ReleaseLibrary" () AS LONG
#ENDIF

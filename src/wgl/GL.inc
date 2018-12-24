'/*++ BUILD Version: 0004    // Increment this if a change has global effects

'Copyright (c) 1985-96, Microsoft Corporation

'Module Name:

'    gl.h

'Abstract:

'    Procedure declarations, constant definitions and macros for the OpenGL
'    component.

'--*/

#IF NOT %DEF(%GL_INC)
    %GL_INC = 1

'#INCLUDE ONCE "windows.inc"

'/*
'** Copyright 1996 Silicon Graphics, Inc.
'** All Rights Reserved.
'**
'** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
'** the contents of this file may not be disclosed to third parties, copied or
'** duplicated in any form, in whole or in part, without the prior written
'** permission of Silicon Graphics, Inc.
'**
'** RESTRICTED RIGHTS LEGEND:
'** Use, duplication or disclosure by the Government is subject to restrictions
'** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
'** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
'** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
'** rights reserved under the Copyright Laws of the United States.
'*/

'typedef unsigned int GLenum;
'typedef unsigned char GLboolean;
'typedef unsigned int GLbitfield;
'typedef signed char GLbyte;
'typedef short GLshort;
'typedef int GLint;
'typedef int GLsizei;
'typedef unsigned char GLubyte;
'typedef unsigned short GLushort;
'typedef unsigned int GLuint;
'typedef float GLfloat;
'typedef float GLclampf;
'typedef double GLdouble;
'typedef double GLclampd;
'typedef void GLvoid;

'/*************************************************************/

'/* Version */
%GL_VERSION_1_1                    = 1

'/* AccumOp */
%GL_ACCUM                          = &H0100???
%GL_LOAD                           = &H0101???
%GL_RETURN                         = &H0102???
%GL_MULT                           = &H0103???
%GL_ADD                            = &H0104???

'/* AlphaFunction */
%GL_NEVER                          = &H0200???
%GL_LESS                           = &H0201???
%GL_EQUAL                          = &H0202???
%GL_LEQUAL                         = &H0203???
%GL_GREATER                        = &H0204???
%GL_NOTEQUAL                       = &H0205???
%GL_GEQUAL                         = &H0206???
%GL_ALWAYS                         = &H0207???

'/* AttribMask */
%GL_CURRENT_BIT                    = &H00000001???
%GL_POINT_BIT                      = &H00000002???
%GL_LINE_BIT                       = &H00000004???
%GL_POLYGON_BIT                    = &H00000008???
%GL_POLYGON_STIPPLE_BIT            = &H00000010???
%GL_PIXEL_MODE_BIT                 = &H00000020???
%GL_LIGHTING_BIT                   = &H00000040???
%GL_FOG_BIT                        = &H00000080???
%GL_DEPTH_BUFFER_BIT               = &H00000100???
%GL_ACCUM_BUFFER_BIT               = &H00000200???
%GL_STENCIL_BUFFER_BIT             = &H00000400???
%GL_VIEWPORT_BIT                   = &H00000800???
%GL_TRANSFORM_BIT                  = &H00001000???
%GL_ENABLE_BIT                     = &H00002000???
%GL_COLOR_BUFFER_BIT               = &H00004000???
%GL_HINT_BIT                       = &H00008000???
%GL_EVAL_BIT                       = &H00010000???
%GL_LIST_BIT                       = &H00020000???
%GL_TEXTURE_BIT                    = &H00040000???
%GL_SCISSOR_BIT                    = &H00080000???
%GL_ALL_ATTRIB_BITS                = &H000fffff???

'/* BeginMode */
%GL_POINTS                         = &H0000???
%GL_LINES                          = &H0001???
%GL_LINE_LOOP                      = &H0002???
%GL_LINE_STRIP                     = &H0003???
%GL_TRIANGLES                      = &H0004???
%GL_TRIANGLE_STRIP                 = &H0005???
%GL_TRIANGLE_FAN                   = &H0006???
%GL_QUADS                          = &H0007???
%GL_QUAD_STRIP                     = &H0008???
%GL_POLYGON                        = &H0009???

'/* BlendingFactorDest */
%GL_ZERO                           = 0???
%GL_ONE                            = 1???
%GL_SRC_COLOR                      = &H0300???
%GL_ONE_MINUS_SRC_COLOR            = &H0301???
%GL_SRC_ALPHA                      = &H0302???
%GL_ONE_MINUS_SRC_ALPHA            = &H0303???
%GL_DST_ALPHA                      = &H0304???
%GL_ONE_MINUS_DST_ALPHA            = &H0305???

'/* BlendingFactorSrc */
'/*      GL_ZERO */
'/*      GL_ONE */
%GL_DST_COLOR                      = &H0306???
%GL_ONE_MINUS_DST_COLOR            = &H0307???
%GL_SRC_ALPHA_SATURATE             = &H0308???
'/*      GL_SRC_ALPHA */
'/*      GL_ONE_MINUS_SRC_ALPHA */
'/*      GL_DST_ALPHA */
'/*      GL_ONE_MINUS_DST_ALPHA */

'/* Boolean */
%GL_TRUE                           = 1
%GL_FALSE                          = 0

'/* ClearBufferMask */
'/*      GL_COLOR_BUFFER_BIT */
'/*      GL_ACCUM_BUFFER_BIT */
'/*      GL_STENCIL_BUFFER_BIT */
'/*      GL_DEPTH_BUFFER_BIT */

'/* ClientArrayType */
'/*      GL_VERTEX_ARRAY */
'/*      GL_NORMAL_ARRAY */
'/*      GL_COLOR_ARRAY */
'/*      GL_INDEX_ARRAY */
'/*      GL_TEXTURE_COORD_ARRAY */
'/*      GL_EDGE_FLAG_ARRAY */

'/* ClipPlaneName */
%GL_CLIP_PLANE0                    = &H3000???
%GL_CLIP_PLANE1                    = &H3001???
%GL_CLIP_PLANE2                    = &H3002???
%GL_CLIP_PLANE3                    = &H3003???
%GL_CLIP_PLANE4                    = &H3004???
%GL_CLIP_PLANE5                    = &H3005???

'/* ColorMaterialFace */
'/*      GL_FRONT */
'/*      GL_BACK */
'/*      GL_FRONT_AND_BACK */

'/* ColorMaterialParameter */
'/*      GL_AMBIENT */
'/*      GL_DIFFUSE */
'/*      GL_SPECULAR */
'/*      GL_EMISSION */
'/*      GL_AMBIENT_AND_DIFFUSE */

'/* ColorPointerType */
'/*      GL_BYTE */
'/*      GL_UNSIGNED_BYTE */
'/*      GL_SHORT */
'/*      GL_UNSIGNED_SHORT */
'/*      GL_INT */
'/*      GL_UNSIGNED_INT */
'/*      GL_FLOAT */
'/*      GL_DOUBLE */

'/* CullFaceMode */
'/*      GL_FRONT */
'/*      GL_BACK */
'/*      GL_FRONT_AND_BACK */

'/* DataType */
%GL_BYTE                           = &H1400???
%GL_UNSIGNED_BYTE                  = &H1401???
%GL_SHORT                          = &H1402???
%GL_UNSIGNED_SHORT                 = &H1403???
%GL_INT                            = &H1404???
%GL_UNSIGNED_INT                   = &H1405???
%GL_FLOAT                          = &H1406???
%GL_2_BYTES                        = &H1407???
%GL_3_BYTES                        = &H1408???
%GL_4_BYTES                        = &H1409???
%GL_DOUBLE                         = &H140A???

'/* DepthFunction */
'/*      GL_NEVER */
'/*      GL_LESS */
'/*      GL_EQUAL */
'/*      GL_LEQUAL */
'/*      GL_GREATER */
'/*      GL_NOTEQUAL */
'/*      GL_GEQUAL */
'/*      GL_ALWAYS */

'/* DrawBufferMode */
%GL_NONE                           = 0???
%GL_FRONT_LEFT                     = &H0400???
%GL_FRONT_RIGHT                    = &H0401???
%GL_BACK_LEFT                      = &H0402???
%GL_BACK_RIGHT                     = &H0403???
%GL_FRONT                          = &H0404???
%GL_BACK                           = &H0405???
%GL_LEFT                           = &H0406???
%GL_RIGHT                          = &H0407???
%GL_FRONT_AND_BACK                 = &H0408???
%GL_AUX0                           = &H0409???
%GL_AUX1                           = &H040A???
%GL_AUX2                           = &H040B???
%GL_AUX3                           = &H040C???

'/* Enable */
'/*      GL_FOG */
'/*      GL_LIGHTING */
'/*      GL_TEXTURE_1D */
'/*      GL_TEXTURE_2D */
'/*      GL_LINE_STIPPLE */
'/*      GL_POLYGON_STIPPLE */
'/*      GL_CULL_FACE */
'/*      GL_ALPHA_TEST */
'/*      GL_BLEND */
'/*      GL_INDEX_LOGIC_OP */
'/*      GL_COLOR_LOGIC_OP */
'/*      GL_DITHER */
'/*      GL_STENCIL_TEST */
'/*      GL_DEPTH_TEST */
'/*      GL_CLIP_PLANE0 */
'/*      GL_CLIP_PLANE1 */
'/*      GL_CLIP_PLANE2 */
'/*      GL_CLIP_PLANE3 */
'/*      GL_CLIP_PLANE4 */
'/*      GL_CLIP_PLANE5 */
'/*      GL_LIGHT0 */
'/*      GL_LIGHT1 */
'/*      GL_LIGHT2 */
'/*      GL_LIGHT3 */
'/*      GL_LIGHT4 */
'/*      GL_LIGHT5 */
'/*      GL_LIGHT6 */
'/*      GL_LIGHT7 */
'/*      GL_TEXTURE_GEN_S */
'/*      GL_TEXTURE_GEN_T */
'/*      GL_TEXTURE_GEN_R */
'/*      GL_TEXTURE_GEN_Q */
'/*      GL_MAP1_VERTEX_3 */
'/*      GL_MAP1_VERTEX_4 */
'/*      GL_MAP1_COLOR_4 */
'/*      GL_MAP1_INDEX */
'/*      GL_MAP1_NORMAL */
'/*      GL_MAP1_TEXTURE_COORD_1 */
'/*      GL_MAP1_TEXTURE_COORD_2 */
'/*      GL_MAP1_TEXTURE_COORD_3 */
'/*      GL_MAP1_TEXTURE_COORD_4 */
'/*      GL_MAP2_VERTEX_3 */
'/*      GL_MAP2_VERTEX_4 */
'/*      GL_MAP2_COLOR_4 */
'/*      GL_MAP2_INDEX */
'/*      GL_MAP2_NORMAL */
'/*      GL_MAP2_TEXTURE_COORD_1 */
'/*      GL_MAP2_TEXTURE_COORD_2 */
'/*      GL_MAP2_TEXTURE_COORD_3 */
'/*      GL_MAP2_TEXTURE_COORD_4 */
'/*      GL_POINT_SMOOTH */
'/*      GL_LINE_SMOOTH */
'/*      GL_POLYGON_SMOOTH */
'/*      GL_SCISSOR_TEST */
'/*      GL_COLOR_MATERIAL */
'/*      GL_NORMALIZE */
'/*      GL_AUTO_NORMAL */
'/*      GL_VERTEX_ARRAY */
'/*      GL_NORMAL_ARRAY */
'/*      GL_COLOR_ARRAY */
'/*      GL_INDEX_ARRAY */
'/*      GL_TEXTURE_COORD_ARRAY */
'/*      GL_EDGE_FLAG_ARRAY */
'/*      GL_POLYGON_OFFSET_POINT */
'/*      GL_POLYGON_OFFSET_LINE */
'/*      GL_POLYGON_OFFSET_FILL */

'/* ErrorCode */
%GL_NO_ERROR                       = 0???
%GL_INVALID_ENUM                   = &H0500???
%GL_INVALID_VALUE                  = &H0501???
%GL_INVALID_OPERATION              = &H0502???
%GL_STACK_OVERFLOW                 = &H0503???
%GL_STACK_UNDERFLOW                = &H0504???
%GL_OUT_OF_MEMORY                  = &H0505???

'/* FeedBackMode */
%GL_2D                             = &H0600???
%GL_3D                             = &H0601???
%GL_3D_COLOR                       = &H0602???
%GL_3D_COLOR_TEXTURE               = &H0603???
%GL_4D_COLOR_TEXTURE               = &H0604???

'/* FeedBackToken */
%GL_PASS_THROUGH_TOKEN             = &H0700???
%GL_POINT_TOKEN                    = &H0701???
%GL_LINE_TOKEN                     = &H0702???
%GL_POLYGON_TOKEN                  = &H0703???
%GL_BITMAP_TOKEN                   = &H0704???
%GL_DRAW_PIXEL_TOKEN               = &H0705???
%GL_COPY_PIXEL_TOKEN               = &H0706???
%GL_LINE_RESET_TOKEN               = &H0707???

'/* FogMode */
'/*      GL_LINEAR */
%GL_EXP                            = &H0800???
%GL_EXP2                           = &H0801???


'/* FogParameter */
'/*      GL_FOG_COLOR */
'/*      GL_FOG_DENSITY */
'/*      GL_FOG_END */
'/*      GL_FOG_INDEX */
'/*      GL_FOG_MODE */
'/*      GL_FOG_START */

'/* FrontFaceDirection */
%GL_CW                             = &H0900???
%GL_CCW                            = &H0901???

'/* GetMapTarget */
%GL_COEFF                          = &H0A00???
%GL_ORDER                          = &H0A01???
%GL_DOMAIN                         = &H0A02???

'/* GetPixelMap */
'/*      GL_PIXEL_MAP_I_TO_I */
'/*      GL_PIXEL_MAP_S_TO_S */
'/*      GL_PIXEL_MAP_I_TO_R */
'/*      GL_PIXEL_MAP_I_TO_G */
'/*      GL_PIXEL_MAP_I_TO_B */
'/*      GL_PIXEL_MAP_I_TO_A */
'/*      GL_PIXEL_MAP_R_TO_R */
'/*      GL_PIXEL_MAP_G_TO_G */
'/*      GL_PIXEL_MAP_B_TO_B */
'/*      GL_PIXEL_MAP_A_TO_A */

'/* GetPointerTarget */
'/*      GL_VERTEX_ARRAY_POINTER */
'/*      GL_NORMAL_ARRAY_POINTER */
'/*      GL_COLOR_ARRAY_POINTER */
'/*      GL_INDEX_ARRAY_POINTER */
'/*      GL_TEXTURE_COORD_ARRAY_POINTER */
'/*      GL_EDGE_FLAG_ARRAY_POINTER */

'/* GetTarget */
%GL_CURRENT_COLOR                  = &H0B00???
%GL_CURRENT_INDEX                  = &H0B01???
%GL_CURRENT_NORMAL                 = &H0B02???
%GL_CURRENT_TEXTURE_COORDS         = &H0B03???
%GL_CURRENT_RASTER_COLOR           = &H0B04???
%GL_CURRENT_RASTER_INDEX           = &H0B05???
%GL_CURRENT_RASTER_TEXTURE_COORDS  = &H0B06???
%GL_CURRENT_RASTER_POSITION        = &H0B07???
%GL_CURRENT_RASTER_POSITION_VALID  = &H0B08???
%GL_CURRENT_RASTER_DISTANCE        = &H0B09???
%GL_POINT_SMOOTH                   = &H0B10???
%GL_POINT_SIZE                     = &H0B11???
%GL_POINT_SIZE_RANGE               = &H0B12???
%GL_POINT_SIZE_GRANULARITY         = &H0B13???
%GL_LINE_SMOOTH                    = &H0B20???
%GL_LINE_WIDTH                     = &H0B21???
%GL_LINE_WIDTH_RANGE               = &H0B22???
%GL_LINE_WIDTH_GRANULARITY         = &H0B23???
%GL_LINE_STIPPLE                   = &H0B24???
%GL_LINE_STIPPLE_PATTERN           = &H0B25???
%GL_LINE_STIPPLE_REPEAT            = &H0B26???
%GL_LIST_MODE                      = &H0B30???
%GL_MAX_LIST_NESTING               = &H0B31???
%GL_LIST_BASE                      = &H0B32???
%GL_LIST_INDEX                     = &H0B33???
%GL_POLYGON_MODE                   = &H0B40???
%GL_POLYGON_SMOOTH                 = &H0B41???
%GL_POLYGON_STIPPLE                = &H0B42???
%GL_EDGE_FLAG                      = &H0B43???
%GL_CULL_FACE                      = &H0B44???
%GL_CULL_FACE_MODE                 = &H0B45???
%GL_FRONT_FACE                     = &H0B46???
%GL_LIGHTING                       = &H0B50???
%GL_LIGHT_MODEL_LOCAL_VIEWER       = &H0B51???
%GL_LIGHT_MODEL_TWO_SIDE           = &H0B52???
%GL_LIGHT_MODEL_AMBIENT            = &H0B53???
%GL_SHADE_MODEL                    = &H0B54???
%GL_COLOR_MATERIAL_FACE            = &H0B55???
%GL_COLOR_MATERIAL_PARAMETER       = &H0B56???
%GL_COLOR_MATERIAL                 = &H0B57???
%GL_FOG                            = &H0B60???
%GL_FOG_INDEX                      = &H0B61???
%GL_FOG_DENSITY                    = &H0B62???
%GL_FOG_START                      = &H0B63???
%GL_FOG_END                        = &H0B64???
%GL_FOG_MODE                       = &H0B65???
%GL_FOG_COLOR                      = &H0B66???
%GL_DEPTH_RANGE                    = &H0B70???
%GL_DEPTH_TEST                     = &H0B71???
%GL_DEPTH_WRITEMASK                = &H0B72???
%GL_DEPTH_CLEAR_VALUE              = &H0B73???
%GL_DEPTH_FUNC                     = &H0B74???
%GL_ACCUM_CLEAR_VALUE              = &H0B80???
%GL_STENCIL_TEST                   = &H0B90???
%GL_STENCIL_CLEAR_VALUE            = &H0B91???
%GL_STENCIL_FUNC                   = &H0B92???
%GL_STENCIL_VALUE_MASK             = &H0B93???
%GL_STENCIL_FAIL                   = &H0B94???
%GL_STENCIL_PASS_DEPTH_FAIL        = &H0B95???
%GL_STENCIL_PASS_DEPTH_PASS        = &H0B96???
%GL_STENCIL_REF                    = &H0B97???
%GL_STENCIL_WRITEMASK              = &H0B98???
%GL_MATRIX_MODE                    = &H0BA0???
%GL_NORMALIZE                      = &H0BA1???
%GL_VIEWPORT                       = &H0BA2???
%GL_MODELVIEW_STACK_DEPTH          = &H0BA3???
%GL_PROJECTION_STACK_DEPTH         = &H0BA4???
%GL_TEXTURE_STACK_DEPTH            = &H0BA5???
%GL_MODELVIEW_MATRIX               = &H0BA6???
%GL_PROJECTION_MATRIX              = &H0BA7???
%GL_TEXTURE_MATRIX                 = &H0BA8???
%GL_ATTRIB_STACK_DEPTH             = &H0BB0???
%GL_CLIENT_ATTRIB_STACK_DEPTH      = &H0BB1???
%GL_ALPHA_TEST                     = &H0BC0???
%GL_ALPHA_TEST_FUNC                = &H0BC1???
%GL_ALPHA_TEST_REF                 = &H0BC2???
%GL_DITHER                         = &H0BD0???
%GL_BLEND_DST                      = &H0BE0???
%GL_BLEND_SRC                      = &H0BE1???
%GL_BLEND                          = &H0BE2???
%GL_LOGIC_OP_MODE                  = &H0BF0???
%GL_INDEX_LOGIC_OP                 = &H0BF1???
%GL_COLOR_LOGIC_OP                 = &H0BF2???
%GL_AUX_BUFFERS                    = &H0C00???
%GL_DRAW_BUFFER                    = &H0C01???
%GL_READ_BUFFER                    = &H0C02???
%GL_SCISSOR_BOX                    = &H0C10???
%GL_SCISSOR_TEST                   = &H0C11???
%GL_INDEX_CLEAR_VALUE              = &H0C20???
%GL_INDEX_WRITEMASK                = &H0C21???
%GL_COLOR_CLEAR_VALUE              = &H0C22???
%GL_COLOR_WRITEMASK                = &H0C23???
%GL_INDEX_MODE                     = &H0C30???
%GL_RGBA_MODE                      = &H0C31???
%GL_DOUBLEBUFFER                   = &H0C32???
%GL_STEREO                         = &H0C33???
%GL_RENDER_MODE                    = &H0C40???
%GL_PERSPECTIVE_CORRECTION_HINT    = &H0C50???
%GL_POINT_SMOOTH_HINT              = &H0C51???
%GL_LINE_SMOOTH_HINT               = &H0C52???
%GL_POLYGON_SMOOTH_HINT            = &H0C53???
%GL_FOG_HINT                       = &H0C54???
%GL_TEXTURE_GEN_S                  = &H0C60???
%GL_TEXTURE_GEN_T                  = &H0C61???
%GL_TEXTURE_GEN_R                  = &H0C62???
%GL_TEXTURE_GEN_Q                  = &H0C63???
%GL_PIXEL_MAP_I_TO_I               = &H0C70???
%GL_PIXEL_MAP_S_TO_S               = &H0C71???
%GL_PIXEL_MAP_I_TO_R               = &H0C72???
%GL_PIXEL_MAP_I_TO_G               = &H0C73???
%GL_PIXEL_MAP_I_TO_B               = &H0C74???
%GL_PIXEL_MAP_I_TO_A               = &H0C75???
%GL_PIXEL_MAP_R_TO_R               = &H0C76???
%GL_PIXEL_MAP_G_TO_G               = &H0C77???
%GL_PIXEL_MAP_B_TO_B               = &H0C78???
%GL_PIXEL_MAP_A_TO_A               = &H0C79???
%GL_PIXEL_MAP_I_TO_I_SIZE          = &H0CB0???
%GL_PIXEL_MAP_S_TO_S_SIZE          = &H0CB1???
%GL_PIXEL_MAP_I_TO_R_SIZE          = &H0CB2???
%GL_PIXEL_MAP_I_TO_G_SIZE          = &H0CB3???
%GL_PIXEL_MAP_I_TO_B_SIZE          = &H0CB4???
%GL_PIXEL_MAP_I_TO_A_SIZE          = &H0CB5???
%GL_PIXEL_MAP_R_TO_R_SIZE          = &H0CB6???
%GL_PIXEL_MAP_G_TO_G_SIZE          = &H0CB7???
%GL_PIXEL_MAP_B_TO_B_SIZE          = &H0CB8???
%GL_PIXEL_MAP_A_TO_A_SIZE          = &H0CB9???
%GL_UNPACK_SWAP_BYTES              = &H0CF0???
%GL_UNPACK_LSB_FIRST               = &H0CF1???
%GL_UNPACK_ROW_LENGTH              = &H0CF2???
%GL_UNPACK_SKIP_ROWS               = &H0CF3???
%GL_UNPACK_SKIP_PIXELS             = &H0CF4???
%GL_UNPACK_ALIGNMENT               = &H0CF5???
%GL_PACK_SWAP_BYTES                = &H0D00???
%GL_PACK_LSB_FIRST                 = &H0D01???
%GL_PACK_ROW_LENGTH                = &H0D02???
%GL_PACK_SKIP_ROWS                 = &H0D03???
%GL_PACK_SKIP_PIXELS               = &H0D04???
%GL_PACK_ALIGNMENT                 = &H0D05???
%GL_MAP_COLOR                      = &H0D10???
%GL_MAP_STENCIL                    = &H0D11???
%GL_INDEX_SHIFT                    = &H0D12???
%GL_INDEX_OFFSET                   = &H0D13???
%GL_RED_SCALE                      = &H0D14???
%GL_RED_BIAS                       = &H0D15???
%GL_ZOOM_X                         = &H0D16???
%GL_ZOOM_Y                         = &H0D17???
%GL_GREEN_SCALE                    = &H0D18???
%GL_GREEN_BIAS                     = &H0D19???
%GL_BLUE_SCALE                     = &H0D1A???
%GL_BLUE_BIAS                      = &H0D1B???
%GL_ALPHA_SCALE                    = &H0D1C???
%GL_ALPHA_BIAS                     = &H0D1D???
%GL_DEPTH_SCALE                    = &H0D1E???
%GL_DEPTH_BIAS                     = &H0D1F???
%GL_MAX_EVAL_ORDER                 = &H0D30???
%GL_MAX_LIGHTS                     = &H0D31???
%GL_MAX_CLIP_PLANES                = &H0D32???
%GL_MAX_TEXTURE_SIZE               = &H0D33???
%GL_MAX_PIXEL_MAP_TABLE            = &H0D34???
%GL_MAX_ATTRIB_STACK_DEPTH         = &H0D35???
%GL_MAX_MODELVIEW_STACK_DEPTH      = &H0D36???
%GL_MAX_NAME_STACK_DEPTH           = &H0D37???
%GL_MAX_PROJECTION_STACK_DEPTH     = &H0D38???
%GL_MAX_TEXTURE_STACK_DEPTH        = &H0D39???
%GL_MAX_VIEWPORT_DIMS              = &H0D3A???
%GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  = &H0D3B???
%GL_SUBPIXEL_BITS                  = &H0D50???
%GL_INDEX_BITS                     = &H0D51???
%GL_RED_BITS                       = &H0D52???
%GL_GREEN_BITS                     = &H0D53???
%GL_BLUE_BITS                      = &H0D54???
%GL_ALPHA_BITS                     = &H0D55???
%GL_DEPTH_BITS                     = &H0D56???
%GL_STENCIL_BITS                   = &H0D57???
%GL_ACCUM_RED_BITS                 = &H0D58???
%GL_ACCUM_GREEN_BITS               = &H0D59???
%GL_ACCUM_BLUE_BITS                = &H0D5A???
%GL_ACCUM_ALPHA_BITS               = &H0D5B???
%GL_NAME_STACK_DEPTH               = &H0D70???
%GL_AUTO_NORMAL                    = &H0D80???
%GL_MAP1_COLOR_4                   = &H0D90???
%GL_MAP1_INDEX                     = &H0D91???
%GL_MAP1_NORMAL                    = &H0D92???
%GL_MAP1_TEXTURE_COORD_1           = &H0D93???
%GL_MAP1_TEXTURE_COORD_2           = &H0D94???
%GL_MAP1_TEXTURE_COORD_3           = &H0D95???
%GL_MAP1_TEXTURE_COORD_4           = &H0D96???
%GL_MAP1_VERTEX_3                  = &H0D97???
%GL_MAP1_VERTEX_4                  = &H0D98???
%GL_MAP2_COLOR_4                   = &H0DB0???
%GL_MAP2_INDEX                     = &H0DB1???
%GL_MAP2_NORMAL                    = &H0DB2???
%GL_MAP2_TEXTURE_COORD_1           = &H0DB3???
%GL_MAP2_TEXTURE_COORD_2           = &H0DB4???
%GL_MAP2_TEXTURE_COORD_3           = &H0DB5???
%GL_MAP2_TEXTURE_COORD_4           = &H0DB6???
%GL_MAP2_VERTEX_3                  = &H0DB7???
%GL_MAP2_VERTEX_4                  = &H0DB8???
%GL_MAP1_GRID_DOMAIN               = &H0DD0???
%GL_MAP1_GRID_SEGMENTS             = &H0DD1???
%GL_MAP2_GRID_DOMAIN               = &H0DD2???
%GL_MAP2_GRID_SEGMENTS             = &H0DD3???
%GL_TEXTURE_1D                     = &H0DE0???
%GL_TEXTURE_2D                     = &H0DE1???
%GL_FEEDBACK_BUFFER_POINTER        = &H0DF0???
%GL_FEEDBACK_BUFFER_SIZE           = &H0DF1???
%GL_FEEDBACK_BUFFER_TYPE           = &H0DF2???
%GL_SELECTION_BUFFER_POINTER       = &H0DF3???
%GL_SELECTION_BUFFER_SIZE          = &H0DF4???
'/*      GL_TEXTURE_BINDING_1D */
'/*      GL_TEXTURE_BINDING_2D */
'/*      GL_VERTEX_ARRAY */
'/*      GL_NORMAL_ARRAY */
'/*      GL_COLOR_ARRAY */
'/*      GL_INDEX_ARRAY */
'/*      GL_TEXTURE_COORD_ARRAY */
'/*      GL_EDGE_FLAG_ARRAY */
'/*      GL_VERTEX_ARRAY_SIZE */
'/*      GL_VERTEX_ARRAY_TYPE */
'/*      GL_VERTEX_ARRAY_STRIDE */
'/*      GL_NORMAL_ARRAY_TYPE */
'/*      GL_NORMAL_ARRAY_STRIDE */
'/*      GL_COLOR_ARRAY_SIZE */
'/*      GL_COLOR_ARRAY_TYPE */
'/*      GL_COLOR_ARRAY_STRIDE */
'/*      GL_INDEX_ARRAY_TYPE */
'/*      GL_INDEX_ARRAY_STRIDE */
'/*      GL_TEXTURE_COORD_ARRAY_SIZE */
'/*      GL_TEXTURE_COORD_ARRAY_TYPE */
'/*      GL_TEXTURE_COORD_ARRAY_STRIDE */
'/*      GL_EDGE_FLAG_ARRAY_STRIDE */
'/*      GL_POLYGON_OFFSET_FACTOR */
'/*      GL_POLYGON_OFFSET_UNITS */

'/* GetTextureParameter */
'/*      GL_TEXTURE_MAG_FILTER */
'/*      GL_TEXTURE_MIN_FILTER */
'/*      GL_TEXTURE_WRAP_S */
'/*      GL_TEXTURE_WRAP_T */
%GL_TEXTURE_WIDTH                  = &H1000???
%GL_TEXTURE_HEIGHT                 = &H1001???
%GL_TEXTURE_INTERNAL_FORMAT        = &H1003???
%GL_TEXTURE_BORDER_COLOR           = &H1004???
%GL_TEXTURE_BORDER                 = &H1005???
'/*      GL_TEXTURE_RED_SIZE */
'/*      GL_TEXTURE_GREEN_SIZE */
'/*      GL_TEXTURE_BLUE_SIZE */
'/*      GL_TEXTURE_ALPHA_SIZE */
'/*      GL_TEXTURE_LUMINANCE_SIZE */
'/*      GL_TEXTURE_INTENSITY_SIZE */
'/*      GL_TEXTURE_PRIORITY */
'/*      GL_TEXTURE_RESIDENT */

'/* HintMode */
%GL_DONT_CARE                      = &H1100???
%GL_FASTEST                        = &H1101???
%GL_NICEST                         = &H1102???

'/* HintTarget */
'/*      GL_PERSPECTIVE_CORRECTION_HINT */
'/*      GL_POINT_SMOOTH_HINT */
'/*      GL_LINE_SMOOTH_HINT */
'/*      GL_POLYGON_SMOOTH_HINT */
'/*      GL_FOG_HINT */
'/*      GL_PHONG_HINT */

'/* IndexPointerType */
'/*      GL_SHORT */
'/*      GL_INT */
'/*      GL_FLOAT */
'/*      GL_DOUBLE */

'/* LightModelParameter */
'/*      GL_LIGHT_MODEL_AMBIENT */
'/*      GL_LIGHT_MODEL_LOCAL_VIEWER */
'/*      GL_LIGHT_MODEL_TWO_SIDE */

'/* LightName */
%GL_LIGHT0                         = &H4000???
%GL_LIGHT1                         = &H4001???
%GL_LIGHT2                         = &H4002???
%GL_LIGHT3                         = &H4003???
%GL_LIGHT4                         = &H4004???
%GL_LIGHT5                         = &H4005???
%GL_LIGHT6                         = &H4006???
%GL_LIGHT7                         = &H4007???

'/* LightParameter */
%GL_AMBIENT                        = &H1200???
%GL_DIFFUSE                        = &H1201???
%GL_SPECULAR                       = &H1202???
%GL_POSITION                       = &H1203???
%GL_SPOT_DIRECTION                 = &H1204???
%GL_SPOT_EXPONENT                  = &H1205???
%GL_SPOT_CUTOFF                    = &H1206???
%GL_CONSTANT_ATTENUATION           = &H1207???
%GL_LINEAR_ATTENUATION             = &H1208???
%GL_QUADRATIC_ATTENUATION          = &H1209???

'/* InterleavedArrays */
'/*      GL_V2F */
'/*      GL_V3F */
'/*      GL_C4UB_V2F */
'/*      GL_C4UB_V3F */
'/*      GL_C3F_V3F */
'/*      GL_N3F_V3F */
'/*      GL_C4F_N3F_V3F */
'/*      GL_T2F_V3F */
'/*      GL_T4F_V4F */
'/*      GL_T2F_C4UB_V3F */
'/*      GL_T2F_C3F_V3F */
'/*      GL_T2F_N3F_V3F */
'/*      GL_T2F_C4F_N3F_V3F */
'/*      GL_T4F_C4F_N3F_V4F */

'/* ListMode */
%GL_COMPILE                        = &H1300???
%GL_COMPILE_AND_EXECUTE            = &H1301???

'/* ListNameType */
'/*      GL_BYTE */
'/*      GL_UNSIGNED_BYTE */
'/*      GL_SHORT */
'/*      GL_UNSIGNED_SHORT */
'/*      GL_INT */
'/*      GL_UNSIGNED_INT */
'/*      GL_FLOAT */
'/*      GL_2_BYTES */
'/*      GL_3_BYTES */
'/*      GL_4_BYTES */

'/* LogicOp */
%GL_CLEAR                          = &H1500???
%GL_AND                            = &H1501???
%GL_AND_REVERSE                    = &H1502???
%GL_COPY                           = &H1503???
%GL_AND_INVERTED                   = &H1504???
%GL_NOOP                           = &H1505???
%GL_XOR                            = &H1506???
%GL_OR                             = &H1507???
%GL_NOR                            = &H1508???
%GL_EQUIV                          = &H1509???
%GL_INVERT                         = &H150A???
%GL_OR_REVERSE                     = &H150B???
%GL_COPY_INVERTED                  = &H150C???
%GL_OR_INVERTED                    = &H150D???
%GL_NAND                           = &H150E???
%GL_SET                            = &H150F???

'/* MapTarget */
'/*      GL_MAP1_COLOR_4 */
'/*      GL_MAP1_INDEX */
'/*      GL_MAP1_NORMAL */
'/*      GL_MAP1_TEXTURE_COORD_1 */
'/*      GL_MAP1_TEXTURE_COORD_2 */
'/*      GL_MAP1_TEXTURE_COORD_3 */
'/*      GL_MAP1_TEXTURE_COORD_4 */
'/*      GL_MAP1_VERTEX_3 */
'/*      GL_MAP1_VERTEX_4 */
'/*      GL_MAP2_COLOR_4 */
'/*      GL_MAP2_INDEX */
'/*      GL_MAP2_NORMAL */
'/*      GL_MAP2_TEXTURE_COORD_1 */
'/*      GL_MAP2_TEXTURE_COORD_2 */
'/*      GL_MAP2_TEXTURE_COORD_3 */
'/*      GL_MAP2_TEXTURE_COORD_4 */
'/*      GL_MAP2_VERTEX_3 */
'/*      GL_MAP2_VERTEX_4 */

'/* MaterialFace */
'/*      GL_FRONT */
'/*      GL_BACK */
'/*      GL_FRONT_AND_BACK */

'/* MaterialParameter */
%GL_EMISSION                       = &H1600???
%GL_SHININESS                      = &H1601???
%GL_AMBIENT_AND_DIFFUSE            = &H1602???
%GL_COLOR_INDEXES                  = &H1603???
'/*      GL_AMBIENT */
'/*      GL_DIFFUSE */
'/*      GL_SPECULAR */

'/* MatrixMode */
%GL_MODELVIEW                      = &H1700???
%GL_PROJECTION                     = &H1701???
%GL_TEXTURE                        = &H1702???

'/* MeshMode1 */
'/*      GL_POINT */
'/*      GL_LINE */

'/* MeshMode2 */
'/*      GL_POINT */
'/*      GL_LINE */
'/*      GL_FILL */

'/* NormalPointerType */
'/*      GL_BYTE */
'/*      GL_SHORT */
'/*      GL_INT */
'/*      GL_FLOAT */
'/*      GL_DOUBLE */

'/* PixelCopyType */
%GL_COLOR                          = &H1800???
%GL_DEPTH                          = &H1801???
%GL_STENCIL                        = &H1802???

'/* PixelFormat */
%GL_COLOR_INDEX                    = &H1900???
%GL_STENCIL_INDEX                  = &H1901???
%GL_DEPTH_COMPONENT                = &H1902???
%GL_RED                            = &H1903???
%GL_GREEN                          = &H1904???
%GL_BLUE                           = &H1905???
%GL_ALPHA                          = &H1906???
%GL_RGB                            = &H1907???
%GL_RGBA                           = &H1908???
%GL_LUMINANCE                      = &H1909???
%GL_LUMINANCE_ALPHA                = &H190A???

'/* PixelMap */
'/*      GL_PIXEL_MAP_I_TO_I */
'/*      GL_PIXEL_MAP_S_TO_S */
'/*      GL_PIXEL_MAP_I_TO_R */
'/*      GL_PIXEL_MAP_I_TO_G */
'/*      GL_PIXEL_MAP_I_TO_B */
'/*      GL_PIXEL_MAP_I_TO_A */
'/*      GL_PIXEL_MAP_R_TO_R */
'/*      GL_PIXEL_MAP_G_TO_G */
'/*      GL_PIXEL_MAP_B_TO_B */
'/*      GL_PIXEL_MAP_A_TO_A */

'/* PixelStore */
'/*      GL_UNPACK_SWAP_BYTES */
'/*      GL_UNPACK_LSB_FIRST */
'/*      GL_UNPACK_ROW_LENGTH */
'/*      GL_UNPACK_SKIP_ROWS */
'/*      GL_UNPACK_SKIP_PIXELS */
'/*      GL_UNPACK_ALIGNMENT */
'/*      GL_PACK_SWAP_BYTES */
'/*      GL_PACK_LSB_FIRST */
'/*      GL_PACK_ROW_LENGTH */
'/*      GL_PACK_SKIP_ROWS */
'/*      GL_PACK_SKIP_PIXELS */
'/*      GL_PACK_ALIGNMENT */

'/* PixelTransfer */
'/*      GL_MAP_COLOR */
'/*      GL_MAP_STENCIL */
'/*      GL_INDEX_SHIFT */
'/*      GL_INDEX_OFFSET */
'/*      GL_RED_SCALE */
'/*      GL_RED_BIAS */
'/*      GL_GREEN_SCALE */
'/*      GL_GREEN_BIAS */
'/*      GL_BLUE_SCALE */
'/*      GL_BLUE_BIAS */
'/*      GL_ALPHA_SCALE */
'/*      GL_ALPHA_BIAS */
'/*      GL_DEPTH_SCALE */
'/*      GL_DEPTH_BIAS */

'/* PixelType */
%GL_BITMAP                         = &H1A00???
'/*      GL_BYTE */
'/*      GL_UNSIGNED_BYTE */
'/*      GL_SHORT */
'/*      GL_UNSIGNED_SHORT */
'/*      GL_INT */
'/*      GL_UNSIGNED_INT */
'/*      GL_FLOAT */

'/* PolygonMode */
%GL_POINT                          = &H1B00???
%GL_LINE                           = &H1B01???
%GL_FILL                           = &H1B02???

'/* ReadBufferMode */
'/*      GL_FRONT_LEFT */
'/*      GL_FRONT_RIGHT */
'/*      GL_BACK_LEFT */
'/*      GL_BACK_RIGHT */
'/*      GL_FRONT */
'/*      GL_BACK */
'/*      GL_LEFT */
'/*      GL_RIGHT */
'/*      GL_AUX0 */
'/*      GL_AUX1 */
'/*      GL_AUX2 */
'/*      GL_AUX3 */

'/* RenderingMode */
%GL_RENDER                         = &H1C00???
%GL_FEEDBACK                       = &H1C01???
%GL_SELECT                         = &H1C02???

'/* ShadingModel */
%GL_FLAT                           = &H1D00???
%GL_SMOOTH                         = &H1D01???


'/* StencilFunction */
'/*      GL_NEVER */
'/*      GL_LESS */
'/*      GL_EQUAL */
'/*      GL_LEQUAL */
'/*      GL_GREATER */
'/*      GL_NOTEQUAL */
'/*      GL_GEQUAL */
'/*      GL_ALWAYS */

'/* StencilOp */
'/*      GL_ZERO */
%GL_KEEP                           = &H1E00???
%GL_REPLACE                        = &H1E01???
%GL_INCR                           = &H1E02???
%GL_DECR                           = &H1E03???
'/*      GL_INVERT */

'/* StringName */
%GL_VENDOR                         = &H1F00???
%GL_RENDERER                       = &H1F01???
%GL_VERSION                        = &H1F02???
%GL_EXTENSIONS                     = &H1F03???

'/* TextureCoordName */
%GL_S                              = &H2000???
%GL_T                              = &H2001???
%GL_R                              = &H2002???
%GL_Q                              = &H2003???

'/* TexCoordPointerType */
'/*      GL_SHORT */
'/*      GL_INT */
'/*      GL_FLOAT */
'/*      GL_DOUBLE */

'/* TextureEnvMode */
%GL_MODULATE                       = &H2100???
%GL_DECAL                          = &H2101???
'/*      GL_BLEND */
'/*      GL_REPLACE */

'/* TextureEnvParameter */
%GL_TEXTURE_ENV_MODE               = &H2200???
%GL_TEXTURE_ENV_COLOR              = &H2201???

'/* TextureEnvTarget */
%GL_TEXTURE_ENV                    = &H2300???

'/* TextureGenMode */
%GL_EYE_LINEAR                     = &H2400???
%GL_OBJECT_LINEAR                  = &H2401???
%GL_SPHERE_MAP                     = &H2402???

'/* TextureGenParameter */
%GL_TEXTURE_GEN_MODE               = &H2500???
%GL_OBJECT_PLANE                   = &H2501???
%GL_EYE_PLANE                      = &H2502???

'/* TextureMagFilter */
%GL_NEAREST                        = &H2600???
%GL_LINEAR                         = &H2601???

'/* TextureMinFilter */
'/*      GL_NEAREST */
'/*      GL_LINEAR */
%GL_NEAREST_MIPMAP_NEAREST         = &H2700???
%GL_LINEAR_MIPMAP_NEAREST          = &H2701???
%GL_NEAREST_MIPMAP_LINEAR          = &H2702???
%GL_LINEAR_MIPMAP_LINEAR           = &H2703???

'/* TextureParameterName */
%GL_TEXTURE_MAG_FILTER             = &H2800???
%GL_TEXTURE_MIN_FILTER             = &H2801???
%GL_TEXTURE_WRAP_S                 = &H2802???
%GL_TEXTURE_WRAP_T                 = &H2803???
'/*      GL_TEXTURE_BORDER_COLOR */
'/*      GL_TEXTURE_PRIORITY */

'/* TextureTarget */
'/*      GL_TEXTURE_1D */
'/*      GL_TEXTURE_2D */
'/*      GL_PROXY_TEXTURE_1D */
'/*      GL_PROXY_TEXTURE_2D */

'/* TextureWrapMode */
%GL_CLAMP                          = &H2900???
%GL_REPEAT                         = &H2901???

'/* VertexPointerType */
'/*      GL_SHORT */
'/*      GL_INT */
'/*      GL_FLOAT */
'/*      GL_DOUBLE */

'/* ClientAttribMask */
%GL_CLIENT_PIXEL_STORE_BIT         = &H00000001
%GL_CLIENT_VERTEX_ARRAY_BIT        = &H00000002
%GL_CLIENT_ALL_ATTRIB_BITS         = &Hffffffff

'/* polygon_offset */
%GL_POLYGON_OFFSET_FACTOR          = &H8038???
%GL_POLYGON_OFFSET_UNITS           = &H2A00???
%GL_POLYGON_OFFSET_POINT           = &H2A01???
%GL_POLYGON_OFFSET_LINE            = &H2A02???
%GL_POLYGON_OFFSET_FILL            = &H8037???

'/* texture */
%GL_ALPHA4                         = &H803B???
%GL_ALPHA8                         = &H803C???
%GL_ALPHA12                        = &H803D???
%GL_ALPHA16                        = &H803E???
%GL_LUMINANCE4                     = &H803F???
%GL_LUMINANCE8                     = &H8040???
%GL_LUMINANCE12                    = &H8041???
%GL_LUMINANCE16                    = &H8042???
%GL_LUMINANCE4_ALPHA4              = &H8043???
%GL_LUMINANCE6_ALPHA2              = &H8044???
%GL_LUMINANCE8_ALPHA8              = &H8045???
%GL_LUMINANCE12_ALPHA4             = &H8046???
%GL_LUMINANCE12_ALPHA12            = &H8047???
%GL_LUMINANCE16_ALPHA16            = &H8048???
%GL_INTENSITY                      = &H8049???
%GL_INTENSITY4                     = &H804A???
%GL_INTENSITY8                     = &H804B???
%GL_INTENSITY12                    = &H804C???
%GL_INTENSITY16                    = &H804D???
%GL_R3_G3_B2                       = &H2A10???
%GL_RGB4                           = &H804F???
%GL_RGB5                           = &H8050???
%GL_RGB8                           = &H8051???
%GL_RGB10                          = &H8052???
%GL_RGB12                          = &H8053???
%GL_RGB16                          = &H8054???
%GL_RGBA2                          = &H8055???
%GL_RGBA4                          = &H8056???
%GL_RGB5_A1                        = &H8057???
%GL_RGBA8                          = &H8058???
%GL_RGB10_A2                       = &H8059???
%GL_RGBA12                         = &H805A???
%GL_RGBA16                         = &H805B???
%GL_TEXTURE_RED_SIZE               = &H805C???
%GL_TEXTURE_GREEN_SIZE             = &H805D???
%GL_TEXTURE_BLUE_SIZE              = &H805E???
%GL_TEXTURE_ALPHA_SIZE             = &H805F???
%GL_TEXTURE_LUMINANCE_SIZE         = &H8060???
%GL_TEXTURE_INTENSITY_SIZE         = &H8061???
%GL_PROXY_TEXTURE_1D               = &H8063???
%GL_PROXY_TEXTURE_2D               = &H8064???

'/* texture_object */
%GL_TEXTURE_PRIORITY               = &H8066???
%GL_TEXTURE_RESIDENT               = &H8067???
%GL_TEXTURE_BINDING_1D             = &H8068???
%GL_TEXTURE_BINDING_2D             = &H8069???

'/* vertex_array */
%GL_VERTEX_ARRAY                   = &H8074???
%GL_NORMAL_ARRAY                   = &H8075???
%GL_COLOR_ARRAY                    = &H8076???
%GL_INDEX_ARRAY                    = &H8077???
%GL_TEXTURE_COORD_ARRAY            = &H8078???
%GL_EDGE_FLAG_ARRAY                = &H8079???
%GL_VERTEX_ARRAY_SIZE              = &H807A???
%GL_VERTEX_ARRAY_TYPE              = &H807B???
%GL_VERTEX_ARRAY_STRIDE            = &H807C???
%GL_NORMAL_ARRAY_TYPE              = &H807E???
%GL_NORMAL_ARRAY_STRIDE            = &H807F???
%GL_COLOR_ARRAY_SIZE               = &H8081???
%GL_COLOR_ARRAY_TYPE               = &H8082???
%GL_COLOR_ARRAY_STRIDE             = &H8083???
%GL_INDEX_ARRAY_TYPE               = &H8085???
%GL_INDEX_ARRAY_STRIDE             = &H8086???
%GL_TEXTURE_COORD_ARRAY_SIZE       = &H8088???
%GL_TEXTURE_COORD_ARRAY_TYPE       = &H8089???
%GL_TEXTURE_COORD_ARRAY_STRIDE     = &H808A???
%GL_EDGE_FLAG_ARRAY_STRIDE         = &H808C???
%GL_VERTEX_ARRAY_POINTER           = &H808E???
%GL_NORMAL_ARRAY_POINTER           = &H808F???
%GL_COLOR_ARRAY_POINTER            = &H8090???
%GL_INDEX_ARRAY_POINTER            = &H8091???
%GL_TEXTURE_COORD_ARRAY_POINTER    = &H8092???
%GL_EDGE_FLAG_ARRAY_POINTER        = &H8093???
%GL_V2F                            = &H2A20???
%GL_V3F                            = &H2A21???
%GL_C4UB_V2F                       = &H2A22???
%GL_C4UB_V3F                       = &H2A23???
%GL_C3F_V3F                        = &H2A24???
%GL_N3F_V3F                        = &H2A25???
%GL_C4F_N3F_V3F                    = &H2A26???
%GL_T2F_V3F                        = &H2A27???
%GL_T4F_V4F                        = &H2A28???
%GL_T2F_C4UB_V3F                   = &H2A29???
%GL_T2F_C3F_V3F                    = &H2A2A???
%GL_T2F_N3F_V3F                    = &H2A2B???
%GL_T2F_C4F_N3F_V3F                = &H2A2C???
%GL_T4F_C4F_N3F_V4F                = &H2A2D???

'/* Extensions */
%GL_EXT_vertex_array               = 1
%GL_EXT_bgra                       = 1
%GL_EXT_paletted_texture           = 1
%GL_WIN_swap_hint                  = 1
%GL_WIN_draw_range_elements        = 1
'// #define GL_WIN_phong_shading              1
'// #define GL_WIN_specular_fog               1

'/* EXT_vertex_array */
%GL_VERTEX_ARRAY_EXT               = &H8074???
%GL_NORMAL_ARRAY_EXT               = &H8075???
%GL_COLOR_ARRAY_EXT                = &H8076???
%GL_INDEX_ARRAY_EXT                = &H8077???
%GL_TEXTURE_COORD_ARRAY_EXT        = &H8078???
%GL_EDGE_FLAG_ARRAY_EXT            = &H8079???
%GL_VERTEX_ARRAY_SIZE_EXT          = &H807A???
%GL_VERTEX_ARRAY_TYPE_EXT          = &H807B???
%GL_VERTEX_ARRAY_STRIDE_EXT        = &H807C???
%GL_VERTEX_ARRAY_COUNT_EXT         = &H807D???
%GL_NORMAL_ARRAY_TYPE_EXT          = &H807E???
%GL_NORMAL_ARRAY_STRIDE_EXT        = &H807F???
%GL_NORMAL_ARRAY_COUNT_EXT         = &H8080???
%GL_COLOR_ARRAY_SIZE_EXT           = &H8081???
%GL_COLOR_ARRAY_TYPE_EXT           = &H8082???
%GL_COLOR_ARRAY_STRIDE_EXT         = &H8083???
%GL_COLOR_ARRAY_COUNT_EXT          = &H8084???
%GL_INDEX_ARRAY_TYPE_EXT           = &H8085???
%GL_INDEX_ARRAY_STRIDE_EXT         = &H8086???
%GL_INDEX_ARRAY_COUNT_EXT          = &H8087???
%GL_TEXTURE_COORD_ARRAY_SIZE_EXT   = &H8088???
%GL_TEXTURE_COORD_ARRAY_TYPE_EXT   = &H8089???
%GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = &H808A???
%GL_TEXTURE_COORD_ARRAY_COUNT_EXT  = &H808B???
%GL_EDGE_FLAG_ARRAY_STRIDE_EXT     = &H808C???
%GL_EDGE_FLAG_ARRAY_COUNT_EXT      = &H808D???
%GL_VERTEX_ARRAY_POINTER_EXT       = &H808E???
%GL_NORMAL_ARRAY_POINTER_EXT       = &H808F???
%GL_COLOR_ARRAY_POINTER_EXT        = &H8090???
%GL_INDEX_ARRAY_POINTER_EXT        = &H8091???
%GL_TEXTURE_COORD_ARRAY_POINTER_EXT = &H8092???
%GL_EDGE_FLAG_ARRAY_POINTER_EXT    = &H8093???
%GL_DOUBLE_EXT                     = %GL_DOUBLE

'/* EXT_bgra */
%GL_BGR_EXT                        = &H80E0???
%GL_BGRA_EXT                       = &H80E1???

'/* EXT_paletted_texture */

'/* These must match the GL_COLOR_TABLE_*_SGI enumerants */
%GL_COLOR_TABLE_FORMAT_EXT         = &H80D8???
%GL_COLOR_TABLE_WIDTH_EXT          = &H80D9???
%GL_COLOR_TABLE_RED_SIZE_EXT       = &H80DA???
%GL_COLOR_TABLE_GREEN_SIZE_EXT     = &H80DB???
%GL_COLOR_TABLE_BLUE_SIZE_EXT      = &H80DC???
%GL_COLOR_TABLE_ALPHA_SIZE_EXT     = &H80DD???
%GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = &H80DE???
%GL_COLOR_TABLE_INTENSITY_SIZE_EXT = &H80DF???

%GL_COLOR_INDEX1_EXT               = &H80E2???
%GL_COLOR_INDEX2_EXT               = &H80E3???
%GL_COLOR_INDEX4_EXT               = &H80E4???
%GL_COLOR_INDEX8_EXT               = &H80E5???
%GL_COLOR_INDEX12_EXT              = &H80E6???
%GL_COLOR_INDEX16_EXT              = &H80E7???

'/* WIN_draw_range_elements */
%GL_MAX_ELEMENTS_VERTICES_WIN      = &H80E8???
%GL_MAX_ELEMENTS_INDICES_WIN       = &H80E9???

'/* WIN_phong_shading */
%GL_PHONG_WIN                      = &H80EA???
%GL_PHONG_HINT_WIN                 = &H80EB???

'/* WIN_specular_fog */
%GL_FOG_SPECULAR_TEXTURE_WIN       = &H80EC???

'/* For compatibility with OpenGL v1.0 */
%GL_LOGIC_OP = %GL_INDEX_LOGIC_OP
%GL_TEXTURE_COMPONENTS = %GL_TEXTURE_INTERNAL_FORMAT

'/*************************************************************/

' ========================================================================================
' WINGDIAPI void APIENTRY glAccum (GLenum op, GLfloat value);
' Operates on the accumulation buffer.
' ========================================================================================
DECLARE SUB glAccum LIB "OPENGL32.DLL" ALIAS "glAccum" ( _
   BYVAL op AS DWORD _                                  ' GLenum op
 , BYVAL value AS SINGLE _                              ' GLfloat value
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glAlphaFunc (GLenum func, GLclampf ref);
' Enables your application to set the alpha test function.
' ========================================================================================
DECLARE SUB glAlphaFunc LIB "OPENGL32.DLL" ALIAS "glAlphaFunc" ( _
   BYVAL func AS DWORD _                                ' GLEnum func
 , BYVAL ref AS SINGLE _                                ' GLclampf ref
 )                                                      ' void

' ========================================================================================
' WINGDIAPI GLboolean APIENTRY glAreTexturesResident (GLsizei n, const GLuint *textures,
' GLboolean *residences);
' Determines whether specified texture objects are resident in texture memory.
' ========================================================================================
DECLARE FUNCTION glAreTexturesResident LIB "OPENGL32.DLL" ALIAS "glAreTexturesResident" ( _
   BYVAL n AS LONG _                                    ' GLsizei n
 , BYREF textures AS DWORD _                            ' const GLuint *textures
 , BYREF residences AS BYTE _                           ' GLboolean *residences
 ) AS BYTE                                              ' GLboolean

' ========================================================================================
' WINGDIAPI void APIENTRY glArrayElement (GLint i);
' Specifies the array elements used to render a vertex.
' ========================================================================================
DECLARE SUB glArrayElement LIB "OPENGL32.DLL" ALIAS "glArrayElement" ( _
   BYVAL i AS LONG _                                    ' GLint i
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glBegin (GLenum mode);
' The glBegin and glEnd functions delimit the vertices of a primitive or a group of like
' primitives.
' ========================================================================================
DECLARE SUB glBegin LIB "OPENGL32.DLL" ALIAS "glBegin" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glBindTexture (GLenum target, GLuint texture);
' Enables the creation of a named texture that is bound to a texture target.
' ========================================================================================
DECLARE SUB glBindTexture LIB "OPENGL32.DLL" ALIAS "glBindTexture" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL texture AS DWORD _                             ' GLuint texture
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glBitmap (GLsizei width, GLsizei height, GLfloat xorig,
' GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap);
' Draws a bitmap.
' ========================================================================================
DECLARE SUB glBitmap LIB "OPENGL32.DLL" ALIAS "glBitmap" ( _
   BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 , BYVAL xorig AS SINGLE _                              ' GLfloat xorig
 , BYVAL yorig AS SINGLE _                              ' GLfloat yorig
 , BYVAL xmove AS SINGLE _                              ' GLfloat xmove
 , BYVAL ymove AS SINGLE _                              ' GLfloat ymove
 , BYREF bitmap AS BYTE _                               ' const GLubyte *bitmap
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glBlendFunc (GLenum sfactor, GLenum dfactor);
' Specifies pixel arithmetic.
' ========================================================================================
DECLARE SUB glBlendFunc LIB "OPENGL32.DLL" ALIAS "glBlendFunc" ( _
   BYVAL sfactor AS DWORD _                             ' GLEnum sfactor
 , BYVAL dfactor AS DWORD _                             ' GLEnum dfactor
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glCallList (GLuint list);
' Executes a display list.
' ========================================================================================
DECLARE SUB glCallList LIB "OPENGL32.DLL" ALIAS "glCallList" ( _
   BYVAL list AS DWORD _                                ' GLuint list
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glCallLists (GLsizei n, GLenum type, const GLvoid *lists);
' Executes a list of display lists.
' ========================================================================================
DECLARE SUB glCallLists LIB "OPENGL32.DLL" ALIAS "glCallLists" ( _
   BYVAL n AS LONG _                                    ' GLsizei n
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF lists AS ANY _                                 ' const GLvoid *lists
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glClear (GLbitfield mask);
' Clears buffers to preset values.
' ========================================================================================
DECLARE SUB glClear LIB "OPENGL32.DLL" ALIAS "glClear" ( _
   BYVAL mask AS DWORD _                                ' GLbitfield mask
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glClearAccum (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
' Specifies the clear values for the accumulation buffer.
' ========================================================================================
DECLARE SUB glClearAccum LIB "OPENGL32.DLL" ALIAS "glClearAccum" ( _
   BYVAL red AS SINGLE _                                ' GLfloat red
 , BYVAL green AS SINGLE _                              ' GLfloat green
 , BYVAL blue AS SINGLE _                               ' GLfloat blue
 , BYVAL alpha AS SINGLE _                              ' GLfloat alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glClearColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
' Specifies clear values for the color buffers.
' ========================================================================================
DECLARE SUB glClearColor LIB "OPENGL32.DLL" ALIAS "glClearColor" ( _
   BYVAL red AS SINGLE _                                ' GLclampf red
 , BYVAL green AS SINGLE _                              ' GLclampf green
 , BYVAL blue AS SINGLE _                               ' GLclampf blue
 , BYVAL alpha AS SINGLE _                              ' GLclampf alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glClearDepth (GLclampd depth);
' Specifies the clear value for the depth buffer.
' ========================================================================================
DECLARE SUB glClearDepth LIB "OPENGL32.DLL" ALIAS "glClearDepth" ( _
   BYVAL depth AS DOUBLE _                              ' GLclampd depth
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glClearIndex (GLfloat c);
' Specifies the clear value for the color-index buffers.
' ========================================================================================
DECLARE SUB glClearIndex LIB "OPENGL32.DLL" ALIAS "glClearIndex" ( _
   BYVAL c AS SINGLE _                                  ' GLfloat c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glClearStencil (GLint s);
' Specifies the clear value for the stencil buffer.
' ========================================================================================
DECLARE SUB glClearStencil LIB "OPENGL32.DLL" ALIAS "glClearStencil" ( _
   BYVAL s AS LONG _                                    ' GLint s
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glClipPlane (GLenum plane, const GLdouble *equation);
' Specifies a plane against which all geometry is clipped.
' ========================================================================================
DECLARE SUB glClipPlane LIB "OPENGL32.DLL" ALIAS "glClipPlane" ( _
   BYVAL plane AS DWORD _                               ' GLEnum plane
 , BYREF equation AS DOUBLE _                           ' const GLdouble *equation
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3b (GLbyte red, GLbyte green, GLbyte blue);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3b LIB "OPENGL32.DLL" ALIAS "glColor3b" ( _
   BYVAL red AS BYTE _                                  ' GLbyte red
 , BYVAL green AS BYTE _                                ' GLbyte green
 , BYVAL blue AS BYTE _                                 ' GLbyte blue
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3bv (const GLbyte *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3bv LIB "OPENGL32.DLL" ALIAS "glColor3bv" ( _
   BYREF v AS BYTE _                                    ' GLbyte *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3d (GLdouble red, GLdouble green, GLdouble blue);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3d LIB "OPENGL32.DLL" ALIAS "glColor3d" ( _
   BYVAL red AS DOUBLE _                                ' GLdouble red
 , BYVAL green AS DOUBLE _                              ' GLdouble green
 , BYVAL blue AS DOUBLE _                               ' GLdouble blue
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3dv (const GLdouble *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3dv LIB "OPENGL32.DLL" ALIAS "glColor3dv" ( _
   BYREF v AS DOUBLE _                                  ' GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3f (GLfloat red, GLfloat green, GLfloat blue);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3f LIB "OPENGL32.DLL" ALIAS "glColor3f" ( _
   BYVAL red AS SINGLE _                                ' GLfloat red
 , BYVAL green AS SINGLE _                              ' GLfloat green
 , BYVAL blue AS SINGLE _                               ' GLfloat blue
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3fv (const GLfloat *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3fv LIB "OPENGL32.DLL" ALIAS "glColor3fv" ( _
   BYREF v AS SINGLE _                                  ' GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3i (GLint red, GLint green, GLint blue);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3i LIB "OPENGL32.DLL" ALIAS "glColor3i" ( _
   BYVAL red AS LONG _                                  ' GLint red
 , BYVAL green AS LONG _                                ' GLint green
 , BYVAL blue AS LONG _                                 ' GLint blue
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3iv (const GLint *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3iv LIB "OPENGL32.DLL" ALIAS "glColor3iv" ( _
   BYREF v AS LONG _                                    ' GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3s (GLshort red, GLshort green, GLshort blue);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3s LIB "OPENGL32.DLL" ALIAS "glColor3s" ( _
   BYVAL red AS INTEGER _                               ' GLshort red
 , BYVAL green AS INTEGER _                             ' GLshort green
 , BYVAL blue AS INTEGER _                              ' GLshort blue
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3sv (const GLshort *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3sv LIB "OPENGL32.DLL" ALIAS "glColor3sv" ( _
   BYREF v AS INTEGER _                                 ' GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3ub (GLubyte red, GLubyte green, GLubyte blue);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3ub LIB "OPENGL32.DLL" ALIAS "glColor3ub" ( _
   BYVAL red AS BYTE _                                  ' GLubyte red
 , BYVAL green AS BYTE _                                ' GLubyte green
 , BYVAL blue AS BYTE _                                 ' GLubyte blue
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3ubv (const GLubyte *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3ubv LIB "OPENGL32.DLL" ALIAS "glColor3ubv" ( _
   BYREF v AS BYTE _                                    ' GLubyte *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3ui (GLuint red, GLuint green, GLuint blue);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3ui LIB "OPENGL32.DLL" ALIAS "glColor3ui" ( _
   BYVAL red AS DWORD _                                 ' GLuint red
 , BYVAL green AS DWORD _                               ' GLuint green
 , BYVAL blue AS DWORD _                                ' GLuint blue
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3uiv (const GLuint *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3uiv LIB "OPENGL32.DLL" ALIAS "glColor3uiv" ( _
   BYREF v AS DWORD _                                   ' GLuint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3us (GLushort red, GLushort green, GLushort blue);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3us LIB "OPENGL32.DLL" ALIAS "glColor3us" ( _
   BYVAL red AS WORD _                                  ' GLushort red
 , BYVAL green AS WORD _                                ' GLushort green
 , BYVAL blue AS WORD _                                 ' GLushort blue
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor3usv (const GLushort *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor3usv LIB "OPENGL32.DLL" ALIAS "glColor3usv" ( _
   BYREF v AS WORD _                                    ' GLushort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4b (GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4b LIB "OPENGL32.DLL" ALIAS "glColor4b" ( _
   BYVAL red AS BYTE _                                  ' GLbyte red
 , BYVAL green AS BYTE _                                ' GLbyte green
 , BYVAL blue AS BYTE _                                 ' GLbyte blue
 , BYVAL alpha AS BYTE _                                ' GLbyte alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4bv (const GLbyte *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4bv LIB "OPENGL32.DLL" ALIAS "glColor4bv" ( _
   BYREF v AS BYTE _                                    ' GLbyte *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4d (GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4d LIB "OPENGL32.DLL" ALIAS "glColor4d" ( _
   BYVAL red AS DOUBLE _                                ' GLdouble red
 , BYVAL green AS DOUBLE _                              ' GLdouble green
 , BYVAL blue AS DOUBLE _                               ' GLdouble blue
 , BYVAL alpha AS DOUBLE _                              ' GLdouble alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4dv (const GLdouble *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4dv LIB "OPENGL32.DLL" ALIAS "glColor4dv" ( _
   BYREF v AS DOUBLE _                                  ' GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4f (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4f LIB "OPENGL32.DLL" ALIAS "glColor4f" ( _
   BYVAL red AS SINGLE _                                ' GLfloat red
 , BYVAL green AS SINGLE _                              ' GLfloat green
 , BYVAL blue AS SINGLE _                               ' GLfloat blue
 , BYVAL alpha AS SINGLE _                              ' GLfloat alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4fv (const GLfloat *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4fv LIB "OPENGL32.DLL" ALIAS "glColor4fv" ( _
   BYREF v AS SINGLE _                                  ' GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4i (GLint red, GLint green, GLint blue, GLint alpha);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4i LIB "OPENGL32.DLL" ALIAS "glColor4i" ( _
   BYVAL red AS LONG _                                  ' GLint red
 , BYVAL green AS LONG _                                ' GLint green
 , BYVAL blue AS LONG _                                 ' GLint blue
 , BYVAL alpha AS LONG _                                ' GLint alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4iv (const GLint *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4iv LIB "OPENGL32.DLL" ALIAS "glColor4iv" ( _
   BYREF v AS LONG _                                    ' GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4s (GLshort red, GLshort green, GLshort blue, GLshort alpha);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4s LIB "OPENGL32.DLL" ALIAS "glColor4s" ( _
   BYVAL red AS INTEGER _                               ' GLshort red
 , BYVAL green AS INTEGER _                             ' GLshort green
 , BYVAL blue AS INTEGER _                              ' GLshort blue
 , BYVAL alpha AS INTEGER _                             ' GLshort alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4sv (const GLshort *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4sv LIB "OPENGL32.DLL" ALIAS "glColor4sv" ( _
   BYREF v AS INTEGER _                                 ' GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4ub (GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4ub LIB "OPENGL32.DLL" ALIAS "glColor4ub" ( _
   BYVAL red AS BYTE _                                  ' GLubyte red
 , BYVAL green AS BYTE _                                ' GLubyte green
 , BYVAL blue AS BYTE _                                 ' GLubyte blue
 , BYVAL alpha AS BYTE _                                ' GLubyte alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4ubv (const GLubyte *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4ubv LIB "OPENGL32.DLL" ALIAS "glColor4ubv" ( _
   BYREF v AS BYTE _                                    ' GLubyte *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4ui (GLuint red, GLuint green, GLuint blue, GLuint alpha);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4ui LIB "OPENGL32.DLL" ALIAS "glColor4ui" ( _
   BYVAL red AS DWORD _                                 ' GLuint red
 , BYVAL green AS DWORD _                               ' GLuint green
 , BYVAL blue AS DWORD _                                ' GLuint blue
 , BYVAL alpha AS DWORD _                               ' GLuint alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4uiv (const GLuint *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4uiv LIB "OPENGL32.DLL" ALIAS "glColor4uiv" ( _
   BYREF v AS DWORD _                                   ' GLuint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4us (GLushort red, GLushort green, GLushort blue, GLushort alpha);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4us LIB "OPENGL32.DLL" ALIAS "glColor4us" ( _
   BYVAL red AS WORD _                                  ' GLushort red
 , BYVAL green AS WORD _                                ' GLushort green
 , BYVAL blue AS WORD _                                 ' GLushort blue
 , BYVAL alpha AS WORD _                                ' GLushort alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColor4usv (const GLushort *v);
' Sets the current color.
' ========================================================================================
DECLARE SUB glColor4usv LIB "OPENGL32.DLL" ALIAS "glColor4usv" ( _
   BYREF v AS WORD _                                    ' GLushort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
' Enables and disables writing of frame-buffer color components.
' ========================================================================================
DECLARE SUB glColorMask LIB "OPENGL32.DLL" ALIAS "glColorMask" ( _
   BYVAL red AS BYTE _                                  ' GLboolean red
 , BYVAL green AS BYTE _                                ' GLboolean green
 , BYVAL blue AS BYTE _                                 ' GLboolean blue
 , BYVAL alpha AS BYTE _                                ' GLboolean alpha
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColorMaterial (GLenum face, GLenum mode);
' Causes a material color to track the current color.
' ========================================================================================
DECLARE SUB glColorMaterial LIB "OPENGL32.DLL" ALIAS "glColorMaterial" ( _
   BYVAL face AS DWORD _                                ' GLEnum face
 , BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glColorPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
' Defines an array of colors.
' ========================================================================================
DECLARE SUB glColorPointer LIB "OPENGL32.DLL" ALIAS "glColorPointer" ( _
   BYVAL size AS LONG _                                 ' GLint size
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYVAL stride AS LONG _                               ' GLsizei stride
 , BYREF pointer AS ANY _                               ' const GLvoid *pointer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glCopyPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum type);
' Copies pixels in the framebuffer.
' ========================================================================================
DECLARE SUB glCopyPixels LIB "OPENGL32.DLL" ALIAS "glCopyPixels" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 , BYVAL type AS DWORD _                                ' GLEnum type
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level,
' GLEnum internalFormat, GLint x, GLint y, GLsizei width, GLint border);
' Copies pixels from the framebuffer into a one-dimensional texture image.
' ========================================================================================
DECLARE SUB glCopyTexImage1D LIB "OPENGL32.DLL" ALIAS "glCopyTexImage1D" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL internalFormat AS DWORD _                      ' GLEnum internalFormat
 , BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL border AS LONG _                               ' GLint border
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level,
' GLEnum internalFormat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
' Copies pixels from the framebuffer into a two-dimensional texture image.
' ========================================================================================
DECLARE SUB glCopyTexImage2D LIB "OPENGL32.DLL" ALIAS "glCopyTexImage2D" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL internalFormat AS DWORD _                      ' GLEnum internalFormat
 , BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 , BYVAL border AS LONG _                               ' GLint border
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset,
' GLint x, GLint y, GLsizei width);
' Copies a sub-image of a one-dimensional texture image from the framebuffer.
' ========================================================================================
DECLARE SUB glCopyTexSubImage1D LIB "OPENGL32.DLL" ALIAS "glCopyTexSubImage1D" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL xoffset AS LONG _                              ' GLint xoffset
 , BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL width AS LONG _                                ' GLsizei width
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset,
' GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
' Copies a sub-image of a two-dimensional texture image from the framebuffer.
' ========================================================================================
DECLARE SUB glCopyTexSubImage2D LIB "OPENGL32.DLL" ALIAS "glCopyTexSubImage2D" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL xoffset AS LONG _                              ' GLint xoffset
 , BYVAL yoffset AS LONG _                              ' GLint yoffset
 , BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glCullFace (GLenum mode);
' Specifies whether front- or back-facing facets can be culled.
' ========================================================================================
DECLARE SUB glCullFace LIB "OPENGL32.DLL" ALIAS "glCullFace" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDeleteLists (GLuint list, GLsizei range);
' Deletes a contiguous group of display lists.
' ========================================================================================
DECLARE SUB glDeleteLists LIB "OPENGL32.DLL" ALIAS "glDeleteLists" ( _
   BYVAL list AS DWORD _                                ' GLuint list
 , BYVAL range AS LONG _                                ' GLsizei range
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
' Deletes named textures.
' ========================================================================================
DECLARE SUB glDeleteTextures LIB "OPENGL32.DLL" ALIAS "glDeleteTextures" ( _
   BYVAL n AS LONG _                                    ' GLsizei n
 , BYREF textures AS DWORD _                            ' const GLuint *textures
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDepthFunc (GLenum func);
' Specifies the value used for depth-buffer comparisons.
' ========================================================================================
DECLARE SUB glDepthFunc LIB "OPENGL32.DLL" ALIAS "glDepthFunc" ( _
   BYVAL func AS DWORD _                                ' GLEnum func
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDepthMask (GLboolean flag);
' Enables or disables writing into the depth buffer.
' ========================================================================================
DECLARE SUB glDepthMask LIB "OPENGL32.DLL" ALIAS "glDepthMask" ( _
   BYVAL flag AS BYTE _                                 ' GLboolean flag
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDepthRange (GLclampd zNear, GLclampd zFar);
' Specifies the mapping of z values from normalized device coordinates to window coordinates.
' ========================================================================================
DECLARE SUB glDepthRange LIB "OPENGL32.DLL" ALIAS "glDepthRange" ( _
   BYVAL zNear AS DOUBLE _                              ' GLclampd zNear
 , BYVAL zFar AS DOUBLE _                               ' GLclampd zFar
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDisable (GLenum cap);
' Disables an OpenGL capability.
' ========================================================================================
DECLARE SUB glDisable LIB "OPENGL32.DLL" ALIAS "glDisable" ( _
   BYVAL cap AS DWORD _                                 ' GLEnum cap
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDisableClientState (GLenum array);
' Disables an array.
' ========================================================================================
DECLARE SUB glDisableClientState LIB "OPENGL32.DLL" ALIAS "glDisableClientState" ( _
   BYVAL array AS DWORD _                               ' GLEnum array
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
' Specifies multiple primitives to render.
' ========================================================================================
DECLARE SUB glDrawArrays LIB "OPENGL32.DLL" ALIAS "glDrawArrays" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 , BYVAL first AS LONG _                                ' GLint first
 , BYVAL count AS LONG _                                ' GLsizei count
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDrawBuffer (GLenum mode);
' Specifies which color buffers are to be drawn into.
' ========================================================================================
DECLARE SUB glDrawBuffer LIB "OPENGL32.DLL" ALIAS "glDrawBuffer" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type,
' const GLvoid *indices);
' Renders primitives from array data.
' ========================================================================================
DECLARE SUB glDrawElements LIB "OPENGL32.DLL" ALIAS "glDrawElements" ( _
   BYVAL mode aS DWORD _                                ' GLEnum mode
 , BYVAL count AS LONG _                                ' GLsizei count
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF indices AS ANY _                               ' GLvoid *indices
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glDrawPixels (GLsizei width, GLsizei height, GLenum format,
' GLEnum type, const GLvoid *pixels);
' Writes a block of pixels to the framebuffer.
' ========================================================================================
DECLARE SUB glDrawPixels LIB "OPENGL32.DLL" ALIAS "glDrawPixels" ( _
   BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF pixels AS ANY _                                ' GLvoid *pixels
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEdgeFlag (GLboolean flag);
' Flag edges as either boundary or nonboundary.
' ========================================================================================
DECLARE SUB glEdgeFlag LIB "OPENGL32.DLL" ALIAS "glEdgeFlag" ( _
   BYVAL flag AS BYTE _                                 ' GLboolean flag
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEdgeFlag (GLboolean flag);
' Flag edges as either boundary or nonboundary.
' ========================================================================================
DECLARE SUB glEdgeFlagPointer LIB "OPENGL32.DLL" ALIAS "glEdgeFlagPointer" ( _
   BYVAL stride AS LONG _                               ' GLsizei stride
 , BYREF pointer AS ANY _                               ' GLvoid *pointer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEdgeFlagv (const GLboolean *flag);
' Flag edges as either boundary or nonboundary.
' ========================================================================================
DECLARE SUB glEdgeFlagv LIB "OPENGL32.DLL" ALIAS "glEdgeFlagv" ( _
   BYREF flag AS BYTE _                                 ' *GLboolean *flag
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEnable (GLenum cap);
' Enables an OpenGL capability.
' ========================================================================================
DECLARE SUB glEnable LIB "OPENGL32.DLL" ALIAS "glEnable" ( _
   BYVAL cap AS DWORD _                                 ' GLEnum cap
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEnableClientState (GLenum array);
' Enables an array.
' ========================================================================================
DECLARE SUB glEnableClientState LIB "OPENGL32.DLL" ALIAS "glEnableClientState" ( _
   BYVAL array AS DWORD _                               ' GLEnum array
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEnd (void);
' The glBegin and glEnd functions delimit the vertices of a primitive or a group of like
' primitives.
' ========================================================================================
DECLARE SUB glEnd LIB "OPENGL32.DLL" ALIAS "glEnd" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEndList (void);
' The glNewList and glEndList functions create or replace a display list.
' ========================================================================================
DECLARE SUB glEndList LIB "OPENGL32.DLL" ALIAS "glEndList" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalCoord1d (GLdouble u);
' Evaluates enabled one-dimensional maps.
' ========================================================================================
DECLARE SUB glEvalCoord1d  LIB "OPENGL32.DLL" ALIAS "glEvalCoord1d" ( _
   BYVAL u AS DOUBLE _                                  ' GLdouble u
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalCoord1dv (const GLdouble *u);
' Evaluates enabled one-dimensional maps.
' ========================================================================================
DECLARE SUB glEvalCoord1dv LIB "OPENGL32.DLL" ALIAS "glEvalCoord1dv" ( _
   BYREF u AS DOUBLE _                                  ' const GLdouble *u
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalCoord1f (GLfloat u);
' Evaluates enabled one-dimensional maps.
' ========================================================================================
DECLARE SUB glEvalCoord1f LIB "OPENGL32.DLL" ALIAS "glEvalCoord1f" ( _
   BYVAL u AS SINGLE _                                  ' GLfloat u
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalCoord1fv (const GLfloat *u);
' Evaluates enabled one-dimensional maps.
' ========================================================================================
DECLARE SUB glEvalCoord1fv LIB "OPENGL32.DLL" ALIAS "glEvalCoord1fv" ( _
   BYREF u AS SINGLE _                                  ' const GLfloat *u
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalCoord2d (GLdouble u, GLdouble v);
' Evaluates enabled two-dimensional maps.
' ========================================================================================
DECLARE SUB glEvalCoord2d LIB "OPENGL32.DLL" ALIAS "glEvalCoord2d" ( _
   BYVAL u AS DOUBLE _                                  ' GLdouble u
 , BYVAL v AS DOUBLE _                                  ' GLdouble v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalCoord2dv (const GLdouble *u);
' Evaluates enabled two-dimensional maps.
' ========================================================================================
DECLARE SUB glEvalCoord2dv LIB "OPENGL32.DLL" ALIAS "glEvalCoord2dv" ( _
   BYREF u AS DOUBLE _                                  ' const GLdouble *u
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalCoord2f (GLfloat u, GLfloat v);
' Evaluates enabled two-dimensional maps.
' ========================================================================================
DECLARE SUB glEvalCoord2f LIB "OPENGL32.DLL" ALIAS "glEvalCoord2f" ( _
   BYVAL u AS SINGLE _                                  ' GLfloat u
 , BYVAL v AS SINGLE _                                  ' GLfloat v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalCoord2fv (const GLfloat *u);
' Evaluates enabled two-dimensional maps.
' ========================================================================================
DECLARE SUB glEvalCoord2fv LIB "OPENGL32.DLL" ALIAS "glEvalCoord2fv" ( _
   BYREF u AS SINGLE _                                  ' const GLfloat *u
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalMesh1 (GLenum mode, GLint i1, GLint i2);
' Compute a one-dimensional grid of points or lines.
' ========================================================================================
DECLARE SUB glEvalMesh1 LIB "OPENGL32.DLL" ALIAS "glEvalMesh1" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 , BYVAL i1 AS LONG _                                   ' GLint i1
 , BYVAL i2 AS LONG _                                   ' GLint i2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalMesh2 (GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2);
' Compute a two-dimensional grid of points or lines.
' ========================================================================================
DECLARE SUB glEvalMesh2 LIB "OPENGL32.DLL" ALIAS "glEvalMesh2" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 , BYVAL i1 AS LONG _                                   ' GLint i1
 , BYVAL i2 AS LONG _                                   ' GLint i2
 , BYVAL j1 AS LONG _                                   ' GLint j1
 , BYVAL j2 AS LONG _                                   ' GLint j2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalPoint1 (GLint i);
' Generate and evaluate a single point in a mesh.
' ========================================================================================
DECLARE SUB glEvalPoint1 LIB "OPENGL32.DLL" ALIAS "glEvalPoint1" ( _
   BYVAL i AS LONG _                                    ' GLint i
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glEvalPoint2 (GLint i, GLint j);
' Generate and evaluate a single point in a mesh.
' ========================================================================================
DECLARE SUB glEvalPoint2 LIB "OPENGL32.DLL" ALIAS "glEvalPoint2" ( _
   BYVAL i AS LONG _                                    ' GLint i
 , BYVAL j AS LONG _                                    ' GLint j
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFeedbackBuffer (GLsizei size, GLenum type, GLfloat *buffer);
' controls feedback mode.
' ========================================================================================
DECLARE SUB glFeedbackBuffer LIB "OPENGL32.DLL" ALIAS "glFeedbackBuffer" ( _
   BYVAL size AS LONG _                                 ' GLsizei size
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF buffer AS SINGLE _                             ' GLfloat *buffer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFinish (void);
' Blocks until all OpenGL execution is complete.
' ========================================================================================
DECLARE SUB glFinish LIB "OPENGL32.DLL" ALIAS "glFinish" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFlush (void);
' Forces execution of OpenGL functions in finite time.
' ========================================================================================
DECLARE SUB glFlush LIB "OPENGL32.DLL" ALIAS "glFlush" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFogf (GLenum pname, GLfloat param);
' Specify fog parameters.
' ========================================================================================
DECLARE SUB glFogf LIB "OPENGL32.DLL" ALIAS "glFogf" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFogfv (GLenum pname, const GLfloat *params);
' Specify fog parameters.
' ========================================================================================
DECLARE SUB glFogfv LIB "OPENGL32.DLL" ALIAS "glFogfv" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' const GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFogi (GLenum pname, GLint param);
' Specify fog parameters.
' ========================================================================================
DECLARE SUB glFogi LIB "OPENGL32.DLL" ALIAS "glFogi" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFogiv (GLenum pname, const GLint *params);
' Specify fog parameters.
' ========================================================================================
DECLARE SUB glFogiv LIB "OPENGL32.DLL" ALIAS "glFogiv" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFrontFace (GLenum mode);
' Defines front- and back-facing polygons.
' ========================================================================================
DECLARE SUB glFrontFace LIB "OPENGL32.DLL" ALIAS "glFrontFace" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glFrustum (GLdouble left, GLdouble right, GLdouble bottom,
' GLdouble top, GLdouble zNear, GLdouble zFar);
' Multiplies the current matrix by a perspective matrix.
' ========================================================================================
DECLARE SUB glFrustum LIB "OPENGL32.DLL" ALIAS "glFrustum" ( _
   BYVAL left AS DOUBLE _                               ' GLdouble left
 , BYVAL right AS DOUBLE _                              ' GLdouble right
 , BYVAL bottom AS DOUBLE _                             ' GLdouble bottom
 , BYVAL top AS DOUBLE _                                ' GLdouble top
 , BYVAL zNear AS DOUBLE _                              ' GLdouble zNear
 , BYVAL zFar AS DOUBLE _                               ' GLdouble zFar
 )                                                      ' void

' ========================================================================================
' WINGDIAPI GLuint APIENTRY glGenLists (GLsizei range);
' Generates a contiguous set of empty display lists.
' ========================================================================================
DECLARE FUNCTION glGenLists LIB "OPENGL32.DLL" ALIAS "glGenLists" ( _
   BYVAL range AS LONG _                                ' GLsizei range
 ) AS DWORD                                             ' GLuint

' ========================================================================================
' WINGDIAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
' Generates texture names.
' ========================================================================================
DECLARE SUB glGenTextures LIB "OPENGL32.DLL" ALIAS "glGenTextures" ( _
   BYVAL n AS LONG _                                    ' GLsizei n
 , BYREF textures AS DWORD _                            ' GLuint *textures
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetBooleanv (GLenum pname, GLboolean *params);
' Return the value or values of a selected parameter.
' ========================================================================================
DECLARE SUB glGetBooleanv LIB "OPENGL32.DLL" ALIAS "glGetBooleanv" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS BYTE _                               ' GLboolean *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetClipPlane (GLenum plane, GLdouble *equation);
' Returns the coefficients of the specified clipping plane.
' ========================================================================================
DECLARE SUB glGetClipPlane LIB "OPENGL32.DLL" ALIAS "glGetClipPlane" ( _
   BYVAL plane AS DWORD _                               ' GLEnum plane
 , BYREF equation AS DOUBLE _                           ' GLdouble *equation
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetDoublev (GLenum pname, GLdouble *params);
' Return the value or values of a selected parameter.
' ========================================================================================
DECLARE SUB glGetDoublev LIB "OPENGL32.DLL" ALIAS "glGetDoublev" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS DOUBLE _                             ' GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI GLenum APIENTRY glGetError (void);
' Returns error information.
' ========================================================================================
DECLARE FUNCTION glGetError LIB "OPENGL32.DLL" ALIAS "glGetError" ( _
 ) AS DWORD                                             ' GLenum

' ========================================================================================
' WINGDIAPI void APIENTRY glGetFloatv (GLenum pname, GLfloat *params);
' Return the value or values of a selected parameter.
' ========================================================================================
DECLARE SUB glGetFloatv LIB "OPENGL32.DLL" ALIAS "glGetFloatv" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetIntegerv (GLenum pname, GLint *params);
' Return the value or values of a selected parameter.
' ========================================================================================
DECLARE SUB glGetIntegerv LIB "OPENGL32.DLL" ALIAS "glGetIntegerv" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetLightfv (GLenum light, GLenum pname, GLfloat *params);
' Return light source parameter values.
' ========================================================================================
DECLARE SUB glGetLightfv LIB "OPENGL32.DLL" ALIAS "glGetLightfv" ( _
   BYVAL light AS DWORD _                               ' GLEnum light
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetLightiv (GLenum light, GLenum pname, GLint *params);
' Return light source parameter values.
' ========================================================================================
DECLARE SUB glGetLightiv LIB "OPENGL32.DLL" ALIAS "glGetLightiv" ( _
   BYVAL light AS DWORD _                               ' GLEnum light
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetMapdv (GLenum target, GLenum query, GLdouble *v);
' Return evaluator parameters.
' ========================================================================================
DECLARE SUB glGetMapdv LIB "OPENGL32.DLL" ALIAS "glGetMapdv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL query AS DWORD _                               ' GLEnum query
 , BYREF v AS DOUBLE _                                  ' GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetMapfv (GLenum target, GLenum query, GLfloat *v);
' Return evaluator parameters.
' ========================================================================================
DECLARE SUB glGetMapfv LIB "OPENGL32.DLL" ALIAS "glGetMapfv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL query AS DWORD _                               ' GLEnum query
 , BYREF v AS SINGLE _                                  ' GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetMapiv (GLenum target, GLenum query, GLint *v);
' Return evaluator parameters.
' ========================================================================================
DECLARE SUB glGetMapiv LIB "OPENGL32.DLL" ALIAS "glGetMapiv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL query AS DWORD _                               ' GLEnum query
 , BYREF v AS LONG _                                    ' GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetMaterialfv (GLenum face, GLenum pname, GLfloat *params);
' Return material parameters.
' ========================================================================================
DECLARE SUB glGetMaterialfv LIB "OPENGL32.DLL" ALIAS "glGetMaterialfv" ( _
   BYVAL face AS DWORD _                                ' GLEnum face
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetMaterialiv (GLenum face, GLenum pname, GLint *params);
' Return material parameters.
' ========================================================================================
DECLARE SUB glGetMaterialiv LIB "OPENGL32.DLL" ALIAS "glGetMaterialiv" ( _
   BYVAL face AS DWORD _                                ' GLEnum face
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF LONG _                                         ' GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetPixelMapfv (GLenum map, GLfloat *values);
' Return the specified pixel map.
' ========================================================================================
DECLARE SUB glGetPixelMapfv LIB "OPENGL32.DLL" ALIAS "glGetPixelMapfv" ( _
   BYVAL map AS DWORD _                                 ' GLEnum map
 , BYREF values AS SINGLE _                             ' GLfloat *values
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetPixelMapuiv (GLenum map, GLuint *values);
' Return the specified pixel map.
' ========================================================================================
DECLARE SUB glGetPixelMapuiv LIB "OPENGL32.DLL" ALIAS "glGetPixelMapuiv" ( _
   BYVAL map AS DWORD _                                 ' GLEnum map
 , BYREF values AS DWORD _                              ' GLuint *values
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetPixelMapusv (GLenum map, GLushort *values);
' Return the specified pixel map.
' ========================================================================================
DECLARE SUB glGetPixelMapusv LIB "OPENGL32.DLL" ALIAS "glGetPixelMapusv" ( _
   BYVAL map AS DWORD _                                 ' GLEnum map
 , BYREF values AS WORD _                               ' GLushort *values
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetPointerv (GLenum pname, GLvoid* *params);
' Returns the address of a vertex data array.
' ========================================================================================
DECLARE SUB glGetPointerv LIB "OPENGL32.DLL" ALIAS "glGetPointerv" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS ANY _                                ' GLvoid *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetPolygonStipple (GLubyte *mask);
' Returns the polygon stipple pattern.
' ========================================================================================
DECLARE SUB glGetPolygonStipple LIB "OPENGL32.DLL" ALIAS "glGetPolygonStipple" ( _
   BYREF mask AS BYTE _                                 ' GLubyte *mask
 )                                                      ' void

' ========================================================================================
' WINGDIAPI const GLubyte * APIENTRY glGetString (GLenum name);
' Returns a string describing the current OpenGL connection.
' ========================================================================================
DECLARE FUNCTION glGetString LIB "OPENGL32.DLL" ALIAS "glGetString" ( _
   BYVAL name AS DWORD _                                ' GLEnum name
 ) AS DWORD                                             ' const *GLubyte

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexEnvfv (GLenum target, GLenum pname, GLfloat *params);
' Return texture environment parameters.
' ========================================================================================
DECLARE SUB glGetTexEnvfv LIB "OPENGL32.DLL" ALIAS "glGetTexEnvfv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexEnviv (GLenum target, GLenum pname, GLint *params);
' Return texture environment parameters.
' ========================================================================================
DECLARE SUB glGetTexEnviv LIB "OPENGL32.DLL" ALIAS "glGetTexEnviv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexGendv (GLenum coord, GLenum pname, GLdouble *params);
' Return texture coordinate generation parameters.
' ========================================================================================
DECLARE SUB glGetTexGendv LIB "OPENGL32.DLL" ALIAS "glGetTexGendv" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS DOUBLE _                             ' GLdouble *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexGenfv (GLenum coord, GLenum pname, GLfloat *params);
' Return texture coordinate generation parameters.
' ========================================================================================
DECLARE SUB glGetTexGenfv LIB "OPENGL32.DLL" ALIAS "glGetTexGenfv" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexGeniv (GLenum coord, GLenum pname, GLint *params);
' Return texture coordinate generation parameters.
' ========================================================================================
DECLARE SUB glGetTexGeniv LIB "OPENGL32.DLL" ALIAS "glGetTexGeniv" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format,
' GLEnum type, GLvoid *pixels);
' Returns a texture image.
' ========================================================================================
DECLARE SUB glGetTexImage LIB "OPENGL32.DLL" ALIAS "glGetTexImage" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF pixels AS ANY _                                ' GLvoid *pixels
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level,
' GLEnum pname, GLfloat *params);
' Return texture parameter values for a specific level of detail.
' ========================================================================================
DECLARE SUB glGetTexLevelParameterfv LIB "OPENGL32.DLL" ALIAS "glGetTexLevelParameterfv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level,
' GLEnum pname, GLint *params);
' Return texture parameter values for a specific level of detail.
' ========================================================================================
DECLARE SUB glGetTexLevelParameteriv LIB "OPENGL32.DLL" ALIAS "glGetTexLevelParameteriv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
' Return texture parameter values.
' ========================================================================================
DECLARE SUB glGetTexParameterfv LIB "OPENGL32.DLL" ALIAS "glGetTexParameterfv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
' Return texture parameter values.
' ========================================================================================
DECLARE SUB glGetTexParameteriv LIB "OPENGL32.DLL" ALIAS "glGetTexParameteriv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glHint (GLenum target, GLenum mode);
' Specifies implementation-specific hints.
' ========================================================================================
DECLARE SUB glHint LIB "OPENGL32.DLL" ALIAS "glHint" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexMask (GLuint mask);
' Controls the writing of individual bits in the color-index buffers.
' ========================================================================================
DECLARE SUB glIndexMask LIB "OPENGL32.DLL" ALIAS "glIndexMask" ( _
   BYVAL mask AS DWORD _                                ' GLuint mask
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
' Defines an array of color indexes.
' ========================================================================================
DECLARE SUB glIndexPointer LIB "OPENGL32.DLL" ALIAS "glIndexPointer" ( _
   BYVAL type AS DWORD _                                ' GLEnum type
 , BYVAL stride AS LONG _                               ' GLsizei stride
 , BYREF pointer AS ANY _                               ' const GLvoid *pointer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexd (GLdouble c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexd LIB "OPENGL32.DLL" ALIAS "glIndexd" ( _
   BYVAL c AS DOUBLE _                                  ' GLdouble c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexdv (const GLdouble *c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexdv LIB "OPENGL32.DLL" ALIAS "glIndexdv" ( _
   BYREF c AS DOUBLE _                                  ' const GLdouble *c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexf (GLfloat c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexf LIB "OPENGL32.DLL" ALIAS "glIndexf" ( _
   BYVAL c AS SINGLE _                                  ' GLfloat c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexfv (const GLfloat *c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexfv LIB "OPENGL32.DLL" ALIAS "glIndexfv" ( _
   BYREF c AS SINGLE _                                  ' *GLfloat c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexi (GLint c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexi LIB "OPENGL32.DLL" ALIAS "glIndexi" ( _
   BYVAL c AS LONG _                                    ' GLint c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexiv (const GLint *c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexiv LIB "OPENGL32.DLL" ALIAS "glIndexiv" ( _
   BYREF c AS LONG _                                    ' const GLint *c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexs (GLshort c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexs LIB "OPENGL32.DLL" ALIAS "glIndexs" ( _
   BYVAL c AS INTEGER _                                 ' GLshort c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexsv (const GLshort *c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexsv LIB "OPENGL32.DLL" ALIAS "glIndexsv" ( _
   BYREF c AS INTEGER _                                 ' const GLshort *c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexub (GLubyte c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexub LIB "OPENGL32.DLL" ALIAS "glIndexub" ( _
   BYVAL c AS BYTE _                                    ' GLubyte c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);
' Sets the current color index.
' ========================================================================================
DECLARE SUB glIndexubv LIB "OPENGL32.DLL" ALIAS "glIndexubv" ( _
   BYREF c AS BYTE _                                    ' const GLubyte *c
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glInitNames (void);
' Initializes the name stack.
' ========================================================================================
DECLARE SUB glInitNames LIB "OPENGL32.DLL" ALIAS "glInitNames" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glInterleavedArrays (GLenum format, GLsizei stride, const GLvoid *pointer);
' Simultaneously specifies and enables several interleaved arrays in a larger aggregate array.
' ========================================================================================
DECLARE SUB glInterleavedArrays LIB "OPENGL32.DLL" ALIAS "glInterleavedArrays" ( _
   BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL stride AS LONG _                               ' GLsizei stride
 , BYREF pointer AS ANY _                               ' const GLvoid *pointer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
' Tests whether a capability is enabled.
' ========================================================================================
DECLARE FUNCTION glIsEnabled LIB "OPENGL32.DLL" ALIAS "glIsEnabled" ( _
   BYVAL cap AS DWORD _                                 ' GLEnum cap
 ) AS BYTE                                              ' GLboolean

' ========================================================================================
' WINGDIAPI GLboolean APIENTRY glIsList (GLuint list);
' Tests for display list existence.
' ========================================================================================
DECLARE FUNCTION glIsList LIB "OPENGL32.DLL" ALIAS "glIsList" ( _
   BYVAL GLuint AS DWORD _                              ' GLuint
 ) AS BYTE                                              ' GLboolean

' ========================================================================================
' WINGDIAPI GLboolean APIENTRY glIsTexture (GLuint texture);
' Determines if a name corresponds to a texture.
' ========================================================================================
DECLARE FUNCTION glIsTexture LIB "OPENGL32.DLL" ALIAS "glIsTexture" ( _
   BYVAL GLuint AS DWORD _                              ' GLuint
 ) AS BYTE                                              ' GLboolean

' ========================================================================================
' WINGDIAPI void APIENTRY glLightModelf (GLenum pname, GLfloat param);
' Sets the lighting model parameters.
' ========================================================================================
DECLARE SUB glLightModelf LIB "OPENGL32.DLL" ALIAS "glLightModelf" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLightModelfv (GLenum pname, const GLfloat *params);
' Sets the lighting model parameters.
' ========================================================================================
DECLARE SUB glLightModelfv LIB "OPENGL32.DLL" ALIAS "glLightModelfv" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' const GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLightModeli (GLenum pname, GLint param);
' Sets the lighting model parameters.
' ========================================================================================
DECLARE SUB glLightModeli LIB "OPENGL32.DLL" ALIAS "glLightModeli" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLightModeliv (GLenum pname, const GLint *params);
' Sets the lighting model parameters.
' ========================================================================================
DECLARE SUB glLightModeliv LIB "OPENGL32.DLL" ALIAS "glLightModeliv" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' const GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLightf (GLenum light, GLenum pname, GLfloat param);
' Sets light-source parameters.
' ========================================================================================
DECLARE SUB glLightf LIB "OPENGL32.DLL" ALIAS "glLightf" ( _
   BYVAL light AS DWORD _                               ' GLEnum light
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLightfv (GLenum light, GLenum pname, const GLfloat *params);
' Sets light-source parameters.
' ========================================================================================
DECLARE SUB glLightfv LIB "OPENGL32.DLL" ALIAS "glLightfv" ( _
   BYVAL light AS DWORD _                               ' GLEnum light
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' const GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLighti (GLenum light, GLenum pname, GLint param);
' Sets light-source parameters.
' ========================================================================================
DECLARE SUB glLighti LIB "OPENGL32.DLL" ALIAS "glLighti" ( _
   BYVAL light AS DWORD _                               ' GLEnum light
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLightiv (GLenum light, GLenum pname, const GLint *params);
' Sets light-source parameters.
' ========================================================================================
DECLARE SUB glLightiv LIB "OPENGL32.DLL" ALIAS "glLightiv" ( _
   BYVAL light AS DWORD _                               ' GLEnum light
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' const GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLineStipple (GLint factor, GLushort pattern);
' Specifies the line stipple pattern.
' ========================================================================================
DECLARE SUB glLineStipple LIB "OPENGL32.DLL" ALIAS "glLineStipple" ( _
   BYVAL factor AS LONG _                               ' GLint factor
 , BYVAL pattern AS WORD _                              ' GLushort pattern
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLineWidth (GLfloat width);
' Specifies the width of rasterized lines.
' ========================================================================================
DECLARE SUB glLineWidth LIB "OPENGL32.DLL" ALIAS "glLineWidth" ( _
   BYVAL width AS SINGLE _                              ' GLfloat width
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glListBase (GLuint base);
' Sets the display list base for glCallLists.
' ========================================================================================
DECLARE SUB glListBase LIB "OPENGL32.DLL" ALIAS "glListBase" ( _
   BYVAL base AS DWORD _                                ' GLuint base
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLoadIdentity (void);
' Replaces the current matrix with the identity matrix.
' ========================================================================================
DECLARE SUB glLoadIdentity LIB "OPENGL32.DLL" ALIAS "glLoadIdentity" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLoadMatrixd (const GLdouble *m);
' Replaces the current matrix with an arbitrary matrix.
' ========================================================================================
DECLARE SUB glLoadMatrixd LIB "OPENGL32.DLL" ALIAS "glLoadMatrixd" ( _
   BYREF m AS DOUBLE _                                  ' const GLdouble *m
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLoadMatrixf (const GLfloat *m);
' Replaces the current matrix with an arbitrary matrix.
' ========================================================================================
DECLARE SUB glLoadMatrixf LIB "OPENGL32.DLL" ALIAS "glLoadMatrixf" ( _
   BYREF m AS SINGLE _                                  ' const GLfloat *m
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLoadName (GLuint name);
' Loads a name onto the name stack.
' ========================================================================================
DECLARE SUB glLoadName LIB "OPENGL32.DLL" ALIAS "glLoadName" ( _
   BYVAL name AS DWORD _                                ' GLuint name
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glLogicOp (GLenum opcode);
' Specifies a logical pixel operation for color index rendering.
' ========================================================================================
DECLARE SUB glLogicOp LIB "OPENGL32.DLL" ALIAS "glLogicOp" ( _
   BYVAL op AS DWORD _                                  ' GLenum opcode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMap1d (GLenum target, GLdouble u1, GLdouble u2, GLint stride,
' GLint order, const GLdouble *points);
' Defines a one-dimensional evaluator.
' ========================================================================================
DECLARE SUB glMap1d LIB "OPENGL32.DLL" ALIAS "glMap1d" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL u1 AS DOUBLE _                                 ' GLdouble u1
 , BYVAL u2 AS DOUBLE _                                 ' GLdouble u2
 , BYVAL stride AS LONG _                               ' GLint stride
 , BYVAL order AS LONG _                                ' GLint order
 , BYREF points AS DOUBLE _                             ' const GLdouble *points
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMap1f (GLenum target, GLfloat u1, GLfloat u2, GLint stride,
' GLint order, const GLfloat *points);
' Defines a one-dimensional evaluator.
' ========================================================================================
DECLARE SUB glMap1f LIB "OPENGL32.DLL" ALIAS "glMap1f" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL u1 AS SINGLE _                                 ' GLfloat u1
 , BYVAL u2 AS SINGLE _                                 ' GLfloat u2
 , BYVAL stride AS LONG _                               ' GLint stride
 , BYVAL order AS LONG _                                ' GLint order
 , BYREF points AS SINGLE _                             ' const GLfloat *points
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMap2d (GLenum target, GLdouble u1, GLdouble u2, GLint ustride,
' GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points);
' Defines a two-dimensional evaluator.
' ========================================================================================
DECLARE SUB glMap2d LIB "OPENGL32.DLL" ALIAS "glMap2d" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL u1 AS DOUBLE _                                 ' GLdouble u1
 , BYVAL u2 AS DOUBLE _                                 ' GLdouble u2
 , BYVAL ustride AS LONG _                              ' GLint ustride
 , BYVAL uorder AS LONG _                               ' GLint uorder
 , BYVAL v1 AS DOUBLE _                                 ' GLdouble v1
 , BYVAL v2 AS DOUBLE _                                 ' GLdouble v2
 , BYVAL vstride AS LONG _                              ' GLint vstride
 , BYVAL vorder AS LONG _                               ' GLint vorder
 , BYREF points AS DOUBLE _                             ' const GLdouble *points
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMap2f (GLenum target, GLfloat u1, GLfloat u2, GLint ustride,
' GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points);
' Defines a two-dimensional evaluator.
' ========================================================================================
DECLARE SUB glMap2f LIB "OPENGL32.DLL" ALIAS "glMap2f" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL u1 AS SINGLE _                                 ' GLfloat u1
 , BYVAL u2 AS SINGLE _                                 ' GLfloat u2
 , BYVAL ustride AS LONG _                              ' GLint ustride
 , BYVAL uorder AS LONG _                               ' GLint uorder
 , BYVAL v1 AS SINGLE _                                 ' GLfloat v1
 , BYVAL v2 AS SINGLE _                                 ' GLfloat v2
 , BYVAL vstride AS LONG _                              ' GLint vstride
 , BYVAL vorder AS LONG _                               ' GLint vorder
 , BYREF points AS SINGLE _                             ' const GLfloat *points
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMapGrid1d (GLint un, GLdouble u1, GLdouble u2);
' Defines a one-dimensional mesh.
' ========================================================================================
DECLARE SUB glMapGrid1d LIB "OPENGL32.DLL" ALIAS "glMapGrid1d" ( _
   BYVAL un AS LONG _                                   ' GLint un
 , BYVAL u1 AS DOUBLE  _                                ' GLdouble u1
 , BYVAL u2 AS DOUBLE _                                 ' GLdouble u2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMapGrid1f (GLint un, GLfloat u1, GLfloat u2);
' Defines a one-dimensional mesh.
' ========================================================================================
DECLARE SUB glMapGrid1f LIB "OPENGL32.DLL" ALIAS "glMapGrid1f" ( _
   BYVAL un AS LONG _                                   ' GLint un
 , BYVAL u1 AS SINGLE _                                 ' GLfloat u1
 , BYVAL u2 AS SINGLE _                                 ' GLfloat u2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMapGrid2d (GLint un, GLdouble u1, GLdouble u2, GLint vn,
' GLdouble v1, GLdouble v2);
' Defines a two-dimensional mesh.
' ========================================================================================
DECLARE SUB glMapGrid2d LIB "OPENGL32.DLL" ALIAS "glMapGrid2d" ( _
   BYVAL un AS LONG _                                   ' GLint un
 , BYVAL u1 AS DOUBLE _                                 ' GLdouble u1
 , BYVAL u2 AS DOUBLE _                                 ' GLdouble u2
 , BYVAL vn AS LONG _                                   ' GLint vn
 , BYVAL v1 AS DOUBLE _                                 ' GLdouble v1
 , BYVAL v2 AS DOUBLE _                                 ' GLdouble v2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMapGrid2f (GLint un, GLfloat u1, GLfloat u2, GLint vn,
' GLfloat v1, GLfloat v2);
' Defines a two-dimensional mesh.
' ========================================================================================
DECLARE SUB glMapGrid2f LIB "OPENGL32.DLL" ALIAS "glMapGrid2f" ( _
   BYVAL un AS LONG _                                   ' GLint un
 , BYVAL u1 AS SINGLE _                                 ' GLfloat u1
 , BYVAL u2 AS SINGLE _                                 ' GLfloat u2
 , BYVAL vn AS LONG _                                   ' GLint vn
 , BYVAL v1 AS SINGLE _                                 ' GLfloat v1
 , BYVAL v2 AS SINGLE _                                 ' GLfloat v2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMaterialf (GLenum face, GLenum pname, GLfloat param);
' Specifies material parameters for the lighting model.
' ========================================================================================
DECLARE SUB glMaterialf LIB "OPENGL32.DLL" ALIAS "glMaterialf" ( _
   BYVAL face AS DWORD _                                ' GLEnum face
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMaterialfv (GLenum face, GLenum pname, const GLfloat *params);
' Specifies material parameters for the lighting model.
' ========================================================================================
DECLARE SUB glMaterialfv LIB "OPENGL32.DLL" ALIAS "glMaterialfv" ( _
   BYVAL face AS DWORD _                                ' GLEnum face
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' const GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMateriali (GLenum face, GLenum pname, GLint param);
' Specifies material parameters for the lighting model.
' ========================================================================================
DECLARE SUB glMateriali LIB "OPENGL32.DLL" ALIAS "glMateriali" ( _
   BYVAL face AS DWORD _                                ' GLEnum face
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMaterialiv (GLenum face, GLenum pname, const GLint *params);
' Specifies material parameters for the lighting model.
' ========================================================================================
DECLARE SUB glMaterialiv LIB "OPENGL32.DLL" ALIAS "glMaterialiv" ( _
   BYVAL face AS DWORD _                                ' GLEnum face
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' const GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMatrixMode (GLenum mode);
' Multiply the current matrix by an arbitrary matrix.
' ========================================================================================
DECLARE SUB glMatrixMode LIB "OPENGL32.DLL" ALIAS "glMatrixMode" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMultMatrixd (const GLdouble *m);
' Multiply the current matrix by an arbitrary matrix.
' ========================================================================================
DECLARE SUB glMultMatrixd LIB "OPENGL32.DLL" ALIAS "glMultMatrixd" ( _
   BYREF m AS DOUBLE _                                  ' const GLdouble *m
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glMultMatrixf (const GLfloat *m);
' Multiply the current matrix by an arbitrary matrix.
' ========================================================================================
DECLARE SUB glMultMatrixf LIB "OPENGL32.DLL" ALIAS "glMultMatrixf" ( _
   BYREF m AS SINGLE _                                  ' const GLfloat *m
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNewList (GLuint list, GLenum mode);
' The glNewList and glEndList functions create or replace a display list.
' ========================================================================================
DECLARE SUB glNewList LIB "OPENGL32.DLL" ALIAS "glNewList" ( _
   BYVAL list AS DWORD _                                ' GLuint list
 , BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3b (GLbyte nx, GLbyte ny, GLbyte nz);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3b LIB "OPENGL32.DLL" ALIAS "glNormal3b" ( _
   BYVAL nx AS BYTE _                                   ' GLbyte nx
 , BYVAL ny AS BYTE _                                   ' GLbyte ny
 , BYVAL nz AS BYTE _                                   ' GLbyte nz
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3bv (const GLbyte *v);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3bv LIB "OPENGL32.DLL" ALIAS "glNormal3bv" ( _
   BYREF v AS BYTE _                                    ' *GLbyte *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3d (GLdouble nx, GLdouble ny, GLdouble nz);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3d LIB "OPENGL32.DLL" ALIAS "glNormal3d" ( _
   BYVAL nx AS DOUBLE _                                 ' GLdouble nx
 , BYVAL ny AS DOUBLE _                                 ' GLdouble ny
 , BYVAL nz AS DOUBLE _                                 ' GLdouble nz
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3dv (const GLdouble *v);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3dv LIB "OPENGL32.DLL" ALIAS "glNormal3dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3f (GLfloat nx, GLfloat ny, GLfloat nz);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3f LIB "OPENGL32.DLL" ALIAS "glNormal3f" ( _
   BYVAL nx AS SINGLE _                                 ' GLfloat nx
 , BYVAL ny AS SINGLE _                                 ' GLfloat ny
 , BYVAL nz AS SINGLE _                                 ' GLfloat nz
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3fv (const GLfloat *v);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3fv LIB "OPENGL32.DLL" ALIAS "glNormal3fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3i (GLint nx, GLint ny, GLint nz);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3i LIB "OPENGL32.DLL" ALIAS "glNormal3i" ( _
   BYVAL nx AS LONG _                                   ' GLint nx
 , BYVAL ny AS LONG _                                   ' GLint ny
 , BYVAL nz AS LONG _                                   ' GLint nz
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3iv (const GLint *v);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3iv LIB "OPENGL32.DLL" ALIAS "glNormal3iv" ( _
   BYREF v AS LONG _                                    ' *GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3s (GLshort nx, GLshort ny, GLshort nz);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3s LIB "OPENGL32.DLL" ALIAS "glNormal3s" ( _
   BYVAL nx AS INTEGER _                                ' GLshort nx
 , BYVAL ny AS INTEGER _                                ' GLshort ny
 , BYVAL nz AS INTEGER _                                ' GLshort nz
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormal3sv (const GLshort *v);
' Sets the current normal vector.
' ========================================================================================
DECLARE SUB glNormal3sv LIB "OPENGL32.DLL" ALIAS "glNormal3sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glNormalPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
' Defines an array of normals.
' ========================================================================================
DECLARE SUB glNormalPointer LIB "OPENGL32.DLL" ALIAS "glNormalPointer" ( _
   BYVAL type AS DWORD _                                ' GLEnum type
 , BYVAL stride AS LONG _                               ' GLsizei stride
 , BYREF pointer AS ANY _                               ' const GLvoid *pointer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glOrtho (GLdouble left, GLdouble right, GLdouble bottom,
' GLdouble top, GLdouble zNear, GLdouble zFar);
' Multiplies the current matrix by an orthographic matrix.
' ========================================================================================
DECLARE SUB glOrtho LIB "OPENGL32.DLL" ALIAS "glOrtho" ( _
   BYVAL left AS DOUBLE _                               ' GLdouble left
 , BYVAL right AS DOUBLE _                              ' GLdouble right
 , BYVAL bottom AS DOUBLE _                             ' GLdouble bottom
 , BYVAL top AS DOUBLE _                                ' GLdouble top
 , BYVAL zNear AS DOUBLE _                              ' GLdouble zNear
 , BYVAL zFar AS DOUBLE _                               ' GLdouble zFar
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPassThrough (GLfloat token);
' Places a marker in the feedback buffer.
' ========================================================================================
DECLARE SUB glPassThrough LIB "OPENGL32.DLL" ALIAS "glPassThrough" ( _
   BYVAL token AS SINGLE _                              ' GLfloat token
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPixelMapfv (GLenum map, GLsizei mapsize, const GLfloat *values);
' Sets up pixel transfer maps.
' ========================================================================================
DECLARE SUB glPixelMapfv LIB "OPENGL32.DLL" ALIAS "glPixelMapfv" ( _
   BYVAL map AS DWORD _                                 ' GLEnum map
 , BYVAL mapsize AS LONG _                              ' GLsizei mapsize
 , BYREF values AS SINGLE _                             ' const GLfloat *values
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPixelMapuiv (GLenum map, GLsizei mapsize, const GLuint *values);
' Sets up pixel transfer maps.
' ========================================================================================
DECLARE SUB glPixelMapuiv LIB "OPENGL32.DLL" ALIAS "glPixelMapuiv" ( _
   BYVAL map AS DWORD _                                 ' GLEnum map
 , BYVAL mapsize AS LONG _                              ' GLsizei mapsize
 , BYREF values AS DWORD _                              ' const GLuint *values
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPixelMapusv (GLenum map, GLsizei mapsize, const GLushort *values);
' Sets up pixel transfer maps.
' ========================================================================================
DECLARE SUB glPixelMapusv LIB "OPENGL32.DLL" ALIAS "glPixelMapusv" ( _
   BYVAL map AS DWORD _                                 ' GLEnum map
 , BYVAL mapsize AS LONG _                              ' GLsizei mapsize
 , BYREF values AS WORD _                               ' const GLushort *values
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPixelStoref (GLenum pname, GLfloat param);
' Sets pixel storage modes.
' ========================================================================================
DECLARE SUB glPixelStoref LIB "OPENGL32.DLL" ALIAS "glPixelStoref" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPixelStorei (GLenum pname, GLint param);
' Sets pixel storage modes.
' ========================================================================================
DECLARE SUB glPixelStorei LIB "OPENGL32.DLL" ALIAS "glPixelStorei" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPixelTransferf (GLenum pname, GLfloat param);
' Sets pixel transfer modes.
' ========================================================================================
DECLARE SUB glPixelTransferf LIB "OPENGL32.DLL" ALIAS "glPixelTransferf" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPixelTransferi (GLenum pname, GLint param);
' Sets pixel transfer modes.
' ========================================================================================
DECLARE SUB glPixelTransferi LIB "OPENGL32.DLL" ALIAS "glPixelTransferi" ( _
   BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPixelZoom (GLfloat xfactor, GLfloat yfactor);
' Specifies the pixel zoom factors.
' ========================================================================================
DECLARE SUB glPixelZoom LIB "OPENGL32.DLL" ALIAS "glPixelZoom" ( _
   BYVAL xfactor AS SINGLE _                            ' GLfloat xfactor
 , BYVAL yfactor AS SINGLE _                            ' GLfloat yfactor
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPointSize (GLfloat size);
' Specifies the diameter of rasterized points.
' ========================================================================================
DECLARE SUB glPointSize LIB "OPENGL32.DLL" ALIAS "glPointSize" ( _
   BYVAL size AS SINGLE _                               ' GLfloat size
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPolygonMode (GLenum face, GLenum mode);
' Selects a polygon rasterization mode.
' ========================================================================================
DECLARE SUB glPolygonMode LIB "OPENGL32.DLL" ALIAS "glPolygonMode" ( _
   BYVAL face AS DWORD _                                ' GLEnum face
 , BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
' Sets the scale and units OpenGL uses to calculate depth values.
' ========================================================================================
DECLARE SUB glPolygonOffset LIB "OPENGL32.DLL" ALIAS "glPolygonOffset" ( _
   BYVAL factor AS SINGLE _                             ' GLfloat factor
 , BYVAL units AS SINGLE _                              ' GLfloat units
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPolygonStipple (const GLubyte *mask);
' Sets the polygon stippling pattern.
' ========================================================================================
DECLARE SUB glPolygonStipple LIB "OPENGL32.DLL" ALIAS "glPolygonStipple" ( _
   BYREF mask AS BYTE _                                 ' const GLubyte *mask
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPopAttrib (void);
' Pops the attribute stack.
' ========================================================================================
DECLARE SUB glPopAttrib LIB "OPENGL32.DLL" ALIAS "glPopAttrib" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPopClientAttrib (void);
' Restores groups of client-state variables on the client-attribute stack.
' ========================================================================================
DECLARE SUB glPopClientAttrib LIB "OPENGL32.DLL" ALIAS "glPopClientAttrib" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPopMatrix (void);
' Pops the current matrix stack.
' ========================================================================================
DECLARE SUB glPopMatrix LIB "OPENGL32.DLL" ALIAS "glPopMatrix" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPopName (void);
' Pops the name stack.
' ========================================================================================
DECLARE SUB glPopName LIB "OPENGL32.DLL" ALIAS "glPopName" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPrioritizeTextures (GLsizei n, const GLuint *textures,
' const GLclampf *priorities);
' Sets the residence priority of textures.
' ========================================================================================
DECLARE SUB glPrioritizeTextures LIB "OPENGL32.DLL" ALIAS "glPrioritizeTextures" ( _
   BYVAL n AS LONG _                                    ' GLsizei n
 , BYREF textures AS DWORD _                            ' const GLuint *textures
 , BYREF priorities AS SINGLE _                         ' const GLclampf *priorities
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPushAttrib (GLbitfield mask);
' Pushes the attribute stack.
' ========================================================================================
DECLARE SUB glPushAttrib LIB "OPENGL32.DLL" ALIAS "glPushAttrib" ( _
   BYVAL mask AS DWORD _                                ' GLbitfield mask
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);
' Saves groups of client-state variables on the client-attribute stack.
' ========================================================================================
DECLARE SUB glPushClientAttrib LIB "OPENGL32.DLL" ALIAS "glPushClientAttrib" ( _
   BYVAL mask AS DWORD _                                ' GLbitfield mask
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPushMatrix (void);
' Pushes the current matrix stack.
' ========================================================================================
DECLARE SUB glPushMatrix LIB "OPENGL32.DLL" ALIAS "glPushMatrix" ()   ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glPushName (GLuint name);
' Pushes the name stack.
' ========================================================================================
DECLARE SUB glPushName LIB "OPENGL32.DLL" ALIAS "glPushName" ( _
   BYVAL name AS DWORD _                                ' GLuint name
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos2d (GLdouble x, GLdouble y);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos2d LIB "OPENGL32.DLL" ALIAS "glRasterPos2d" ( _
   BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos2dv (const GLdouble *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos2dv LIB "OPENGL32.DLL" ALIAS "glRasterPos2dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos2f (GLfloat x, GLfloat y);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos2f LIB "OPENGL32.DLL" ALIAS "glRasterPos2f" ( _
   BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos2fv (const GLfloat *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos2fv LIB "OPENGL32.DLL" ALIAS "glRasterPos2fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos2i (GLint x, GLint y);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos2i LIB "OPENGL32.DLL" ALIAS "glRasterPos2i" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos2iv (const GLint *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos2iv LIB "OPENGL32.DLL" ALIAS "glRasterPos2iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos2s (GLshort x, GLshort y);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos2s LIB "OPENGL32.DLL" ALIAS "glRasterPos2s" ( _
   BYVAL x AS INTEGER _                                 ' GLshort x
 , BYVAL y AS INTEGER _                                 ' GLshort y
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos2sv (const GLshort *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos2sv LIB "OPENGL32.DLL" ALIAS "glRasterPos2sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos3d (GLdouble x, GLdouble y, GLdouble z);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos3d LIB "OPENGL32.DLL" ALIAS "glRasterPos3d" ( _
   BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 , BYVAL z AS DOUBLE _                                  ' GLdouble z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos3dv (const GLdouble *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos3dv LIB "OPENGL32.DLL" ALIAS "glRasterPos3dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos3f (GLfloat x, GLfloat y, GLfloat z);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos3f LIB "OPENGL32.DLL" ALIAS "glRasterPos3f" ( _
   BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 , BYVAL z AS SINGLE _                                  ' GLfloat z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos3fv (const GLfloat *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos3fv LIB "OPENGL32.DLL" ALIAS "glRasterPos3fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos3i (GLint x, GLint y, GLint z);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos3i LIB "OPENGL32.DLL" ALIAS "glRasterPos3i" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL z AS LONG _                                    ' GLint z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos3iv (const GLint *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos3iv LIB "OPENGL32.DLL" ALIAS "glRasterPos3iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos3s (GLshort x, GLshort y, GLshort z);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos3s LIB "OPENGL32.DLL" ALIAS "glRasterPos3s" ( _
   BYVAL x AS INTEGER _                                 ' GLshort x
 , BYVAL y AS INTEGER _                                 ' GLshort y
 , BYVAL z AS INTEGER _                                 ' GLshort z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos3sv (const GLshort *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos3sv LIB "OPENGL32.DLL" ALIAS "glRasterPos3sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos4d (GLdouble x, GLdouble y, GLdouble z, GLdouble w);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos4d LIB "OPENGL32.DLL" ALIAS "glRasterPos4d" ( _
   BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 , BYVAL z AS DOUBLE _                                  ' GLdouble z
 , BYVAL w AS DOUBLE _                                  ' GLdouble w
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos4dv (const GLdouble *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos4dv LIB "OPENGL32.DLL" ALIAS "glRasterPos4dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos4f (GLfloat x, GLfloat y, GLfloat z, GLfloat w);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos4f LIB "OPENGL32.DLL" ALIAS "glRasterPos4f" ( _
   BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 , BYVAL z AS SINGLE _                                  ' GLfloat z
 , BYVAL w AS SINGLE _                                  ' GLfloat w
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos4fv (const GLfloat *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos4fv LIB "OPENGL32.DLL" ALIAS "glRasterPos4fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos4i (GLint x, GLint y, GLint z, GLint w);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos4i LIB "OPENGL32.DLL" ALIAS "glRasterPos4i" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL z AS LONG _                                    ' GLint z
 , BYVAL w AS LONG _                                    ' GLint w
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos4iv (const GLint *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos4iv LIB "OPENGL32.DLL" ALIAS "glRasterPos4iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos4s (GLshort x, GLshort y, GLshort z, GLshort w);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos4s LIB "OPENGL32.DLL" ALIAS "glRasterPos4s" ( _
   BYVAL x AS INTEGER _                                 ' GLshort x
 , BYVAL y AS INTEGER _                                 ' GLshort y
 , BYVAL z AS INTEGER _                                 ' GLshort z
 , BYVAL w AS INTEGER _                                 ' GLshort w
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRasterPos4sv (const GLshort *v);
' Specifies the raster position for pixel operations.
' ========================================================================================
DECLARE SUB glRasterPos4sv LIB "OPENGL32.DLL" ALIAS "glRasterPos4sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glReadBuffer (GLenum mode);
' Selects a color buffer source for pixels.
' ========================================================================================
DECLARE SUB glReadBuffer LIB "OPENGL32.DLL" ALIAS "glReadBuffer" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height,
' GLEnum format, GLenum type, GLvoid *pixels);
' Reads a block of pixels from the framebuffer.
' ========================================================================================
DECLARE SUB glReadPixels LIB "OPENGL32.DLL" ALIAS "glReadPixels" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF pixels AS ANY _                                ' GLvoid *pixels
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRectd (GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2);
' Draws a rectangle.
' ========================================================================================
DECLARE SUB glRectd LIB "OPENGL32.DLL" ALIAS "glRectd" ( _
   BYVAL x1 AS DOUBLE _                                 ' GLdouble x1
 , BYVAL y1 AS DOUBLE _                                 ' GLdouble y1
 , BYVAL x2 AS DOUBLE _                                 ' GLdouble x2
 , BYVAL y2 AS DOUBLE _                                 ' GLdouble y2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRectdv (const GLdouble *v1, const GLdouble *v2);
' Draws a rectangle.
' ========================================================================================
DECLARE SUB glRectdv LIB "OPENGL32.DLL" ALIAS "glRectdv" ( _
   BYREF v1 AS DOUBLE _                                 ' const GLdouble *v1
 , BYREF v2 AS DOUBLE _                                 ' const GLdouble *v2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRectf (GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2);
' Draws a rectangle.
' ========================================================================================
DECLARE SUB glRectf LIB "OPENGL32.DLL" ALIAS "glRectf" ( _
   BYVAL x1 AS SINGLE _                                 ' GLfloat x1
 , BYVAL y1 AS SINGLE _                                 ' GLfloat y1
 , BYVAL x2 AS SINGLE _                                 ' GLfloat x2
 , BYVAL y2 AS SINGLE _                                 ' GLfloat y2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRectfv (const GLfloat *v1, const GLfloat *v2);
' Draws a rectangle.
' ========================================================================================
DECLARE SUB glRectfv LIB "OPENGL32.DLL" ALIAS "glRectfv" ( _
   BYREF v1 AS SINGLE _                                 ' *GLfloat v1
 , BYREF v2 AS SINGLE _                                 ' *GLfloat v2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRecti (GLint x1, GLint y1, GLint x2, GLint y2);
' Draws a rectangle.
' ========================================================================================
DECLARE SUB glRecti LIB "OPENGL32.DLL" ALIAS "glRecti" ( _
   BYVAL x1 AS LONG _                                   ' GLint x1
 , BYVAL y1 AS LONG _                                   ' GLint y1
 , BYVAL x2 AS LONG _                                   ' GLint x2
 , BYVAL y2 AS LONG _                                   ' GLint y2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRectiv (const GLint *v1, const GLint *v2);
' Draws a rectangle.
' ========================================================================================
DECLARE SUB glRectiv LIB "OPENGL32.DLL" ALIAS "glRectiv" ( _
   BYREF v1 AS LONG _                                   ' const GLint *v1
 , BYREF v2 AS LONG _                                   ' const GLint *v2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRects (GLshort x1, GLshort y1, GLshort x2, GLshort y2);
' Draws a rectangle.
' ========================================================================================
DECLARE SUB glRects LIB "OPENGL32.DLL" ALIAS "glRects" ( _
   BYVAL x1 AS INTEGER _                                ' GLshort x1
 , BYVAL y1 AS INTEGER _                                ' GLshort y1
 , BYVAL x2 AS INTEGER _                                ' GLshort x2
 , BYVAL y2 AS INTEGER _                                ' GLshort y2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRectsv (const GLshort *v1, const GLshort *v2);
' Draws a rectangle.
' ========================================================================================
DECLARE SUB glRectsv LIB "OPENGL32.DLL" ALIAS "glRectsv" ( _
   BYREF v1 AS INTEGER _                                ' const GLshort *v1
 , BYREF v2 AS INTEGER _                                ' const GLshort *v2
 )                                                      ' void

' ========================================================================================
' WINGDIAPI GLint APIENTRY glRenderMode (GLenum mode);
' Sets the rasterization mode.
' ========================================================================================
DECLARE FUNCTION glRenderMode LIB "OPENGL32.DLL" ALIAS "glRenderMode" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 ) AS LONG                                              ' int

' ========================================================================================
' WINGDIAPI void APIENTRY glRotated (GLdouble angle, GLdouble x, GLdouble y, GLdouble z);
' Multiplies the current matrix by a rotation matrix.
' ========================================================================================
DECLARE SUB glRotated LIB "OPENGL32.DLL" ALIAS "glRotated" ( _
   BYVAL angle AS DOUBLE _                              ' GLdouble angle
 , BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 , BYVAL z AS DOUBLE _                                  ' GLdouble z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glRotatef (GLfloat angle, GLfloat x, GLfloat y, GLfloat z);
' Multiplies the current matrix by a rotation matrix.
' ========================================================================================
DECLARE SUB glRotatef LIB "OPENGL32.DLL" ALIAS "glRotatef" ( _
   BYVAL angle AS SINGLE _                              ' GLfloat angle
 , BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 , BYVAL z AS SINGLE _                                  ' GLfloat z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glScaled (GLdouble x, GLdouble y, GLdouble z);
' Multiplies the current matrix by a general scaling matrix.
' ========================================================================================
DECLARE SUB glScaled LIB "OPENGL32.DLL" ALIAS "glScaled" ( _
   BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 , BYVAL z AS DOUBLE _                                  ' GLdouble z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glScalef (GLfloat x, GLfloat y, GLfloat z);
' Multiplies the current matrix by a general scaling matrix.
' ========================================================================================
DECLARE SUB glScalef LIB "OPENGL32.DLL" ALIAS "glScalef" ( _
   BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 , BYVAL z AS SINGLE _                                  ' GLfloat z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
' Defines the scissor box.
' ========================================================================================
DECLARE SUB glScissor LIB "OPENGL32.DLL" ALIAS "glScissor" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y aS LONG _                                    ' GLint y
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glSelectBuffer (GLsizei size, GLuint *buffer);
' Establishes a buffer for selection mode values.
' ========================================================================================
DECLARE SUB glSelectBuffer LIB "OPENGL32.DLL" ALIAS "glSelectBuffer" ( _
   BYVAL size AS LONG _                                 ' GLsizei size
 , BYREF buffer AS DWORD _                              ' GLuint *buffer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glShadeModel (GLenum mode);
' Selects flat or smooth shading.
' ========================================================================================
DECLARE SUB glShadeModel LIB "OPENGL32.DLL" ALIAS "glShadeModel" ( _
   BYVAL mode AS DWORD _                                ' GLEnum mode
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
' Sets the function and reference value for stencil testing.
' ========================================================================================
DECLARE SUB glStencilFunc LIB "OPENGL32.DLL" ALIAS "glStencilFunc" ( _
   BYVAL func AS DWORD _                                ' GLEnum func
 , BYVAL ref AS LONG _                                  ' GLint ref
 , BYVAL mask AS DWORD _                                ' GLuint mask
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glStencilMask (GLuint mask);
' Controls the writing of individual bits in the stencil planes.
' ========================================================================================
DECLARE SUB glStencilMask LIB "OPENGL32.DLL" ALIAS "glStencilMask" ( _
   BYVAL mask AS DWORD _                                ' GLuint mask
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
' Sets the stencil test actions.
' ========================================================================================
DECLARE SUB glStencilOp LIB "OPENGL32.DLL" ALIAS "glStencilOp" ( _
   BYVAL fail AS DWORD _                                ' GLEnum fail
 , BYVAL zfail AS DWORD _                               ' GLEnum zfail
 , BYVAL zpass AS DWORD _                               ' GLEnum zpass
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord1d (GLdouble s);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord1d LIB "OPENGL32.DLL" ALIAS "glTexCoord1d" ( _
   BYVAL s AS DOUBLE _                                  ' GLdouble s
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord1dv (const GLdouble *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord1dv LIB "OPENGL32.DLL" ALIAS "glTexCoord1dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord1f (GLfloat s);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord1f LIB "OPENGL32.DLL" ALIAS "glTexCoord1f" ( _
   BYVAL s AS SINGLE _                                  ' GLfloat s
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord1fv (const GLfloat *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord1fv LIB "OPENGL32.DLL" ALIAS "glTexCoord1fv" ( _
   BYREF v AS SINGLE _                                  ' GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord1i (GLint s);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord1i LIB "OPENGL32.DLL" ALIAS "glTexCoord1i" ( _
   BYVAL s AS LONG _                                    ' GLint s
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord1iv (const GLint *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord1iv LIB "OPENGL32.DLL" ALIAS "glTexCoord1iv" ( _
   BYREF v AS LONG _                                    ' const GLint v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord1s (GLshort s);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord1s LIB "OPENGL32.DLL" ALIAS "glTexCoord1s" ( _
   BYVAL s AS INTEGER _                                 ' GLshort s
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord1sv (const GLshort *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord1sv LIB "OPENGL32.DLL" ALIAS "glTexCoord1sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord2d (GLdouble s, GLdouble t);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord2d LIB "OPENGL32.DLL" ALIAS "glTexCoord2d" ( _
   BYVAL s AS DOUBLE _                                  ' GLdouble s
 , BYVAL t AS DOUBLE _                                  ' GLdouble t
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord2dv (const GLdouble *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord2dv LIB "OPENGL32.DLL" ALIAS "glTexCoord2dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord2f (GLfloat s, GLfloat t);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord2f LIB "OPENGL32.DLL" ALIAS "glTexCoord2f" ( _
   BYVAL s AS SINGLE _                                  ' GLfloat s
 , BYVAL t AS SINGLE _                                  ' GLfloat t
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord2fv (const GLfloat *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord2fv LIB "OPENGL32.DLL" ALIAS "glTexCoord2fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord2i (GLint s, GLint t);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord2i LIB "OPENGL32.DLL" ALIAS "glTexCoord2i" ( _
   BYVAL s AS LONG _                                    ' GLint s
 , BYVAL t AS LONG _                                    ' GLint t
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord2iv (const GLint *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord2iv LIB "OPENGL32.DLL" ALIAS "glTexCoord2iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord2s (GLshort s, GLshort t);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord2s LIB "OPENGL32.DLL" ALIAS "glTexCoord2s" ( _
   BYVAL s AS INTEGER _                                 ' GLshort s
 , BYVAL t AS INTEGER _                                 ' GLshort t
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord2sv (const GLshort *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord2sv LIB "OPENGL32.DLL" ALIAS "glTexCoord2sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord3d (GLdouble s, GLdouble t, GLdouble r);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord3d LIB "OPENGL32.DLL" ALIAS "glTexCoord3d" ( _
   BYVAL s AS DOUBLE _                                  ' GLdouble s
 , BYVAL t AS DOUBLE _                                  ' GLdouble t
 , BYVAL r AS DOUBLE _                                  ' GLdouble r
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord3dv (const GLdouble *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord3dv LIB "OPENGL32.DLL" ALIAS "glTexCoord3dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord3f (GLfloat s, GLfloat t, GLfloat r);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord3f LIB "OPENGL32.DLL" ALIAS "glTexCoord3f" ( _
   BYVAL s AS SINGLE _                                  ' GLfloat s
 , BYVAL t AS SINGLE _                                  ' GLfloat t
 , BYVAL r AS SINGLE _                                  ' GLfloat r
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord3fv (const GLfloat *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord3fv LIB "OPENGL32.DLL" ALIAS "glTexCoord3fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord3i (GLint s, GLint t, GLint r);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord3i LIB "OPENGL32.DLL" ALIAS "glTexCoord3i" ( _
   BYVAL s AS LONG _                                    ' GLint s
 , BYVAL t AS LONG _                                    ' GLint t
 , BYVAL r AS LONG _                                    ' GLint r
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord3iv (const GLint *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord3iv LIB "OPENGL32.DLL" ALIAS "glTexCoord3iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord3s (GLshort s, GLshort t, GLshort r);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord3s LIB "OPENGL32.DLL" ALIAS "glTexCoord3s" ( _
   BYVAL s AS INTEGER _                                 ' GLshort s
 , BYVAL t AS INTEGER _                                 ' GLshort t
 , BYVAL r AS INTEGER _                                 ' GLshort r
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord3sv (const GLshort *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord3sv LIB "OPENGL32.DLL" ALIAS "glTexCoord3sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord4d (GLdouble s, GLdouble t, GLdouble r, GLdouble q);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord4d LIB "OPENGL32.DLL" ALIAS "glTexCoord4d" ( _
   BYVAL s AS DOUBLE _                                  ' GLdouble s
 , BYVAL t AS DOUBLE _                                  ' GLdouble t
 , BYVAL r AS DOUBLE _                                  ' GLdouble r
 , BYVAL q AS DOUBLE _                                  ' GLdouble q
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord4dv (const GLdouble *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord4dv LIB "OPENGL32.DLL" ALIAS "glTexCoord4dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord4f (GLfloat s, GLfloat t, GLfloat r, GLfloat q);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord4f LIB "OPENGL32.DLL" ALIAS "glTexCoord4f" ( _
   BYVAL s AS SINGLE _                                  ' GLfloat s
 , BYVAL t AS SINGLE _                                  ' GLfloat t
 , BYVAL r AS SINGLE _                                  ' GLfloat r
 , BYVAL q AS SINGLE _                                  ' GLfloat q
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord4fv (const GLfloat *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord4fv LIB "OPENGL32.DLL" ALIAS "glTexCoord4fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord4i (GLint s, GLint t, GLint r, GLint q);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord4i LIB "OPENGL32.DLL" ALIAS "glTexCoord4i" ( _
   BYVAL s AS LONG _                                    ' GLint s
 , BYVAL t AS LONG _                                    ' GLint t
 , BYVAL r AS LONG _                                    ' GLint r
 , BYVAL q AS LONG _                                    ' GLint q
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord4iv (const GLint *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord4iv LIB "OPENGL32.DLL" ALIAS "glTexCoord4iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord4s (GLshort s, GLshort t, GLshort r, GLshort q);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord4s LIB "OPENGL32.DLL" ALIAS "glTexCoord4s" ( _
   BYVAL s AS INTEGER _                                 ' GLshort s
 , BYVAL t AS INTEGER _                                 ' GLshort t
 , BYVAL r AS INTEGER _                                 ' GLshort r
 , BYVAL q AS INTEGER _                                 ' GLshort q
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoord4sv (const GLshort *v);
' Sets the current texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoord4sv LIB "OPENGL32.DLL" ALIAS "glTexCoord4sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexCoordPointer (GLint size, GLenum type, GLsizei stride,
' const GLvoid *pointer);
' Defines an array of texture coordinates.
' ========================================================================================
DECLARE SUB glTexCoordPointer LIB "OPENGL32.DLL" ALIAS "glTexCoordPointer" ( _
   BYVAL size AS LONG _                                 ' GLint size
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYVAL stride AS LONG _                               ' GLsizei stride
 , BYREF pointer AS ANY _                               ' GLvoid *pointer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexEnvf (GLenum target, GLenum pname, GLfloat param);
' Sets texture environment parameters.
' ========================================================================================
DECLARE SUB glTexEnvf LIB "OPENGL32.DLL" ALIAS "glTexEnvf" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexEnvfv (GLenum target, GLenum pname, const GLfloat *params);
' Sets texture environment parameters.
' ========================================================================================
DECLARE SUB glTexEnvfv LIB "OPENGL32.DLL" ALIAS "glTexEnvfv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' const GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexEnvi (GLenum target, GLenum pname, GLint param);
' Sets texture environment parameters.
' ========================================================================================
DECLARE SUB glTexEnvi LIB "OPENGL32.DLL" ALIAS "glTexEnvi" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexEnviv (GLenum target, GLenum pname, const GLint *params);
' Sets texture environment parameters.
' ========================================================================================
DECLARE SUB glTexEnviv LIB "OPENGL32.DLL" ALIAS "glTexEnviv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' const GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexGend (GLenum coord, GLenum pname, GLdouble param);
' Controls the generation of texture coordinates.
' ========================================================================================
DECLARE SUB glTexGend LIB "OPENGL32.DLL" ALIAS "glTexGend" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS DOUBLE _                              ' GLdouble param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexGendv (GLenum coord, GLenum pname, const GLdouble *params);
' Controls the generation of texture coordinates.
' ========================================================================================
DECLARE SUB glTexGendv LIB "OPENGL32.DLL" ALIAS "glTexGendv" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS DOUBLE _                             ' const GLdouble *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexGenf (GLenum coord, GLenum pname, GLfloat param);
' Controls the generation of texture coordinates.
' ========================================================================================
DECLARE SUB glTexGenf LIB "OPENGL32.DLL" ALIAS "glTexGenf" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexGenfv (GLenum coord, GLenum pname, const GLfloat *params);
' Controls the generation of texture coordinates.
' ========================================================================================
DECLARE SUB glTexGenfv LIB "OPENGL32.DLL" ALIAS "glTexGenfv" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' const GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexGeni (GLenum coord, GLenum pname, GLint param);
' Controls the generation of texture coordinates.
' ========================================================================================
DECLARE SUB glTexGeni LIB "OPENGL32.DLL" ALIAS "glTexGeni" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexGeniv (GLenum coord, GLenum pname, const GLint *params);
' Controls the generation of texture coordinates.
' ========================================================================================
DECLARE SUB glTexGeniv LIB "OPENGL32.DLL" ALIAS "glTexGeniv" ( _
   BYVAL coord AS DWORD _                               ' GLEnum coord
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' const GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexImage1D (GLenum target, GLint level, GLint internalformat,
' GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels);
' Specifies a one-dimensional texture image.
' ========================================================================================
DECLARE SUB glTexImage1D LIB "OPENGL32.DLL" ALIAS "glTexImage1D" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL internalformat AS LONG _                       ' GLint internalformat
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL border AS LONG _                               ' GLint border
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF pixels AS ANY _                                ' const GLvoid *pixels
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexImage2D (GLenum target, GLint level, GLint internalformat,
' GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels);
' Specifies a two-dimensional texture image.
' ========================================================================================
DECLARE SUB glTexImage2D LIB "OPENGL32.DLL" ALIAS "glTexImage2D" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL internalformat AS LONG _                       ' GLint internalformat
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 , BYVAL border AS LONG _                               ' GLint border
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF pixels AS ANY _                                ' const GLvoid *pixels
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
' Sets texture parameters.
' ========================================================================================
DECLARE SUB glTexParameterf LIB "OPENGL32.DLL" ALIAS "glTexParameterf" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS SINGLE _                              ' GLfloat param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
' Sets texture parameters.
' ========================================================================================
DECLARE SUB glTexParameterfv LIB "OPENGL32.DLL" ALIAS "glTexParameterfv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS SINGLE _                             ' const GLfloat *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexParameteri (GLenum target, GLenum pname, GLint param);
' Sets texture parameters.
' ========================================================================================
DECLARE SUB glTexParameteri LIB "OPENGL32.DLL" ALIAS "glTexParameteri" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYVAL param AS LONG _                                ' GLint param
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
' Sets texture parameters.
' ========================================================================================
DECLARE SUB glTexParameteriv LIB "OPENGL32.DLL" ALIAS "glTexParameteriv" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL pname AS DWORD _                               ' GLEnum pname
 , BYREF params AS LONG _                               ' const GLint *params
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset,
' GLsizei width, GLenum format, GLenum type, const GLvoid *pixels);
' Specifies a portion of an existing one-dimensional texture image.
' ========================================================================================
DECLARE SUB glTexSubImage1D LIB "OPENGL32.DLL" ALIAS "glTexSubImage1D" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL xoffset AS LONG _                              ' GLint xoffset
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF pixels AS ANY _                                ' const GLvoid *pixels
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset,
' GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
' Specifies a portion of an existing one-dimensional texture image.
' ========================================================================================
DECLARE SUB glTexSubImage2D LIB "OPENGL32.DLL" ALIAS "glTexSubImage2D" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL level AS LONG _                                ' GLint level
 , BYVAL xoffset AS LONG _                              ' GLint xoffset
 , BYVAL yoffset AS LONG _                              ' GLint yoffset
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF pixels AS ANY _                                ' const GLvoid *pixels
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTranslated (GLdouble x, GLdouble y, GLdouble z);
' Multiplies the current matrix by a translation matrix.
' ========================================================================================
DECLARE SUB glTranslated LIB "OPENGL32.DLL" ALIAS "glTranslated" ( _
   BYVAL x AS DOUBLE _                                  ' GLdoubl x
 , BYVAL y AS DOUBLE _                                  ' GLdoubl y
 , BYVAL z AS DOUBLE _                                  ' GLdouble z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glTranslatef (GLfloat x, GLfloat y, GLfloat z);
' Multiplies the current matrix by a translation matrix.
' ========================================================================================
DECLARE SUB glTranslatef LIB "OPENGL32.DLL" ALIAS "glTranslatef" ( _
   BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 , BYVAL z AS SINGLE _                                  ' GLfloat z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex2d (GLdouble x, GLdouble y);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex2d LIB "OPENGL32.DLL" ALIAS "glVertex2d" ( _
   BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex2dv (const GLdouble *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex2dv LIB "OPENGL32.DLL" ALIAS "glVertex2dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex2f (GLfloat x, GLfloat y);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex2f LIB "OPENGL32.DLL" ALIAS "glVertex2f" ( _
   BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex2fv (const GLfloat *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex2fv LIB "OPENGL32.DLL" ALIAS "glVertex2fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex2i (GLint x, GLint y);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex2i LIB "OPENGL32.DLL" ALIAS "glVertex2i" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex2iv (const GLint *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex2iv LIB "OPENGL32.DLL" ALIAS "glVertex2iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex2s (GLshort x, GLshort y);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex2s LIB "OPENGL32.DLL" ALIAS "glVertex2s" ( _
   BYVAL x AS INTEGER _                                 ' GLshort x
 , BYVAL y AS INTEGER _                                 ' GLshort y
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex2sv (const GLshort *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex2sv LIB "OPENGL32.DLL" ALIAS "glVertex2sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex3d (GLdouble x, GLdouble y, GLdouble z);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex3d LIB "OPENGL32.DLL" ALIAS "glVertex3d" ( _
   BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 , BYVAL z AS DOUBLE _                                  ' GLdouble z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex3dv (const GLdouble *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex3dv LIB "OPENGL32.DLL" ALIAS "glVertex3dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex3f (GLfloat x, GLfloat y, GLfloat z);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex3f LIB "OPENGL32.DLL" ALIAS "glVertex3f" ( _
   BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 , BYVAL z AS SINGLE _                                  ' GLfloat z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex3fv (const GLfloat *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex3fv LIB "OPENGL32.DLL" ALIAS "glVertex3fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex3i (GLint x, GLint y, GLint z);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex3i LIB "OPENGL32.DLL" ALIAS "glVertex3i" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL z AS LONG _                                    ' GLint z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex3iv (const GLint *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex3iv LIB "OPENGL32.DLL" ALIAS "glVertex3iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex3s (GLshort x, GLshort y, GLshort z);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex3s LIB "OPENGL32.DLL" ALIAS "glVertex3s" ( _
   BYVAL x AS INTEGER _                                 ' GLshort x
 , BYVAL y AS INTEGER _                                 ' GLshort y
 , BYVAL z AS INTEGER _                                 ' GLshort z
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex3sv (const GLshort *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex3sv LIB "OPENGL32.DLL" ALIAS "glVertex3sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex4d (GLdouble x, GLdouble y, GLdouble z, GLdouble w);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex4d LIB "OPENGL32.DLL" ALIAS "glVertex4d" ( _
   BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 , BYVAL z AS DOUBLE _                                  ' GLdouble z
 , BYVAL w AS DOUBLE _                                  ' GLdouble w
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex4dv (const GLdouble *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex4dv LIB "OPENGL32.DLL" ALIAS "glVertex4dv" ( _
   BYREF v AS DOUBLE _                                  ' const GLdouble *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex4f (GLfloat x, GLfloat y, GLfloat z, GLfloat w);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex4f LIB "OPENGL32.DLL" ALIAS "glVertex4f" ( _
   BYVAL x AS SINGLE _                                  ' GLfloat x
 , BYVAL y AS SINGLE _                                  ' GLfloat y
 , BYVAL z AS SINGLE _                                  ' GLfloat z
 , BYVAL w AS SINGLE _                                  ' GLfloat w
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex4fv (const GLfloat *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex4fv LIB "OPENGL32.DLL" ALIAS "glVertex4fv" ( _
   BYREF v AS SINGLE _                                  ' const GLfloat *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex4i (GLint x, GLint y, GLint z, GLint w);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex4i LIB "OPENGL32.DLL" ALIAS "glVertex4i" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL z AS LONG _                                    ' GLint z
 , BYVAL w AS LONG _                                    ' GLint w
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex4iv (const GLint *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex4iv LIB "OPENGL32.DLL" ALIAS "glVertex4iv" ( _
   BYREF v AS LONG _                                    ' const GLint *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex4s (GLshort x, GLshort y, GLshort z, GLshort w);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex4s LIB "OPENGL32.DLL" ALIAS "glVertex4s" ( _
   BYVAL x AS INTEGER _                                 ' GLshort x
 , BYVAL y AS INTEGER _                                 ' GLshort y
 , BYVAL z AS INTEGER _                                 ' GLshort z
 , BYVAL w AS INTEGER _                                 ' GLshort w
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertex4sv (const GLshort *v);
' Specifies a vertex.
' ========================================================================================
DECLARE SUB glVertex4sv LIB "OPENGL32.DLL" ALIAS "glVertex4sv" ( _
   BYREF v AS INTEGER _                                 ' const GLshort *v
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glVertexPointer (GLint size, GLenum type, GLsizei stride,
' const GLvoid *pointer);
' Defines an array of vertex data.
' ========================================================================================
DECLARE SUB glVertexPointer LIB "OPENGL32.DLL" ALIAS "glVertexPointer" ( _
   BYVAL size AS LONG _                                 ' GLint size
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYVAL stride AS LONG _                               ' GLsizei stride
 , BYREF pointer AS ANY _                               ' GLvoid *pointer
 )                                                      ' void

' ========================================================================================
' WINGDIAPI void APIENTRY glViewport (GLint x, GLint y, GLsizei width, GLsizei height);
' Sets the viewport.
' ========================================================================================
DECLARE SUB glViewport LIB "OPENGL32.DLL" ALIAS "glViewport" ( _
   BYVAL x AS LONG _                                    ' GLint x
 , BYVAL y AS LONG _                                    ' GLint y
 , BYVAL width AS LONG _                                ' GLsizei width
 , BYVAL height AS LONG _                               ' GLsizei height
 )                                                      ' void

'/* EXT_vertex_array */
'typedef void (APIENTRY * PFNGLARRAYELEMENTEXTPROC) (GLint i);
'SUB GLARRAYELEMENTEXTPROC (BYVAL i AS LONG)

'typedef void (APIENTRY * PFNGLDRAWARRAYSEXTPROC) (GLenum mode, GLint first, GLsizei count);
'SUB GLDRAWARRAYSEXTPROC (BYVAL mode AS DWORD, BYVAL first AS LONG, BYVAL count AS LONG)

'typedef void (APIENTRY * PFNGLVERTEXPOINTEREXTPROC) (GLint size, GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer);
'SUB GLVERTEXPOINTEREXTPROC (BYVAL nSize AS LONG, BYVAL nType AS DWORD, BYVAL stride AS LONG, BYVAL count AS LONG, BYVAL ppointer AS DWORD)

'typedef void (APIENTRY * PFNGLNORMALPOINTEREXTPROC) (GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer);
'SUB GLNORMALPOINTEREXTPROC (BYVAL nType AS DWORD, BYVAL stride AS LONG, BYVAL count AS LONG, BYVAL ppointer AS DWORD)

'typedef void (APIENTRY * PFNGLCOLORPOINTEREXTPROC) (GLint size, GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer);
'SUB GLCOLORPOINTEREXTPROC (BYVAL nSize AS LONG, BYVAL nType AS DWORD, BYVAL stride AS LONG, BYVAL count AS LONG, BYVAL ppointer AS DWORD)

'typedef void (APIENTRY * PFNGLINDEXPOINTEREXTPROC) (GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer);
'SUB GLINDEXPOINTEREXTPROC (BYVAL nTyhpe AS DWORD, BYVAL stride AS LONG, BYVAL count AS LONG, BYVAL ppointer AS DWORD)

'typedef void (APIENTRY * PFNGLTEXCOORDPOINTEREXTPROC) (GLint size, GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer);
'SUB GLTEXCOORDPOINTEREXTPROC (BYVAL nSize AS LONG, BYVAL nType AS DWORD, BYVAL stride AS LONG, BYVAL count AS LONG, BYVAL ppointer AS DWORD)

'typedef void (APIENTRY * PFNGLEDGEFLAGPOINTEREXTPROC) (GLsizei stride, GLsizei count, const GLboolean *pointer);
'SUB GLEDGEFLAGPOINTEREXTPROC (BYVAL stride AS LONG, BYVAL count AS LONG, BYVAL ppointer AS DWORD)

'typedef void (APIENTRY * PFNGLGETPOINTERVEXTPROC) (GLenum pname, GLvoid* *params);
'SUB GLGETPOINTERVEXTPROC (BYVAL pname AS DWORD, BYVAL params AS DWORD)

'typedef void (APIENTRY * PFNGLARRAYELEMENTARRAYEXTPROC)(GLenum mode, GLsizei count, const GLvoid* pi);
'SUB GLARRAYELEMENTARRAYEXTPROC (BYVAL mode AS DWORD, BYVAL count AS LONG, BYVAL pi AS DWORD)

'/* WIN_draw_range_elements */
'typedef void (APIENTRY * PFNGLDRAWRANGEELEMENTSWINPROC) (GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const GLvoid *indices);
'SUB GLDRAWRANGEELEMENTSWINPROC (BYVAL mode AS DWORD, BYVAL start AS DWORD, BYVAL uend AS DWORD, BYVAL count AS LONG, BYVAL nType AS DWORD, BYVAL indices AS DWORD)

'/* WIN_swap_hint */
'typedef void (APIENTRY * PFNGLADDSWAPHINTRECTWINPROC)  (GLint x, GLint y, GLsizei width, GLsizei height);
'SUB GLADDSWAPHINTRECTWINPROC )BYVAL x AS LONG, BYVAL y AS LONG, BYVAL nWidth AS LONG, BYVAL nHeight AS LONG)

'/* EXT_paletted_texture */
'typedef void (APIENTRY * PFNGLCOLORTABLEEXTPROC)
'    (GLenum target, GLenum internalFormat, GLsizei width, GLenum format,
'     GLenum type, const GLvoid *data);
'SUB GLCOLORTABLEEXTPROC (BYVAL nTarget AS DWORD, BYVAL internalFormat AS DWORD, BYVAL nWidth AS LONG, BYVAL nFormat AS DWORD, _
'     BYVAL nType AS DWORD, BYVAL pdata AS DWORD)

'typedef void (APIENTRY * PFNGLCOLORSUBTABLEEXTPROC)
'    (GLenum target, GLsizei start, GLsizei count, GLenum format,
'     GLenum type, const GLvoid *data);
'SUB GLCOLORSUBTABLEEXTPROC (BYVAL nTarget AS DWORD, BYVAL start AS LONG, BYVAL count AS LONG, BYVAL nFormat AS DWORD, _
'    BYVAL nType AS DWORD, BYVAL pdata AS DWORD)

'typedef void (APIENTRY * PFNGLGETCOLORTABLEEXTPROC)
'    (GLenum target, GLenum format, GLenum type, GLvoid *data);
'SUB GLGETCOLORTABLEEXTPROC (BYVAL nTarget AS DWORD, BYVAL nFormat AS DWORD, BYVAL nType AS DWORD, BYVAL pdata AS DWORD)

'typedef void (APIENTRY * PFNGLGETCOLORTABLEPARAMETERIVEXTPROC)
'    (GLenum target, GLenum pname, GLint *params);
'SUB GLGETCOLORTABLEPARAMETERIVEXTPROC (BYVAL nTarget AS DWORD, BYVAL pname AS DWORD, BYVAL params AS DWORD)

'typedef void (APIENTRY * PFNGLGETCOLORTABLEPARAMETERFVEXTPROC)
'    (GLenum target, GLenum pname, GLfloat *params);
'SUB GLGETCOLORTABLEPARAMETERFVEXTPROC (BYVAL nTarget AS DWORD, BYVAL pname AS DWORD, BYVAL params AS DWORD)


' ########################################################################################
'                                 *** HELPER FUNCTIONS ***
' ########################################################################################

' =======================================================================================
' Returns a string describing some aspect of the current OpenGL connection.
' Parameter:
' dwName : One of the following symbolic constants:
'   %GL_VENDOR
'      Returns the company responsible for this OpenGL implementation.
'      This name does not change from release to release.
'   %GL_RENDERER
'      Returns the name of the renderer. This name is typically specific to a
'      particular configuration of a hardware platform. It does not change from release
'      to release.
'   %GL_VERSION
'      Returns a version or release number.
'   %GL_EXTENSIONS
'      Returns a space-separated list of supported extensions to OpenGL.
' Error codes:
'   %GL_INVALID_ENUM
'      dwName was not an accepted value.
'   %GL_INVALID_OPERATION
'      glGetString was called between a call to glBegin and the corresponding call to glEnd.
' =======================================================================================
FUNCTION GL_GetString (BYVAL dwName AS DWORD) AS STRING

   LOCAL strOut AS STRING
   LOCAL pStr AS DWORD
   LOCAL nLen AS LONG

   pStr = glGetString(dwName)
   IF pStr THEN
      nLen = lstrlen(BYVAL pStr)
      IF nLen THEN
         strOut = SPACE$(nLen)
         MoveMemory BYVAL STRPTR(strOut), BYVAL pStr, nLen
         IF RIGHT$(strOut, 1) <> " " THEN strOut += strOut & " "
         FUNCTION = strOut
      END IF
   END IF

END FUNCTION
' =======================================================================================

' =======================================================================================
' Checks if an extension is supported.
' Parameter:
' strExtensionName
'   Name of the extension.
' Return value:
'   %TRUE or %FALSE.
' =======================================================================================
FUNCTION GL_IsExtensionSupported (BYVAL strExtensionName AS STRING) AS LONG
   FUNCTION = INSTR(UCASE$(GL_GetSTring(%GL_EXTENSIONS)), UCASE$(strExtensionName) & " ") <> 0
END FUNCTION
' =======================================================================================

#ENDIF   ' #IF NOT %DEF(%GL_INC)

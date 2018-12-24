'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles everything related to entities
' =============================================================================

' -----------------------------------------------------------------------------
'
' [!] Assembler routines used, can behave odd when using PB debugger
'
' -----------------------------------------------------------------------------
'
' [TODO]: Would be nice to have some kind of radius function for lights
' [TODO]: Copying of entities could have some more params regarding their names
' [TODO]: Might be advantageous to take advantage of PB OOP code, but it currently
'         does not allow duplicating of the same instance, wich would make problem
'         with Copying entities ... maybe I got it wrong
'
' -----------------------------------------------------------------------------

' -- Double precision is good for precision when calculating transformations,
' -- on the other side graphic card drivers usually operate with SINGLE
' -- There is probably some performance hit, but should be little
TYPE TBGL_tVector3D
  x AS DOUBLE
  y AS DOUBLE
  z AS DOUBLE
END TYPE

' -- Type used in all entities, it contains all common data for them
TYPE t_EntityInfo
  EntType     AS LONG       ' kind of entity
  EntParent   AS LONG       ' parent ID

  ' -- Common properties
  use     AS LONG           ' is in use
  userPointer AS LONG       ' user pointer to data

  ' - Position ( m )
  x       AS DOUBLE
  y       AS DOUBLE
  z       AS DOUBLE

  ' - Backup Position ( m )
  bux       AS DOUBLE
  buy       AS DOUBLE
  buz       AS DOUBLE

  ' - Size ( *factor )
  sx      AS DOUBLE
  sy      AS DOUBLE
  sz      AS DOUBLE

  ' -- Local axes data
  LocalAxisX  AS TBGL_tVector3D
  LocalAxisY  AS TBGL_tVector3D
  LocalAxisZ  AS TBGL_tVector3D

END TYPE
' -- You need to create a dummy pointer for each of the structure types
GLOBAL dummy_PtrTo_t_EntityInfo         AS t_EntityInfo PTR

TYPE tEntityIdentifier
  scene   AS LONG
  entity  AS LONG
END TYPE

GLOBAL gCallingEntity AS tEntityIdentifier

%TBGL_ColumnRow = 0
%TBGL_RowColumn = 1

' -----------------------------------------------------------------------------
GLOBAL Scene_LastID AS LONG
GLOBAL Scene_UserData_LastID AS LONG
GLOBAL Scene_Name_LastID AS LONG
' -- Main entity data storage
GLOBAL MyArrayOf_TBGLScene()                AS STRING    ' Array scenes x entities
GLOBAL TBGLSceneEntities()                  AS STRING     ' Array of entities, it dimensions just in time :P
GLOBAL MyArrayOf_TBGLScene_EntityMaxID()    AS LONG     ' Max real count of entities in each scene

' -- Entity name handling
GLOBAL MyArrayOf_TBGLSceneNames() AS STRING
GLOBAL TBGLSceneEntitiesNames() AS STRING

' -- Entity data storage
GLOBAL MyArrayOf_TBGLSceneUserData() AS STRING
GLOBAL TBGLSceneEntitiesUserData() AS STRING

GLOBAL TBGLScene_Max  AS LONG
GLOBAL TBGLEntity_MaxDefault AS LONG

' -----------------------------------------------------------------------------
' -- Few useful macros, they really help make code cleaner :P

MACRO DegToRad(dpDegrees) = (dpDegrees*0.0174532925199433##)
MACRO RadToDeg(dpRadians) = (dpRadians*57.29577951308232##)

MACRO MatrixSetIdentity( array16 )
  ARRAY ASSIGN array16() = 1.0#, 0.0#, 0.0#, 0.0#, _
                           0.0#, 1.0#, 0.0#, 0.0#, _
                           0.0#, 0.0#, 1.0#, 0.0#, _
                           0.0#, 0.0#, 0.0#, 1.0#
END MACRO

' -- Assigning zeros is faster than RESET statement
MACRO Reset_TBGL_tVector3D(v)
  v.x = 0
  v.y = 0
  v.z = 0
END MACRO

' -- Assignment shortcut for vectors
MACRO F3dVector(V, a, b, c)
  V.x = a
  V.y = b
  V.z = c
END MACRO

' -- Axis defaults
MACRO DefaultAxisZ_X = 0.0#
MACRO DefaultAxisZ_Y = 0.0#
MACRO DefaultAxisZ_Z = 1.0#

MACRO DefaultAxisY_X = 0.0#
MACRO DefaultAxisY_Y = 1.0#
MACRO DefaultAxisY_Z = 0.0#

MACRO DefaultAxisX_X = 1.0#
MACRO DefaultAxisX_Y = 0.0#
MACRO DefaultAxisX_Z = 0.0#

' -- Vector normalization
MACRO Normalize3dVector( v )
  MACROTEMP l
  DIM l AS DOUBLE

  l# = SQR(v.x#*v.x#+v.y#*v.y#+v.z#*v.z#)'GetF3dVectorLength(v)

  IF l# <> 0.0# THEN
    v.x# = v.x# / l#
    v.y# = v.y# / l#
    v.z# = v.z# / l#
  END IF

END MACRO

' --
MACRO VectorCross( r, v, v2 )
  r.x = v.y * v2.z - v.z * v2.y
  r.y = v.z * v2.x - v.x * v2.z
  r.z = v.x * v2.y - v.y * v2.x
END MACRO

' -- Little bit of goniometry
MACRO ArcCos(value) = (pi / 2 - ATN(Value / SQR(1 - Value * Value)))
MACRO ArcSin(value) = (ATN(Value / SQR(1 - Value * Value)))

MACRO Fill3x3( matrix, ax, ay, az )      ' unused
    ARRAY ASSIGN matrix() = ax.x, ax.y, ax.z, _
                            ay.x, ay.y, ay.z, _
                            az.x, az.y, az.z
END MACRO

MACRO Get3x3( matrix, ax, ay, az )       ' unused

    ax.x = matrix( 1, 1)
    ax.y = matrix( 2, 1)
    ax.z = matrix( 3, 1)

    ay.x = matrix( 1, 2)
    ay.y = matrix( 2, 2)
    ay.z = matrix( 3, 2)

    az.x = matrix( 1, 3)
    az.y = matrix( 2, 3)
    az.z = matrix( 3, 3)

END MACRO

' -----------------------------------------------------------------------------

' -- Passing data to entity buffer
MACRO macro_t_EntityInfo_FillData(ptrTo_t_EntityInfo, nEntType, nEntParent, nUse, nx, ny, nz)

  @ptrTo_t_EntityInfo.EntType   = nEntType
  @ptrTo_t_EntityInfo.EntParent = nEntParent
  @ptrTo_t_EntityInfo.use       = nUse

  @ptrTo_t_EntityInfo.x         = nx
  @ptrTo_t_EntityInfo.y         = ny
  @ptrTo_t_EntityInfo.z         = nz

  @ptrTo_t_EntityInfo.sx        = 1
  @ptrTo_t_EntityInfo.sy        = 1
  @ptrTo_t_EntityInfo.sz        = 1

  F3DVector( @ptrTo_t_EntityInfo.LocalAxisZ, DefaultAxisZ_X, DefaultAxisZ_Y, DefaultAxisZ_Z )
  F3DVector( @ptrTo_t_EntityInfo.LocalAxisY, DefaultAxisY_X, DefaultAxisY_Y, DefaultAxisY_Z )
  F3DVector( @ptrTo_t_EntityInfo.LocalAxisX, DefaultAxisX_X, DefaultAxisX_Y, DefaultAxisX_Z )

END MACRO

' -----------------------------------------------------------------------------

%TBGL_ENTITY_EMPTY          = 0
%TBGL_ENTITY_LIGHT          = 1
%TBGL_ENTITY_CAMERA         = 2
%TBGL_ENTITY_PIVOT          = 3
%TBGL_ENTITY_BOUNDINGSPHERE = 4

%TBGL_ENTITY_MODEL          = 5
%TBGL_ENTITY_DISPLAYLIST    = 6

%TBGL_ENTITY_BOX            = 7
%TBGL_ENTITY_SPHERE         = 8
%TBGL_ENTITY_TORUS          = 9
%TBGL_ENTITY_CYLINDER       = 10

%TBGL_ENTITY_FUNCSLOT       = 11

' -- Entity specific functions
#INCLUDE "entity_Model.inc"
#INCLUDE "entity_DisplayList.inc"
#INCLUDE "entity_Camera.inc"
#INCLUDE "entity_Light.inc"
#INCLUDE "entity_Pivot.inc"
#INCLUDE "entity_BoundingSphere.inc"
#INCLUDE "entity_Box.inc"
#INCLUDE "entity_Sphere.inc"
#INCLUDE "entity_Torus.inc"
#INCLUDE "entity_Cylinder.inc"
#INCLUDE "entity_FuncSlot.inc"
' -- Entity common functions are listed in this file

' -----------------------------------------------------------------------------
' -- SCENE
' -----------------------------------------------------------------------------

' -- Creates new scene space, please not destroying scene is done via creating new one!
SUB Exec_TBGL_SceneCreate()

  LOCAL ScriptSceneID AS EXT

  ScriptSceneID = thinBASIC_Parse1Number
  IF ScriptSceneID < 1 THEN EXIT SUB

  IF ScriptSceneID > UBOUND(MyArrayOf_TBGLScene) THEN
    REDIM PRESERVE MyArrayOf_TBGLScene(1 TO ScriptSceneID)
    REDIM PRESERVE MyArrayOf_TBGLScene_EntityMaxID(1 TO ScriptSceneID)
    REDIM PRESERVE MyArrayOf_TBGLSceneUserData(1 TO ScriptSceneID)
    REDIM PRESERVE MyArrayOf_TBGLSceneNames(1 TO ScriptSceneID)
  END IF

  ' -- A LONG (4 bytes) can store a string
  ' -- Here we create an array of pointer to dynamic strings. Each string will be the storage of other objects
  MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)  = TBGLEntity_MaxDefault

  LOCAL FillString AS STRING
  FillString = STRING$(MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID) * 4&, 0)
  MyArrayOf_TBGLScene(ScriptSceneID)              = FillString
  MyArrayOf_TBGLSceneUserData(ScriptSceneID)      = FillString
  MyArrayOf_TBGLSceneNames(ScriptSceneID)          = FillString

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)
  '

END SUB

' -- Renders scene
SUB Exec_TBGL_SceneRender()

  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT

  ScriptSceneID = thinBASIC_Parse1Number
  internal_SetEntityOverlay(ScriptSceneID)

  ' -- First pass cameras
  LOCAL p, o, az, ay AS TBGL_tVector3D
  LOCAL maxEntities AS LONG
  LOCAL fixFOV, fixColorMask AS LONG
  maxEntities = MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)

  FOR i = 1 TO maxEntities

    ' -- What entity is here?
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(i))
    IF @dummy_PtrTo_t_EntityInfo.EntType <> %TBGL_ENTITY_CAMERA OR @dummy_PtrTo_t_EntityInfo.Use = 0 THEN ITERATE FOR

    ' -- If it is camera, we can continue setting its parameters
    dummy_PtrTo_t_Entity_Camera = macro_t_Entity_Camera_MemPos(TBGLSceneEntities(i))
    IF @dummy_PtrTo_t_Entity_Camera.FOV <> 45 THEN
      fixFOV = 1
      g_defaultFOV = @dummy_PtrTo_t_Entity_Camera.FOV
      internal_RefreshViewportSoft()
    END IF

    ' -- If camera supports color mask, we will set it
    IF @dummy_PtrTo_t_Entity_Camera.ColorMask <> 15 THEN
      fixColorMask = 1
      glColorMask(BIT(@dummy_PtrTo_t_Entity_Camera.ColorMask, 0), BIT(@dummy_PtrTo_t_Entity_Camera.ColorMask, 1), BIT(@dummy_PtrTo_t_Entity_Camera.ColorMask, 2), BIT(@dummy_PtrTo_t_Entity_Camera.ColorMask, 3))
    END IF

    Reset_TBGL_tVector3D(o)
    Reset_TBGL_tVector3D(az)
    Reset_TBGL_tVector3D(ay)

    ' -- We will evaluate camera true position and orientation (useful in case camera is parent of something else)
    CALL internal_TRACKPOS( dummy_PtrTo_t_EntityInfo, p, o, az, ay)

    ' -- This is smarter use of gluLookAt than default TBGL_Camera, can be rotated arbitrary
    gluLookAt( p.x, p.y, p.z, _
               p.x+az.x,p.y+az.y,p.z+az.z, _
               ay.x, ay.y, ay.z)

    EXIT FOR

  NEXT

  ' -- Second pass for lighting
  ' -- We need to set all changeable params to used sources
  DIM Vector4(0 TO 3) AS LOCAL SINGLE
  DIM Vector3(0 TO 2) AS LOCAL SINGLE
  LOCAL Vector1 AS SINGLE

  LOCAL Lighter, LightSrc AS LONG
  Lighter = 0
  LightSrc =  %GL_LIGHT1-1

  FOR i = 1 TO maxEntities
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(i))
    IF @dummy_PtrTo_t_EntityInfo.EntType <> %TBGL_ENTITY_LIGHT THEN ITERATE FOR
    IF @dummy_PtrTo_t_EntityInfo.Use = 0 THEN ITERATE FOR

    ' -- If we deal with light, we can set all necessary
    INCR Lighter
    IF Lighter = 1 THEN
      glEnable (%GL_LIGHTING)
    ELSEIF Lighter > 7 THEN
      EXIT FOR
    END IF

    ' -- Here we assign one of available HW light sources to our entity dynamically
    LightSrc = CHOOSE(Lighter, %GL_LIGHT1, %GL_LIGHT2, %GL_LIGHT3, %GL_LIGHT4, %GL_LIGHT5, %GL_LIGHT6, %GL_LIGHT7 )
    glEnable (LightSrc)

    dummy_PtrTo_t_Entity_Light = macro_t_Entity_Light_MemPos(TBGLSceneEntities(i))
    Reset_TBGL_tVector3D(o)
    Reset_TBGL_tVector3D(az)

    ' -- We get light position here, [!] it is necessary to do this AFTER camera is set
    CALL internal_TRACKPOS( dummy_PtrTo_t_EntityInfo, p, o, az)

    IF @dummy_PtrTo_t_Entity_Light.cType <> %TBGL_LIGHTTYPE_DIRECTIONAL THEN
      ARRAY ASSIGN Vector4() = p.x, p.y, p.z, 1.0!
    ELSE
      ARRAY ASSIGN Vector4() = -@dummy_PtrTo_t_EntityInfo.LocalAxisZ.x, -@dummy_PtrTo_t_EntityInfo.LocalAxisZ.y, -@dummy_PtrTo_t_EntityInfo.LocalAxisZ.z, 0.0!
    END IF

    ' -- OpenGL stuff, setting params
    glLightfv LightSrc, %GL_POSITION, Vector4(0)

    glLightfv LightSrc, %GL_DIFFUSE, @dummy_PtrTo_t_Entity_Light.rgba(0)
    glLightfv LightSrc, %GL_AMBIENT, @dummy_PtrTo_t_Entity_Light.ambient(0)
    glLightfv LightSrc, %GL_SPECULAR, @dummy_PtrTo_t_Entity_Light.specular(0)

    Vector1 = @dummy_PtrTo_t_Entity_Light.Cutoff
    glLightfv LightSrc, %GL_SPOT_CUTOFF, Vector1

    IF @dummy_PtrTo_t_Entity_Light.cType = %TBGL_LIGHTTYPE_SPOT THEN
      ARRAY ASSIGN Vector3() = az.x, az.y, az.z
      glLightfv LightSrc, %GL_SPOT_DIRECTION,  Vector3(0)
      Vector1 = @dummy_PtrTo_t_Entity_Light.BorderFade
      glLightfv LightSrc, %GL_SPOT_EXPONENT,  Vector1
    END IF

  NEXT

  ' -- Third pass are renderable objects


  ' -- Important to take care of texturing, to disable it after rendering in case it is not normally used
  LOCAL wasTexturingOn AS LONG
  glGetIntegerV(%GL_TEXTURE_2D, gUseTexture)
  wasTexturingOn = gUseTexture

  FOR i = 1 TO maxEntities
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(i))

    IF @dummy_PtrTo_t_EntityInfo.EntType < %TBGL_ENTITY_MODEL OR @dummy_PtrTo_t_EntityInfo.Use = 0 THEN ITERATE FOR

    ' -- Here we handle entity according to type
    ' -- It is routine - retrieve handle to entity+ data (specific to given type) and then take appropriate action
    ' -- Push/Pop is necessary to not inherit transformations from consequent entities
    SELECT CASE AS LONG @dummy_PtrTo_t_EntityInfo.EntType
      CASE %TBGL_ENTITY_MODEL

        dummy_PtrTo_t_Entity_Model = macro_t_Entity_Model_MemPos(TBGLSceneEntities(i))

        glPushMatrix

          internal_DoTransformationOfEntity(dummy_PtrTo_t_EntityInfo)

            IF @dummy_PtrTo_t_EntityInfo.sx <> 1 OR @dummy_PtrTo_t_EntityInfo.sy <> 1 OR @dummy_PtrTo_t_EntityInfo.sz <> 1 THEN glScaleF @dummy_PtrTo_t_EntityInfo.sx, @dummy_PtrTo_t_EntityInfo.sy, @dummy_PtrTo_t_EntityInfo.sz
            internal_DrawModel(@dummy_PtrTo_t_Entity_Model.slot, @dummy_PtrTo_t_Entity_Model.ForceTex, @dummy_PtrTo_t_Entity_Model.ForceR, @dummy_PtrTo_t_Entity_Model.ForceG, @dummy_PtrTo_t_Entity_Model.ForceB)

         glPopMatrix


      CASE %TBGL_ENTITY_DISPLAYLIST

        dummy_PtrTo_t_Entity_DisplayList = macro_t_Entity_DisplayList_MemPos(TBGLSceneEntities(i))

        IF @dummy_PtrTo_t_Entity_DisplayList.ForceTex > 0 THEN
          glEnable %GL_TEXTURE_2D
          glBindTexture %GL_TEXTURE_2D, lpTexture(@dummy_PtrTo_t_Entity_DisplayList.ForceTex)
          gUseTexture = 1
        ELSEIF gUseTexture <> 0 THEN
          glDisable %GL_TEXTURE_2D
          gUseTexture = 0
        END IF

        glPushMatrix
          internal_DoTransformationOfEntity(dummy_PtrTo_t_EntityInfo)
            IF @dummy_PtrTo_t_EntityInfo.sx <> 1 OR @dummy_PtrTo_t_EntityInfo.sy <> 1 OR @dummy_PtrTo_t_EntityInfo.sz <> 1 THEN glScaleF @dummy_PtrTo_t_EntityInfo.sx, @dummy_PtrTo_t_EntityInfo.sy, @dummy_PtrTo_t_EntityInfo.sz
            IF @dummy_PtrTo_t_Entity_DisplayList.ForceR >= 0 THEN glcolor3ub @dummy_PtrTo_t_Entity_DisplayList.ForceR, @dummy_PtrTo_t_Entity_DisplayList.ForceG, @dummy_PtrTo_t_Entity_DisplayList.ForceB
            IF @dummy_PtrTo_t_Entity_DisplayList.ForceTex > 0 THEN glBindTexture %GL_TEXTURE_2D, lpTexture(@dummy_PtrTo_t_Entity_DisplayList.ForceTex)

               glCallList @dummy_PtrTo_t_Entity_DisplayList.slot

            IF @dummy_PtrTo_t_Entity_DisplayList.ForceR >= 0 THEN glColor3UB 255,255,255
        glPopMatrix


      CASE %TBGL_ENTITY_BOX

        dummy_PtrTo_t_Entity_Box = macro_t_Entity_Box_MemPos(TBGLSceneEntities(i))

        glPushMatrix
          internal_DoTransformationOfEntity(dummy_PtrTo_t_EntityInfo)
            IF @dummy_PtrTo_t_Entity_Box.ForceTex = -1 THEN
                glDisable %GL_Texture_2D
                gUseTexture = 0
            ELSEIF @dummy_PtrTo_t_Entity_Box.ForceTex > 0 THEN
                glEnable %GL_TEXTURE_2D
                glBindTexture %GL_TEXTURE_2D, lpTexture(@dummy_PtrTo_t_Entity_Box.ForceTex)
                gUseTexture = 1
            END IF

            IF @dummy_PtrTo_t_EntityInfo.sx <> 1 OR @dummy_PtrTo_t_EntityInfo.sy <> 1 OR @dummy_PtrTo_t_EntityInfo.sz <> 1 THEN glScaleF @dummy_PtrTo_t_EntityInfo.sx, @dummy_PtrTo_t_EntityInfo.sy, @dummy_PtrTo_t_EntityInfo.sz

             glColor3ub @dummy_PtrTo_t_Entity_Box.cRGB(0), @dummy_PtrTo_t_Entity_Box.cRGB(1), @dummy_PtrTo_t_Entity_Box.cRGB(2)

             internal_RenderBox @dummy_PtrTo_t_Entity_Box.a, @dummy_PtrTo_t_Entity_Box.b, @dummy_PtrTo_t_Entity_Box.c

             IF @dummy_PtrTo_t_Entity_Box.cRGB(0) <> 255 AND @dummy_PtrTo_t_Entity_Box.cRGB(1) <> 255 AND @dummy_PtrTo_t_Entity_Box.cRGB(2) <> 255 THEN glColor3UB 255,255,255
        glPopMatrix


      CASE %TBGL_ENTITY_SPHERE

        dummy_PtrTo_t_Entity_Sphere = macro_t_Entity_Sphere_MemPos(TBGLSceneEntities(i))

        glPushMatrix
          internal_DoTransformationOfEntity(dummy_PtrTo_t_EntityInfo)
            IF @dummy_PtrTo_t_Entity_sphere.ForceTex = -1 THEN
                glDisable %GL_Texture_2D
                gUseTexture = 0
            ELSEIF @dummy_PtrTo_t_Entity_sphere.ForceTex > 0 THEN
                glEnable %GL_TEXTURE_2D
                glBindTexture %GL_TEXTURE_2D, lpTexture(@dummy_PtrTo_t_Entity_sphere.ForceTex)
                gUseTexture = 1
            END IF

            IF @dummy_PtrTo_t_EntityInfo.sx <> 1 OR @dummy_PtrTo_t_EntityInfo.sy <> 1 OR @dummy_PtrTo_t_EntityInfo.sz <> 1 THEN glScaleF @dummy_PtrTo_t_EntityInfo.sx, @dummy_PtrTo_t_EntityInfo.sy, @dummy_PtrTo_t_EntityInfo.sz
               glColor3ub @dummy_PtrTo_t_Entity_sphere.cRGB(0), @dummy_PtrTo_t_Entity_sphere.cRGB(1), @dummy_PtrTo_t_Entity_sphere.cRGB(2)

               internal_RenderSphere @dummy_PtrTo_t_Entity_sphere.r

               IF @dummy_PtrTo_t_Entity_sphere.cRGB(0) <> 255 AND @dummy_PtrTo_t_Entity_sphere.cRGB(1) <> 255 AND @dummy_PtrTo_t_Entity_sphere.cRGB(2) <> 255 THEN glColor3UB 255,255,255
        glPopMatrix


      CASE %TBGL_ENTITY_TORUS

        dummy_PtrTo_t_Entity_Torus = macro_t_Entity_Torus_MemPos(TBGLSceneEntities(i))

        glPushMatrix
          internal_DoTransformationOfEntity(dummy_PtrTo_t_EntityInfo)
            IF @dummy_PtrTo_t_Entity_Torus.ForceTex = -1 THEN
                glDisable %GL_Texture_2D
                gUseTexture = 0
            ELSEIF @dummy_PtrTo_t_Entity_Torus.ForceTex > 0 THEN
                glEnable %GL_TEXTURE_2D
                glBindTexture %GL_TEXTURE_2D, lpTexture(@dummy_PtrTo_t_Entity_Torus.ForceTex)
                gUseTexture = 1
            END IF

            IF @dummy_PtrTo_t_EntityInfo.sx <> 1 OR @dummy_PtrTo_t_EntityInfo.sy <> 1 OR @dummy_PtrTo_t_EntityInfo.sz <> 1 THEN glScaleF @dummy_PtrTo_t_EntityInfo.sx, @dummy_PtrTo_t_EntityInfo.sy, @dummy_PtrTo_t_EntityInfo.sz
               glColor3ub @dummy_PtrTo_t_Entity_Torus.cRGB(0), @dummy_PtrTo_t_Entity_Torus.cRGB(1), @dummy_PtrTo_t_Entity_Torus.cRGB(2)

               internal_RenderTorus @dummy_PtrTo_t_Entity_Torus.r, @dummy_PtrTo_t_Entity_Torus.r2

               IF @dummy_PtrTo_t_Entity_Torus.cRGB(0) <> 255 AND @dummy_PtrTo_t_Entity_Torus.cRGB(1) <> 255 AND @dummy_PtrTo_t_Entity_Torus.cRGB(2) <> 255 THEN glColor3UB 255,255,255
        glPopMatrix


      CASE %TBGL_ENTITY_CYLINDER

        dummy_PtrTo_t_Entity_Cylinder = macro_t_Entity_Cylinder_MemPos(TBGLSceneEntities(i))

        glPushMatrix
          internal_DoTransformationOfEntity(dummy_PtrTo_t_EntityInfo)
            IF @dummy_PtrTo_t_Entity_Cylinder.ForceTex = -1 THEN
                glDisable %GL_Texture_2D
                gUseTexture = 0
            ELSEIF @dummy_PtrTo_t_Entity_Cylinder.ForceTex > 0 THEN
                glEnable %GL_TEXTURE_2D
                glBindTexture %GL_TEXTURE_2D, lpTexture(@dummy_PtrTo_t_Entity_Cylinder.ForceTex)
                gUseTexture = 1
            END IF

            IF @dummy_PtrTo_t_EntityInfo.sx <> 1 OR @dummy_PtrTo_t_EntityInfo.sy <> 1 OR @dummy_PtrTo_t_EntityInfo.sz <> 1 THEN glScaleF @dummy_PtrTo_t_EntityInfo.sx, @dummy_PtrTo_t_EntityInfo.sy, @dummy_PtrTo_t_EntityInfo.sz
               glColor3ub @dummy_PtrTo_t_Entity_Cylinder.cRGB(0), @dummy_PtrTo_t_Entity_Cylinder.cRGB(1), @dummy_PtrTo_t_Entity_Cylinder.cRGB(2)

               internal_RenderCylinder @dummy_PtrTo_t_Entity_Cylinder.r, @dummy_PtrTo_t_Entity_Cylinder.r2, @dummy_PtrTo_t_Entity_Cylinder.h, @dummy_PtrTo_t_Entity_Cylinder.capped

               IF @dummy_PtrTo_t_Entity_Cylinder.cRGB(0) <> 255 AND @dummy_PtrTo_t_Entity_Cylinder.cRGB(1) <> 255 AND @dummy_PtrTo_t_Entity_Cylinder.cRGB(2) <> 255 THEN glColor3UB 255,255,255
        glPopMatrix

      CASE %TBGL_ENTITY_FUNCSLOT

        dummy_PtrTo_t_Entity_FuncSlot = macro_t_Entity_FuncSlot_MemPos(TBGLSceneEntities(i))

        IF @dummy_PtrTo_t_Entity_FuncSlot.ForceTex > 0 THEN
          glEnable %GL_TEXTURE_2D
          glBindTexture %GL_TEXTURE_2D, lpTexture(@dummy_PtrTo_t_Entity_FuncSlot.ForceTex)
          gUseTexture = 1
        ELSEIF gUseTexture <> 0 THEN
          glDisable %GL_TEXTURE_2D
          gUseTexture = 0
        END IF

        glPushMatrix
          internal_DoTransformationOfEntity(dummy_PtrTo_t_EntityInfo)
            IF @dummy_PtrTo_t_EntityInfo.sx <> 1 OR @dummy_PtrTo_t_EntityInfo.sy <> 1 OR @dummy_PtrTo_t_EntityInfo.sz <> 1 THEN glScaleF @dummy_PtrTo_t_EntityInfo.sx, @dummy_PtrTo_t_EntityInfo.sy, @dummy_PtrTo_t_EntityInfo.sz
            IF @dummy_PtrTo_t_Entity_FuncSlot.ForceR >= 0 THEN glcolor3ub @dummy_PtrTo_t_Entity_FuncSlot.ForceR, @dummy_PtrTo_t_Entity_FuncSlot.ForceG, @dummy_PtrTo_t_Entity_FuncSlot.ForceB
            IF @dummy_PtrTo_t_Entity_FuncSlot.ForceTex > 0 THEN glBindTexture %GL_TEXTURE_2D, lpTexture(@dummy_PtrTo_t_Entity_FuncSlot.ForceTex) ' [?] TWICE?

              gCallingEntity.scene = ScriptSceneID
              gCallingEntity.entity= i

              thinBasic_FunctionSimpleCall_ByPtr(@dummy_PtrTo_t_Entity_FuncSlot.fPTR)

            IF @dummy_PtrTo_t_Entity_FuncSlot.ForceR >= 0 THEN glColor3UB 255,255,255
        glPopMatrix

    END SELECT
  NEXT

  ' -- This code is related to targetting and restoring original position
  FOR i = 1 TO MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(i))
    IF @dummy_PtrTo_t_EntityInfo.EntParent < 0 THEN
      @dummy_PtrTo_t_EntityInfo.EntParent = -@dummy_PtrTo_t_EntityInfo.EntParent
      @dummy_PtrTo_t_EntityInfo.x = @dummy_PtrTo_t_EntityInfo.bux
      @dummy_PtrTo_t_EntityInfo.y = @dummy_PtrTo_t_EntityInfo.buy
      @dummy_PtrTo_t_EntityInfo.z = @dummy_PtrTo_t_EntityInfo.buz
    END IF

  NEXT

  ' -- Restoring states
  IF wasTexturingOn THEN
    glEnable %GL_TEXTURE_2D
    gUseTexture = 1
  ELSE
    glDisable %GL_TEXTURE_2D
    gUseTexture = 0
  END IF

  IF fixFOV THEN
    g_defaultFOV = 45
    internal_RefreshViewportSoft()
  END IF

  IF fixColorMask THEN
    glColorMask(1, 1, 1, 1)
  END IF

END SUB

' -- Scene collide
SUB Exec_TBGL_SceneCollide()

  REGISTER i AS LONG, j AS LONG

  LOCAL ScriptSceneID AS EXT

  LOCAL p, o, az, ay AS TBGL_tVector3D
  LOCAL maxEntities AS LONG

  ScriptSceneID = thinBASIC_Parse1Number
  internal_SetEntityOverlay(ScriptSceneID)

  maxEntities = MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)

  ' -- First pass checks how many collideable primitives there are and collects them together
  LOCAL maxCollideEntities AS LONG
  LOCAL cID AS LONG
  DIM collideEntities(1 TO maxEntities) AS LONG

  FOR i = 1 TO maxEntities

    ' -- What entity is here?
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(i))
    IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_BOUNDINGSPHERE AND @dummy_PtrTo_t_EntityInfo.Use <> 0 THEN
      cID += 1
      collideEntities(cID) = i
    END IF

  NEXT
  maxCollideEntities = cID
  LOCAL p2 AS TBGL_tVector3D
  LOCAL x, y, z, radius AS SINGLE
  LOCAL x2, y2, z2, radius2 AS SINGLE
  LOCAL dx, dy, dz, rplusr2 AS SINGLE
  LOCAL length AS SINGLE
  LOCAL distance AS SINGLE
  LOCAL dummy_PtrTo_t_EntityInfo2 AS t_EntityInfo PTR
  LOCAL dummy_PtrTo_t_Entity_BoundingSphere2 AS t_Entity_BoundingSphere PTR

  RESET g_CollisionInfo

  ' -- Second pass performs the tests
  FOR i = 1 TO maxCollideEntities
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(collideEntities(i)))

    ' -- Here we handle entity according to type
    ' -- It is routine - retrieve handle to entity+ data (specific to given type) and then take appropriate action
    SELECT CASE AS LONG @dummy_PtrTo_t_EntityInfo.EntType
      CASE %TBGL_ENTITY_BOUNDINGSPHERE

        dummy_PtrTo_t_Entity_BoundingSphere = macro_t_Entity_Model_MemPos(TBGLSceneEntities(collideEntities(i)))
        ' -- Zero PTR means passive collider
        IF @dummy_PtrTo_t_Entity_BoundingSphere.fPTR = 0 THEN ITERATE FOR

        internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)

        x       = p.x
        y       = p.y
        z       = p.z
        radius  = @dummy_PtrTo_t_Entity_BoundingSphere.Radius

        @dummy_PtrTo_t_Entity_BoundingSphere.collided = 0 ' -- For now, we will see

        FOR j = 1 TO maxCollideEntities
          IF j = i THEN ITERATE FOR
          dummy_PtrTo_t_EntityInfo2 = STRPTR(TBGLSceneEntities(collideEntities(j)))

          ' -- Sphere vs. Sphere
          IF @dummy_PtrTo_t_EntityInfo2.EntType = %TBGL_ENTITY_BOUNDINGSPHERE THEN

            dummy_PtrTo_t_Entity_BoundingSphere2 = macro_t_Entity_Model_MemPos(TBGLSceneEntities(collideEntities(j)))
            internal_TRACKPOS(dummy_PtrTo_t_EntityInfo2, p2)

            x2      = p2.x
            y2      = p2.y
            z2      = p2.z
            radius2 = @dummy_PtrTo_t_Entity_BoundingSphere2.Radius

            rplusr2 = radius+radius2
            dx = x2-x
            IF dx > rplusr2 THEN ITERATE FOR
            dy = y2-y
            IF dy > rplusr2 THEN ITERATE FOR
            dz = z2-z
            IF dz > rplusr2 THEN ITERATE FOR
            distance = SQR(dx*dx + dy*dy + dz*dz)

            ' -- Too big distance, those two will not collide
            IF distance >= rplusr2 THEN ITERATE FOR

            @dummy_PtrTo_t_Entity_BoundingSphere.collided = 1

            ' -- Ok, if we are here, we do collide

              ' -- Collect collision info

              ' -- Depth of impact
              g_CollisionInfo.depth       = rplusr2 - distance

              ' -- Evaluating normal bounce vector
              g_CollisionInfo.normal.x    = -dx'x - x2
              g_CollisionInfo.normal.y    = -dy'y - y2
              g_CollisionInfo.normal.z    = -dz'z - z2

              length = SQR(g_CollisionInfo.normal.x*g_CollisionInfo.normal.x+g_CollisionInfo.normal.y*g_CollisionInfo.normal.y+g_CollisionInfo.normal.z*g_CollisionInfo.normal.z)

              g_CollisionInfo.normal.x /= length
              g_CollisionInfo.normal.y /= length
              g_CollisionInfo.normal.z /= length

              ' -- Point of collision
              g_CollisionInfo.position.x  = x + g_CollisionInfo.normal.x * (g_CollisionInfo.depth-radius)
              g_CollisionInfo.position.y  = y + g_CollisionInfo.normal.y * (g_CollisionInfo.depth-radius)
              g_CollisionInfo.position.z  = z + g_CollisionInfo.normal.z * (g_CollisionInfo.depth-radius)

              ' -- Add information which entity collides
              g_CollisionInfo.scene   = ScriptSceneID
              g_CollisionInfo.entity1 = collideEntities(i)
              g_CollisionInfo.entity2 = collideEntities(j)

              ' -- Call our handler
              thinBasic_FunctionSimpleCall_ByPtr(@dummy_PtrTo_t_Entity_BoundingSphere.fPTR)

          END IF

        NEXT

    END SELECT
  NEXT

END SUB

' -- Passes all entities matching the criteria to array
FUNCTION Exec_TBGL_EntityEnumByDataSignature() AS EXT

  REGISTER EntityID AS LONG, Index AS LONG
  LOCAL maxEntities AS LONG
  LOCAL ScriptSceneID AS EXT
  LOCAL ElementSignature AS EXT
  LOCAL lArrayCellPtr AS LONG
  LOCAL lArrayCellAbsPos AS LONG
  LOCAL VariablePtrOdd, MainType, SubType, ElementsAreFixed, TotElements, ElementSize AS LONG
  LOCAL AbsolutePosition AS LONG


  IF thinBasic_CheckOpenParens_Mandatory() THEN
    thinBasic_ParseNumber(ScriptSceneID)
    maxEntities = MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)
    internal_SetEntityOverlay(ScriptSceneID)
    internal_SetEntityOverlayUserData(ScriptSceneID)

    IF thinBasic_CheckComma_Mandatory() THEN

      thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

      ' -- First dimensioning is to max elements possible
      thinBasic_VariableRedim(lArrayCellPtr, %FALSE, maxEntities, 0, 0)
      TotElements = maxEntities

      IF thinBasic_CheckComma_Mandatory() THEN
        thinBasic_ParseNumber(ElementSignature)

        IF thinBasic_CheckCloseParens_Mandatory() THEN
          FOR EntityID = 1 TO maxEntities

            ' -- What entity is here?
            dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

            ' -- Is there some kind of interesting data
            IF TBGLSceneEntitiesUserData(EntityID) <> "" THEN

              ' -- Get its signature
              DIM Signature AS LONG
              Signature = CVL(TBGLSceneEntitiesUserData(EntityID))


              ' -- Does it match what we seek
              IF Signature = ElementSignature THEN
                ' -- Yes, so lets write the entity ID to target array
                INCR Index

                AbsolutePosition = lArrayCellAbsPos+(Index-1)
                IF AbsolutePosition > TotElements THEN EXIT FOR

                ' -- Write entity ID if suitable
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, AbsolutePosition, EntityID )
              END IF
            END IF
          NEXT

          ' -- Redim the array to match closely the number of found items
          thinBasic_VariableRedim(lArrayCellPtr, %TRUE, MAX(Index, 1), 0, 0)

          FUNCTION = Index
        END IF
      END IF
    END IF
  END IF

END FUNCTION

' -----------------------------------------------------------------------------
' -- GENERAL PURPOSE ENTITY FUNCTIONS, THOSE ENTITY SPECIFIC ARE STICKED IN THEIR INCLUDES
' -----------------------------------------------------------------------------
SUB Exec_TBGL_EntityDestroy()
  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT

  thinBASIC_Parse2Numbers(ScriptSceneID, EntityID)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
  @dummy_PtrTo_t_EntityInfo.entType = %TBGL_ENTITY_EMPTY

  ' -- Entity destruction here is more or less marking entity as ready to be reused
  ' -- It is better for speed this way, we waste just few bytes

END SUB

' -- Entity attach
SUB Exec_TBGL_EntityAttach()
  LOCAL sceneID, entityID, parentEntityID, MODE AS EXT
  thinBASIC_Parse4Numbers(sceneID, entityID, parentEntityID, MODE)

  IF parentEntityID THEN
    internal_SetEntityOverlay(sceneID)

    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(entityID))

    SELECT CASE MODE
      CASE %TBGL_DEFAULT  ' -- Just changing flag
        @dummy_PtrTo_t_EntityInfo.entParent = parentEntityID
        EXIT SUB

      CASE %TBGL_RESET  ' -- Just changing flag and reseting position/rotation
        @dummy_PtrTo_t_EntityInfo.entParent = parentEntityID
        @dummy_PtrTo_t_EntityInfo.x         = 0
        @dummy_PtrTo_t_EntityInfo.y         = 0
        @dummy_PtrTo_t_EntityInfo.z         = 0

        F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisX, DefaultAxisX_X, DefaultAxisX_Y, DefaultAxisX_Z )
        F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisY, DefaultAxisY_X, DefaultAxisY_Y, DefaultAxisY_Z )
        F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisZ, DefaultAxisZ_X, DefaultAxisZ_Y, DefaultAxisZ_Z )

        EXIT SUB
    END SELECT
  END IF
END SUB

' -- Entity detach
SUB Exec_TBGL_EntityDetach()
  LOCAL sceneID, entityID, MODE AS EXT
  thinBASIC_Parse3Numbers(sceneID, entityID, MODE)

  internal_SetEntityOverlay(sceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(entityID))
  ' -- Nothing to detach from
  IF @dummy_PtrTo_t_EntityInfo.entParent = 0 THEN EXIT SUB

  SELECT CASE MODE
    CASE %TBGL_DEFAULT  ' -- Just changing flag
      @dummy_PtrTo_t_EntityInfo.entParent = 0
      EXIT SUB

    CASE %TBGL_RESET  ' -- Just changing flag and reseting position/rotation
      @dummy_PtrTo_t_EntityInfo.entParent = 0
      @dummy_PtrTo_t_EntityInfo.x         = 0
      @dummy_PtrTo_t_EntityInfo.y         = 0
      @dummy_PtrTo_t_EntityInfo.z         = 0

      F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisX, DefaultAxisX_X, DefaultAxisX_Y, DefaultAxisX_Z )
      F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisY, DefaultAxisY_X, DefaultAxisY_Y, DefaultAxisY_Z )
      F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisZ, DefaultAxisZ_X, DefaultAxisZ_Y, DefaultAxisZ_Z )

      EXIT SUB

    CASE %TBGL_PRESERVE ' -- Changing flag + "dereferencing" position and orientation at the time of detach
      LOCAL o, p, r, u, v AS TBGL_tVector3D

      ' -- Retrieve position, X, Y, Z axis
      internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, v, u)

      VectorCross(r,u,v)

      @dummy_PtrTo_t_EntityInfo.x         = p.x
      @dummy_PtrTo_t_EntityInfo.y         = p.y
      @dummy_PtrTo_t_EntityInfo.z         = p.z

      F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisX, r.x, r.y, r.z )
      F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisY, u.x, u.y, u.z )
      F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisZ, v.x, v.y, v.z )

      @dummy_PtrTo_t_EntityInfo.entParent = 0
      EXIT SUB

  END SELECT

END SUB

SUB Exec_TBGL_EntitySetParent()
  LOCAL sceneID, entityID, parentEntityID AS EXT
  thinBASIC_Parse3Numbers(sceneID, entityID, parentEntityID)

  internal_SetEntityOverlay(sceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(entityID))

  @dummy_PtrTo_t_EntityInfo.entParent = parentEntityID
END SUB

FUNCTION Exec_TBGL_EntityGetParent() AS EXT
  LOCAL sceneID, entityID AS EXT
  thinBASIC_Parse2Numbers(sceneID, entityID)

  internal_SetEntityOverlay(sceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(entityID))

  FUNCTION = @dummy_PtrTo_t_EntityInfo.entParent
END FUNCTION


' -- Entity user defined pointer
SUB Exec_TBGL_EntitySetUserPointer()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL uPointer AS EXT

  thinBASIC_Parse3Numbers(ScriptSceneID, EntityID, uPointer)
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  @dummy_PtrTo_t_EntityInfo.userPointer = uPointer

END SUB

FUNCTION Exec_TBGL_EntityGetUserPointer() AS EXT

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT

  thinBASIC_Parse2Numbers(ScriptSceneID, EntityID)
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  FUNCTION = @dummy_PtrTo_t_EntityInfo.userPointer

END FUNCTION

' -- Entity user defined data are stored locally, generally more useful
SUB Exec_TBGL_EntitySetUserData()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL VariablePtr   AS LONG
  LOCAL MainType      AS LONG
  LOCAL SubType       AS LONG
  LOCAL ElementsAreFixed AS LONG
  LOCAL TotElements   AS LONG
  LOCAL ElementSize   AS LONG
  LOCAL DataPtr       AS LONG
  LOCAL AbsPos        AS LONG
  '
  LOCAL sBuffer AS STRING

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID
      IF thinBasic_CheckComma_Mandatory THEN

        thinBasic_ParseVariableInfo( VariablePtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, DataPtr, AbsPos )
        IF thinBasic_CheckCloseParens_Mandatory THEN

          ' -- We mark STRINGs differentely for STRPTR/VARPTR purposes
          IF ElementsAreFixed = %FALSE AND MainType = %VarMainType_IsString THEN
            LOCAL p AS STRING PTR
            p = DataPtr
            sBuffer = LEFT$(@p, LEN(@p))
          ELSE
            ' -- 1234 is magic number which helps to differentiate STRING for numbers and ASCIIZ
            sBuffer = PEEK$( DataPtr, ElementSize )+MKL$(1234)
          END IF

          internal_SetEntityOverlayUserData(ScriptSceneID)

          TBGLSceneEntitiesUserData(EntityID) = sBuffer

        END IF
      END IF
    END IF
  END IF

END SUB

FUNCTION Exec_TBGL_EntityGetUserDataPointer() AS EXT

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT

  thinBasic_Parse2Numbers(ScriptSceneID, EntityID)

  internal_SetEntityOverlayUserData(ScriptSceneID)

  ' -- 1234 is magic number which helps to differentiate STRING for numbers and ASCIIZ
  IF RIGHT$(TBGLSceneEntitiesUserData(EntityID), 4) = MKL$(1234) THEN
    FUNCTION = STRPTR(TBGLSceneEntitiesUserData(EntityID))
  ELSE
    FUNCTION = VARPTR(TBGLSceneEntitiesUserData(EntityID))
  END IF

END FUNCTION

' -- Names are stored separately
SUB Exec_TBGL_EntitySetName()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL sName AS STRING

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID
      IF thinBasic_CheckComma_Mandatory THEN
        thinBasic_ParseString sName
        IF thinBasic_CheckCloseParens_Mandatory THEN

          internal_SetEntityOverlayNames(ScriptSceneID)
          TBGLSceneEntitiesNames(EntityID) = sName

        END IF
      END IF
    END IF
  END IF

END SUB

FUNCTION Exec_TBGL_EntityGetName() AS STRING

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT

  thinBasic_Parse2Numbers(ScriptSceneID, EntityID)

  internal_SetEntityOverlayNames(ScriptSceneID)
  FUNCTION = TBGLSceneEntitiesNames(EntityID)

END FUNCTION

' -- Retrieving free slots
' -- Seeks for empty entity
FUNCTION Exec_TBGL_EntityGetFreeID() AS EXT
  REGISTER i AS LONG
  LOCAL ScriptSceneID AS EXT, EntityFrom AS EXT
  LOCAL maxEntities AS LONG

  EntityFrom = 1
  thinBASIC_ParseXNumbers(1,2, ScriptSceneID, EntityFrom )

  internal_SetEntityOverlay(ScriptSceneID)

  maxEntities = MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)


  IF EntityFrom <= maxEntities THEN

    ' -- Seek for empty ID
    FOR i = EntityFrom TO maxEntities
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(i))
      IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_EMPTY THEN FUNCTION = i : EXIT FUNCTION
    NEXT

    ' -- Nothing found, we will allocate new
    internal_EntityRedimArrayIfNeeded(ScriptSceneID, maxEntities+32)
    FUNCTION = maxEntities + 1
    EXIT FUNCTION
  ELSE

    internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityFrom+32)
    FUNCTION = EntityFrom
    EXIT FUNCTION
  END IF

END FUNCTION


' -- Use flag
SUB Exec_TBGL_EntitySetUse()
  REGISTER i AS LONG
  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL Use AS EXT

  thinBASIC_Parse3Numbers(ScriptSceneID, EntityID, Use)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  ' -- Very simple ... unless dealing with cameras :D
  ' -- Here we need to make sure all cameras are disabled
  IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_CAMERA THEN

    FOR i = 1 TO MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(i))
      IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_CAMERA THEN @dummy_PtrTo_t_EntityInfo.Use = 0     ' -- Disables all other cameras
    NEXT

    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))  ' -- Get back proper pointer
  END IF

  ' -- Routine, just assigning value
  @dummy_PtrTo_t_EntityInfo.Use = Use

END SUB

FUNCTION Exec_TBGL_EntityGetUse() AS EXT
  REGISTER i AS LONG
  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT

  thinBASIC_Parse2Numbers(ScriptSceneID, EntityID)
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  FUNCTION = @dummy_PtrTo_t_EntityInfo.Use

END FUNCTION

' -- Copies entity to another entity slot
SUB Exec_TBGL_EntityCopyTo()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID, EntityID2, PARENT AS EXT
  LOCAL lEntityType, Param, Param2, Param3, Param4, Param5 AS LONG
  LOCAL dummy_PtrTo_t_EntityInfoCopyFrom AS t_EntityInfo PTR

  thinBASIC_ParseXNumbers(3,4,ScriptSceneID, EntityID, EntityID2, PARENT)

  IF entityID2 > MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID) THEN
    ' -- A LONG (4 bytes) can store a string
    ' -- Here we create an array of pointer to dynamic strings. Each string will be the storage of other objects
    LOCAL FillString AS STRING
    FillString = STRING$((entityID2-MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)) * 4&, 0)
    MyArrayOf_TBGLScene(ScriptSceneID)              = MyArrayOf_TBGLScene(ScriptSceneID)         + FillString
    MyArrayOf_TBGLSceneUserData(ScriptSceneID)      = MyArrayOf_TBGLSceneUserData(ScriptSceneID) + FillString
    MyArrayOf_TBGLSceneNames(ScriptSceneID)         = MyArrayOf_TBGLSceneNames(ScriptSceneID)    + FillString
    MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)  = entityID2
    Scene_LastID = -1
    Scene_Name_LastID = -1
    Scene_UserData_LastID = -1

  END IF
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfoCopyFrom = STRPTR(TBGLSceneEntities(EntityID))
  lEntityType = @dummy_PtrTo_t_EntityInfoCopyFrom.EntType

  SELECT CASE AS LONG LEntityType
    CASE %TBGL_ENTITY_MODEL
      TBGLSceneEntities(EntityID2) = macro_t_Entity_Model_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))
        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_MODEL, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)


      '---Now point the object info pointer to point to the initial of the specific object data.
        dummy_PtrTo_t_Entity_Model = macro_t_Entity_Model_MemPos(TBGLSceneEntities(EntityID))
        Param = @dummy_PtrTo_t_Entity_Model.slot
        Param2 = @dummy_PtrTo_t_Entity_Model.ForceTex
        Param3 = @dummy_PtrTo_t_Entity_Model.ForceR
        Param4 = @dummy_PtrTo_t_Entity_Model.ForceG
        Param5 = @dummy_PtrTo_t_Entity_Model.ForceB

        dummy_PtrTo_t_Entity_Model = macro_t_Entity_Model_MemPos(TBGLSceneEntities(EntityID2))
        macro_t_Entity_Model_FillData(dummy_PtrTo_t_Entity_Model, Param )
        @dummy_PtrTo_t_Entity_Model.ForceTex = Param2
        @dummy_PtrTo_t_Entity_Model.ForceR = Param3
        @dummy_PtrTo_t_Entity_Model.ForceG = Param4
        @dummy_PtrTo_t_Entity_Model.ForceB = Param5


   CASE %TBGL_ENTITY_DISPLAYLIST

      TBGLSceneEntities(EntityID2) = macro_t_Entity_DisplayList_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_DISPLAYLIST, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)


      '---Now point the object info pointer to point to the initial of the specific object data.
        dummy_PtrTo_t_Entity_DisplayList = macro_t_Entity_DisplayList_MemPos(TBGLSceneEntities(EntityID))
        Param = @dummy_PtrTo_t_Entity_DisplayList.slot
        Param2 = @dummy_PtrTo_t_Entity_DisplayList.ForceTex
        Param3 = @dummy_PtrTo_t_Entity_DisplayList.ForceR
        Param4 = @dummy_PtrTo_t_Entity_DisplayList.ForceG
        Param5 = @dummy_PtrTo_t_Entity_DisplayList.ForceB

        dummy_PtrTo_t_Entity_DisplayList = macro_t_Entity_DisplayList_MemPos(TBGLSceneEntities(EntityID2))
        macro_t_Entity_DisplayList_FillData(dummy_PtrTo_t_Entity_DisplayList, Param )
        @dummy_PtrTo_t_Entity_DisplayList.ForceTex = Param2
        @dummy_PtrTo_t_Entity_DisplayList.ForceR = Param3
        @dummy_PtrTo_t_Entity_DisplayList.ForceG = Param4
        @dummy_PtrTo_t_Entity_DisplayList.ForceB = Param5


   CASE %TBGL_ENTITY_CAMERA
      TBGLSceneEntities(EntityID2) = macro_t_Entity_Camera_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_CAMERA, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)


      '---Now point the object info pointer to point to the initial of the specific object data.
        dummy_PtrTo_t_Entity_Camera = macro_t_Entity_Camera_MemPos(TBGLSceneEntities(EntityID))
        Param = @dummy_PtrTo_t_Entity_Camera.cType
        dummy_PtrTo_t_Entity_Camera = macro_t_Entity_Camera_MemPos(TBGLSceneEntities(EntityID2))
        macro_t_Entity_Camera_FillData(dummy_PtrTo_t_Entity_Camera, Param )

   CASE %TBGL_ENTITY_LIGHT
      TBGLSceneEntities(EntityID2) = macro_t_Entity_Light_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_Light, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)', _

      '---Now point the object info pointer to point to the initial of the specific object data.
        dummy_PtrTo_t_Entity_Light = macro_t_Entity_Light_MemPos(TBGLSceneEntities(EntityID))
        Param = @dummy_PtrTo_t_Entity_Light.cType
        dummy_PtrTo_t_Entity_Light = macro_t_Entity_Light_MemPos(TBGLSceneEntities(EntityID2))
        macro_t_Entity_Light_FillData(dummy_PtrTo_t_Entity_Light, Param )

    CASE %TBGL_ENTITY_PIVOT
      TBGLSceneEntities(EntityID2) = macro_t_Entity_Pivot_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_PIVOT, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)', _

    CASE %TBGL_ENTITY_BOX
      TBGLSceneEntities(EntityID2) = macro_t_Entity_Box_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_BOX, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)', _

      '---Now point the object info pointer to point to the initial of the specific object data.
        dummy_PtrTo_t_Entity_Box = macro_t_Entity_Box_MemPos(TBGLSceneEntities(EntityID))
        LOCAL old_BoxPTR AS t_Entity_Box PTR
        old_BoxPTR = dummy_PtrTo_t_Entity_Box
        dummy_PtrTo_t_Entity_Box = macro_t_Entity_Box_MemPos(TBGLSceneEntities(EntityID2))
        macro_t_Entity_Box_FillData(dummy_PtrTo_t_Entity_Box, @old_BoxPTR.a, @old_BoxPTR.b, @old_BoxPTR.c, @old_BoxPTR.ForceTex, @old_BoxPTR.cRGB(0), @old_BoxPTR.cRGB(1), @old_BoxPTR.cRGB(2) )

    CASE %TBGL_ENTITY_SPHERE
      TBGLSceneEntities(EntityID2) = macro_t_Entity_Sphere_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_Sphere, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)', _

      '---Now point the object info pointer to point to the initial of the specific object data.
        dummy_PtrTo_t_Entity_Sphere = macro_t_Entity_Sphere_MemPos(TBGLSceneEntities(EntityID))
        LOCAL old_SpherePTR AS t_Entity_Sphere PTR
        old_SpherePTR = dummy_PtrTo_t_Entity_Sphere
        dummy_PtrTo_t_Entity_Sphere = macro_t_Entity_Sphere_MemPos(TBGLSceneEntities(EntityID2))
        macro_t_Entity_Sphere_FillData(dummy_PtrTo_t_Entity_Sphere, @old_SpherePTR.r, @old_SpherePTR.ForceTex, @old_SpherePTR.cRGB(0), @old_SpherePTR.cRGB(1), @old_SpherePTR.cRGB(2) )

    CASE %TBGL_ENTITY_CYLINDER
      TBGLSceneEntities(EntityID2) = macro_t_Entity_Cylinder_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_Cylinder, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)', _

      '---Now point the object info pointer to point to the initial of the specific object data.
        dummy_PtrTo_t_Entity_Cylinder = macro_t_Entity_Cylinder_MemPos(TBGLSceneEntities(EntityID))
        LOCAL old_CylinderPTR AS t_Entity_Cylinder PTR
        old_CylinderPTR = dummy_PtrTo_t_Entity_Cylinder
        dummy_PtrTo_t_Entity_Cylinder = macro_t_Entity_Cylinder_MemPos(TBGLSceneEntities(EntityID2))
        macro_t_Entity_Cylinder_FillData(dummy_PtrTo_t_Entity_Cylinder, @old_CylinderPTR.r, @old_CylinderPTR.r2, @old_CylinderPTR.h, @old_CylinderPTR.ForceTex, @old_CylinderPTR.cRGB(0), @old_CylinderPTR.cRGB(1), @old_CylinderPTR.cRGB(2), _
                                         @old_CylinderPTR.capped )

    CASE %TBGL_ENTITY_TORUS
      TBGLSceneEntities(EntityID2) = macro_t_Entity_Torus_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
        dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

        macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_Torus, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                     @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)', _

      '---Now point the object info pointer to point to the initial of the specific object data.
        dummy_PtrTo_t_Entity_Torus = macro_t_Entity_Torus_MemPos(TBGLSceneEntities(EntityID))
        LOCAL old_TorusPTR AS t_Entity_Torus PTR
        old_TorusPTR = dummy_PtrTo_t_Entity_Torus
        dummy_PtrTo_t_Entity_Torus = macro_t_Entity_Torus_MemPos(TBGLSceneEntities(EntityID2))
        macro_t_Entity_Torus_FillData(dummy_PtrTo_t_Entity_Torus, @old_TorusPTR.r, @old_TorusPTR.r2, @old_TorusPTR.ForceTex, @old_TorusPTR.cRGB(0), @old_TorusPTR.cRGB(1), @old_TorusPTR.cRGB(2) )

    CASE %TBGL_ENTITY_BOUNDINGSPHERE
      TBGLSceneEntities(EntityID2) = macro_t_Entity_BoundingSphere_MemAlloc

      '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

      macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_BoundingSphere, IIF( PARENT = 0, @dummy_PtrTo_t_EntityInfoCopyFrom.EntParent, PARENT),1, _
                                                                                                          @dummy_PtrTo_t_EntityInfoCopyFrom.x, @dummy_PtrTo_t_EntityInfoCopyFrom.y, @dummy_PtrTo_t_EntityInfoCopyFrom.z)', _

      '---Now point the object info pointer to point to the initial of the specific object data.
      dummy_PtrTo_t_Entity_BoundingSphere = macro_t_Entity_BoundingSphere_MemPos(TBGLSceneEntities(EntityID))

      LOCAL old_BoundingSpherePTR AS t_Entity_BoundingSphere PTR
      old_BoundingSpherePTR = dummy_PtrTo_t_Entity_BoundingSphere
      dummy_PtrTo_t_Entity_BoundingSphere = macro_t_Entity_BoundingSphere_MemPos(TBGLSceneEntities(EntityID2))
      macro_t_Entity_BoundingSphere_FillData(dummy_PtrTo_t_Entity_BoundingSphere, @old_BoundingSpherePTR.fPTR, @old_BoundingSpherePTR.Radius)

  END SELECT

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB


' -- Entity Position Related
SUB Exec_TBGL_EntitySetPos()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL x, y, z AS EXT

  thinBasic_ParseXNumbers(4, 5, ScriptSceneID, EntityID, x, y, z)
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  @dummy_PtrTo_t_EntityInfo.x = x
  @dummy_PtrTo_t_EntityInfo.y = y
  @dummy_PtrTo_t_EntityInfo.z = z

END SUB

SUB Exec_TBGL_EntityGetPos()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL GlobalPosition AS EXT

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID

      ' -- Overlay memory
      internal_SetEntityOverlay(ScriptSceneID)

      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

      LOCAL lVariablePtr1     AS LONG
      LOCAL lVariableAbsPos1  AS LONG

      LOCAL lVariablePtr2     AS LONG
      LOCAL lVariableAbsPos2  AS LONG

      LOCAL lVariablePtr3     AS LONG
      LOCAL lVariableAbsPos3  AS LONG

      ' -- Get variables
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
          IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
            thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)
            IF thinBasic_CheckCloseParens_Optional THEN

              '---Now assign values to referenced variables
              thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @dummy_PtrTo_t_EntityInfo.x)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @dummy_PtrTo_t_EntityInfo.y)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @dummy_PtrTo_t_EntityInfo.z)

            ELSE
              IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                thinBASIC_ParseNumber GlobalPosition

                IF GlobalPosition <> 0 THEN
                  LOCAL p AS TBGL_tVector3D

                  internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)        ' -- BUG ?

                  '---Now assign values to referenced variables
                  thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, p.x)
                  thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, p.y)
                  thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, p.z)

                ELSE

                  '---Now assign values to referenced variables
                  thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @dummy_PtrTo_t_EntityInfo.x)
                  thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @dummy_PtrTo_t_EntityInfo.y)
                  thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @dummy_PtrTo_t_EntityInfo.z)
                END IF
                thinBasic_CheckCloseParens_Mandatory
              END IF
            END IF
          END IF
        END IF
      END IF
    END IF
  END IF

END SUB

SUB Exec_TBGL_EntityMove()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL dx, dy, dz AS EXT

  thinBasic_ParseXNumbers(4, 5, ScriptSceneID, EntityID, dx, dy, dz)
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
  @dummy_PtrTo_t_EntityInfo.x += dx
  @dummy_PtrTo_t_EntityInfo.y += dy
  @dummy_PtrTo_t_EntityInfo.z += dz

END SUB

SUB Exec_TBGL_EntityPush()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL dx, dy, dz AS EXT

  thinBasic_ParseXNumbers(4, 5, ScriptSceneID, EntityID, dx, dy, dz)
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  ' -- And add values
  @dummy_PtrTo_t_EntityInfo.x += @dummy_PtrTo_t_EntityInfo.LocalAxisX.x*dx + @dummy_PtrTo_t_EntityInfo.LocalAxisY.x*dy + @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x*dz
  @dummy_PtrTo_t_EntityInfo.y += @dummy_PtrTo_t_EntityInfo.LocalAxisX.y*dx + @dummy_PtrTo_t_EntityInfo.LocalAxisY.y*dy + @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y*dz
  @dummy_PtrTo_t_EntityInfo.z += @dummy_PtrTo_t_EntityInfo.LocalAxisX.z*dx + @dummy_PtrTo_t_EntityInfo.LocalAxisY.z*dy + @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z*dz

END SUB

SUB Exec_TBGL_EntityTrackPos()

  LOCAL ScriptSceneID, EntityID, x, y, z AS EXT
  LOCAL o, p, xAxis, yAxis, zAxis, tracked AS TBGL_tVector3D

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID
      IF thinBasic_CheckComma_Mandatory THEN
        thinBasic_ParseNumber x
        IF thinBasic_CheckComma_Mandatory THEN
          thinBasic_ParseNumber y
          IF thinBasic_CheckComma_Mandatory THEN
            thinBasic_ParseNumber z

              internal_SetEntityOverlay(ScriptSceneID)
              dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

              internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, zAxis, yAxis)
              VectorCross(xAxis,yAxis,zAxis)

              tracked.x = p.x + xAxis.x * x + yAxis.x * y + zAxis.x * z
              tracked.y = p.y + xAxis.y * x + yAxis.y * y + zAxis.y * z
              tracked.z = p.z + xAxis.z * x + yAxis.z * y + zAxis.z * z

              internal_Parse3Byref (tracked.x, tracked.y, tracked.z)
'              msgbox STR$(xAxis.x)+STR$(xAxis.y)+STR$(xAxis.z)+$CRLF+ _
'                     STR$(yAxis.x)+STR$(yAxis.y)+STR$(yAxis.z)+$CRLF+ _
'                     STR$(zAxis.x)+STR$(zAxis.y)+STR$(zAxis.z)+$CRLF+ _
'                     STR$(p.x)+STR$(p.y)+STR$(p.z)+$CRLF+ _
'                     STR$(x)+STR$(y)+STR$(z)+$CRLF+ _
'                     STR$(tracked.x)+STR$(tracked.y)+STR$(tracked.z)+$CRLF

'            o.x = x
'            o.y = y
'            o.z = z
'            internal_SetEntityOverlay(ScriptSceneID)
'
'            dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
'
'            internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o)
'
'            internal_Parse3Byref (p.x,p.y,p.z)


          END IF
        END IF
      END IF
    END IF
  END IF

END SUB

FUNCTION EXEC_TBGL_EntityFindNearestByPos() AS EXT

  REGISTER EntityID AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityType, x, y, z, rangeX, rangeY, rangeZ, GlobalCoordinates AS EXT
  LOCAL parsed AS LONG

  parsed = thinBasic_ParseXNumbers(8, 9, ScriptSceneID, EntityType, x, y, z, rangeX, rangeY, rangeZ, GlobalCoordinates )

  internal_SetEntityOverlayUserData(ScriptSceneID)

  LOCAL MaxEntities, result AS LONG
  LOCAL maxx, minx, maxy, miny, maxz, minz, dist, mindist AS DOUBLE
  maxx = x + rangeX
  minx = x - rangeX
  maxy = y + rangeY
  miny = y - rangeY
  maxz = z + rangeZ
  minz = z - rangeZ

  mindist = 1.79*10^300
  result  = 0
  maxEntities = MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)

  IF GlobalCoordinates THEN
    LOCAL p AS TBGL_tVector3D

    FOR EntityID = 1 TO maxEntities
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
      IF @dummy_PtrTo_t_EntityInfo.EntType = EntityType AND @dummy_PtrTo_t_EntityInfo.Use <> 0& THEN
        internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)

        IF p.x >= minx AND p.x <= maxx AND _
           p.y >= miny AND p.y <= maxy AND _
           p.z >= minz AND p.z <= maxz THEN

            dist =  SQR((x - p.x)*(x - p.x)+ _
                        (y - p.y)*(y - p.y)+ _
                        (z - p.z)*(z - p.z))

           IF dist < mindist THEN
             mindist = dist
             result  = EntityID
           END IF
        END IF

      END IF
    NEXT

  ELSE

    FOR EntityID = 1 TO maxEntities

      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
      IF @dummy_PtrTo_t_EntityInfo.EntType = EntityType AND @dummy_PtrTo_t_EntityInfo.Use <> 0& THEN

        IF @dummy_PtrTo_t_EntityInfo.x >= minx AND @dummy_PtrTo_t_EntityInfo.x <= maxx AND _
           @dummy_PtrTo_t_EntityInfo.y >= miny AND @dummy_PtrTo_t_EntityInfo.y <= maxy AND _
           @dummy_PtrTo_t_EntityInfo.z >= minz AND @dummy_PtrTo_t_EntityInfo.z <= maxz THEN

           dist = SQR((x - @dummy_PtrTo_t_EntityInfo.x)*(x - @dummy_PtrTo_t_EntityInfo.x)+ _
                      (y - @dummy_PtrTo_t_EntityInfo.y)*(y - @dummy_PtrTo_t_EntityInfo.y)+ _
                      (z - @dummy_PtrTo_t_EntityInfo.z)*(z - @dummy_PtrTo_t_EntityInfo.z))

           IF dist < mindist THEN
             mindist = dist
             result  = EntityID
           END IF
        END IF
      END IF
    NEXT

  END IF

  FUNCTION = result

END FUNCTION

FUNCTION EXEC_TBGL_EntityFindByPos() AS EXT

  REGISTER EntityID AS LONG

  LOCAL ScriptSceneID AS EXT

  LOCAL EntityType, x, y, z, GlobalCoordinates AS EXT
  LOCAL parsed AS LONG

  parsed = thinBasic_ParseXNumbers(5, 6, ScriptSceneID, EntityType, x, y, z, GlobalCoordinates )

  internal_SetEntityOverlayUserData(ScriptSceneID)

  LOCAL MaxEntities AS LONG

  maxEntities = MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)

  IF GlobalCoordinates THEN
    LOCAL p AS TBGL_tVector3D

    FOR EntityID = 1 TO maxEntities
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
      IF @dummy_PtrTo_t_EntityInfo.EntType = EntityType THEN
        internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)
        IF p.x = x AND p.y = y AND p.z = z THEN
          FUNCTION = EntityID
          EXIT FUNCTION
        END IF
      END IF
    NEXT

  ELSE

    FOR EntityID = 1 TO maxEntities

      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
      IF @dummy_PtrTo_t_EntityInfo.EntType = EntityType THEN

        IF @dummy_PtrTo_t_EntityInfo.x = x AND @dummy_PtrTo_t_EntityInfo.y = y AND @dummy_PtrTo_t_EntityInfo.z = z THEN
          FUNCTION = EntityID
          EXIT FUNCTION
        END IF

      END IF
    NEXT

  END IF

  FUNCTION = 0

END FUNCTION

' --

FUNCTION EXEC_TBGL_EntityFindNearest() AS EXT

  REGISTER EntityID AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityType, EntityFrom, rangeX, rangeY, rangeZ, GlobalCoordinates AS EXT
  LOCAL parsed AS LONG, x, y, z AS DOUBLE
  LOCAL MaxEntities, result AS LONG
  LOCAL maxx, minx, maxy, miny, maxz, minz, dist, mindist AS DOUBLE
  LOCAL p AS TBGL_tVector3D

  parsed = thinBasic_ParseXNumbers(6, 7, ScriptSceneID, EntityType, EntityFrom, rangeX, rangeY, rangeZ, GlobalCoordinates )

  internal_SetEntityOverlayUserData(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityFrom))

  IF GlobalCoordinates THEN
    internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)
    x = p.x
    y = p.y
    z = p.z
  ELSE
    x = @dummy_PtrTo_t_EntityInfo.x
    y = @dummy_PtrTo_t_EntityInfo.y
    z = @dummy_PtrTo_t_EntityInfo.z
  END IF

  maxx = x + rangeX
  minx = x - rangeX
  maxy = y + rangeY
  miny = y - rangeY
  maxz = z + rangeZ
  minz = z - rangeZ

  mindist = 1.79*10^300
  result  = 0
  maxEntities = MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)


  IF GlobalCoordinates THEN
    ' -- Pokud v Globalnich


    FOR EntityID = 1 TO maxEntities
      IF EntityFrom = EntityID THEN ITERATE FOR

      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
      IF @dummy_PtrTo_t_EntityInfo.EntType = EntityType AND @dummy_PtrTo_t_EntityInfo.Use <> 0& THEN
        internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)
        IF p.x >= minx AND p.x <= maxx AND _
           p.y >= miny AND p.y <= maxy AND _
           p.z >= minz AND p.z <= maxz THEN

           dist =  SQR((x - p.x)*(x - p.x)+ _
                      (y - p.y)*(y - p.y)+ _
                      (z - p.z)*(z - p.z))

           IF dist < mindist THEN
             mindist = dist
             result  = EntityID
           END IF
        END IF
      END IF
    NEXT

  ELSE
    ' -- Pokud v lokalnich
    FOR EntityID = 1 TO maxEntities
      IF EntityFrom = EntityID THEN ITERATE FOR
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
      IF @dummy_PtrTo_t_EntityInfo.EntType = EntityType AND @dummy_PtrTo_t_EntityInfo.Use <> 0& THEN

        IF @dummy_PtrTo_t_EntityInfo.x >= minx AND @dummy_PtrTo_t_EntityInfo.x <= maxx AND _
           @dummy_PtrTo_t_EntityInfo.y >= miny AND @dummy_PtrTo_t_EntityInfo.y <= maxy AND _
           @dummy_PtrTo_t_EntityInfo.z >= minz AND @dummy_PtrTo_t_EntityInfo.z <= maxz THEN

           dist = SQR((x - @dummy_PtrTo_t_EntityInfo.x)*(x - @dummy_PtrTo_t_EntityInfo.x)+ _
                      (y - @dummy_PtrTo_t_EntityInfo.y)*(y - @dummy_PtrTo_t_EntityInfo.y)+ _
                      (z - @dummy_PtrTo_t_EntityInfo.z)*(z - @dummy_PtrTo_t_EntityInfo.z))
           IF dist < mindist THEN
             mindist = dist
             result  = EntityID
           END IF
        END IF
      END IF
    NEXT

  END IF

  FUNCTION = result

END FUNCTION

' -- Distance between entities
FUNCTION EXEC_TBGL_EntityGetDistance() AS EXT

  REGISTER EntityID AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityFrom, EntityTo AS EXT
  LOCAL x1, y1, z1 AS DOUBLE
  LOCAL x2, y2, z2 AS DOUBLE
  LOCAL xd, yd, zd AS DOUBLE
  LOCAL p AS TBGL_tVector3D

  thinBasic_Parse3Numbers(ScriptSceneID, EntityFrom, EntityTo)

  internal_SetEntityOverlayUserData(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityFrom))
  internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)
  x1 = p.x
  y1 = p.y
  z1 = p.z

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityTo))
  internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)
  x2 = p.x
  y2 = p.y
  z2 = p.z

  xd = (x1-x2)
  yd = (y1-y2)
  zd = (z1-z2)

  FUNCTION = SQR(xd*xd + yd*yd + zd*zd)

END FUNCTION

' -- Distance to point
FUNCTION EXEC_TBGL_EntityGetDistancePos() AS EXT

  REGISTER EntityID AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityFrom AS EXT
  LOCAL x1, y1, z1 AS DOUBLE
  LOCAL x2, y2, z2 AS EXT
  LOCAL xd, yd, zd AS DOUBLE
  LOCAL p AS TBGL_tVector3D

  thinBasic_Parse5Numbers(ScriptSceneID, EntityFrom, x2, y2, z2)

  internal_SetEntityOverlayUserData(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityFrom))
  internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p)
  x1 = p.x
  y1 = p.y
  z1 = p.z

  xd = (x1-x2)
  yd = (y1-y2)
  zd = (z1-z2)

  FUNCTION = SQR(xd*xd + yd*yd + zd*zd)

END FUNCTION


' -- Scaling
SUB Exec_TBGL_EntitySetScale()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL x, y, z AS EXT
  LOCAL nParsed AS EXT

  nParsed = thinBasic_ParseXNumbers( 3, 5, ScriptSceneID, EntityID, x, y, z)
  ''thinBASIC_Parse5Numbers(ScriptSceneID, EntityID, x, y, z)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
  SELECT CASE AS LONG nParsed
    CASE 3
      @dummy_PtrTo_t_EntityInfo.sx = x
      @dummy_PtrTo_t_EntityInfo.sy = x
      @dummy_PtrTo_t_EntityInfo.sz = x

    CASE 4
      @dummy_PtrTo_t_EntityInfo.sx = x
      @dummy_PtrTo_t_EntityInfo.sy = y
      @dummy_PtrTo_t_EntityInfo.sz = 1

    CASE ELSE
      @dummy_PtrTo_t_EntityInfo.sx = x
      @dummy_PtrTo_t_EntityInfo.sy = y
      @dummy_PtrTo_t_EntityInfo.sz = z

  END SELECT

END SUB

SUB Exec_TBGL_EntityGetScale()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID

      ' -- Overlay memory
      internal_SetEntityOverlay(ScriptSceneID)

      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

      LOCAL lVariablePtr1     AS LONG
      LOCAL lVariableAbsPos1  AS LONG

      LOCAL lVariablePtr2     AS LONG
      LOCAL lVariableAbsPos2  AS LONG

      LOCAL lVariablePtr3     AS LONG
      LOCAL lVariableAbsPos3  AS LONG

      ' -- Get variables
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
          IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
            thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)
            IF thinBasic_CheckCloseParens_Mandatory THEN

              '---Now assign values to referenced variables
              thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @dummy_PtrTo_t_EntityInfo.sx)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @dummy_PtrTo_t_EntityInfo.sy)
              thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @dummy_PtrTo_t_EntityInfo.sz)

            END IF
          END IF
        END IF
      END IF
    END IF
  END IF

END SUB

' -- Entitity Angle orientation
SUB Exec_TBGL_EntitySetRot()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL x, y, z AS EXT
  LOCAL n AS LONG
  n = thinBASIC_ParseXNumbers(3, 5, ScriptSceneID, EntityID, x, y, z)
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisZ, DefaultAxisZ_X, DefaultAxisZ_Y, DefaultAxisZ_Z )
  F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisY, DefaultAxisY_X, DefaultAxisY_Y, DefaultAxisY_Z )
  F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisX, DefaultAxisX_X, DefaultAxisX_Y, DefaultAxisX_Z )

  IF n = 3 THEN
    Vectors_RotateZ(x, @dummy_PtrTo_t_EntityInfo)
  ELSE
    Vectors_RotateZ(z, @dummy_PtrTo_t_EntityInfo)
    Vectors_RotateY(-y, @dummy_PtrTo_t_EntityInfo)
    Vectors_RotateX(-x, @dummy_PtrTo_t_EntityInfo)
  END IF

END SUB


SUB Exec_TBGL_EntityTurn()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL dx, dy, dz AS EXT
  LOCAL n AS LONG

  n = thinBASIC_ParseXNumbers(3, 5, ScriptSceneID, EntityID, dx, dy, dz)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  IF n = 3 THEN
    IF dx THEN Vectors_RotateZ(dx, @dummy_PtrTo_t_EntityInfo) ' 2D case
  ELSE
    IF dx THEN Vectors_RotateX(-dx, @dummy_PtrTo_t_EntityInfo)
    IF dy THEN Vectors_RotateY(-dy, @dummy_PtrTo_t_EntityInfo)
    IF dz THEN Vectors_RotateZ(dz, @dummy_PtrTo_t_EntityInfo)
  END IF
END SUB

SUB Exec_TBGL_EntitySetTarget()

  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID, EntityID2, LastParam AS EXT
  LOCAL target_dummy_PtrTo_t_EntityInfo AS t_EntityInfo PTR

  LOCAL e AS TBGL_tVector3D
  LOCAL t AS TBGL_tVector3D

  LOCAL vx,vy,vz, vl AS DOUBLE
    LOCAL ranglex, rangley, ranglez AS DOUBLE

  thinBASIC_ParseXNumbers(3, 4,ScriptSceneID, EntityID, EntityID2, LastParam)

  IF LastParam <> 0 THEN
    SWAP EntityID2, LastParam
  ELSE
    LastParam = EntityID
  END IF

  internal_SetEntityOverlay(ScriptSceneID)
  target_dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

  FOR i = EntityID TO LastParam
    dummy_PtrTo_t_EntityInfo        = STRPTR(TBGLSceneEntities(i))

    F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisZ, DefaultAxisZ_X, DefaultAxisZ_Y, DefaultAxisZ_Z )
    F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisY, DefaultAxisY_X, DefaultAxisY_Y, DefaultAxisY_Z )
    F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisX, DefaultAxisX_X, DefaultAxisX_Y, DefaultAxisX_Z )

    internal_TRACKPOS( dummy_PtrTo_t_EntityInfo , e)
    internal_TRACKPOS( target_dummy_PtrTo_t_EntityInfo, t)

    IF @dummy_PtrTo_t_EntityInfo.EntParent <> 0 THEN

      @dummy_PtrTo_t_EntityInfo.bux = @dummy_PtrTo_t_EntityInfo.x
      @dummy_PtrTo_t_EntityInfo.buy = @dummy_PtrTo_t_EntityInfo.y
      @dummy_PtrTo_t_EntityInfo.buz = @dummy_PtrTo_t_EntityInfo.z

      @dummy_PtrTo_t_EntityInfo.x = e.x
      @dummy_PtrTo_t_EntityInfo.y = e.y
      @dummy_PtrTo_t_EntityInfo.z = e.z

      @dummy_PtrTo_t_EntityInfo.EntParent = -ABS(@dummy_PtrTo_t_EntityInfo.EntParent)

    END IF

    vx = t.x - e.x
    vy = t.y - e.y
    vz = t.z - e.z

    vl = SQR(vx*vx + vy*vy + vz*vz)
    vx = vx / vl
    vy = vy / vl
    vz = vz / vl

    IF vz = 0.0# THEN vz = 0.0000000001#

    ranglex = -RadToDeg(ATN(vy/SQR(vz*vz+vx*vx)))+180
    rangley = -RadToDeg(ATN(vx/vz))+IIF(vz<0,180,0)+180
    ranglez = 180

    Vectors_RotateY(rangley, @dummy_PtrTo_t_EntityInfo )
    Vectors_RotateX(ranglex, @dummy_PtrTo_t_EntityInfo )
    Vectors_RotateZ(ranglez, @dummy_PtrTo_t_EntityInfo )

  NEXT

END SUB

SUB Exec_TBGL_EntitySetTargetPos()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL x,y,z AS EXT

  LOCAL vx,vy,vz, vl AS DOUBLE
  thinBASIC_Parse5Numbers(ScriptSceneID, EntityID, x, y, z)

  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo        = STRPTR(TBGLSceneEntities(EntityID))

  LOCAL e AS TBGL_tVector3D

  LOCAL t AS TBGL_tVector3D

  F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisZ, DefaultAxisZ_X, DefaultAxisZ_Y, DefaultAxisZ_Z )
  F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisY, DefaultAxisY_X, DefaultAxisY_Y, DefaultAxisY_Z )
  F3DVector( @dummy_PtrTo_t_EntityInfo.LocalAxisX, DefaultAxisX_X, DefaultAxisX_Y, DefaultAxisX_Z )

  internal_TRACKPOS( dummy_PtrTo_t_EntityInfo , e)

  t.x = x
  t.y = y
  t.z = z

  LOCAL ranglex, rangley, ranglez AS DOUBLE

  IF @dummy_PtrTo_t_EntityInfo.EntParent <> 0 THEN

    @dummy_PtrTo_t_EntityInfo.bux = @dummy_PtrTo_t_EntityInfo.x
    @dummy_PtrTo_t_EntityInfo.buy = @dummy_PtrTo_t_EntityInfo.y
    @dummy_PtrTo_t_EntityInfo.buz = @dummy_PtrTo_t_EntityInfo.z

    @dummy_PtrTo_t_EntityInfo.x = e.x
    @dummy_PtrTo_t_EntityInfo.y = e.y
    @dummy_PtrTo_t_EntityInfo.z = e.z

    @dummy_PtrTo_t_EntityInfo.EntParent = -ABS(@dummy_PtrTo_t_EntityInfo.EntParent)

  END IF

    vx = t.x - e.x
    vy = t.y - e.y
    vz = t.z - e.z

    vl = SQR(vx*vx + vy*vy + vz*vz)
    vx /= vl
    vy /= vl
    vz /= vl

    IF vz = 0.0# THEN vz = 0.0000000001#

    ranglex = -RadToDeg(ATN(vy/SQR(vz*vz+vx*vx)))+180
    rangley = -RadToDeg(ATN(vx/vz))+IIF(vz<0,180,0)+180
    ranglez = 180

    Vectors_RotateY(rangley, @dummy_PtrTo_t_EntityInfo )
    Vectors_RotateX(ranglex, @dummy_PtrTo_t_EntityInfo )
    Vectors_RotateZ(ranglez, @dummy_PtrTo_t_EntityInfo )

END SUB

SUB Exec_TBGL_EntityGetZAxis()

  LOCAL ScriptSceneID, EntityID AS EXT
  LOCAL o, p, v AS TBGL_tVector3D

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID

      internal_SetEntityOverlay(ScriptSceneID)
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

      internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, v)

      internal_Parse3Byref (v.x, v.y, v.z)


    END IF
  END IF

END SUB

SUB Exec_TBGL_EntityGetYAxis()

  LOCAL ScriptSceneID, EntityID AS EXT
  LOCAL o, p, v, u AS TBGL_tVector3D

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID

      internal_SetEntityOverlay(ScriptSceneID)
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

      internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, v, u)

      internal_Parse3Byref (u.x, u.y, u.z)

    END IF
  END IF

END SUB

SUB Exec_TBGL_EntityGetXAxis()

  LOCAL ScriptSceneID, EntityID AS EXT
  LOCAL o, p, v, u, r AS TBGL_tVector3D

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID

      internal_SetEntityOverlay(ScriptSceneID)
      dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

      internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, v, u)

      VectorCross(r,u,v)
      internal_Parse3Byref (r.x, r.y, r.z)

    END IF
  END IF

END SUB
' --

SUB Exec_TBGL_EntityGetMatrix()
  REGISTER i AS LONG
  LOCAL ScriptSceneID, EntityID, matrixType AS EXT
  LOCAL lArrayCellPtr AS LONG
  LOCAL lArrayCellAbsPos AS LONG
  LOCAL VariablePtrOdd, MainType, SubType, ElementsAreFixed, TotElements, ElementSize AS LONG

  LOCAL o, p, v, u, r AS TBGL_tVector3D

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID
      IF thinBasic_CheckComma_Mandatory THEN
        ' -- Parse the array
        thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

        IF thinBasic_CheckComma_Optional THEN
          thinBasic_ParseNumber matrixType
        END IF

        IF thinBasic_CheckCloseParens_Mandatory THEN

          ' -- Calculate vectors
          internal_SetEntityOverlay(ScriptSceneID)
          dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

          internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, v, u)
          ' r = x
          ' u = y
          ' v = z
          VectorCross(r,u,v)

          IF @dummy_PtrTo_t_EntityInfo.sx <> 1 THEN
            r.x *= @dummy_PtrTo_t_EntityInfo.sx
            r.y *= @dummy_PtrTo_t_EntityInfo.sx
            r.z *= @dummy_PtrTo_t_EntityInfo.sx
          END IF

          IF @dummy_PtrTo_t_EntityInfo.sy <> 1 THEN
            u.x *= @dummy_PtrTo_t_EntityInfo.sy
            u.y *= @dummy_PtrTo_t_EntityInfo.sy
            u.z *= @dummy_PtrTo_t_EntityInfo.sy
          END IF

          IF @dummy_PtrTo_t_EntityInfo.sz <> 1 THEN
            v.x *= @dummy_PtrTo_t_EntityInfo.sz
            v.y *= @dummy_PtrTo_t_EntityInfo.sz
            v.z *= @dummy_PtrTo_t_EntityInfo.sz
          END IF
          SELECT CASE AS LONG TotElements
            CASE 16
              IF matrixType = %TBGL_ColumnRow THEN
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  1, r.x )   ' (1, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  2, r.y )   ' (2, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  3, r.z )   ' (3, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  4, 0.0 )   ' (4, 1)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  5, u.x )   ' (1, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  6, u.y )   ' (2, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  7, u.z )   ' (3, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  8, 0.0 )   ' (4, 2)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  9, v.x )   ' (1, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 10, v.y )   ' (2, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 11, v.z )   ' (3, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 12, 0.0 )   ' (4, 3)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 13, p.x )   ' (1, 4)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 14, p.y )   ' (2, 4)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 15, p.z )   ' (3, 4)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 16, 1.0 )   ' (4, 4)
              ELSE
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  1, r.x )   ' (1, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  5, r.y )   ' (1, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  9, r.z )   ' (1, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 13, 0.0 )   ' (1, 4)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  2, u.x )   ' (2, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  6, u.y )   ' (2, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 10, u.z )   ' (2, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 14, 0.0 )   ' (2, 4)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  3, v.x )   ' (3, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  7, v.y )   ' (3, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 11, v.z )   ' (3, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 15, 0.0 )   ' (3, 4)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  4, p.x )   ' (4, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  8, p.y )   ' (4, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 12, p.z )   ' (4, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr, 16, 1.0 )   ' (4, 4)
              END IF

            CASE 9
              IF matrixType = %TBGL_ColumnRow THEN
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  1, r.x )   ' (1, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  2, r.y )   ' (2, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  3, r.z )   ' (3, 1)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  4, u.x )   ' (1, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  5, u.y )   ' (2, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  6, u.z )   ' (3, 2)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  7, v.x )   ' (1, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  8, v.y )   ' (2, 3)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  9, v.z )   ' (3, 3)
              ELSE
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  1, r.x )   ' (1, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  4, r.y )   ' (1, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  7, r.z )   ' (1, 3)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  2, u.x )   ' (2, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  5, u.y )   ' (2, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  8, u.z )   ' (2, 3)

                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  3, v.x )   ' (3, 1)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  6, v.y )   ' (3, 2)
                thinBasic_ChangeVariableNumberDirect( lArrayCellPtr,  9, v.z )   ' (3, 3)
              END IF

            CASE ELSE
              tbglerror("Invalid matrix size for TBGL_EntityGetMatrix, matrix must have 9 or 16 elements")
          END SELECT
        END IF
      END IF
    END IF
  END IF

END SUB

SUB Exec_TBGL_EntitySetMatrix()
  REGISTER i AS LONG
  LOCAL ScriptSceneID, EntityID, matrixType AS EXT
  LOCAL lArrayCellPtr AS LONG
  LOCAL lArrayCellAbsPos AS LONG
  LOCAL VariablePtrOdd, MainType, SubType, ElementsAreFixed, TotElements, ElementSize AS LONG

  IF thinBasic_CheckOpenParens_Mandatory THEN
    thinBasic_ParseNumber ScriptSceneID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber EntityID
      IF thinBasic_CheckComma_Mandatory THEN
        ' -- Parse the array
        thinBasic_ParseVariableInfo(lArrayCellPtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, VariablePtrOdd, lArrayCellAbsPos)

        IF thinBasic_CheckComma_Optional THEN
          thinBasic_ParseNumber matrixType
        END IF

        IF thinBasic_CheckCloseParens_Mandatory THEN
          ' -- Calculate vectors
          internal_SetEntityOverlay(ScriptSceneID)
          dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
          SELECT CASE AS LONG TotElements
            CASE 16
              IF matrixType = %TBGL_ColumnRow THEN
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  1)
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  2)
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  3)

                @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  5)
                @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  6)
                @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  7)

                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  9)
                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr, 10)
                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr, 11)

                @dummy_PtrTo_t_EntityInfo.x            = thinBasic_GetVariableNumberDirect(lArrayCellPtr, 13)
                @dummy_PtrTo_t_EntityInfo.y            = thinBasic_GetVariableNumberDirect(lArrayCellPtr, 14)
                @dummy_PtrTo_t_EntityInfo.z            = thinBasic_GetVariableNumberDirect(lArrayCellPtr, 15)
              ELSE
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  1)
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  5)
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  9)

                @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  2)
                @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  6)
                @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr, 10)

                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  3)
                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  7)
                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr, 11)

                @dummy_PtrTo_t_EntityInfo.x            = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  4)
                @dummy_PtrTo_t_EntityInfo.y            = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  8)
                @dummy_PtrTo_t_EntityInfo.z            = thinBasic_GetVariableNumberDirect(lArrayCellPtr, 12)
              END IF

            CASE 9
              IF matrixType = %TBGL_ColumnRow THEN
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  1)
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  2)
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  3)

                @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  4)
                @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  5)
                @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  6)

                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  7)
                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  8)
                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  9)
              ELSE
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  1)
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  4)
                @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  7)

                @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  2)
                @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  5)
                @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  8)

                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  3)
                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  6)
                @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = thinBasic_GetVariableNumberDirect(lArrayCellPtr,  9)
              END IF

            CASE ELSE
              tbglerror("Invalid matrix size for TBGL_EntitySetMatrix, matrix must have 9 or 16 elements")
          END SELECT
        END IF
      END IF
    END IF
  END IF

END SUB

' --
SUB Exec_TBGL_EntitySetXYAxis()

  LOCAL v, u, r AS TBGL_tVector3D

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL xX, xY, xZ, yX, yY, yZ AS EXT

  thinBASIC_ParseXNumbers(8,8,ScriptSceneID, EntityID, xX, xY, xZ, yX, yY, yZ )
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  r.x = xX
  r.y = xY
  r.z = xZ

  u.x = yX
  u.y = yY
  u.z = yZ

  VectorCross(v,r,u)

  @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = xX
  @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = xY
  @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = xZ

  @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = yX
  @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = yY
  @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = yZ

  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = v.X
  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = v.Y
  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = v.Z

END SUB

SUB Exec_TBGL_EntitySetXZAxis()

  LOCAL v, u, r AS TBGL_tVector3D

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL xX, xY, xZ, zX, zY, zZ AS EXT

  thinBASIC_ParseXNumbers(8,8,ScriptSceneID, EntityID, xX, xY, xZ, zX, zY, zZ )
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  r.x = xX
  r.y = xY
  r.z = xZ

  v.x = zX
  v.y = zY
  v.z = zZ

  VectorCross(u,v,r)

  @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = xX
  @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = xY
  @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = xZ

  @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = u.X
  @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = u.Y
  @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = u.Z

  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = zX
  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = zY
  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = zZ

END SUB

SUB Exec_TBGL_EntitySetYZAxis()

  LOCAL v, u, r AS TBGL_tVector3D

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL yX, yY, yZ, zX, zY, zZ AS EXT

  thinBASIC_ParseXNumbers(8,8,ScriptSceneID, EntityID, yX, yY, yZ, zX, zY, zZ )
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  u.x = yX
  u.y = yY
  u.z = yZ

  v.x = zX
  v.y = zY
  v.z = zZ

  VectorCross(r,u,v)

  @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = r.x
  @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = r.y
  @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = r.z

  @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = yX
  @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = yY
  @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = yZ

  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = zX
  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = zY
  @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = zZ

END SUB

SUB Exec_TBGL_EntitySyncAxes()

  LOCAL o, p, v, u, r AS TBGL_tVector3D

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID, EntityID2 AS EXT
  LOCAL GlobalCoordinates AS EXT
  LOCAL local_dummy_PtrTo_t_EntityInfo AS t_EntityInfo PTR

  thinBASIC_ParseXNumbers(3,4,ScriptSceneID, EntityID, EntityID2, GlobalCoordinates )
  internal_SetEntityOverlay(ScriptSceneID)
  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
  local_dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID2))

  IF GlobalCoordinates = %TRUE THEN
    internal_TRACKPOS(local_dummy_PtrTo_t_EntityInfo, p, o, v, u)
    VectorCross(r,u,v) ' -- Ve get X axis from Y, Z one

    @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = r.x
    @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = r.y
    @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = r.z

    @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = u.x
    @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = u.y
    @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = u.z

    @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = v.x
    @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = v.y
    @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = v.z
  ELSE
    @dummy_PtrTo_t_EntityInfo.LocalAxisX.x = @local_dummy_PtrTo_t_EntityInfo.LocalAxisX.x
    @dummy_PtrTo_t_EntityInfo.LocalAxisX.y = @local_dummy_PtrTo_t_EntityInfo.LocalAxisX.y
    @dummy_PtrTo_t_EntityInfo.LocalAxisX.z = @local_dummy_PtrTo_t_EntityInfo.LocalAxisX.z

    @dummy_PtrTo_t_EntityInfo.LocalAxisY.x = @local_dummy_PtrTo_t_EntityInfo.LocalAxisY.x
    @dummy_PtrTo_t_EntityInfo.LocalAxisY.y = @local_dummy_PtrTo_t_EntityInfo.LocalAxisY.y
    @dummy_PtrTo_t_EntityInfo.LocalAxisY.z = @local_dummy_PtrTo_t_EntityInfo.LocalAxisY.z

    @dummy_PtrTo_t_EntityInfo.LocalAxisZ.x = @local_dummy_PtrTo_t_EntityInfo.LocalAxisZ.x
    @dummy_PtrTo_t_EntityInfo.LocalAxisZ.y = @local_dummy_PtrTo_t_EntityInfo.LocalAxisZ.y
    @dummy_PtrTo_t_EntityInfo.LocalAxisZ.z = @local_dummy_PtrTo_t_EntityInfo.LocalAxisZ.z
  END IF

END SUB

FUNCTION Exec_TBGL_EntityGetAngleXZ() AS EXT

  LOCAL x1, y1, z1 AS DOUBLE
  LOCAL x1x, y1y, z1z AS DOUBLE
  LOCAL xT, yT, zT AS DOUBLE

  LOCAL Angle AS DOUBLE

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL EntityTargetID AS EXT
  LOCAL EntityAxis AS EXT

  LOCAL o, p, v AS TBGL_tVector3D

  LOCAL AngleBonus  AS LONG

  thinBasic_Parse4Numbers(ScriptSceneID, EntityID, EntityTargetID, EntityAxis)
  AngleBonus = IIF(EntityAxis = %TBGL_Z, 0, -90)


  ' -- Overlay memory
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  x1 = @dummy_PtrTo_t_EntityInfo.x
  y1 = @dummy_PtrTo_t_EntityInfo.y
  z1 = @dummy_PtrTo_t_EntityInfo.z

  internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, v)

  x1x = v.x
  y1y = v.y
  z1z = v.z

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityTargetID))

  xT = @dummy_PtrTo_t_EntityInfo.x
  yT = @dummy_PtrTo_t_EntityInfo.y
  zT = @dummy_PtrTo_t_EntityInfo.z

  xT -= x1
  zT -= z1
  IF xT = 0 AND zT = 0 THEN
    FUNCTION = %TBGL_UNDEFINED
    EXIT FUNCTION

  ELSE
    Angle = internal_ATN2( z1z, x1x ) - internal_ATN2( zT, xT )

    IF Angle < -180-AngleBonus THEN
      Angle = Angle + 360
    ELSEIF Angle > 180-AngleBonus THEN
      Angle = Angle - 360
    END IF

    FUNCTION = Angle+AngleBonus
    EXIT FUNCTION
  END IF

END FUNCTION

FUNCTION Exec_TBGL_EntityGetAngleXY() AS EXT

  LOCAL x1, y1, z1 AS DOUBLE
  LOCAL x1x, y1y, z1z AS DOUBLE
  LOCAL xT, yT, zT AS DOUBLE

  LOCAL Angle AS DOUBLE

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL EntityTargetID AS EXT
  LOCAL EntityAxis AS EXT

  LOCAL o, p, v, u, r AS TBGL_tVector3D

  LOCAL AngleBonus  AS LONG

  thinBasic_Parse4Numbers(ScriptSceneID, EntityID, EntityTargetID, EntityAxis)

  AngleBonus = IIF(EntityAxis = %TBGL_X, 0, -90)
  ' -- Overlay memory
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  x1 = @dummy_PtrTo_t_EntityInfo.x
  y1 = @dummy_PtrTo_t_EntityInfo.y
  z1 = @dummy_PtrTo_t_EntityInfo.z

  internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, v, u)

  VectorCross(r,u,v)

  x1x = r.x
  y1y = r.y
  z1z = r.z

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityTargetID))

  xT = @dummy_PtrTo_t_EntityInfo.x
  yT = @dummy_PtrTo_t_EntityInfo.y
  zT = @dummy_PtrTo_t_EntityInfo.z

  xT -= x1
  yT -= y1

  IF xT = 0 AND yT = 0 THEN
    FUNCTION = %TBGL_UNDEFINED
    EXIT FUNCTION

  ELSE

    Angle = internal_ATN2( x1x, y1y ) - internal_ATN2( xT, yT )

    IF Angle < -180-AngleBonus THEN
      Angle = Angle + 360
    ELSEIF Angle > 180-AngleBonus THEN
      Angle = Angle - 360
    END IF

    FUNCTION = Angle+AngleBonus
    EXIT FUNCTION

  END IF

END FUNCTION

FUNCTION Exec_TBGL_EntityGetAngleYZ() AS EXT

  LOCAL x1, y1, z1 AS DOUBLE
  LOCAL x1x, y1y, z1z AS DOUBLE
  LOCAL xT, yT, zT AS DOUBLE

  LOCAL Angle AS DOUBLE

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL EntityTargetID AS EXT
  LOCAL EntityAxis AS EXT

  LOCAL o, p, v, u AS TBGL_tVector3D

  LOCAL AngleBonus  AS LONG

  thinBasic_Parse4Numbers(ScriptSceneID, EntityID, EntityTargetID, EntityAxis)
  AngleBonus = IIF(EntityAxis = %TBGL_Y, 0, -90)

  ' -- Overlay memory
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  x1 = @dummy_PtrTo_t_EntityInfo.x
  y1 = @dummy_PtrTo_t_EntityInfo.y
  z1 = @dummy_PtrTo_t_EntityInfo.z

  internal_TRACKPOS(dummy_PtrTo_t_EntityInfo, p, o, v, u)

  x1x = u.x
  y1y = u.y
  z1z = u.z

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityTargetID))

  xT = @dummy_PtrTo_t_EntityInfo.x
  yT = @dummy_PtrTo_t_EntityInfo.y
  zT = @dummy_PtrTo_t_EntityInfo.z

  zT -= z1
  yT -= y1

  IF zT = 0 AND yT = 0 THEN
    FUNCTION = %TBGL_UNDEFINED
    EXIT FUNCTION

  ELSE
    Angle = internal_ATN2( y1y, z1z ) - internal_ATN2( yT, zT )

    IF Angle < -180-AngleBonus THEN
      Angle = Angle + 360
    ELSEIF Angle > 180-AngleBonus THEN
      Angle = Angle - 360
    END IF

    FUNCTION = Angle+AngleBonus
    EXIT FUNCTION

  END IF

END FUNCTION


' -- Texture override
SUB Exec_TBGL_EntitySetTexture()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL slot AS EXT

  thinBASIC_Parse3Numbers(ScriptSceneID, EntityID, slot)
  IF slot = 0 THEN slot = -1
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  SELECT CASE AS LONG @dummy_PtrTo_t_EntityInfo.EntType
    CASE %TBGL_ENTITY_MODEL
      dummy_PtrTo_t_Entity_Model = macro_t_Entity_Model_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_Model.ForceTex = slot

    CASE %TBGL_ENTITY_DISPLAYLIST
      dummy_PtrTo_t_Entity_DisplayList = macro_t_Entity_DisplayList_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_DisplayList.ForceTex = slot

    CASE %TBGL_ENTITY_BOX
      dummy_PtrTo_t_Entity_BOX = macro_t_Entity_Box_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_BOX.ForceTex = slot

    CASE %TBGL_ENTITY_SPHERE
      dummy_PtrTo_t_Entity_SPHERE = macro_t_Entity_Sphere_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_SPHERE.ForceTex = slot

    CASE %TBGL_ENTITY_CYLINDER
      dummy_PtrTo_t_Entity_CYLINDER = macro_t_Entity_Cylinder_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_CYLINDER.ForceTex = slot

    CASE %TBGL_ENTITY_TORUS
      dummy_PtrTo_t_Entity_Torus = macro_t_Entity_Torus_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_Torus.ForceTex = slot

    CASE %TBGL_ENTITY_FUNCSLOT
      dummy_PtrTo_t_Entity_FuncSlot = macro_t_Entity_FuncSlot_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_FuncSlot.ForceTex = slot

  END SELECT
END SUB

FUNCTION Exec_TBGL_EntityGetTexture() AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT

  LOCAL slot AS LONG

  thinBASIC_Parse2Numbers(ScriptSceneID, EntityID)

  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  slot = 0

  SELECT CASE AS LONG @dummy_PtrTo_t_EntityInfo.EntType
    CASE %TBGL_ENTITY_MODEL
      dummy_PtrTo_t_Entity_Model = macro_t_Entity_Model_MemPos(TBGLSceneEntities(EntityID))
      slot = @dummy_PtrTo_t_Entity_Model.ForceTex

    CASE %TBGL_ENTITY_DISPLAYLIST
      dummy_PtrTo_t_Entity_DisplayList = macro_t_Entity_DisplayList_MemPos(TBGLSceneEntities(EntityID))
      slot = @dummy_PtrTo_t_Entity_DisplayList.ForceTex

    CASE %TBGL_ENTITY_BOX
      dummy_PtrTo_t_Entity_BOX = macro_t_Entity_Box_MemPos(TBGLSceneEntities(EntityID))
      slot = @dummy_PtrTo_t_Entity_BOX.ForceTex

    CASE %TBGL_ENTITY_SPHERE
      dummy_PtrTo_t_Entity_SPHERE = macro_t_Entity_Sphere_MemPos(TBGLSceneEntities(EntityID))
      slot = @dummy_PtrTo_t_Entity_SPHERE.ForceTex

    CASE %TBGL_ENTITY_CYLINDER
      dummy_PtrTo_t_Entity_CYLINDER = macro_t_Entity_Cylinder_MemPos(TBGLSceneEntities(EntityID))
      slot = @dummy_PtrTo_t_Entity_CYLINDER.ForceTex

    CASE %TBGL_ENTITY_TORUS
      dummy_PtrTo_t_Entity_Torus = macro_t_Entity_Torus_MemPos(TBGLSceneEntities(EntityID))
      slot = @dummy_PtrTo_t_Entity_Torus.ForceTex

    CASE %TBGL_ENTITY_FUNCSLOT
      dummy_PtrTo_t_Entity_FuncSlot = macro_t_Entity_FuncSlot_MemPos(TBGLSceneEntities(EntityID))
      slot = @dummy_PtrTo_t_Entity_FuncSlot.ForceTex

  END SELECT

  IF slot = 0 THEN
    slot = -1
    FUNCTION = slot
  END IF

END FUNCTION

' -- Colors
SUB Exec_TBGL_EntitySetColor()
  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL r,g,b AS EXT

  thinBASIC_Parse5Numbers(ScriptSceneID, EntityID, r, g, b)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  SELECT CASE AS LONG @dummy_PtrTo_t_EntityInfo.EntType
    CASE %TBGL_ENTITY_MODEL
      dummy_PtrTo_t_Entity_Model = macro_t_Entity_Model_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_Model.ForceR = r
      @dummy_PtrTo_t_Entity_Model.ForceG = g
      @dummy_PtrTo_t_Entity_Model.ForceB = b

    CASE %TBGL_ENTITY_DISPLAYLIST
      dummy_PtrTo_t_Entity_DisplayList = macro_t_Entity_DisplayList_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_DisplayList.ForceR = r
      @dummy_PtrTo_t_Entity_DisplayList.ForceG = g
      @dummy_PtrTo_t_Entity_DisplayList.ForceB = b

    CASE %TBGL_ENTITY_LIGHT
      dummy_PtrTo_t_Entity_LIGHT = macro_t_Entity_Light_MemPos(TBGLSceneEntities(EntityID))

      @dummy_PtrTo_t_Entity_LIGHT.rgba(0) = r/255
      @dummy_PtrTo_t_Entity_LIGHT.rgba(1) = g/255
      @dummy_PtrTo_t_Entity_LIGHT.rgba(2) = b/255


   CASE %TBGL_ENTITY_BOX
      dummy_PtrTo_t_Entity_BOX = macro_t_Entity_Box_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_BOX.cRGB(0) = r
      @dummy_PtrTo_t_Entity_BOX.cRGB(1) = g
      @dummy_PtrTo_t_Entity_BOX.cRGB(2) = b

   CASE %TBGL_ENTITY_SPHERE
      dummy_PtrTo_t_Entity_SPHERE = macro_t_Entity_Sphere_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_SPHERE.cRGB(0) = r
      @dummy_PtrTo_t_Entity_SPHERE.cRGB(1) = g
      @dummy_PtrTo_t_Entity_SPHERE.cRGB(2) = b

   CASE %TBGL_ENTITY_CYLINDER
      dummy_PtrTo_t_Entity_CYLINDER = macro_t_Entity_Cylinder_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_CYLINDER.cRGB(0) = r
      @dummy_PtrTo_t_Entity_CYLINDER.cRGB(1) = g
      @dummy_PtrTo_t_Entity_CYLINDER.cRGB(2) = b

  CASE %TBGL_ENTITY_TORUS
      dummy_PtrTo_t_Entity_Torus = macro_t_Entity_Torus_MemPos(TBGLSceneEntities(EntityID))
      @dummy_PtrTo_t_Entity_Torus.cRGB(0) = r
      @dummy_PtrTo_t_Entity_Torus.cRGB(1) = g
      @dummy_PtrTo_t_Entity_Torus.cRGB(2) = b


  CASE %TBGL_ENTITY_FUNCSLOT
    dummy_PtrTo_t_Entity_FuncSlot = macro_t_Entity_FuncSlot_MemPos(TBGLSceneEntities(EntityID))
    @dummy_PtrTo_t_Entity_FuncSlot.ForceR = r
    @dummy_PtrTo_t_Entity_FuncSlot.ForceG = g
    @dummy_PtrTo_t_Entity_FuncSlot.ForceB = b

  END SELECT

END SUB

' ==============================================================================================
' -----------------------------------------------------------------------------
' -- INITIALIZATION AND AUXILIARY ROUTINES
' -----------------------------------------------------------------------------

SUB internal_SceneSubsystemInit()

  Scene_LastID = -1
  Scene_Name_LastID = -1
  Scene_UserData_LastID = -1

  ' -- These are just start values
  TBGLScene_Max            = 32
  TBGLEntity_MaxDefault    = 1024

  REDIM MyArrayOf_TBGLScene(1& TO TBGLScene_Max) AS STRING            ' -- Array index specifies scene, string contained inside contains "noodle" with each entity data
  REDIM MyArrayOf_TBGLScene_EntityMaxID(1& TO TBGLScene_Max) AS LONG  ' -- Array to hold max entity index values in each scene

  REDIM MyArrayOf_TBGLSceneNames(1& TO TBGLScene_Max) AS STRING       ' -- Array index specifies scene, string contained inside contains "noodle" with each entity name
  REDIM MyArrayOf_TBGLSceneUserData(1& TO TBGLScene_Max) AS STRING    ' -- Array index specifies scene, string contained inside contains "noodle" with each entity USER data

END SUB

' -- Our precious overlay trio - as we have all in abstract string buffers, it is useful to have overlay helpers

' -- Entity data overlay
SUB internal_SetEntityOverlay( BYVAL ScriptSceneID AS LONG )

  IF Scene_LastID <> ScriptSceneID THEN
    REDIM TBGLSceneEntities(1& TO MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)) AS STRING AT STRPTR(MyArrayOf_TBGLScene(ScriptSceneID))
    Scene_LastID = ScriptSceneID
  END IF

END SUB

' -- Entity user data overlay
SUB internal_SetEntityOverlayUserData( BYVAL ScriptSceneID AS LONG )

  IF Scene_UserData_LastID <> ScriptSceneID THEN
    REDIM TBGLSceneEntitiesUserData(1& TO MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)) AS STRING AT STRPTR(MyArrayOf_TBGLSceneUserData(ScriptSceneID))
    Scene_UserData_LastID = ScriptSceneID
  END IF

END SUB

' -- Entity names overlay
SUB internal_SetEntityOverlayNames( BYVAL ScriptSceneID AS LONG )

  IF Scene_Name_LastID <> ScriptSceneID THEN

    REDIM TBGLSceneEntitiesNames(1& TO MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)) AS STRING AT STRPTR(MyArrayOf_TBGLSceneNames(ScriptSceneID))
    Scene_Name_LastID = ScriptSceneID

  END IF

END SUB

' -- When our entity stack grows, we need to allocate more space
SUB internal_EntityRedimArrayIfNeeded( ScriptSceneID AS EXT, entityID AS EXT )
  IF entityID > MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID) THEN

    ' -- A LONG (4 bytes) can store a string
    ' -- Here we create an array of pointer to dynamic strings. Each string will be the storage of other objects
    LOCAL FillString AS STRING
    FillString = STRING$((entityID-MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)) * 4&, 0)
    MyArrayOf_TBGLScene(ScriptSceneID)              = MyArrayOf_TBGLScene(ScriptSceneID)         + FillString
    MyArrayOf_TBGLSceneUserData(ScriptSceneID)      = MyArrayOf_TBGLSceneUserData(ScriptSceneID) + FillString
    MyArrayOf_TBGLSceneNames(ScriptSceneID)         = MyArrayOf_TBGLSceneNames(ScriptSceneID)    + FillString

    MyArrayOf_TBGLScene_EntityMaxID(ScriptSceneID)  = entityID

    Scene_LastID = -1
    Scene_Name_LastID = -1
    Scene_UserData_LastID = -1

  END IF
END SUB

' -- Function to track position of given entity or point on it
' -- P - point we get back
' -- O - offset vector
' -- V - local Z axis
' -- U - local Y axis
SUB internal_TRACKPOS( BYVAL Entity AS t_EntityInfo PTR, BYREF p AS TBGL_tVector3D, OPTIONAL BYREF o AS TBGL_tVector3D, BYREF v AS TBGL_tVector3D, BYREF u AS TBGL_tVector3D )

  REGISTER i AS LONG

  DIM Stack(0 TO 32) AS LOCAL t_EntityInfo PTR, StackID AS BYTE
  LOCAL n AS LONG
  LOCAL LowerBound AS LONG

  LOCAL O_defined, V_defined, U_defined AS LONG

  StackID = 1
  Stack(StackID) = Entity

  DIM Matrix3x3(8)      AS LOCAL DOUBLE ' -- 0 .. 9
  DIM Matrix3x3_New(8)  AS LOCAL DOUBLE
  DIM Matrix3x3_Res(8)  AS LOCAL DOUBLE

  ARRAY ASSIGN Matrix3x3() = 1.0#, 0.0#, 0.0#, 0.0#, 1.0#, 0.0#, 0.0#, 0.0#, 1.0#

  DO
    n = @Entity.EntParent

    IF n > 0 THEN
      INCR StackID
      Stack(StackID) = STRPTR(TBGLSceneEntities(n))
      Entity = STRPTR(TBGLSceneEntities(n))
    ELSE
      EXIT LOOP
    END IF

  LOOP

  LowerBound = IIF(VARPTR(o.x), 0, 1)
  O_defined = VARPTR(o.x)
  U_defined = VARPTR(u.x)
  V_defined = VARPTR(v.x)

  FOR i = StackID TO lowerBound STEP -1

    IF i > 0 AND i <> StackID THEN

      p.x += Matrix3x3(0) * @Stack(i).x + Matrix3x3(3) * @Stack(i).y + Matrix3x3(6) * @Stack(i).z
      p.y += Matrix3x3(1) * @Stack(i).x + Matrix3x3(4) * @Stack(i).y + Matrix3x3(7) * @Stack(i).z
      p.z += Matrix3x3(2) * @Stack(i).x + Matrix3x3(5) * @Stack(i).y + Matrix3x3(8) * @Stack(i).z

    ELSEIF i = 0 THEN

      IF O_defined = 0 OR (o.x = 0 AND o.y = 0 AND o.z = 0) THEN EXIT IF

      MatrixMul3x3DFPU(Matrix3x3_Res(), Matrix3x3(), Matrix3x3_New())
      MAT Matrix3x3() = Matrix3x3_Res()

      p.x += Matrix3x3(0) * o.x + Matrix3x3(3) * o.y + Matrix3x3(6) * o.z
      p.y += Matrix3x3(1) * o.x + Matrix3x3(4) * o.y + Matrix3x3(7) * o.z
      p.z += Matrix3x3(2) * o.x + Matrix3x3(5) * o.y + Matrix3x3(8) * o.z

    ELSEIF i = StackID THEN

      p.x = @Stack(i).x
      p.y = @Stack(i).y
      p.z = @Stack(i).z

      IF V_defined THEN
        v.x = @Stack(i).LocalAxisZ.x
        v.y = @Stack(i).LocalAxisZ.y
        v.z = @Stack(i).LocalAxisZ.z
      END IF

      IF U_defined THEN
        u.x = @Stack(i).LocalAxisY.x
        u.y = @Stack(i).LocalAxisY.y
        u.z = @Stack(i).LocalAxisY.z
      END IF

    END IF

    IF i > 0 THEN ARRAY ASSIGN Matrix3x3_New() = @Stack(i).LocalAxisX.x, @Stack(i).LocalAxisX.y, @Stack(i).LocalAxisX.z, _
                                                 @Stack(i).LocalAxisY.x, @Stack(i).LocalAxisY.y, @Stack(i).LocalAxisY.z, _
                                                 @Stack(i).LocalAxisZ.x, @Stack(i).LocalAxisZ.y, @Stack(i).LocalAxisZ.z

    MatrixMul3x3DFPU(Matrix3x3_Res(), Matrix3x3(), Matrix3x3_New())

    MAT Matrix3x3() = Matrix3x3_Res()

    IF i > 0 AND i <> StackID THEN

      IF V_Defined THEN
        v.x# = Matrix3x3( 6)
        v.y# = Matrix3x3( 7)
        v.z# = Matrix3x3( 8)
      END IF

      IF U_Defined THEN
        u.x# = Matrix3x3( 3)
        u.y# = Matrix3x3( 4)
        u.z# = Matrix3x3( 5)
      END IF

    END IF

  NEXT

END SUB

' -- Used only if entity has parent - it traces all transformations
SUB internal_DoTransformationOfEntity( BYVAL Entity AS t_EntityInfo PTR )

  LOCAL local_dummy_PtrTo_t_EntityInfo AS t_EntityInfo PTR
  DIM matrix16(1 TO 16) AS LOCAL DOUBLE
  local_dummy_PtrTo_t_EntityInfo = Entity

  IF @local_dummy_PtrTo_t_EntityInfo.EntParent > 0 THEN
    internal_DoTransformationOfEntity( STRPTR(TBGLSceneEntities(@local_dummy_PtrTo_t_EntityInfo.EntParent)) )
  END IF

  ARRAY ASSIGN Matrix16() = @local_dummy_PtrTo_t_EntityInfo.LocalAxisX.x, @local_dummy_PtrTo_t_EntityInfo.LocalAxisX.y, @local_dummy_PtrTo_t_EntityInfo.LocalAxisX.z, 0.0#,_
                            @local_dummy_PtrTo_t_EntityInfo.LocalAxisY.x, @local_dummy_PtrTo_t_EntityInfo.LocalAxisY.y, @local_dummy_PtrTo_t_EntityInfo.LocalAxisY.z, 0.0#,_
                            @local_dummy_PtrTo_t_EntityInfo.LocalAxisZ.x, @local_dummy_PtrTo_t_EntityInfo.LocalAxisZ.y, @local_dummy_PtrTo_t_EntityInfo.LocalAxisZ.z, 0.0#,_
                            @local_dummy_PtrTo_t_EntityInfo.x           , @local_dummy_PtrTo_t_EntityInfo.y           , @local_dummy_PtrTo_t_EntityInfo.z           , 1.0#


  ' -- In the end we apply all transformations at once in GL
  glMultMatrixD( Matrix16(1) )

END SUB

' -- Few utility functions for rotations
SUB Vectors_RotateX( BYVAL Angle AS DOUBLE, BYREF Entity AS t_EntityInfo)

  Angle = DegToRad(Angle)

  LOCAL ca AS DOUBLE
  LOCAL sa AS DOUBLE
  ca = COS(Angle)
  sa = SIN(Angle)

  ' -- Rotate LocalAxisZ around the X axis :
  Entity.LocalAxisZ.x = Entity.LocalAxisZ.x * Ca + Entity.LocalAxisY.x * Sa
  Entity.LocalAxisZ.y = Entity.LocalAxisZ.y * Ca + Entity.LocalAxisY.y * Sa
  Entity.LocalAxisZ.z = Entity.LocalAxisZ.z * Ca + Entity.LocalAxisY.z * Sa

  Normalize3dVector(Entity.LocalAxisZ)

  ' -- now compute the new LocalAxisY (by cross product)
  VectorCross( Entity.LocalAxisY, Entity.LocalAxisZ, Entity.LocalAxisX )

END SUB

SUB Vectors_RotateY( BYVAL Angle AS DOUBLE, BYREF Entity AS t_EntityInfo)

  Angle = DegToRad(Angle)

  LOCAL ca AS DOUBLE
  LOCAL sa AS DOUBLE
  ca = COS(Angle)
  sa = SIN(Angle)

  ' -- ROTATE LocalAxisZ around the up vector:
  Entity.LocalAxisZ.x = Entity.LocalAxisZ.x * Ca - Entity.LocalAxisX.x * Sa
  Entity.LocalAxisZ.y = Entity.LocalAxisZ.y * Ca - Entity.LocalAxisX.y * Sa
  Entity.LocalAxisZ.z = Entity.LocalAxisZ.z * Ca - Entity.LocalAxisX.z * Sa

  Normalize3dVector(Entity.LocalAxisZ)

  ' -- now compute the NEW LocalAxisX (by cross product)
  VectorCross( Entity.LocalAxisX, Entity.LocalAxisY, Entity.LocalAxisZ )
END SUB

SUB Vectors_RotateZ( BYVAL Angle AS DOUBLE, BYREF Entity AS t_EntityInfo)

  Angle = DegToRad(Angle)

  LOCAL ca AS DOUBLE
  LOCAL sa AS DOUBLE
  ca = COS(Angle)
  sa = SIN(Angle)

  ' -- ROTATE LocalAxisZ around the up vector:
  Entity.LocalAxisX.x = Entity.LocalAxisX.x * Ca + Entity.LocalAxisY.x * Sa
  Entity.LocalAxisX.y = Entity.LocalAxisX.y * Ca + Entity.LocalAxisY.y * Sa
  Entity.LocalAxisX.z = Entity.LocalAxisX.z * Ca + Entity.LocalAxisY.z * Sa

  Normalize3dVector(Entity.LocalAxisX)

  ' -- now compute the NEW LocalAxisX (by cross product)
  VectorCross( Entity.LocalAxisY, Entity.LocalAxisZ, Entity.LocalAxisX )

END SUB

' -- ATN2 function
FUNCTION internal_ATN2( BYVAL y AS EXT, BYVAL x AS EXT) AS EXT

  LOCAL ax, ay, phi AS EXT

  IF x = 0.0## AND y = 0.0## THEN

    FUNCTION = 0##

  ELSE

    ax = ABS(x)
    ay = ABS(y)

    IF ax > ay THEN
      phi = RadToDeg( ATN(ay/ax) )
    ELSE
      phi = 90.0## - RadToDeg( ATN(ax/ay) )
    END IF

    IF x < 0.0## THEN phi = 180## - phi
    IF y < 0.0## THEN phi = -phi

    FUNCTION = phi

  END IF

END FUNCTION

' -- Charles Pegge fantastic code for matrix multiplication
SUB MatrixMul3x3DFPU(BYREF mr() AS DOUBLE, BYREF m1() AS DOUBLE, BYREF m2() AS DOUBLE )

  DIM p AS DOUBLE PTR

  p=VARPTR(m1(0))
  ! mov ecx,eax

  p=VARPTR(m2(0))
  ! mov edx,eax

  p=VARPTR(mr(0))


  block:
  ! call column
  ! call column
  ! call column
  EXIT SUB

  COLUMN:
  ! call cell
  ! call cell
  ! call cell
  ! add edx,24
  ! sub ecx,24
  ! ret

  CELL: ' row A * column B

  ! fld   qword ptr [ecx   ]
  ! fmul  qword ptr [edx   ]

  ! fld   qword ptr [ecx+24]
  ! fmul  qword ptr [edx+08]

  ! fld   qword ptr [ecx+48]
  ! fmul  qword ptr [edx+16]

  ! faddp st(1),st(0)
  ! faddp st(1),st(0)
  ! fstp  qword ptr [eax]
  ! add   eax,8
  ! add   ecx,8
  ! ret
END SUB

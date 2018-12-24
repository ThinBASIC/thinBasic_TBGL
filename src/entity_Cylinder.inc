'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- CYLINDER ENTITY
' -----------------------------------------------------------------------------

TYPE t_Entity_Cylinder
  r             AS SINGLE ' bottom radius
  r2            AS SINGLE ' top radius
  h             AS SINGLE ' height
  capped        AS LONG   ' capped or not :)
  cRGB(0 TO 2)  AS BYTE   ' diffuse color
  ForceTex      AS LONG   ' texure used
END TYPE

GLOBAL dummy_PtrTo_t_Entity_Cylinder    AS t_Entity_Cylinder PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_Cylinder_MemAlloc = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_Cylinder), 0)

' -- Returns pointer directly to cylinder data
MACRO macro_t_Entity_Cylinder_MemPos(pStart)  = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_Cylinder_FillData(ptrTo_t_EntityInfo, radius, radius2, height, tex, cr, cg, cb, cap)

  @ptrTo_t_EntityInfo.ForceTex= tex
  @ptrTo_t_EntityInfo.r       = radius
  @ptrTo_t_EntityInfo.r2      = radius2
  @ptrTo_t_EntityInfo.h       = height
  @ptrTo_t_EntityInfo.cRGB(0) = cr
  @ptrTo_t_EntityInfo.cRGB(1) = cg
  @ptrTo_t_EntityInfo.cRGB(2) = cb
  @ptrTo_t_EntityInfo.capped  = cap

END MACRO

' -- Both Cylinder and CylinderCapped differ just in the presence of ... cap :)
SUB Exec_TBGL_EntityCreateCylinder()

  LOCAL ScriptSceneID AS EXT

  LOCAL EntityID AS EXT
  LOCAL radius,radius2, height,tex,r,g,b AS EXT
  LOCAL PARENT AS EXT
  LOCAL n AS LONG

  n = thinBASIC_ParseXNumbers( 2, 10, ScriptSceneID, EntityID, PARENT, radius, radius2, height, tex, r, g, b )

  IF n < 4 THEN radius = 1
  IF n < 5 THEN radius2 = 1
  IF n < 6 THEN height = 1
  IF tex    = 0 THEN tex = -1
  IF n < 8 THEN
    r = 255
    g = 255
    b = 255
  END IF

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)

  TBGLSceneEntities(EntityID) = macro_t_Entity_Cylinder_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_CYLINDER, PARENT, 1, 0, 0, 0)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
    dummy_PtrTo_t_Entity_Cylinder = macro_t_Entity_Cylinder_MemPos(TBGLSceneEntities(EntityID))
    macro_t_Entity_Cylinder_FillData(dummy_PtrTo_t_Entity_Cylinder, radius, radius2, height, tex, r, g, b, 0 )

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

SUB Exec_TBGL_EntityCreateCylinderCapped()

  LOCAL ScriptSceneID AS EXT

  LOCAL EntityID AS EXT
  LOCAL radius,radius2, height,tex,r,g,b AS EXT
  LOCAL PARENT AS EXT
  LOCAL n AS LONG

  n = thinBASIC_ParseXNumbers( 2, 10, ScriptSceneID, EntityID, PARENT, radius, radius2, height, tex, r, g, b )

  IF radius = 0   THEN radius = 1
  IF radius2 = 0  THEN radius2 = 1
  IF height = 0   THEN height = 1
  IF tex    = 0 THEN tex = -1
  IF n < 8 THEN
    r = 255
    g = 255
    b = 255
  END IF

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)

  TBGLSceneEntities(EntityID) = macro_t_Entity_Cylinder_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_CYLINDER, PARENT, 1, 0, 0, 0)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
    dummy_PtrTo_t_Entity_Cylinder = macro_t_Entity_Cylinder_MemPos(TBGLSceneEntities(EntityID))
    macro_t_Entity_Cylinder_FillData(dummy_PtrTo_t_Entity_Cylinder, radius, radius2, height, tex, r, g, b, 1 )

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

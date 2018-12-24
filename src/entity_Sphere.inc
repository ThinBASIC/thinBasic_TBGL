'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- SPHERE ENTITY
' -----------------------------------------------------------------------------

TYPE t_Entity_Sphere
  r             AS SINGLE ' radius
  cRGB(0 TO 2)  AS BYTE   ' diffuse color
  ForceTex      AS LONG   ' texure used
END TYPE

GLOBAL dummy_PtrTo_t_Entity_Sphere      AS t_Entity_Sphere PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_Sphere_MemAlloc = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_Sphere), 0)

' -- Returns pointer directly to sphere data
MACRO macro_t_Entity_Sphere_MemPos(pStart)  = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_Sphere_FillData(ptrTo_t_EntityInfo, radius, tex, cr, cg, cb)

  @ptrTo_t_EntityInfo.ForceTex= tex
  @ptrTo_t_EntityInfo.r       = radius
  @ptrTo_t_EntityInfo.cRGB(0) = cr
  @ptrTo_t_EntityInfo.cRGB(1) = cg
  @ptrTo_t_EntityInfo.cRGB(2) = cb

END MACRO

' -- Creates sphere
SUB Exec_TBGL_EntityCreateSphere()

  LOCAL ScriptSceneID AS EXT

  LOCAL EntityID AS EXT
  LOCAL radius,tex,r,g,b AS EXT
  LOCAL PARENT AS EXT
  LOCAL n AS LONG

  n = thinBASIC_ParseXNumbers( 2, 8, ScriptSceneID, EntityID, PARENT, radius, tex, r, g, b )

  IF radius = 0 THEN radius = 1
  IF tex    = 0 THEN tex = -1
  IF n < 6 THEN
    r = 255
    g = 255
    b = 255
  END IF

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)

  TBGLSceneEntities(EntityID) = macro_t_Entity_Sphere_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_SPHERE, PARENT, 1, 0, 0, 0)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
    dummy_PtrTo_t_Entity_Sphere = macro_t_Entity_Sphere_MemPos(TBGLSceneEntities(EntityID))
    macro_t_Entity_Sphere_FillData(dummy_PtrTo_t_Entity_Sphere, radius, tex, r, g, b )

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

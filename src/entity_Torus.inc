'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- TORUS ENTITY
' -----------------------------------------------------------------------------

TYPE t_Entity_Torus
  r             AS SINGLE ' main radius
  r2            AS SINGLE ' smaller radius
  cRGB(0 TO 2)  AS BYTE   ' diffuse color
  ForceTex      AS LONG   ' texture used
END TYPE

GLOBAL dummy_PtrTo_t_Entity_Torus       AS t_Entity_Torus PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_Torus_MemAlloc = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_Torus), 0)

' -- Returns pointer directly to model data
MACRO macro_t_Entity_Torus_MemPos(pStart)  = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_Torus_FillData(ptrTo_t_EntityInfo, radius, radius2, tex, cr, cg, cb)

  @ptrTo_t_EntityInfo.ForceTex= tex
  @ptrTo_t_EntityInfo.r       = radius
  @ptrTo_t_EntityInfo.r2      = radius2
  @ptrTo_t_EntityInfo.cRGB(0) = cr
  @ptrTo_t_EntityInfo.cRGB(1) = cg
  @ptrTo_t_EntityInfo.cRGB(2) = cb

END MACRO

' -- Creates torus
SUB Exec_TBGL_EntityCreateTorus()

  LOCAL ScriptSceneID AS EXT

  LOCAL EntityID AS EXT
  LOCAL radius,radius2,tex,r,g,b AS EXT
  LOCAL PARENT AS EXT
  LOCAL n AS LONG

  n = thinBASIC_ParseXNumbers( 2, 9, ScriptSceneID, EntityID, PARENT, radius, radius2, tex, r, g, b )

  IF radius = 0 THEN radius = 0.5
  IF radius2 = 0 THEN radius2 = 1
  IF tex    = 0 THEN tex = -1
  IF n < 7 THEN
    r = 255
    g = 255
    b = 255
  END IF

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)

  TBGLSceneEntities(EntityID) = macro_t_Entity_Torus_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_TORUS, PARENT, 1, 0, 0, 0)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
    dummy_PtrTo_t_Entity_Torus = macro_t_Entity_Torus_MemPos(TBGLSceneEntities(EntityID))
    macro_t_Entity_Torus_FillData(dummy_PtrTo_t_Entity_Torus, radius, radius2, tex, r, g, b )

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

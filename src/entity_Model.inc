'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- MODEL ENTITY
' -----------------------------------------------------------------------------

' -- Extension to default model data
TYPE t_Entity_Model
  slot      AS LONG   ' which model slot is used

  ForceR    AS LONG   ' When ForceR, ForceG, ForceB is set to something else than -1, it ... forces color :)
  ForceG    AS LONG
  ForceB    AS LONG

  ForceTex  AS LONG   ' Can force new skin when <> -1
END TYPE

GLOBAL dummy_PtrTo_t_Entity_Model       AS t_Entity_Model PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_Model_MemAlloc       = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_Model), 0)

' -- Returns pointer directly to model data
MACRO macro_t_Entity_Model_MemPos(pStart) = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_Model_FillData(ptrTo_t_EntityInfo, nSlot)
  @ptrTo_t_EntityInfo.slot = nSlot

  @ptrTo_t_EntityInfo.ForceR   = -1
  @ptrTo_t_EntityInfo.ForceG   = -1
  @ptrTo_t_EntityInfo.ForceB   = -1
  @ptrTo_t_EntityInfo.ForceTex = -1
END MACRO

' -- Creates entity
SUB Exec_TBGL_EntityCreateModelSlot()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL Param AS EXT
  LOCAL PARENT AS EXT

  thinBASIC_Parse4Numbers(ScriptSceneID, EntityID, PARENT, Param )

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)

  TBGLSceneEntities(EntityID) = macro_t_Entity_Model_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_MODEL, PARENT, 1, 0, 0, 0)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
    dummy_PtrTo_t_Entity_Model = macro_t_Entity_Model_MemPos(TBGLSceneEntities(EntityID))
    macro_t_Entity_Model_FillData(dummy_PtrTo_t_Entity_Model, Param )

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

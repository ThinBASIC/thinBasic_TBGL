'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- PIVOT ENTITY
' -----------------------------------------------------------------------------

' -- Very basic one, contains only what normal entity info does
' -- Allocates all needed memory
MACRO macro_t_Entity_Pivot_MemAlloc = STRING$(SIZEOF(t_EntityInfo), 0)

' -- Returns pointer directly to pivot data (= entity info data in this case)
MACRO macro_t_Entity_Pivot_MemPos(pStart)  = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Creates entity
SUB Exec_TBGL_EntityCreatePivot()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL px, py, pz AS EXT
  LOCAL PARENT AS EXT

  thinBASIC_ParseXNumbers(2, 6, ScriptSceneID, EntityID, PARENT, px, py, pz)

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)

  TBGLSceneEntities(EntityID) = macro_t_Entity_Pivot_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_PIVOT, PARENT, 1, px, py, pz)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
  ' -- huh nothing here, as PIVOT is bare bone !

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

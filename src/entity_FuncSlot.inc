'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- FUNC SLOT ENTITY
' -----------------------------------------------------------------------------

' -- Extension to default model data
TYPE t_Entity_FuncSlot
  fPTR      AS LONG   ' which display list slot is used

  ForceR    AS LONG   ' forced color
  ForceG    AS LONG
  ForceB    AS LONG

  ForceTex  AS LONG   ' Can force new skin when <> -1
END TYPE

GLOBAL dummy_PtrTo_t_Entity_FuncSlot AS t_Entity_FuncSlot PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_FuncSlot_MemAlloc       = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_FuncSlot), 0)

' -- Returns pointer directly to display list data
MACRO macro_t_Entity_FuncSlot_MemPos(pStart) = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_FuncSlot_FillData(ptrTo_t_EntityInfo, nPTR)
  @ptrTo_t_EntityInfo.fPTR     = nPTR

  @ptrTo_t_EntityInfo.ForceR   = -1
  @ptrTo_t_EntityInfo.ForceG   = -1
  @ptrTo_t_EntityInfo.ForceB   = -1

  @ptrTo_t_EntityInfo.ForceTex = -1
END MACRO

' -- Creates entity
SUB Exec_TBGL_EntityCreateFuncSlot()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL PARENT AS EXT
  LOCAL FuncPTR AS DWORD

  LOCAL HasBrackets AS LONG

  IF thinBasic_CheckOpenParens_Optional THEN HasBrackets = %TRUE

  thinBasic_ParseNumber ScriptSceneID
  IF thinBasic_CheckComma_Mandatory THEN
    thinBasic_ParseNumber EntityID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber PARENT
      IF thinBasic_CheckComma_Mandatory THEN
        'thinBasic_ParseString FuncName

        IF thinBasic_ErrorFree THEN

          ' -- Retrieve pointer to the function
          FuncPtr = thinBasic_FunctionParseAndGetPtr(%FALSE)


          internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

          '---Now we over impose a dummy structure to previously allocated string
          internal_SetEntityOverlay(ScriptSceneID)

          TBGLSceneEntities(EntityID) = macro_t_Entity_FuncSlot_MemAlloc

          '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
            dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
            macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_FuncSlot, PARENT, 1, 0, 0, 0)', 0, 0, 0)

          '---Now point the object info pointer to point to the initial of the specific object data.
            dummy_PtrTo_t_Entity_FuncSlot = macro_t_Entity_FuncSlot_MemPos(TBGLSceneEntities(EntityID))
            macro_t_Entity_FuncSlot_FillData(dummy_PtrTo_t_Entity_FuncSlot, FuncPTR )

          ' -- Now reset name
          internal_SetEntityOverlayNames(ScriptSceneID)
          TBGLSceneEntitiesNames(EntityID) = ""

        END IF

      END IF
    END IF
  END IF

  IF HasBrackets THEN thinBasic_CheckCloseParens_Mandatory

END SUB

FUNCTION Exec_TBGL_CallingEntity() AS EXT
  FUNCTION = VARPTR(gCallingEntity)
END FUNCTION

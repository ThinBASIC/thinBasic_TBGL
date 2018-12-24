'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- BOUNDING SPHERE ENTITY
' -----------------------------------------------------------------------------

TYPE TBGL_tCollisionInfo
  position    as TBGL_tVector3D
  normal      as TBGL_tVector3D
  depth       as double
  scene       as long
  entity1     as long
  entity2     as long
END TYPE

global g_CollisionInfo as TBGL_tCollisionInfo
global g_CollisionType as long

' -- Extension to default entity data
TYPE t_Entity_BoundingSphere
  fPTR      AS LONG   ' pointer to collision handling function

  Radius    AS SINGLE ' radius

  Collided  as byte   ' For debug rendering, this flag is entirely handled by TBGL_SceneRender
END TYPE

GLOBAL dummy_PtrTo_t_Entity_BoundingSphere AS t_Entity_BoundingSphere PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_BoundingSphere_MemAlloc       = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_BoundingSphere), 0)

' -- Returns pointer directly to bounding sphere data
MACRO macro_t_Entity_BoundingSphere_MemPos(pStart) = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_BoundingSphere_FillData(ptrTo_t_EntityInfo, nPTR, nRadius)
  @ptrTo_t_EntityInfo.fPTR     = nPTR

  @ptrTo_t_EntityInfo.Radius   = nRadius
END MACRO

' -- Creates entity
SUB Exec_TBGL_EntityCreateBoundingSphere()

  LOCAL ScriptSceneID, EntityID, Parent, eRadius AS EXT
  LOCAL FuncPTR AS DWORD

  LOCAL HasBrackets AS LONG

  IF thinBasic_CheckOpenParens_Optional THEN HasBrackets = %TRUE

  thinBasic_ParseNumber ScriptSceneID
  IF thinBasic_CheckComma_Mandatory THEN
    thinBasic_ParseNumber EntityID
    IF thinBasic_CheckComma_Mandatory THEN
      thinBasic_ParseNumber PARENT
      IF thinBasic_CheckComma_Mandatory THEN
        thinBasic_ParseNumber eRadius

        IF thinBasic_CheckComma_Optional THEN
          'thinBasic_ParseString FuncName

          IF thinBasic_ErrorFree THEN

            ' -- Retrieve pointer to the function
            FuncPtr = thinBasic_FunctionParseAndGetPtr(%FALSE)

          END IF
        END IF

        internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

        '---Now we over impose a dummy structure to previously allocated string
        internal_SetEntityOverlay(ScriptSceneID)

        TBGLSceneEntities(EntityID) = macro_t_Entity_BoundingSphere_MemAlloc

        '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
          dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
          macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_BoundingSphere, PARENT, 1, 0, 0, 0)', 0, 0, 0)

        '---Now point the object info pointer to point to the initial of the specific object data.
          dummy_PtrTo_t_Entity_BoundingSphere = macro_t_Entity_BoundingSphere_MemPos(TBGLSceneEntities(EntityID))
          macro_t_Entity_BoundingSphere_FillData(dummy_PtrTo_t_Entity_BoundingSphere, FuncPTR, eRadius )

        ' -- Now reset name
        internal_SetEntityOverlayNames(ScriptSceneID)
        TBGLSceneEntitiesNames(EntityID) = ""

      END IF
    END IF
  END IF

  IF HasBrackets THEN thinBasic_CheckCloseParens_Mandatory

END SUB

FUNCTION Exec_TBGL_EntityGetCollisionInfo() AS EXT
  function = varptr(g_CollisionInfo)
END FUNCTION

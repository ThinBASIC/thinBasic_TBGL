'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- CAMERA ENTITY
' -----------------------------------------------------------------------------

' -- Equates for cType
%TBGL_CAMERATYPE_DEFAULT = 0
TYPE t_Entity_Camera
  cType     AS LONG     ' camera type
  FOV       AS DOUBLE   ' field of view
  ColorMask AS LONG     ' color mask filter
END TYPE

GLOBAL dummy_PtrTo_t_Entity_Camera      AS t_Entity_Camera PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_Camera_MemAlloc = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_Camera), 0)

' -- Returns pointer directly to camera data
MACRO macro_t_Entity_Camera_MemPos(pStart)  = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_Camera_FillData(ptrTo_t_EntityInfo, nType)
  @ptrTo_t_EntityInfo.cType     = nType
  @ptrTo_t_EntityInfo.FOV       = 45
  @ptrTo_t_EntityInfo.ColorMask = %TBGL_RED OR %TBGL_GREEN OR %TBGL_BLUE OR %TBGL_ALPHA
END MACRO

' -- Creates entity
SUB Exec_TBGL_EntityCreateCamera()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL PARENT AS EXT

  thinBASIC_ParseXNumbers(2,3,ScriptSceneID , EntityID, PARENT)

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)

  TBGLSceneEntities(EntityID) = macro_t_Entity_Camera_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_CAMERA, PARENT, 1, 0, 0, 0)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
    dummy_PtrTo_t_Entity_Camera = macro_t_Entity_Camera_MemPos(TBGLSceneEntities(EntityID))
    macro_t_Entity_Camera_FillData(dummy_PtrTo_t_Entity_Camera, 0 )

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

' -- Sets field of view
SUB Exec_TBGL_EntitySetFOV()
  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL fov AS EXT

  thinBASIC_Parse3Numbers(ScriptSceneID, EntityID, fov)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  IF fov = %TBGL_DEFAULT THEN
    fov = 45
  ELSE
    fov /= 2
  END IF

  IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_CAMERA THEN
      dummy_PtrTo_t_Entity_Camera = macro_t_Entity_Camera_MemPos(TBGLSceneEntities(EntityID))

      @dummy_PtrTo_t_Entity_Camera.FOV = fov
  END IF

END SUB

' -- Sets color mask ( filter )
SUB Exec_TBGL_EntitySetColorMask()
  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL cm AS EXT, lcm AS LONG

  thinBASIC_Parse3Numbers(ScriptSceneID, EntityID, cm)
  lcm = cm
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  IF lcm = %TBGL_DEFAULT THEN lcm = %TBGL_RED OR %TBGL_GREEN OR %TBGL_BLUE OR %TBGL_ALPHA

  IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_CAMERA THEN
      dummy_PtrTo_t_Entity_Camera = macro_t_Entity_Camera_MemPos(TBGLSceneEntities(EntityID))

      @dummy_PtrTo_t_Entity_Camera.ColorMask = lcm
  END IF

END SUB

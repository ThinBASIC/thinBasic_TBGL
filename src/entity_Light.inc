'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- LIGHT ENTITY
' -----------------------------------------------------------------------------

' -- Equates for cType
%TBGL_LIGHTTYPE_POINT  = 0
%TBGL_LIGHTTYPE_DIRECTIONAL = 1
%TBGL_LIGHTTYPE_SPOT        = 2

TYPE t_Entity_Light
  cType            AS LONG    ' type of light
  rgba(0 TO 3)     AS SINGLE  ' rgba diffuse color, expressed in [0..1] floats
  ambient(0 TO 3)  AS SINGLE  ' rgba ambient color
  specular(0 TO 3) AS SINGLE  ' rgba specular color
  BorderFade       AS SINGLE  ' border fade ratio
  cutoff           AS SINGLE  ' cutoff angle
END TYPE

GLOBAL dummy_PtrTo_t_Entity_Light       AS t_Entity_Light PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_Light_MemAlloc = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_Light), 0)

' -- Returns pointer directly to light data
MACRO macro_t_Entity_Light_MemPos(pStart)  = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_Light_FillData(ptrTo_t_EntityInfo, nType)

  @ptrTo_t_EntityInfo.cType       = nType
  @ptrTo_t_EntityInfo.BorderFade  = 0
  @ptrTo_t_EntityInfo.cutoff      = IIF( nType <> %TBGL_LIGHTTYPE_SPOT, 180, 45)

  @ptrTo_t_EntityInfo.RGBA(0)     = 1!
  @ptrTo_t_EntityInfo.RGBA(1)     = 1!
  @ptrTo_t_EntityInfo.RGBA(2)     = 1!
  @ptrTo_t_EntityInfo.RGBA(3)     = 1!

  @ptrTo_t_EntityInfo.ambient(0)  = 0!
  @ptrTo_t_EntityInfo.ambient(1)  = 0!
  @ptrTo_t_EntityInfo.ambient(2)  = 0!
  @ptrTo_t_EntityInfo.ambient(3)  = 1

  @ptrTo_t_EntityInfo.specular(0) = 0!
  @ptrTo_t_EntityInfo.specular(1) = 0!
  @ptrTo_t_EntityInfo.specular(2) = 0!
  @ptrTo_t_EntityInfo.specular(3) = 1

END MACRO

SUB Exec_TBGL_EntityCreateLight()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL Param AS EXT
  LOCAL PARENT AS EXT

  Param = %TBGL_LIGHTTYPE_POINT
  thinBASIC_ParseXNumbers( 2, 4, ScriptSceneID, EntityID, PARENT, Param )

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)

  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)

  TBGLSceneEntities(EntityID) = macro_t_Entity_Light_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_LIGHT, PARENT, 1, 0, 0, 0)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
    dummy_PtrTo_t_Entity_Light = macro_t_Entity_Light_MemPos(TBGLSceneEntities(EntityID))
    macro_t_Entity_Light_FillData(dummy_PtrTo_t_Entity_Light, Param )

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

' -- Sets cutoff angle
SUB Exec_TBGL_EntitySetCutoff()
  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL cutoff AS EXT

  thinBASIC_Parse3Numbers(ScriptSceneID, EntityID, cutoff)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_LIGHT THEN
      dummy_PtrTo_t_Entity_LIGHT = macro_t_Entity_Light_MemPos(TBGLSceneEntities(EntityID))

      @dummy_PtrTo_t_Entity_LIGHT.cutoff = cutoff
  END IF

END SUB

' -- Specifies how much light fades towards borders
SUB Exec_TBGL_EntitySetBorderFade()
  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL exponent AS EXT

  thinBASIC_Parse3Numbers(ScriptSceneID, EntityID, exponent)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_LIGHT THEN
      dummy_PtrTo_t_Entity_LIGHT = macro_t_Entity_Light_MemPos(TBGLSceneEntities(EntityID))

      ' -- OpenGL uses scale 0 - 128 instead of 0 - 1
      @dummy_PtrTo_t_Entity_LIGHT.BorderFade = exponent*1.28
  END IF

END SUB

' -- Ambient light color
SUB Exec_TBGL_EntitySetAmbient()
  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL r,g,b AS EXT

  thinBASIC_Parse5Numbers(ScriptSceneID, EntityID, r, g, b)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_LIGHT THEN
      dummy_PtrTo_t_Entity_LIGHT = macro_t_Entity_Light_MemPos(TBGLSceneEntities(EntityID))

      @dummy_PtrTo_t_Entity_LIGHT.ambient(0) = r/255
      @dummy_PtrTo_t_Entity_LIGHT.ambient(1) = g/255
      @dummy_PtrTo_t_Entity_LIGHT.ambient(2) = b/255
  END IF

END SUB

' -- Specular light color
SUB Exec_TBGL_EntitySetSpecular()
  REGISTER i AS LONG

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL r,g,b AS EXT

  thinBASIC_Parse5Numbers(ScriptSceneID, EntityID, r, g, b)
  internal_SetEntityOverlay(ScriptSceneID)

  dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))

  IF @dummy_PtrTo_t_EntityInfo.EntType = %TBGL_ENTITY_LIGHT THEN
      dummy_PtrTo_t_Entity_LIGHT = macro_t_Entity_Light_MemPos(TBGLSceneEntities(EntityID))

      @dummy_PtrTo_t_Entity_LIGHT.specular(0) = r/255
      @dummy_PtrTo_t_Entity_LIGHT.specular(1) = g/255
      @dummy_PtrTo_t_Entity_LIGHT.specular(2) = b/255
  END IF

END SUB

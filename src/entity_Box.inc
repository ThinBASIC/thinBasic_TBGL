'  =============================================================================
'   Part of tbgl_Entities.inc
'  =============================================================================

' -----------------------------------------------------------------------------
' -- BOX ENTITY
' -----------------------------------------------------------------------------

TYPE t_Entity_Box
  a             AS SINGLE ' sides of box for x,y,z axis
  b             AS SINGLE
  c             AS SINGLE
  cRGB(0 TO 2)  AS BYTE   ' diffuse color in RGB
  ForceTex      AS LONG   ' texure used
END TYPE

GLOBAL dummy_PtrTo_t_Entity_Box         AS t_Entity_Box PTR

' -- Allocates all needed memory
MACRO macro_t_Entity_Box_MemAlloc = STRING$(SIZEOF(t_EntityInfo) + SIZEOF(t_Entity_Box), 0)

' -- Returns pointer directly to box data
MACRO macro_t_Entity_Box_MemPos(pStart)  = STRPTR(pStart) + SIZEOF(t_EntityInfo)

' -- Fills data local for this type of entity
MACRO macro_t_Entity_Box_FillData(ptrTo_t_EntityInfo, sizeA, sizeB, sizeC, tex, cr, cg, cb )

  @ptrTo_t_EntityInfo.ForceTex= tex
  @ptrTo_t_EntityInfo.a    = sizeA
  @ptrTo_t_EntityInfo.b    = sizeB
  @ptrTo_t_EntityInfo.c    = sizeC
  @ptrTo_t_EntityInfo.cRGB(0) = cr
  @ptrTo_t_EntityInfo.cRGB(1) = cg
  @ptrTo_t_EntityInfo.cRGB(2) = cb

END MACRO

' -- Creates entity
SUB Exec_TBGL_EntityCreateBox()

  LOCAL ScriptSceneID AS EXT
  LOCAL EntityID AS EXT
  LOCAL parentEntity AS EXT
  LOCAL sizeA, sizeB, sizeC,tex,r,g,b AS EXT

  LOCAL n AS LONG
  n = thinBASIC_ParseXNumbers( 2, 10, ScriptSceneID, EntityID, parentEntity, sizeA, sizeB, sizeC, tex, r, g, b )
  IF n < 8 THEN
    r = 255
    g = 255
    b = 255
  END IF
  IF n < 7 THEN tex = -1
  IF tex    = 0 THEN tex = -1
  IF n < 4 THEN
    sizeA = 1
    sizeB = 1
    sizeC = 1
  END IF

  internal_EntityRedimArrayIfNeeded(ScriptSceneID, EntityID)
  '---Now we over impose a dummy structure to previously allocated string
  internal_SetEntityOverlay(ScriptSceneID)
  TBGLSceneEntities(EntityID) = macro_t_Entity_Box_MemAlloc

  '---Now point the object info pointer to point to the first byte (remember here there is the space for t_ObjectInfo
    dummy_PtrTo_t_EntityInfo = STRPTR(TBGLSceneEntities(EntityID))
    macro_t_EntityInfo_FillData(dummy_PtrTo_t_EntityInfo, %TBGL_ENTITY_BOX, parentEntity, 1, 0, 0, 0)', 0, 0, 0)

  '---Now point the object info pointer to point to the initial of the specific object data.
    dummy_PtrTo_t_Entity_Box = macro_t_Entity_Box_MemPos(TBGLSceneEntities(EntityID))
    macro_t_Entity_Box_FillData(dummy_PtrTo_t_Entity_Box, sizeA, sizeB, sizeC, tex, r, g, b )

  ' -- Now reset name
  internal_SetEntityOverlayNames(ScriptSceneID)
  TBGLSceneEntitiesNames(EntityID) = ""

END SUB

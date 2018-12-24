'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : Functions to manage ooold bones
' =============================================================================
'
' -----------------------------------------------------------------------------

#INCLUDE ONCE "tbgl_Models.inc"   ' -- Bones are designed for M15 models

' -----------------------------------------------------------------------------

' -- Transformations
%TRN_NONE = 0
%TRN_ROT  = 1
%TRN_POS  = 2
%TRN_MOV  = 3

' -- Limits
MACRO MAXBONESOURCE = 5000      ' -- Max vertices for one bone
%MAXBONES = 64                  ' -- Max bone number

TYPE tBoneTransform
  AnchorBone        AS BYTE     '
  WhichAnchorIndex  AS BYTE
  trnX              AS SINGLE
  trnY              AS SINGLE
  trnZ              AS SINGLE
  ttype             AS BYTE
END TYPE

TYPE tBoneVertex
  v AS LONG         ' vertex
  vtx(2) AS SINGLE  ' ! 0 to 2
END TYPE

' -- Holds info on vertices in bones
TYPE tBoneBox
  anchorX         AS SINGLE
  anchorY         AS SINGLE
  anchorZ         AS SINGLE

  alteranchorX(1 TO %MAXBONES)         AS SINGLE
  alteranchorY(1 TO %MAXBONES)         AS SINGLE
  alteranchorZ(1 TO %MAXBONES)         AS SINGLE

  anchorindex AS BYTE

  childB          AS BYTE     ' pointer to tree branch containing childs

  transforms(1 TO %MAXBONES)  AS tBoneTransform
  transformsCount AS BYTE

  SOURCE(1 TO MAXBONESOURCE)  AS tBoneVertex ' holds ORIGINAL data, should be read only
  sourceCount     AS LONG
END TYPE

GLOBAL BoneBox()  AS tBoneBox

TYPE tBoneTree
  bonesCount AS BYTE
  awaitingLevel AS BYTE 'To get filled
  CHILD(1 TO %MAXBONES) AS BYTE
END TYPE

GLOBAL BoneTree() AS tBoneTree

' -----------------------------------------------------------------------------

TYPE Ts3D
  x AS SINGLE
  y AS SINGLE
  z AS SINGLE
END TYPE

' -----------------------------------------------------------------------------

' -- Used in bone apply
GLOBAL vtx_x() AS SINGLE
GLOBAL vtx_y() AS SINGLE
GLOBAL vtx_z() AS SINGLE

' -----------------------------------------------------------------------------

' -----------------------------------------------------------------------------
' -- BONE DEFINITION
' -----------------------------------------------------------------------------

' -- Using box
SUB Exec_TBGL_m15DefBoneBox()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  REGISTER i AS LONG, j AS LONG
  LOCAL Model AS EXT, ID AS EXT, minx AS EXT, maxx AS EXT, miny AS EXT, maxy AS EXT, minz AS EXT, maxz AS EXT, anchorX AS EXT, anchorY AS EXT, anchorZ AS EXT

  thinBasic_ParseNumber Model
  LOCAL vertexcount AS LONG
  vertexcount = m15ModelData(model).vertexCount

  REDIM m15ModelBuffer_Overlay(1 TO vertexcount) AT STRPTR(m15ModelBuffer(Model))


  IF thinBasic_CheckComma_Mandatory THEN
    thinBasic_ParseNumber ID
      IF thinBasic_CheckComma_Mandatory THEN
        thinBasic_ParseNumber minx
        IF thinBasic_CheckComma_Mandatory THEN
          thinBasic_ParseNumber maxx
          IF thinBasic_CheckComma_Mandatory THEN
            thinBasic_ParseNumber miny
            IF thinBasic_CheckComma_Mandatory THEN
              thinBasic_ParseNumber maxy
              IF thinBasic_CheckComma_Mandatory THEN
                thinBasic_ParseNumber minz
                IF thinBasic_CheckComma_Mandatory THEN
                  thinBasic_ParseNumber maxz
                  IF thinBasic_CheckComma_Mandatory THEN
                    thinBasic_ParseNumber anchorX
                    IF thinBasic_CheckComma_Mandatory THEN
                      thinBasic_ParseNumber anchorY
                      IF thinBasic_CheckComma_Mandatory THEN
                        thinBasic_ParseNumber anchorZ

                        BoneBox(Model, ID).anchorX = anchorX
                        BoneBox(Model, ID).anchorY = anchorY
                        BoneBox(Model, ID).anchorZ = anchorZ

                        FOR i = 1 TO vertexcount

                          ' -- Simple test whether vertex is or not in bone
                          IF m15ModelBuffer_Overlay(i).vtx(0) >= minx AND m15ModelBuffer_Overlay(i).vtx(0) <= maxx AND _
                             m15ModelBuffer_Overlay(i).vtx(1) >= miny AND m15ModelBuffer_Overlay(i).vtx(1) <= maxy AND _
                             m15ModelBuffer_Overlay(i).vtx(2) >= minz AND m15ModelBuffer_Overlay(i).vtx(2) <= maxz THEN

                             IF j = MAXBONESOURCE THEN
                               TBGLERROR ( "Module limit reached: TBGL_m15DefBoneBox"+STR$(Model)+","+STR$(ID)+" tries to allocate more than"+STR$(MAXBONESOURCE)+" vertices")
                               EXIT FOR
                             END IF

                             INCR j
                             BoneBox(Model, ID).SOURCE(j).v = i
                             BoneBox(Model, ID).SOURCE(j).vtx(0) = m15ModelBuffer_Overlay(i).vtx(0)
                             BoneBox(Model, ID).SOURCE(j).vtx(1) = m15ModelBuffer_Overlay(i).vtx(1)
                             BoneBox(Model, ID).SOURCE(j).vtx(2) = m15ModelBuffer_Overlay(i).vtx(2)
                          END IF

                        NEXT
                          BoneBox(Model, ID).sourceCount = j

                      END IF
                    END IF
                  END IF
                END IF

              END IF
            END IF
          END IF

      END IF
    END IF
  END IF
END SUB

' -- Defines bone according to layer name
SUB Exec_TBGL_m15DefBoneLayer()

  REGISTER i AS LONG, j AS LONG
  LOCAL wantedLayer AS LONG

  LOCAL model, ID, anchorX, anchorY, anchorZ AS EXT
  LOCAL layer AS STRING

  thinBasic_ParseNumber Model
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
    IF thinBasic_CheckComma() THEN
      thinBasic_ParseNumber ID
      IF thinBasic_CheckComma() THEN
        thinBasic_ParseString layer
        IF thinBasic_CheckComma() THEN
          thinBasic_ParseNumber anchorX
          IF thinBasic_CheckComma() THEN
            thinBasic_ParseNumber anchorY
            IF thinBasic_CheckComma() THEN
              thinBasic_ParseNumber anchorZ

              layer = UCASE$(layer)

              wantedLayer = -10
              FOR i = 1 TO PARSECOUNT( m15ModelLayers(Model), "," )
                IF PARSE$(m15ModelLayers(Model), ",", i) = layer THEN
                  wantedLayer = i - 1
                  EXIT FOR
                END IF
              NEXT

              IF wantedLayer = -10 THEN EXIT SUB

              LayerFound:

              BoneBox(Model, ID).anchorX = anchorX
              BoneBox(Model, ID).anchorY = anchorY
              BoneBox(Model, ID).anchorZ = anchorZ

              LOCAL vertexcount AS LONG
              vertexcount = m15ModelData(model).vertexCount

              FOR i = 1 TO vertexcount

                IF m15ModelBuffer_Overlay(i).Layer = wantedLayer THEN
                   IF j = MAXBONESOURCE THEN
                     TBGLERROR ( "Module limit reached: TBGL_m15DefBoneLayer"+STR$(Model)+","+STR$(ID)+" tries to allocate more than"+STR$(MAXBONESOURCE)+" vertices")
                     EXIT FOR
                   END IF

                   INCR j
                   BoneBox(Model, ID).SOURCE(j).v = i
                   BoneBox(Model, ID).SOURCE(j).vtx(0) = m15ModelBuffer_Overlay(i).vtx(0)
                   BoneBox(Model, ID).SOURCE(j).vtx(1) = m15ModelBuffer_Overlay(i).vtx(1)
                   BoneBox(Model, ID).SOURCE(j).vtx(2) = m15ModelBuffer_Overlay(i).vtx(2)
                END IF

              NEXT
              BoneBox(Model, ID).sourceCount = j

            END IF
          END IF
        END IF
      END IF
    END IF

END SUB

' -- Defines bone according to color used
SUB Exec_TBGL_m15DefBoneColor()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  REGISTER i AS LONG, j AS LONG
  LOCAL Model AS EXT, ID AS EXT, anchorX AS EXT, anchorY AS EXT, anchorZ AS EXT, r AS EXT, g AS EXT, b AS EXT
  LOCAL vertexcount AS LONG
  thinBASIC_ParseXNumbers(8,8,Model, ID, r, g, b, anchorX, anchorY, anchorZ )
  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
  vertexcount = m15ModelData(model).vertexCount

  BoneBox(Model, ID).anchorX = anchorX
  BoneBox(Model, ID).anchorY = anchorY
  BoneBox(Model, ID).anchorZ = anchorZ

  FOR i = 1 TO vertexcount

    IF m15ModelBuffer_Overlay(i).clr(0) = r AND m15ModelBuffer_Overlay(i).clr(1) = g AND m15ModelBuffer_Overlay(i).clr(2) = b THEN

       IF j = MAXBONESOURCE THEN
         TBGLERROR ( "Module limit reached: TBGL_m15DefBoneBox"+STR$(Model)+","+STR$(ID)+" tries to allocate more than"+STR$(MAXBONESOURCE)+" vertices")
         EXIT FOR
       END IF

       INCR j
       BoneBox(Model, ID).SOURCE(j).v = i
       BoneBox(Model, ID).SOURCE(j).vtx(0) = m15ModelBuffer_Overlay(i).vtx(0)
       BoneBox(Model, ID).SOURCE(j).vtx(1) = m15ModelBuffer_Overlay(i).vtx(1)
       BoneBox(Model, ID).SOURCE(j).vtx(2) = m15ModelBuffer_Overlay(i).vtx(2)
    END IF

  NEXT
    BoneBox(Model, ID).sourceCount = j

END SUB

' -- Defines bone by adding vertex by vertex
SUB Exec_TBGL_m15DefBoneAddVertex()

    REGISTER  j AS LONG
    LOCAL model, ID, vertexID  AS EXT

    thinBasic_Parse3Numbers(Model, ID, vertexID)
    REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))
    INCR BoneBox(Model, ID).sourceCount
    j = BoneBox(Model, ID).sourceCount
    IF j > MAXBONESOURCE THEN
       DECR BoneBox(Model, ID).sourceCount
       TBGLERROR ( "Module limit reached: TBGL_m15DefBoneBox"+STR$(Model)+","+STR$(ID)+" tries to allocate more than"+STR$(MAXBONESOURCE)+" vertices")
       EXIT SUB
    END IF

    BoneBox(Model, ID).SOURCE(j).v = vertexID
    BoneBox(Model, ID).SOURCE(j).vtx(0) = m15ModelBuffer_Overlay(vertexID).vtx(0)
    BoneBox(Model, ID).SOURCE(j).vtx(1) = m15ModelBuffer_Overlay(vertexID).vtx(1)
    BoneBox(Model, ID).SOURCE(j).vtx(2) = m15ModelBuffer_Overlay(vertexID).vtx(2)


END SUB

' -- Defines bone anchor point separately
SUB Exec_TBGL_m15DefBoneAnchor()

  REGISTER  j AS LONG
  LOCAL model, ID, anchorX, anchorY, anchorZ AS EXT

  thinBasic_Parse5Numbers( Model, ID, anchorX, anchorY, anchorZ)

  BoneBox(Model, ID).anchorX = anchorX
  BoneBox(Model, ID).anchorY = anchorY
  BoneBox(Model, ID).anchorZ = anchorZ

END SUB

' -- Resets definition of bone
  SUB Exec_TBGL_m15DefBoneReset()

    LOCAL model, Bone AS EXT

    thinBasic_Parse2Numbers( Model, Bone )
    RESET BoneBox(Model, Bone)

  END SUB

' -- Makes bone reusable
SUB Exec_TBGL_m15DefBoneEmpty()

  REGISTER i AS LONG
  LOCAL model, Bone AS EXT

  thinBasic_Parse2Numbers( Model, Bone )
  RESET BoneBox(Model, Bone)

  BoneTree(Model, Bone).bonesCount = 0

  FOR i = 1 TO %MAXBONES
    BoneTree(Model, Bone).CHILD(i) = 0  ' not really necessary as bonecount set to zero
  NEXT


END SUB

' -----------------------------------------------------------------------------
' -- BONE NESTING
' -----------------------------------------------------------------------------

' -- Erases links to child bones
SUB Exec_TBGL_m15EraseChildbones()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  REGISTER i AS LONG
  LOCAL Model, Bone AS EXT

  thinBasic_Parse2Numbers( Model, Bone )

  BoneTree(Model, Bone).bonesCount = 0

  FOR i = 1 TO %MAXBONES
    BoneTree(Model, Bone).CHILD(i) = 0  ' not really necessary (it will get overwritten), but to keep it clean
  NEXT

END SUB

' -- Sets child bone
SUB Exec_TBGL_m15SetBoneChild()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  LOCAL Model, Bone, ChildBone AS EXT

  thinBasic_Parse3Numbers( Model, Bone, ChildBone )

  IF BoneTree(Model, Bone).bonesCount = %MAXBONES THEN tbglError "Number of child bones for bone"+STR$(Bone)+" exceeds"+STR$(%MAXBONES) : EXIT SUB

  INCR BoneTree(Model, Bone).bonesCount

  BoneTree(Model, Bone).CHILD(BoneTree(Model, Bone).bonesCount) = ChildBone

END SUB

' -- Gets child bone
FUNCTION Exec_TBGL_m15GetBoneChild() AS EXT
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT FUNCTION
  LOCAL Model, Bone, ChildNumber AS EXT

  thinBasic_Parse3Numbers( Model, Bone, ChildNumber )

  IF ChildNumber > %MAXBONES OR ChildNumber < 0 THEN tbglError "Bone can have maximum of "+STR$(%MAXBONES)+" of child bones" : EXIT FUNCTION

  FUNCTION = BoneTree(Model, Bone).CHILD(ChildNumber)

END FUNCTION

' -- Gets how much child bones
FUNCTION Exec_TBGL_m15GetBoneChildCount() AS EXT
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT FUNCTION
  LOCAL Model, Bone AS EXT

  thinBasic_Parse2Numbers( Model, Bone )

  FUNCTION = BoneTree(Model, Bone).bonesCount

END FUNCTION

' -----------------------------------------------------------------------------
' -- ANIMATING
' -----------------------------------------------------------------------------

' -- Resets bones
SUB Exec_TBGL_m15ResetBones()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
  REGISTER i AS LONG
  LOCAL Model AS EXT

  Model = thinBASIC_Parse1Number

  FOR i = 1 TO %MAXBONES
    BoneBox(Model,i).transformsCount = 0

    BoneBox(Model,i).anchorindex = 1
    BoneBox(Model,i).alteranchorx(1) = BoneBox(Model,i).anchorx
    BoneBox(Model,i).alteranchory(1) = BoneBox(Model,i).anchory
    BoneBox(Model,i).alteranchorz(1) = BoneBox(Model,i).anchorz

  NEXT

END SUB

' -- Rotation of bone on defined axes
' -- [!] Note these functions just put transformations on stack, they do not perform
' --     the transformation itself!
SUB Exec_TBGL_m15RotBone()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  LOCAL Model, Bone, nanglex, nangley, nanglez AS EXT
  thinBasic_Parse5Numbers( Model, Bone, nanglex, nangley, nanglez )

  internal_m15RotBone(Model,Bone,nanglex+180,nangley+180,nanglez+180,Bone)

END SUB

SUB Exec_TBGL_m15RotBoneX()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  LOCAL Model, Bone, nangle AS EXT
  thinBasic_Parse3Numbers( Model, Bone, nangle )

  internal_m15RotBone(Model,Bone,nangle+180,180,180,Bone)

END SUB

SUB Exec_TBGL_m15RotBoneY()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  LOCAL Model, Bone, nangle AS EXT
  thinBasic_Parse3Numbers( Model, Bone, nangle )

  internal_m15RotBone(Model,Bone,180,nangle+180,180,Bone)

END SUB

SUB Exec_TBGL_m15RotBoneZ()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  LOCAL Model, Bone, nangle AS EXT
  thinBasic_Parse3Numbers( Model, Bone, nangle )

  internal_m15RotBone(Model,Bone,180,180,nangle+180,Bone)

END SUB

' -- The most important function, takes transforms from the stack and applies it
SUB Exec_TBGL_m15ApplyBones()
  IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB

  REGISTER i AS LONG, t AS LONG
  LOCAL b AS BYTE

  LOCAL TMAX AS LONG
  LOCAL angle AS SINGLE
  LOCAL tx AS SINGLE
  LOCAL ty AS SINGLE
  LOCAL tz AS SINGLE
  LOCAL anchBone AS LONG, anchorindex AS LONG, dstVtx AS LONG
  LOCAL model AS EXT

  LOCAL cAngle, sAngle AS SINGLE
  LOCAL auxX, auxY, auxZ AS SINGLE
  Model = thinBasic_Parse1Number

  REDIM m15ModelBuffer_Overlay(1 TO m15ModelData(Model).vertexCount) AT STRPTR(m15ModelBuffer(Model))

  DIM OriginalAnchor(1 TO %MAXBONES) AS Ts3D

    FOR b = 1 TO %MAXBONES

      ' -- Backup
      OriginalAnchor(b).x = BoneBox(Model, b).anchorX
      OriginalAnchor(b).y = BoneBox(Model, b).anchorY
      OriginalAnchor(b).z = BoneBox(Model, b).anchorZ

      FOR i = 1 TO BoneBox(Model, b).sourceCount
        dstVtx = BoneBox(Model, b).SOURCE(i).v

        vtx_x(dstVtx) = BoneBox(Model, b).SOURCE(i).vtx(0)
        vtx_y(dstVtx) = BoneBox(Model, b).SOURCE(i).vtx(1)
        vtx_z(dstVtx) = BoneBox(Model, b).SOURCE(i).vtx(2)
      NEXT

      TMAX =  BoneBox(Model, b).transformsCount

      ' -- For all transformations we will go through and perform what must be done
      FOR t = 1 TO TMAX
        anchBone = BoneBox(Model, b).transforms(t).AnchorBone
        anchorindex = BoneBox(Model, b).transforms(t).WhichAnchorIndex
        auxX = BoneBox(Model, anchBone).alteranchorx(anchorindex)
        auxY = BoneBox(Model, anchBone).alteranchory(anchorindex)
        auxZ = BoneBox(Model, anchBone).alteranchorz(anchorindex)

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ''' ROTATION
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        IF BoneBox(Model, anchBone).transforms(t).ttype = %TRN_ROT THEN
          angle = BoneBox(Model, anchBone).transforms(t).trnX

          IF angle = 180 THEN GOTO trnY
          angle = DegToRad(angle)

          cAngle = COS(angle)
          sAngle = SIN(angle)

          FOR i = 1 TO BoneBox(Model, b).sourceCount
            dstVtx = BoneBox(Model, b).SOURCE(i).v
            ty = auxY - vtx_y(dstVtx)
            tz = auxZ - vtx_z(dstVtx)

            vtx_y(dstVtx) = ty*cAngle-tz*sAngle + auxY
            vtx_z(dstVtx) = ty*sAngle+tz*cAngle + auxZ
          NEXT

          trnY:
          angle = BoneBox(Model, anchBone).transforms(t).trnY
          IF angle = 180 THEN GOTO trnZ
          angle = DegToRad(angle)
          cAngle = COS(angle)
          sAngle = SIN(angle)

          FOR i = 1 TO BoneBox(Model, b).sourceCount
            dstVtx = BoneBox(Model, b).SOURCE(i).v
            tx = auxX - vtx_x(dstVtx)
            tz = auxZ - vtx_z(dstVtx)

            vtx_x(dstVtx) = tx*cAngle-tz*sAngle + auxX
            vtx_z(dstVtx) = tx*sAngle+tz*cAngle + auxZ
          NEXT


          trnZ:
          angle = BoneBox(Model, anchBone).transforms(t).trnZ
          IF angle = 180 THEN ITERATE FOR
          angle = DegToRad(angle)
          cAngle = COS(angle)
          sAngle = SIN(angle)

          FOR i = 1 TO BoneBox(Model, b).sourceCount
            dstVtx = BoneBox(Model, b).SOURCE(i).v
            tx = auxX - vtx_x(dstVtx)
            ty = auxY - vtx_y(dstVtx)

            vtx_x(dstVtx) = tx*cAngle-ty*sAngle + auxX
            vtx_y(dstVtx) = tx*sAngle+ty*cAngle + auxY
          NEXT

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ''' POSITIONING
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ELSEIF BoneBox(Model, anchBone).transforms(t).ttype = %TRN_POS THEN
            auxX = BoneBox(Model, b).anchorX
            auxY = BoneBox(Model, b).anchorY
            auxZ = BoneBox(Model, b).anchorZ

            BoneBox(Model, b).anchorX = BoneBox(Model, b).transforms(t).trnX
            BoneBox(Model, b).anchorY = BoneBox(Model, b).transforms(t).trnY
            BoneBox(Model, b).anchorZ = BoneBox(Model, b).transforms(t).trnZ

            FOR i = 1 TO BoneBox(Model, b).sourceCount
              dstVtx = BoneBox(Model, b).SOURCE(i).v

              vtx_x(dstVtx) = vtx_x(dstVtx) - auxX + BoneBox(Model, b).anchorX
              vtx_y(dstVtx) = vtx_y(dstVtx) - auxY + BoneBox(Model, b).anchorY
              vtx_z(dstVtx) = vtx_z(dstVtx) - auxZ + BoneBox(Model, b).anchorZ
            NEXT

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ''' MOVING
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ELSEIF BoneBox(Model, anchBone).transforms(t).ttype = %TRN_MOV THEN
            auxX = BoneBox(Model, b).anchorX
            auxY = BoneBox(Model, b).anchorY
            auxZ = BoneBox(Model, b).anchorZ

            BoneBox(Model, b).anchorX = BoneBox(Model, b).anchorX+BoneBox(Model, b).transforms(t).trnX
            BoneBox(Model, b).anchorY = BoneBox(Model, b).anchorY+BoneBox(Model, b).transforms(t).trnY
            BoneBox(Model, b).anchorZ = BoneBox(Model, b).anchorZ+BoneBox(Model, b).transforms(t).trnZ

            FOR i = 1 TO BoneBox(Model, b).sourceCount
              dstVtx = BoneBox(Model, b).SOURCE(i).v

              vtx_x(dstVtx) = vtx_x(dstVtx) + BoneBox(Model, b).transforms(t).trnX
              vtx_y(dstVtx) = vtx_y(dstVtx) + BoneBox(Model, b).transforms(t).trnY
              vtx_z(dstVtx) = vtx_z(dstVtx) + BoneBox(Model, b).transforms(t).trnZ
            NEXT

        END IF

      NEXT t

      ' -- Recovering
      IF TMAX THEN
        FOR i = 1 TO BoneBox(Model, b).sourceCount
          dstVtx = BoneBox(Model, b).SOURCE(i).v
          m15ModelBuffer_Overlay(dstVtx).vtx(0) = vtx_x(dstVtx)
          m15ModelBuffer_Overlay(dstVtx).vtx(1) = vtx_y(dstVtx)
          m15ModelBuffer_Overlay(dstVtx).vtx(2) = vtx_z(dstVtx)
        NEXT
      END IF

    NEXT  b

    ' -- Recovering original anchors
    FOR b = 1 TO %MAXBONES
      BoneBox(Model, b).anchorX = OriginalAnchor(b).x
      BoneBox(Model, b).anchorY = OriginalAnchor(b).y
      BoneBox(Model, b).anchorZ = OriginalAnchor(b).z
    NEXT  b

END SUB

' =============================================================================

SUB internal_RotateZAroundXY( BYREF x AS SINGLE ,BYREF y AS SINGLE, BYVAL angle AS SINGLE, BYVAL x2 AS SINGLE, BYVAL y2 AS SINGLE )

  LOCAL tx AS SINGLE
  LOCAL ty AS SINGLE
  tx = x2-x
  ty = y2-y

  angle = DegToRad(Angle)
  x = tx*COS(Angle)-ty*SIN(Angle) + x2
  y = tx*SIN(Angle)+ty*COS(Angle) + y2

END SUB

SUB internal_RotateYAroundXZ( BYREF x AS SINGLE ,BYREF z AS SINGLE, BYVAL angle AS SINGLE, BYVAL x2 AS SINGLE, BYVAL z2 AS SINGLE )

  LOCAL tx AS SINGLE
  LOCAL tz AS SINGLE
  tx = x2-x
  tz = z2-z

  angle = DegToRad(Angle)
  x = tx*COS(Angle)-tz*SIN(Angle) + x2
  z = tx*SIN(Angle)+tz*COS(Angle) + z2

END SUB

SUB internal_RotateXAroundYZ( BYREF y AS SINGLE ,BYREF z AS SINGLE, BYVAL angle AS SINGLE, BYVAL y2 AS SINGLE, BYVAL z2 AS SINGLE )

  LOCAL ty AS SINGLE
  LOCAL tz AS SINGLE
  ty = y2-y
  tz = z2-z

  angle = DegToRad(Angle)
  y = ty*COS(Angle)-tz*SIN(Angle) + y2
  z = ty*SIN(Angle)+tz*COS(Angle) + z2

END SUB

' -- Recursive function to set rotate bones transform
SUB internal_m15RotBone( BYVAL Model AS LONG, BYVAL Bone AS LONG, BYVAL nanglex AS SINGLE, BYVAL nangley AS SINGLE, BYVAL nanglez AS SINGLE, BYVAL sourceBone AS LONG)
  REGISTER i AS LONG

  LOCAL chBone AS LONG, chanindex AS LONG, chorindex AS LONG, parorindex AS LONG
  LOCAL x AS SINGLE, y AS SINGLE, z AS SINGLE
  LOCAL x2 AS SINGLE, y2 AS SINGLE, z2 AS SINGLE

  LOCAL tIndex AS LONG ' transformaci

  IF BoneBox(Model,Bone).transformsCount = %MAXBONES THEN tbglError "Number of bone transformations for bone"+STR$(Bone)+" exceeds"+STR$(%MAXBONES) : EXIT SUB
  INCR BoneBox(Model,Bone).transformsCount
  tIndex = BoneBox(Model,Bone).transformsCount

  BoneBox(Model,Bone).transforms(tIndex).ttype= %TRN_ROT
  BoneBox(Model,Bone).transforms(tIndex).trnX = nanglex
  BoneBox(Model,Bone).transforms(tIndex).trnY = nangley
  BoneBox(Model,Bone).transforms(tIndex).trnZ = nanglez

  BoneBox(Model,Bone).transforms(BoneBox(Model,Bone).transformsCount).AnchorBone  = sourceBone

  IF sourceBone <> Bone THEN
    chorindex   = BoneBox(Model, bone).anchorindex
    parorindex  = BoneBox(Model, sourceBone).anchorindex
    INCR BoneBox(Model, bone).anchorindex
    chanindex   = BoneBox(Model, bone).anchorindex

    x2 = BoneBox(Model,sourceBone).alteranchorx(parorindex)
    y2 = BoneBox(Model,sourceBone).alteranchory(parorindex)
    z2 = BoneBox(Model,sourceBone).alteranchorz(parorindex)

    x =  BoneBox(Model,bone).alteranchorx(chorindex) ' x2 -
    y =  BoneBox(Model,bone).alteranchory(chorindex)
    z =  BoneBox(Model,bone).alteranchorz(chorindex)

    internal_RotateXAroundYZ( y, z, nanglex, y2, z2)
    internal_RotateYAroundXZ( x, z, nangley, x2, z2)
    internal_RotateZAroundXY( x, y, nanglez, x2, y2)

    BoneBox(Model,bone).alteranchorx(chanindex) = x
    BoneBox(Model,bone).alteranchory(chanindex) = y
    BoneBox(Model,bone).alteranchorz(chanindex) = z
  END IF

  BoneBox(Model,Bone).transforms(BoneBox(Model,Bone).transformsCount).WhichAnchorIndex   = BoneBox(Model, SourceBone).anchorindex
  IF BoneTree(Model, Bone).bonesCount THEN
    FOR i = 1 TO BoneTree(Model, Bone).bonesCount
      chBone = BoneTree(Model, Bone).CHILD(i)
      internal_m15RotBone(Model,chBone,nanglex,nangley,nanglez,SourceBone) ' Bone
    NEXT
  END IF

END SUB

' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ' -- [O]
  SUB Exec_TBGL_m15AddBoneTreeItem()
    IF m15ModelNum = 0 THEN tbglError $ERR_MODELBUFFER : EXIT SUB
    LOCAL Model, Bone, Level, ChildLevel AS EXT

    thinBasic_Parse4Numbers( Model, Bone, Level, ChildLevel )
    BoneTree(Model, Bone).awaitingLevel = ChildLevel

    LOCAL i AS LONG
    FOR i = 1 TO %MAXBONES
      IF i = Bone THEN ITERATE FOR
      IF BoneTree(Model, i).awaitingLevel = level THEN
        IF BoneTree(Model, i).bonesCount = %MAXBONES THEN tbglError "Number of child bones for bone"+STR$(i)+" exceeds"+STR$(%MAXBONES) : EXIT SUB
        INCR BoneTree(Model, i).bonesCount
        BoneTree(Model, i).CHILD(BoneTree(Model, i).bonesCount) = Bone
      END IF
    NEXT

  END SUB

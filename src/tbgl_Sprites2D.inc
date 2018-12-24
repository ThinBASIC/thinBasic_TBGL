'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles the 2D sprites
' =============================================================================

  TYPE TBGL_tSprite
    xPos                AS SINGLE
    yPos                AS SINGLE
    zPos                AS SINGLE
    oldxPos             AS SINGLE
    oldyPos             AS SINGLE

    x1                  AS SINGLE
    y1                  AS SINGLE
    x2                  AS SINGLE
    y2                  AS SINGLE
    x3                  AS SINGLE
    y3                  AS SINGLE
    x4                  AS SINGLE
    y4                  AS SINGLE

    x1s                 AS SINGLE
    y1s                 AS SINGLE
    x2s                 AS SINGLE
    y2s                 AS SINGLE
    x3s                 AS SINGLE
    y3s                 AS SINGLE
    x4s                 AS SINGLE
    y4s                 AS SINGLE

    x1c                 AS SINGLE
    y1c                 AS SINGLE
    x2c                 AS SINGLE
    y2c                 AS SINGLE
    x3c                 AS SINGLE
    y3c                 AS SINGLE
    x4c                 AS SINGLE
    y4c                 AS SINGLE

    iHFlip              AS LONG
    iVFlip              AS LONG

    tx1(256)            AS SINGLE
    ty1(256)            AS SINGLE
    tx2(256)            AS SINGLE
    ty2(256)            AS SINGLE

    icurrtexFrame       AS LONG

    iColType            AS LONG
    iColRad             AS SINGLE
    iColWidth           AS SINGLE
    iColHeight          AS SINGLE
    iColGroup           AS LONG
    icolwith(32)        AS LONG
    iColMode            AS LONG

    iScale              AS SINGLE
    iAngle              AS SINGLE
    oldIScale           AS SINGLE
    oldIAngle           AS SINGLE
    textureID           AS LONG

    isActive            AS LONG
    isVisible           AS LONG
    isDeleted           AS LONG

    iShadow             AS LONG
    iShadowAlpha        AS SINGLE
    iShadowSize         AS SINGLE
    iShadowXOff         AS SINGLE
    iShadowYOff         AS SINGLE

    iSpeedX             AS SINGLE
    iSpeedY             AS SINGLE
    iSpeedSpin          AS SINGLE
    iSpeed              AS SINGLE
    iSpeedAngle         AS SINGLE
    iSpeedMax           AS SINGLE
    iSpeedMin           AS SINGLE
    iSpeedSpinMax       AS SINGLE
    iSpeedSpinMin       AS SINGLE
    iFriction           AS SINGLE

    iWidth              AS SINGLE
    iHeight             AS SINGLE
    iWidthBase          AS LONG
    iHeightBase         AS LONG
    iWidthTile          AS LONG
    iHeightTile         AS LONG
    iRadius             AS SINGLE

    'ianimTime           AS SINGLE
    'iAnimSpeed          AS SINGLE

    isAnimated          AS LONG
    iFrameCount         AS LONG
    iFrameCols          AS LONG
    iFrameRows          AS LONG

    iAnimGroup          AS LONG

    iStartFrame(64)     AS LONG
    iEndFrame(64)       AS LONG
    iAnimLength(64)     AS LONG
    ianimTime(64)       AS SINGLE
    iAnimSpeed(64)      AS SINGLE
    ' ppppppppppppppppp
    iAnimType(64)       AS BYTE     ' BYTE just to save memory 64*4 and 64*1 makes difference
    iAnimSpin(64)       AS BYTE     ' BYTE just to save memory 64*4 and 64*1 makes difference
    ' ppppppppppppppppp


    iRed                AS SINGLE
    iGreen              AS SINGLE
    iBlue               AS SINGLE
    iAlpha              AS SINGLE


    'animframe     AS LONG
    currAnimCol         AS LONG
    currAnimRow         AS LONG

    userPointer         AS LONG      ' user pointer to data


    sevOnBeforeUpdate   AS LONG
    sevOnAfterUpdate    AS LONG
    sevOnCollision      AS LONG
    sevOnBeforeDraw     AS LONG
    sevOnAfterDraw      AS LONG
    sevOnBeforeAnimate  AS LONG
    sevOnAfterAnimate   AS LONG
    sevOnDelete         AS LONG
    sevOnMouseOver      AS LONG

    iGroup              AS LONG
    iParent             AS LONG
    iParentMode         AS LONG
    ichildCount         AS LONG
    iTag                AS LONG

  END TYPE

  ' -- Event ID's
  %TBGL_OnBeforeUpdate  = 1
  %TBGL_OnAfterUpdate   = 2
  %TBGL_OnCollision     = 3
  %TBGL_OnBeforeDraw    = 4
  %TBGL_OnAfterDraw     = 5
  %TBGL_OnBeforeAnimate = 6
  %TBGL_OnAfterAnimate  = 7
  %TBGL_OnDelete        = 8
  %TBGL_OnMouseOver     = 9

  ' -- Parent modes
  %TBGL_Pos             = 1
  %TBGL_Rotate          = 2
  %TBGL_Scale           = 4
  %TBGL_Delete          = 8
  %TBGL_Speed           = 16
  %TBGL_Spin            = 32
  %TBGL_Orbit           = 64
  %TBGL_ChildRel        = 256

  ' -- Collision modes
  %sctRect     = 1
  %sctCircle   = 2
  %sctBoundBox = 3

  %TBGL_Rectangle         = %sctRect
  %TBGL_Circle            = %sctCircle

  ' -- Animation types
  ' ppppppppppppppppp
  %TBGL_LOOP      = 0
  %TBGL_INTERVAL  = 1
  %TBGL_BOUNCE    = 2

  %TBGL_FORWARD   = 0
  %TBGL_BACKWARD  = 1
  ' ppppppppppppppppp

  GLOBAL spriteTexNum AS LONG
  GLOBAL spritecount AS LONG
  GLOBAL sprites() AS DWORD
  GLOBAL maxsprites AS LONG
  GLOBAL aSprite() AS TBGL_tSprite PTR


  GLOBAL callEventLevel AS LONG
  GLOBAL spritesDeleted AS LONG

  GLOBAL meSprite  AS TBGL_tSprite PTR
  GLOBAL youSprite AS TBGL_tSprite PTR

  ' -- Entity data storage
  GLOBAL SpriteUserData() AS STRING

   TYPE tPointS
    x AS SINGLE
    y AS SINGLE
  END TYPE

  TYPE tLine2D
    p1  AS tPoints
    p2  AS tPoints
  END TYPE


  MACRO ReturnAndExit( value ) = internal_CallSpriteEvent(pSpriteA, pSpriteB, %TBGL_OnCollision) :FUNCTION = value : EXIT FUNCTION
  MACRO Mouse_ReturnAndExit( value ) = internal_CallSpriteEvent(pSpriteA, ibutton, %TBGL_OnMouseOver) :FUNCTION = value : EXIT FUNCTION


  '*****************************************************************
  FUNCTION internal_MemFree(BYREF pMem AS DWORD) AS LONG
  '*****************************************************************
    IF pMem THEN
      FUNCTION = HeapFree( GetProcessHeap(), 0, BYVAL pMem)
      pMem = 0
    END IF
  END FUNCTION


  '*****************************************************************
  FUNCTION internal_MemAlloc(BYVAL nSize AS LONG) AS DWORD
  '*****************************************************************
    IF nSize > 0 THEN
      FUNCTION = HeapAlloc( GetProcessHeap(), %HEAP_ZERO_MEMORY, nSize)
    END IF
  END FUNCTION




  '*****************************************************************
  SUB internal_CallSpriteEvent(BYVAL pSprite AS TBGL_tSprite PTR, BYVAL pSprite2 AS TBGL_tSprite PTR, eventID AS LONG)
  '*****************************************************************
    LOCAL meSpriteOld  AS TBGL_tSprite PTR
    LOCAL youSpriteOld AS TBGL_tSprite PTR
    LOCAL retExt AS EXT

    meSpriteOld = meSprite
    youSpriteOld = youSprite
    meSprite = pSprite
    youSprite = pSprite2
    callEventLevel += 1
    SELECT CASE AS LONG eventID
        CASE %TBGL_OnBeforeUpdate
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnBeforeUpdate, VARPTR (retExt) )
        CASE %TBGL_OnAfterUpdate
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnAfterUpdate, VARPTR (retExt) )
        CASE %TBGL_OnBeforeDraw
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnBeforeDraw, VARPTR (retExt) )
        CASE %TBGL_OnAfterDraw
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnAfterDraw, VARPTR (retExt) )
        CASE %TBGL_OnCollision
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnCollision, VARPTR (retExt) )
        CASE %TBGL_OnBeforeAnimate
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnBeforeAnimate, VARPTR (retExt) )
        CASE %TBGL_OnAfterAnimate
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnAfterAnimate, VARPTR (retExt) )
        CASE %TBGL_OnDelete
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnDelete, VARPTR (retExt) )
        CASE %TBGL_OnMouseOver
            thinBasic_FunctionSimpleCall_ByPtr(@pSprite.sevOnMouseOver, VARPTR (retExt) )
    END SELECT
    callEventLevel -= 1
    meSprite = meSpriteOld
    youSprite = youSpriteOld

  END SUB


  '*****************************************************************
  SUB internal_RotateSprite(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    LOCAL x,y AS SINGLE
    LOCAL SinVal, CosVal, xSinVal, ySinVal, xCosVal, yCosVal, angle AS SINGLE

    angle = @pSprite.iAngle * 0.0174533
    SinVal = SIN(angle)
    CosVal=COS(angle)

    x = -@pSprite.iwidth/2
    y = @pSprite.iheight/2

    xCosVal = x * CosVal
    yCosVal = y * CosVal
    xSinVal = x * SinVal
    ySinVal = y * SinVal

    @pSprite.x1=(xCosVal)-(ySinVal)
    @pSprite.y1=(yCosVal)+(xSinVal)

    'x = x * -1
    @pSprite.x2=(-xCosVal)-( ySinVal)
    @pSprite.y2=( yCosVal)+(-xSinVal)

    'y = y * -1
    @pSprite.x3=(-xCosVal)-(-ySinVal)
    @pSprite.y3=(-yCosVal)+(-xSinVal)

    'x = x * -1
    @pSprite.x4=( xCosVal)-(-ySinVal)
    @pSprite.y4=(-yCosVal)+( xSinVal)

    IF @pSprite.iShadow THEN
        x = -@pSprite.iwidth/2 * @pSprite.iShadowSize
        y = @pSprite.iheight/2 * @pSprite.iShadowSize

        xCosVal = x * CosVal
        yCosVal = y * CosVal
        xSinVal = x * SinVal
        ySinVal = y * SinVal

        @pSprite.x1s=(xCosVal)-(ySinVal)
        @pSprite.y1s=(yCosVal)+(xSinVal)

        'x = x * -1

        @pSprite.x2s=(-xCosVal)-( ySinVal)
        @pSprite.y2s=( yCosVal)+(-xSinVal)

        'y = y * -1
        @pSprite.x3s=(-xCosVal)-(-ySinVal)
        @pSprite.y3s=(-yCosVal)+(-xSinVal)

        'x = x * -1
        @pSprite.x4s=( xCosVal)-(-ySinVal)
        @pSprite.y4s=(-yCosVal)+( xSinVal)
    END IF

    y = @pSprite.icolHeight
    x = -@pSprite.icolWidth

    xCosVal = x * CosVal
    yCosVal = y * CosVal
    xSinVal = x * SinVal
    ySinVal = y * SinVal

    @pSprite.x1c=(xCosVal)-(ySinVal)
    @pSprite.y1c=(yCosVal)+(xSinVal)

    'x = x * -1
    @pSprite.x2c=(-xCosVal)-( ySinVal)
    @pSprite.y2c=( yCosVal)+(-xSinVal)

    'y = y * -1
    @pSprite.x3c=(-xCosVal)-(-ySinVal)
    @pSprite.y3c=(-yCosVal)+(-xSinVal)

    'x = x * -1
    @pSprite.x4c=( xCosVal)-(-ySinVal)
    @pSprite.y4c=(-yCosVal)+( xSinVal)
'END IF

  END SUB




  '*****************************************************************
  SUB internal_AnimateSprite(BYVAL pSprite AS TBGL_tSprite PTR, SPEED AS SINGLE)
  '*****************************************************************

    IF @pSprite.sevOnBeforeAnimate THEN
        internal_CallSpriteEvent(pSprite,0,%TBGL_OnBeforeAnimate)
    END IF

    IF @pSprite.iAnimType(@pSprite.iAnimGroup) = %TBGL_BOUNCE AND @pSprite.iAnimSpin(@pSprite.iAnimGroup) = %TBGL_BACKWARD THEN SPEED *= -1
    @pSprite.ianimtime(@pSprite.iAnimGroup) += @pSprite.iAnimSpeed(@pSprite.iAnimGroup) * SPEED


    ' -- If we go over animation length
    IF @pSprite.ianimtime(@pSprite.iAnimGroup) >= (@pSprite.iEndFrame(@pSprite.iAnimGroup)+1) THEN
      SELECT CASE AS LONG @pSprite.iAnimType(@pSprite.iAnimGroup)
        CASE %TBGL_LOOP
          @pSprite.ianimtime(@pSprite.iAnimGroup) -= (@pSprite.iAnimLength(@pSprite.iAnimGroup))

        CASE %TBGL_INTERVAL
          @pSprite.ianimtime(@pSprite.iAnimGroup) = (@pSprite.iEndFrame(@pSprite.iAnimGroup)+0.99)

        CASE %TBGL_BOUNCE
          @pSprite.ianimtime(@pSprite.iAnimGroup) = 2*(@pSprite.iEndFrame(@pSprite.iAnimGroup)+1) - @pSprite.ianimtime(@pSprite.iAnimGroup) - 1
          @pSprite.ianimspin(@pSprite.iAnimGroup) XOR= 1'%TBGL_BACKWARD

      END SELECT
    END IF

    ' -- If we go under animation begining
    IF @pSprite.ianimtime(@pSprite.iAnimGroup) < (@pSprite.iStartFrame(@pSprite.iAnimGroup)) THEN
      SELECT CASE AS LONG @pSprite.iAnimType(@pSprite.iAnimGroup)
        CASE %TBGL_LOOP
          @pSprite.ianimtime(@pSprite.iAnimGroup) += (@pSprite.iAnimLength(@pSprite.iAnimGroup))

        CASE %TBGL_INTERVAL
          @pSprite.ianimtime(@pSprite.iAnimGroup) = (@pSprite.iStartFrame(@pSprite.iAnimGroup))

        CASE %TBGL_BOUNCE
          @pSprite.ianimtime(@pSprite.iAnimGroup) = 2*(@pSprite.iStartFrame(@pSprite.iAnimGroup)) - @pSprite.ianimtime(@pSprite.iAnimGroup) + 1
          @pSprite.ianimspin(@pSprite.iAnimGroup) XOR= 1' %TBGL_FORWARD

      END SELECT
    END IF

    IF @pSprite.sevOnAfterAnimate THEN
        internal_CallSpriteEvent(pSprite,0,%TBGL_OnAfterAnimate)
    END IF

  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetSpeedAngle() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.iSpeedAngle
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetSpeed() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.iSpeed
  END FUNCTION


  '*****************************************************************
  SUB internal_UpdateSprite(BYVAL pSprite AS TBGL_tSprite PTR, SPEED AS SINGLE)
  '*****************************************************************
    LOCAL absSpeedSpin,currFriction,currspeed, speedangle AS SINGLE

    IF @pSprite.sevOnBeforeUpdate THEN
        internal_CallSpriteEvent(pSprite,0,%TBGL_OnBeforeUpdate)
    END IF

    internal_SpriteSetOldAngle(pSprite)
    @pSprite.iAngle  += @pSprite.iSpeedSpin * SPEED

    IF @pSprite.iAngle < 0.0 THEN
        @pSprite.iAngle = @pSprite.iAngle MOD 360.0 + 360
    ELSE
        IF @pSprite.iAngle > 360.0 THEN
            @pSprite.iAngle = @pSprite.iAngle MOD 360.0
        END IF
    END IF
    internal_RotateSprite(pSprite)


    speedAngle = @pSprite.iSpeedAngle * 0.0174532925
    currSpeed = @pSprite.iSpeed
    currfriction = @pSprite.iFriction * SPEED
    currspeed -= currfriction
    IF currspeed < currfriction THEN currspeed = 0

    @pSprite.iSpeed  = currSpeed
    @pSprite.iSpeedX = SIN(speedangle)*currspeed
    @pSprite.iSpeedY = -COS(speedangle)*currspeed
    internal_SpriteSetOldPos(pSprite)
    @pSprite.xPos    += @pSprite.iSpeedX * SPEED
    @pSprite.YPos    += @pSprite.iSpeedY * SPEED


    'Handle spinning
    IF @pSprite.iSpeedSpin  <> 0 THEN
        absSpeedSpin = @pSprite.iSpeedSpin
        IF absSpeedSpin < 0 THEN absSpeedSpin *= -1
        IF absSpeedSpin < currFriction THEN
            @pSprite.iSpeedSpin = 0.0
        ELSE
           IF @pSprite.iSpeedSpin > 0 THEN
                @pSprite.iSpeedSpin -= currFriction
           ELSE
                @pSprite.iSpeedSpin += currFriction
           END IF
        END IF

    END IF


    IF @pSprite.isAnimated = %TRUE THEN internal_AnimateSprite(pSprite, SPEED)

    IF @pSprite.sevOnAfterUpdate THEN
        internal_CallSpriteEvent(pSprite,0,%TBGL_OnAfterUpdate)
    END IF

  END SUB


  '*****************************************************************
  SUB internal_SizeSprite(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    @pSprite.iWidth  = @pSprite.iWidthBase * @pSprite.iScale
    @pSprite.iHeight = @pSprite.iHeightBase * @pSprite.iScale
    @pSprite.iColRad = @pSprite.iRadius * @pSprite.iScale
    @pSprite.iColWidth = @pSprite.iWidth/2
    @pSprite.iColHeight = @pSprite.iHeight/2
    internal_RotateSprite(pSprite)
  END SUB

  '*****************************************************************
  SUB internal_SpriteSetOldPos(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    @pSprite.oldXPos = @pSprite.xPos
    @pSprite.oldYPos = @pSprite.yPos
  END SUB

  '*****************************************************************
  SUB internal_SpriteSetOldScale(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    @pSprite.oldIScale = @pSprite.iScale
  END SUB


  '*****************************************************************
  SUB internal_SpriteSetOldAngle(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    @pSprite.oldIAngle = @pSprite.iAngle
  END SUB


  '*****************************************************************
  MACRO internal_Vec2DotProduct( a, b ) = (a.x * b.x + a.y * b.y )        ' -- Macro has less overhead
  '*****************************************************************


  '*****************************************************************
  MACRO internal_Between(v, l, h) = (v >= l AND v <= h)     ' -- Macro has less overhead

  '*****************************************************************


  '*****************************************************************
  FUNCTION internal_SpriteCollided_CrossDistance(l1 AS tLine2D, l2 AS tLine2D, safetyZone AS DOUBLE) AS DOUBLE
  '*****************************************************************
      '  returns distance between p1 and crossing of two lines, -1 when no cross found
      LOCAL result AS DOUBLE  '  cross not found
      LOCAL q2 AS DOUBLE
      LOCAL t2 AS DOUBLE
      LOCAL p1start, p1end, p2start, p2end AS tPointS
      LOCAL t AS DOUBLE
      LOCAL k1, k2 AS DOUBLE
      LOCAL xp, yp AS DOUBLE
      LOCAL minx1 AS DOUBLE
      LOCAL maxx1 AS DOUBLE
      LOCAL miny1 AS DOUBLE
      LOCAL maxy1 AS DOUBLE
      LOCAL minx2 AS DOUBLE
      LOCAL maxx2 AS DOUBLE
      LOCAL miny2 AS DOUBLE
      LOCAL maxy2 AS DOUBLE
      LOCAL tmp AS DOUBLE
      LOCAL q1 AS DOUBLE

      result = -1
      p1start.x = l1.p1.x
      p1start.y = l1.p1.y

      p1end.x = l1.p2.x
      p1end.y = l1.p2.y

      p2start.x = l2.p1.x
      p2start.y = l2.p1.y

      p2end.x = l2.p2.x
      p2end.y = l2.p2.y

      t = p1end.X - p1start.X

      k1 = IIF(t, (p1end.Y - p1start.Y) / t, 100000.0     )

      q1 = p1start.Y - (k1 * p1start.X)

      t = p2end.X - p2start.X

      k2 = IIF(t, (p2end.Y - p2start.Y) / t, 100000.0     )

      q2 = p2start.Y - (k2 * p2start.X)
      t2 = k2 - k1


      IF ABS(t2) < 0.0001 THEN
        yp = 100000.0
        xp = 100000.0
      ELSE
        yp = (q1*k2-q2*k1) / t2
        xp = (q1-q2) / t2
      END IF

      '  special cases
      IF (p1end.X - p1start.X) = 0 THEN
        xp = p1start.X
        yp = k2*xp+q2
      END IF

      IF (p2end.X - p2start.X) = 0 THEN
        xp = p2start.X
        yp = k1*xp+q1
      END IF
      '  end of special cases

      minx1 = MIN(p1start.X, p1end.X)
      maxx1 = MAX(p1start.X, p1end.X)
      miny1 = MIN(p1start.Y, p1end.Y)
      maxy1 = MAX(p1start.Y, p1end.Y)
      minx2 = MIN(p2start.X, p2end.X)
      maxx2 = MAX(p2start.X, p2end.X)
      miny2 = MIN(p2start.Y, p2end.Y)
      maxy2 = MAX(p2start.Y, p2end.Y)


      IF  xp + safetyZone < minx1  OR _
          xp - safetyZone > maxx1  OR _
          yp + safetyZone < miny1  OR _
          yp - safetyZone > maxy1  OR _
          xp + safetyZone < minx2  OR _
          xp - safetyZone > maxx2  OR _
          yp + safetyZone < miny2  OR _
          yp - safetyZone > maxy2  THEN

          result = -1

      ELSE
          tmp = ((xp-p1start.X)^2)
          tmp = tmp + ((yp-p1start.Y)^2)
          tmp = SQR(tmp)
          result = tmp
      END IF

      FUNCTION = result

  END FUNCTION






  '*****************************************************************
  FUNCTION internal_SpriteCollided_IsIn(BYREF p AS tpointS, BYREF a AS tpointS, BYREF b AS tpointS, BYREF d AS tpointS) AS LONG
  '*****************************************************************
      LOCAL v, v1, v2 AS tpointS

      'o = a

      'v.X = p.X - o.X
      'v.Y = p.Y - o.Y

      v.X = p.X - a.X
      v.Y = p.Y - a.Y

      v1.X = b.X - a.X
      v1.Y = b.Y - a.Y

      v2.X = d.X - a.X
      v2.Y = d.Y - a.Y


      IF internal_between(internal_Vec2DotProduct(v, v1), 0, internal_Vec2DotProduct(v1, v1)) AND internal_between(internal_Vec2DotProduct(v, v2), 0, internal_Vec2DotProduct(v2, v2)) THEN
        FUNCTION = %true
      ELSE
        FUNCTION = %false
      END IF
  END FUNCTION



  '*****************************************************************
  SUB internal_StoreSprite(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    LOCAL i AS LONG

    IF maxsprites = 0 THEN
        maxsprites = 256
        DIM sprites(maxsprites) AS GLOBAL DWORD
        DIM SpriteUserData(maxsprites) AS GLOBAL STRING
    END IF

    IF spritecount >= maxsprites THEN
        maxsprites += 256
        REDIM PRESERVE sprites(maxsprites) AS GLOBAL DWORD
        REDIM PRESERVE SpriteUserData(maxsprites) AS GLOBAL STRING
    END IF

    spritecount += 1
    FOR i = 1 TO maxsprites
        IF sprites(i) = 0 THEN
            sprites(i) = pSprite
            @pSprite.userpointer = i
            EXIT FOR
        END IF
    NEXT
  END SUB



  '*****************************************************************
  SUB internal_DeleteAllSprites()
  '*****************************************************************
    LOCAL i AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSprite = sprites(i)
            spritecount -= 1
            SpriteUserData(@pSprite.userpointer) = ""
            internal_MemFree(pSprite)
            sprites(i) = 0
        END IF
    NEXT
    spritesDeleted = 0
  END SUB




  '*****************************************************************
  SUB internal_DeleteSprites()
  '*****************************************************************
    LOCAL i AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR

    IF spritesDeleted = 0 THEN EXIT SUB
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSprite = sprites(i)
            IF @pSprite.isDeleted  THEN
                sprites(i) = 0
                spritecount -= 1
                SpriteUserData(@pSprite.userpointer) = ""
                internal_MemFree(pSprite)
            END IF
        END IF
    NEXT
    spritesDeleted = 0
  END SUB


  '*****************************************************************
  SUB internal_RemoveSprite(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    LOCAL i AS LONG

    FOR i = 1 TO maxsprites
        IF sprites(i) = pSprite THEN
            sprites(i) = 0
            spritecount -= 1
            EXIT FOR
        END IF
    NEXT
    SpriteUserData(@pSprite.userpointer) = ""

  END SUB


  '*****************************************************************
  SUB internal_InitSprite(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************

    @pSprite.xPos = 0
    @pSprite.yPos = 0
    @pSprite.zPos = 1.0/256.0
    @pSprite.oldxPos = 0
    @pSprite.oldyPos = 0

    @pSprite.iHFlip = %FALSE
    @pSprite.iVFlip = %FALSE

    @pSprite.iScale = 1.0
    @pSprite.oldIScale = 1.0
    @pSprite.iAngle = 0.0
    @pSprite.oldIAngle = 0.0

    @pSprite.isActive = %TRUE
    @pSprite.isVisible = %TRUE
    @pSprite.isanimated = %FALSE
    @pSprite.isDeleted = %FALSE
    @pSprite.iShadow   = %FALSE

    @pSprite.iRed        = 1.0
    @pSprite.iGreen      = 1.0
    @pSprite.iBlue       = 1.0
    @pSprite.iAlpha      = 1.0


    @pSprite.iSpeedX     = 0.0
    @pSprite.iSpeedY     = 0.0
    @pSprite.iSpeedSpin  = 0.0
    @pSprite.iSpeed      = 0.0
    @pSprite.iSpeedAngle = 0.0
    @pSprite.iFriction   = 0.0

    @pSprite.iAnimSpeed(1) = 0.0
    @pSprite.iAnimLength(1) = 0
    @pSprite.iAnimGroup = 0

    @pSprite.icurrtexFrame = 1

    @pSprite.tx1(1) = 0.000
    @pSprite.ty1(1) = 0.000

    @pSprite.tx2(1) = 1.0
    @pSprite.ty2(1) = 1.0

    @pSprite.sevOnBeforeUpdate  = 0
    @pSprite.sevOnAfterUpdate   = 0
    @pSprite.sevOnCollision     = 0
    @pSprite.sevOnBeforeDraw    = 0
    @pSprite.sevOnAfterDraw     = 0
    @pSprite.sevOnBeforeAnimate = 0
    @pSprite.sevOnAfterAnimate  = 0
    @pSprite.sevOnDelete        = 0
    @pSprite.sevOnMouseOver    = 0

    @pSprite.icurrtexFrame = 1

    @pSprite.iTag = 0
    @pSprite.iGroup = 0
    @pSprite.iParent = 0
    @pSprite.iParentMode = %TBGL_ROTATE OR %TBGL_POS OR %TBGL_SCALE OR %TBGL_DELETE OR %TBGL_SPEED OR %TBGL_SPIN OR %TBGL_ORBIT OR %TBGL_ChildRel
    @pSprite.ichildCount = 0

  END SUB


  '*****************************************************************
  SUB internal_InitSpriteSize(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    LOCAL i AS LONG

    @pSprite.x1 = -@pSprite.iWidth/2
    @pSprite.y1 = @pSprite.iHeight/2

    @pSprite.x2 = @pSprite.iWidth/2
    @pSprite.y2 = @pSprite.iHeight/2

    @pSprite.x3 = @pSprite.iWidth/2
    @pSprite.y3 = -@pSprite.iHeight/2

    @pSprite.x4 = -@pSprite.iWidth/2
    @pSprite.y4 = -@pSprite.iHeight/2


    @pSprite.iColType = %sctRect
    @pSprite.iColGroup = 1
    FOR i = 1 TO 32
        @pSprite.iColWith(i) = %TRUE
    NEXT
    @pSprite.iRadius = MAX(@pSprite.iWidth,@pSprite.iHeight)/2
    @pSprite.iColRad = @pSprite.iRadius
    @pSprite.iColWidth = @pSprite.iWidth/2
    @pSprite.iColHeight = @pSprite.iHeight/2
    @pSprite.iColMode = %FALSE

  END SUB


  '*****************************************************************
  SUB internal_InitSpriteAnim(BYVAL pSprite AS TBGL_tSprite PTR, cw AS LONG, ch AS LONG)
  '*****************************************************************
    LOCAL curranimrow AS LONG, curranimcol AS LONG
    LOCAL i AS LONG

    @pSprite.iframecols = @pSprite.iWidthtile/cw
    @pSprite.iframerows = @pSprite.iHeightTile/ch
    @pSprite.iframecount = @pSprite.iframecols * @pSprite.iframerows

    @pSprite.iAnimGroup = 1
    @pSprite.iStartFrame(@pSprite.iAnimGroup) = 1
    @pSprite.iEndFrame(@pSprite.iAnimGroup) = @pSprite.iframecount
    @pSprite.iAnimLength(@pSprite.iAnimGroup) = @pSprite.iEndFrame(@pSprite.iAnimGroup) - @pSprite.iStartFrame(@pSprite.iAnimGroup) + 1
    @pSprite.ianimtime(@pSprite.iAnimGroup) = 1.0
    @pSprite.iAnimSpeed(@pSprite.iAnimGroup) = 1.0

    FOR curranimrow = 1 TO @pSprite.iFramerows
        FOR curranimcol = 1 TO @pSprite.iFrameCols
        i += 1
            @pSprite.tx1(i) = (1.0/@pSprite.iframecols)*(curranimcol - 1)
            @pSprite.tx2(i) = (1.0/@pSprite.iframecols)*(curranimcol)
            @pSprite.ty1(i) = (1.0/@pSprite.iframerows)*(@pSprite.iframerows - curranimrow)
            @pSprite.ty2(i) = (1.0/@pSprite.iframerows)*(@pSprite.iframerows - curranimrow+1)
        NEXT
    NEXT



  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteLoad() AS EXT
  '*****************************************************************

    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL eRGB AS EXT
    LOCAL lRGB AS LONG
    LOCAL texname AS STRING

    IF thinBasic_CheckOpenParens_Mandatory THEN
        thinBasic_ParseString TexName
        IF thinBasic_CheckCloseParens_Optional THEN
            lRGB = - 1
        ELSE
            IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                thinBasic_ParseNumber eRGB
                lRGB = eRGB
            END IF
            thinBasic_CheckCloseParens_Mandatory
        END IF
    END IF

    pSprite = internal_Memalloc(SIZEOF(TBGL_tSprite))

    internal_InitSprite(pSprite)

    @pSprite.textureID = meIsTextureLoaded( UCASE$(TRIM$(texname) ) )

    IF @pSprite.textureID = 0 THEN
        spriteTexNum += 1
        @pSprite.textureID = 1024 - spriteTexNum
        internal_LoadTexture(TRIM$(texname), @pSprite.textureID, %TBGL_TEX_NEAREST, 1, lRGB )
        glBindTexture( %GL_TEXTURE_2D, lpTexture( @pSprite.textureID ))

        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, @pSprite.iWidthBase )
        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, @pSprite.iHeightBase )
    ELSE
        glBindTexture( %GL_TEXTURE_2D, lpTexture( @pSprite.textureID ))

        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, @pSprite.iWidthBase )
        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, @pSprite.iHeightBase )
    END IF

    glTexParameterI(%gl_texture_2d, %gl_texture_wrap_s, %gl_clamp_to_edge)
    glTexParameterI(%gl_texture_2d, %gl_texture_wrap_t, %gl_clamp_to_edge)


    @pSprite.iWidthTile = @pSprite.iWidthBase
    @pSprite.iHeightTile = @pSprite.iHeightBase

    @pSprite.iWidth = @pSprite.iWidthBase
    @pSprite.iHeight = @pSprite.iHeightBase

    internal_InitSpriteSize(pSprite)

    internal_StoreSprite(pSprite)

    FUNCTION = pSprite
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteCreate() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL etex AS EXT

    etex = thinBasic_Parse1Number
    pSprite = internal_Memalloc(SIZEOF(TBGL_tSprite))

    internal_InitSprite(pSprite)

    @pSprite.textureID = etex
    glBindTexture( %GL_TEXTURE_2D, lpTexture( @pSprite.textureID ))

    GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, @pSprite.iWidthBase )
    GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, @pSprite.iHeightBase )

    glTexParameterI(%gl_texture_2d, %gl_texture_wrap_s, %gl_clamp_to_edge)
    glTexParameterI(%gl_texture_2d, %gl_texture_wrap_t, %gl_clamp_to_edge)

    @pSprite.iWidthTile = @pSprite.iWidthBase
    @pSprite.iHeightTile = @pSprite.iHeightBase

    @pSprite.iWidth = @pSprite.iWidthBase
    @pSprite.iHeight = @pSprite.iHeightBase

    internal_InitSpriteSize(pSprite)

    internal_StoreSprite(pSprite)

    FUNCTION = pSprite
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteCreateAnim() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL etex, ech, ecw AS EXT
    LOCAL cw, ch AS LONG

    thinBasic_Parse3Numbers(etex, ecw, ech)
    pSprite = internal_Memalloc(SIZEOF(TBGL_tSprite))
    ch = ech
    cw = ecw

    internal_InitSprite(pSprite)

    @pSprite.textureID = etex
    @pSprite.isanimated = %TRUE


    glBindTexture( %GL_TEXTURE_2D, lpTexture( @pSprite.textureID ))

    GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, @pSprite.iWidthTile )
    GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, @pSprite.iHeightTile )

    glTexParameterI(%gl_texture_2d, %gl_texture_wrap_s, %gl_clamp_to_edge)
    glTexParameterI(%gl_texture_2d, %gl_texture_wrap_t, %gl_clamp_to_edge)

    @pSprite.iWidthBase = cw
    @pSprite.iHeightBase = ch

    @pSprite.iWidth = cw
    @pSprite.iHeight = ch


    internal_InitSpriteAnim(pSprite, cw, ch)

    internal_InitSpriteSize(pSprite)

    internal_StoreSprite(pSprite)

    FUNCTION = pSprite
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteLoadAnim() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL eRGB, ech, ecw AS EXT
    LOCAL lRGB, cw, ch AS LONG
    LOCAL texname AS STRING

    nParsed = thinBASIC_Parse1StringXNumbers( 2, 3, TexName, ecw, ech, eRGB )
    lRGB = eRGB
    IF nParsed <> 3 THEN lRGB = - 1
    pSprite = internal_Memalloc(SIZEOF(TBGL_tSprite))
    ch = ech
    cw = ecw

    internal_InitSprite(pSprite)

    @pSprite.textureID = meIsTextureLoaded( UCASE$(TRIM$(texname) ) )
    @pSprite.isanimated = %TRUE


    IF @pSprite.textureID = 0 THEN
        spriteTexNum += 1
        @pSprite.textureID = 1024 - spriteTexNum
        internal_LoadTexture(TRIM$(texname), @pSprite.textureID, %TBGL_TEX_NEAREST, 1, lRGB )
        glBindTexture( %GL_TEXTURE_2D, lpTexture( @pSprite.textureID ))

        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, @pSprite.iWidthTile )
        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, @pSprite.iHeightTile )
    ELSE
        glBindTexture( %GL_TEXTURE_2D, lpTexture( @pSprite.textureID ))

        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, @pSprite.iWidthTile )
        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, @pSprite.iHeightTile )
    END IF
    glTexParameterI(%gl_texture_2d, %gl_texture_wrap_s, %gl_clamp_to_edge)
    glTexParameterI(%gl_texture_2d, %gl_texture_wrap_t, %gl_clamp_to_edge)

    @pSprite.iWidthBase = cw
    @pSprite.iHeightBase = ch

    @pSprite.iWidth = cw
    @pSprite.iHeight = ch

    internal_InitSpriteAnim(pSprite,cw,ch)

    internal_InitSpriteSize(pSprite)

    internal_StoreSprite(pSprite)

    FUNCTION = pSprite
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteLoadSheet() AS EXT
  '*****************************************************************
    LOCAL eRGB, ech, ecw AS EXT
    LOCAL lRGB, cw, ch AS LONG
    LOCAL texname AS STRING
    LOCAL textureID,WidthTile,HeightTile AS LONG
    LOCAL framecount,framecols,framerows AS LONG
    LOCAL i AS LONG

    LOCAL spcount AS LONG
    LOCAL currrow AS LONG, currcol AS LONG
    LOCAL pVar AS LONG


    spcount = 0
    IF thinBasic_CheckOpenParens_Mandatory THEN
        thinBasic_ParseString TexName
        IF thinBasic_CheckComma_Mandatory THEN
            thinBAsic_ParseNumber ecw
            IF thinBasic_CheckComma_Mandatory THEN
                thinBAsic_ParseNumber ech
                IF thinBasic_CheckComma_Mandatory THEN
                    'thinBasic_VariableParse(lArrayPtr, lArrayPos)
                    thinBasic_VariableParsePtr(pVar)
                    IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
                    IF thinBasic_CheckComma_Optional THEN
                        thinBAsic_ParseNumber eRGB
                        lRGB = eRGB
                     ELSE
                        lRGB = -1
                    END IF
                    IF thinBasic_CheckCloseParens_Mandatory AND thinBasic_ErrorFree THEN

                        ch = ech
                        cw = ecw

                        textureID = meIsTextureLoaded( UCASE$(TRIM$(texname) ) )
                        IF textureID = 0 THEN
                            spriteTexNum += 1
                            textureID = 1024 - spriteTexNum
                            internal_LoadTexture(TRIM$(texname), textureID, %TBGL_TEX_NEAREST, 1, lRGB )
                        END IF
                        glBindTexture( %GL_TEXTURE_2D, lpTexture( textureID ))
                        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH,  WidthTile )
                        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, HeightTile )


                        framecols = Widthtile/cw
                        framerows = HeightTile/ch
                        framecount = framecols * framerows

                        spCount = framecount
                        DIM aSprite(spCount) AS GLOBAL TBGL_tSprite PTR


                        FOR i = 1 TO spCount
                            aSprite(i) = internal_Memalloc(SIZEOF(TBGL_tSprite))
                            internal_InitSprite(aSprite(i))

                            @aSprite(i).textureID = textureID

                            @aSprite(i).iWidthTile = WidthTile
                            @aSprite(i).iHeightTile = HeightTile
                            @aSprite(i).iWidthBase = cw
                            @aSprite(i).iHeightBase = ch

                            @aSprite(i).iWidth = cw
                            @aSprite(i).iHeight = ch
                            internal_InitSpriteSize(aSprite(i))

                            internal_StoreSprite(aSprite(i))
                        NEXT
                        i = 0
                        FOR currrow = 1 TO Framerows
                            FOR currcol = 1 TO FrameCols
                                i += 1
                                @aSprite(i).tx1(1) = (1.0/FrameCols)*(currcol - 1)
                                @aSprite(i).tx2(1) = (1.0/FrameCols)*(currcol)
                                @aSprite(i).ty1(1) = (1.0/Framerows)*(Framerows - currrow)
                                @aSprite(i).ty2(1) = (1.0/Framerows)*(Framerows - currrow+1)
                            NEXT
                        NEXT

                        glTexParameterI(%gl_texture_2d, %gl_texture_wrap_s, %gl_clamp_to_edge)
                        glTexParameterI(%gl_texture_2d, %gl_texture_wrap_t, %gl_clamp_to_edge)

                        thinBasic_VariableRedim(pVar, %FALSE, spCount, 0, 0)
                        FOR i = 1 TO spCount
                            thinBasic_ChangeVariableNumberDirect (pVar, i, asprite(i))
                        NEXT
                    END IF
                END IF
            END IF
        END IF
    END IF
    FUNCTION = spcount
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteCreateSheet() AS EXT
  '*****************************************************************
    LOCAL etex, eRGB, ech, ecw AS EXT
    LOCAL lRGB, cw, ch AS LONG
    LOCAL textureID,WidthTile,HeightTile AS LONG
    LOCAL framecount,framecols,framerows AS LONG
    LOCAL i AS LONG

    LOCAL spcount AS LONG
    LOCAL currrow AS LONG, currcol AS LONG
    LOCAL pVar AS LONG


    spcount = 0
    IF thinBasic_CheckOpenParens_Mandatory THEN
        thinBasic_ParseNumber etex
        IF thinBasic_CheckComma_Mandatory THEN
            thinBAsic_ParseNumber ecw
            IF thinBasic_CheckComma_Mandatory THEN
                thinBAsic_ParseNumber ech
                IF thinBasic_CheckComma_Mandatory THEN
                    thinBasic_VariableParsePtr(pVar)
                    IF thinBasic_CheckOpenParens_Optional THEN thinBasic_CheckCloseParens_Mandatory
                    IF thinBasic_CheckComma_Optional THEN
                        thinBAsic_ParseNumber eRGB
                        lRGB = eRGB
                     ELSE
                        lRGB = -1
                    END IF
                    IF thinBasic_CheckCloseParens_Mandatory AND thinBasic_ErrorFree THEN

                        ch = ech
                        cw = ecw

                        textureID = etex
                        glBindTexture( %GL_TEXTURE_2D, lpTexture( textureID ))
                        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH,  WidthTile )
                        GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, HeightTile )


                        framecols = Widthtile/cw
                        framerows = HeightTile/ch
                        framecount = framecols * framerows

                        spCount = framecount
                        DIM aSprite(spCount) AS GLOBAL TBGL_tSprite PTR


                        FOR i = 1 TO spCount
                            aSprite(i) = internal_Memalloc(SIZEOF(TBGL_tSprite))
                            internal_InitSprite(aSprite(i))

                            @aSprite(i).textureID = textureID

                            @aSprite(i).iWidthTile = WidthTile
                            @aSprite(i).iHeightTile = HeightTile
                            @aSprite(i).iWidthBase = cw
                            @aSprite(i).iHeightBase = ch

                            @aSprite(i).iWidth = cw
                            @aSprite(i).iHeight = ch
                            internal_InitSpriteSize(aSprite(i))

                            internal_StoreSprite(aSprite(i))
                        NEXT
                        i = 0
                        FOR currrow = 1 TO Framerows
                            FOR currcol = 1 TO FrameCols
                                i += 1
                                @aSprite(i).tx1(1) = (1.0/FrameCols)*(currcol - 1)
                                @aSprite(i).tx2(1) = (1.0/FrameCols)*(currcol)
                                @aSprite(i).ty1(1) = (1.0/Framerows)*(Framerows - currrow)
                                @aSprite(i).ty2(1) = (1.0/Framerows)*(Framerows - currrow+1)
                            NEXT
                        NEXT

                        glTexParameterI(%gl_texture_2d, %gl_texture_wrap_s, %gl_clamp_to_edge)
                        glTexParameterI(%gl_texture_2d, %gl_texture_wrap_t, %gl_clamp_to_edge)

                        thinBasic_VariableRedim(pVar, %FALSE, spCount, 0, 0)
                        FOR i = 1 TO spCount
                            thinBasic_ChangeVariableNumberDirect (pVar, i, asprite(i))
                        NEXT
                    END IF
                END IF
            END IF
        END IF
    END IF
    FUNCTION = spcount
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpritesCount() AS EXT
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG, group AS LONG
    LOCAL nParsed AS LONG
    LOCAL egroup AS EXT
    LOCAL pSprite AS TBGL_tSprite PTR

    nParsed = thinBasic_ParseXNumbers(0,1, egroup)
    IF nParsed = 0 THEN
        FUNCTION = spritecount
        EXIT FUNCTION
    ELSE
        group = egroup
        c = 0
        FOR i = 1 TO maxsprites
            IF sprites(i) THEN
                pSprite = sprites(i)
                IF @pSprite.iGroup <> group AND nparsed THEN ITERATE FOR
                c += 1
            END IF
        NEXT
        FUNCTION = c

    END IF
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteCopy() AS EXT
  '*****************************************************************

    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT
    LOCAL pNewSprite AS TBGL_tSprite PTR
    LOCAL sbuffer AS STRING

    sp = thinBASIC_Parse1Number
    pSprite = sp

    pNewSprite = internal_Memalloc(SIZEOF(TBGL_tSprite))

    POKE$ pNewSprite, PEEK$(pSprite, SIZEOF(tbgl_tsprite))  'copy block
    internal_StoreSprite(pNewSprite)

    sbuffer = SpriteUserdata(@pSprite.UserPointer)
    SpriteUserdata(@pNewSprite.UserPointer) = sbuffer
    FUNCTION = pNewSprite
  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteDelete()
  '*****************************************************************
    LOCAL pSprite, pSpriteParent AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp

    IF @pSprite.sevOnDelete THEN
        internal_CallSpriteEvent(pSprite,0,%TBGL_OnDelete)
    END IF

    IF @pSprite.iParent THEN
        pSpriteParent = @pSprite.iParent
        @pSpriteParent.iChildCount -= 1
        @pSprite.iParent = 0
    END IF
    IF @pSprite.iChildCount THEN internal_ChildSpriteDelete(pSprite)

    IF callEventLevel = 0 THEN
        internal_RemoveSprite(pSprite)
        internal_MemFree(pSprite)
        internal_DeleteSprites()
    ELSE
        @pSprite.isDeleted = %TRUE
        @pSprite.isActive = %FALSE
        @pSprite.isVisible = %FALSE
        spritesDeleted += 1
    END IF
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpritesDeleteAll()
  '*****************************************************************
    LOCAL i AS LONG, iGroup AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL egroup AS EXT
    LOCAL nParsed AS LONG
    egroup = 0
    nParsed = thinBasic_ParseXNumbers(0,1, egroup)

    IF nparsed = 0 AND callEventLevel = 0 THEN
        FOR i = 1 TO maxsprites
            IF sprites(i) THEN
                pSprite = sprites(i)
                IF @pSprite.sevOnDelete THEN
                    internal_CallSpriteEvent(pSprite,0,%TBGL_OnDelete)
                END IF
                spritecount -= 1
                SpriteUserData(@pSprite.userpointer) = ""
                internal_MemFree(pSprite)
                sprites(i) = 0
            END IF
        NEXT
        spritesDeleted = 0
    ELSE
        igroup = egroup
        FOR i = 1 TO maxsprites
            IF sprites(i) THEN
                pSprite = sprites(i)
                IF @pSprite.igroup <> igroup THEN ITERATE FOR

                IF @pSprite.sevOnDelete THEN
                    internal_CallSpriteEvent(pSprite,0,%TBGL_OnDelete)
                END IF

                @pSprite.isDeleted = %TRUE
                @pSprite.isActive = %FALSE
                @pSprite.isVisible = %FALSE
                spritesDeleted += 1
            END IF
        NEXT
        IF callEventLevel = 0 THEN internal_DeleteSprites()
    END IF
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteAddSpin()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, spinSpeed AS EXT

    thinBasic_Parse2Numbers(sp, spinSpeed)
    pSprite = sp
    @pSprite.iSpeedSpin += spinSpeed
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetShadow()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eAlpha, eXOff, eYOff, eSize AS EXT

    eXOff = 32.0
    eYOff = 32.0
    eSize = 0.8

    nParsed = thinBasic_ParseXNumbers(2,5, sp, eAlpha, eXOff, eYOff, eSize)
    pSprite = sp
    IF eAlpha THEN
        @pSprite.iShadow = %True
    ELSE
        @pSprite.iShadow = %False
    END IF
    @pSprite.iShadowAlpha = eAlpha
    @pSprite.iShadowSize  = eSize
    @pSprite.iShadowXOff  = eXOff
    @pSprite.iShadowYOff  = eYOff

    internal_RotateSprite(pSprite)
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetAlpha()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eAlpha AS EXT

    thinBasic_Parse2Numbers(sp, eAlpha)
    pSprite = sp
    @pSprite.iAlpha = eAlpha

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetColor()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eRed, eGreen, eBlue AS EXT

    thinBasic_Parse4Numbers(sp, eRed, eGreen, eBlue)
    pSprite = sp

    @pSprite.iRed = eRed / 255.0
    @pSprite.iGreen = eGreen / 255.0
    @pSprite.iBlue = eBlue / 255.0
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetSpin()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, spinSpeed AS EXT

    thinBasic_Parse2Numbers(sp, spinSpeed)
    pSprite = sp
    @pSprite.iSpeedSpin = spinSpeed
    IF @pSprite.iChildCount THEN internal_ChildSpriteSetSpin(pSprite,@pSprite.iSpeedSpin)
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetAngle()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eangle, erelative AS EXT
    LOCAL oldangle, diffangle AS SINGLE

    erelative = 0
    nParsed = thinBasic_ParseXNumbers(2,3, sp, eangle, erelative)

    pSprite = sp
    oldangle = @pSprite.iAngle
    internal_SpriteSetOldAngle(pSprite)
    IF erelative = 0 THEN
        @pSprite.iAngle = eangle
    ELSE
        @pSprite.iAngle += eangle
        IF @pSprite.iAngle < 0.0 THEN
            @pSprite.iAngle = @pSprite.iAngle MOD 360.0 + 360
        ELSE
            IF @pSprite.iAngle > 360.0 THEN
                @pSprite.iAngle = @pSprite.iAngle MOD 360.0
            END IF
        END IF
    END IF
    diffangle = @pSprite.iAngle - oldangle
    internal_RotateSprite(pSprite)
    IF @pSprite.iChildCount THEN internal_ChildSpriteSetAngle(pSprite, eangle, erelative)
    IF @pSprite.iChildCount THEN internal_ChildSpriteSetOrbit(pSprite, diffangle)

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetTexCoord()
  '*****************************************************************
    LOCAL f AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eframe, ex1, ey1, ex2, ey2 AS EXT

    thinBasic_Parse6Numbers(sp, eframe, ex1, ey1, ex2, ey2)
    pSprite = sp
    f = eframe
    @pSprite.tx1(f) = ex1/(@pSprite.iWidthTile)
    @pSprite.tx2(f) = (ex2+1)/(@pSprite.iWidthTile)

    @pSprite.ty2(f) = (1/@pSprite.iHeightTile)*(@pSprite.iHeightTile - ey1)
    @pSprite.ty1(f) = (1/@pSprite.iHeightTile)*(@pSprite.iHeightTile - ey2-1)

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetBaseSize()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, ewidth, eheight AS EXT

    thinBasic_Parse3Numbers(sp, ewidth, eheight)
    pSprite = sp
    @pSprite.iWidthBase = ewidth
    @pSprite.iHeightBase = eheight
    @pSprite.iWidth = @pSprite.iWidthBase
    @pSprite.iHeight = @pSprite.iHeightBase


    @pSprite.iRadius = MAX(@pSprite.iWidth,@pSprite.iHeight)/2

    internal_SizeSprite(pSprite)
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetTexFrame()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eframe AS EXT

    thinBasic_Parse2Numbers(sp, eframe)
    pSprite = sp
    @pSprite.icurrtexframe = eframe
    IF @pSprite.isanimated = %TRUE THEN @pSprite.iAnimTime(@pSprite.iAnimgroup) = eframe
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetTexture()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, etex AS EXT

    thinBasic_Parse2Numbers(sp, etex)
    pSprite = sp
    @pSprite.textureID = etex
    glBindTexture( %GL_TEXTURE_2D, lpTexture( @pSprite.textureID ))

    GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_WIDTH, @pSprite.iWidthTile )
    GLGETTEXLEVELPARAMETERIV( %GL_TEXTURE_2D, 0, %GL_TEXTURE_HEIGHT, @pSprite.iHeightTile )
  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetTexture() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.textureID
  END FUNCTION

  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetTexFrame() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.icurrtexframe
  END FUNCTION



  '*****************************************************************
  SUB Exec_TBGL_SpriteUpdate()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eSpeed AS EXT
    LOCAL SPEED AS SINGLE
    espeed = 1.0
    nParsed = thinBasic_ParseXNumbers(1,2, sp, eSpeed)
    pSprite = sp
    SPEED = espeed
    internal_UpdateSprite(pSprite, SPEED)
    internal_DeleteSprites()
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpritesAnimateAll()
  '*****************************************************************
    LOCAL i AS LONG
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL eSpeed AS EXT
    LOCAL SPEED AS SINGLE


    espeed = 1.0
    nParsed = thinBasic_ParseXNumbers(0,1, eSpeed)
    SPEED = espeed

    FOR i = 1 TO maxsprites
  Label002:
        IF sprites(i) THEN
            pSprite = sprites(i)
            IF @pSprite.isActive = %FALSE THEN ITERATE FOR
            internal_AnimateSprite(pSprite, SPEED)
            IF pSprite <> sprites(i) THEN GOTO Label002
        END IF
    NEXT
    internal_DeleteSprites()
  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetID() AS EXT
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG, igroup AS LONG, index AS LONG
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL eIndex , eGroup AS EXT

    egroup = 0
    nParsed = thinBasic_ParseXNumbers(1,2, eindex, egroup)
    index = eindex
    igroup = egroup

    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSprite = sprites(i)
            IF @pSprite.iGroup <> igroup AND nparsed=2 THEN ITERATE FOR
            c += 1
            IF c=index THEN
                FUNCTION = sprites(i)
                EXIT FUNCTION
            END IF
        END IF
    NEXT
    FUNCTION = 0
  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpritesUpdateAll()
  '*****************************************************************
    LOCAL i AS LONG
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL eSpeed AS EXT
    LOCAL SPEED AS SINGLE


    espeed = 1.0
    nParsed = thinBasic_ParseXNumbers(0,1, eSpeed)
    SPEED = espeed

    FOR i = 1 TO maxsprites
  Label003:
        IF sprites(i) THEN
            pSprite = sprites(i)
            IF @pSprite.isActive = %FALSE THEN ITERATE FOR
            internal_UpdateSprite(pSprite, SPEED)
            IF pSprite <> sprites(i) THEN GOTO Label003
        END IF
    NEXT
    internal_DeleteSprites()
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpritesDrawAll()
  '*****************************************************************
    LOCAL i AS LONG, igroup AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL egroup AS EXT
    LOCAL nParsed AS LONG
    egroup = 0
    nParsed = thinBasic_ParseXNumbers(0,1, egroup)
    igroup = egroup

    FOR i = 1 TO maxsprites
  Label001:
        IF sprites(i) THEN
            pSprite = sprites(i)
            IF @pSprite.isActive = %FALSE THEN ITERATE FOR

            IF @pSprite.igroup <> igroup AND nparsed THEN ITERATE FOR

            internal_DrawSprite(pSprite)
            IF pSprite <> sprites(i) THEN GOTO Label001
        END IF
    NEXT
    internal_DeleteSprites()
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSlide()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, ex,ey,angle AS EXT

    thinBasic_Parse3Numbers(sp, ex, ey)
    pSprite = sp
    internal_SpriteSetOldPos(pSprite)
    IF ex THEN
        angle = (@pSprite.iAngle) * 0.0174533
        @pSprite.xPos += SIN(angle)*ex
        @pSprite.yPos -= COS(angle)*ex
    END IF
    IF ey THEN
        angle = (@pSprite.iAngle+90) * 0.0174533
        @pSprite.xPos += SIN(angle)*ey
        @pSprite.yPos -= COS(angle)*ey
    END IF
    IF @pSprite.iChildCount THEN internal_ChildSpriteSlide(pSprite, ex, ey, pSprite)
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteAddSpeedXY()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, ex,ey,eflag, angle AS EXT
    eflag = 0
    nParsed = thinBasic_ParseXNumbers(3,4, sp, ex, ey, eFlag)
    pSprite = sp

    IF eFlag THEN
        IF ex THEN
            angle = (@pSprite.iAngle) * 0.0174532925
            @pSprite.iSpeedX += SIN(angle)*ex
            @pSprite.iSpeedY -= COS(angle)*ex
        END IF
        IF ey THEN
            angle = (@pSprite.iAngle+90) * 0.0174532925
            @pSprite.iSpeedX += SIN(angle)*ey
            @pSprite.iSpeedY -= COS(angle)*ey
        END IF
    ELSE
        @pSprite.iSpeedX += ex
        @pSprite.iSpeedY += ey
    END IF
    Angle = internal_ATN2( @pSprite.iSpeedY, @pSprite.iSpeedX )+90.0
    IF Angle < 0.0 THEN
        Angle = Angle MOD 360.0 + 360.0
    ELSE
        IF Angle > 360.0 THEN
            Angle = Angle MOD 360.0
        END IF
    END IF
    @pSprite.iSpeedAngle = angle
    @pSprite.iSpeed = SQR(@pSprite.iSpeedX*@pSprite.iSpeedX+@pSprite.iSpeedY*@pSprite.iSpeedY)
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteAddSpeed()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eSpeed,  eAngle AS EXT
    LOCAL angle, SPEED AS SINGLE

    nParsed = thinBasic_ParseXNumbers(2,3, sp, espeed, eangle)
    pSprite = sp
    IF nParsed = 2 THEN
        angle = @pSprite.iAngle
    ELSE
        angle = eangle
    END IF
    angle *= 0.0174532925
    SPEED = espeed
    @pSprite.iSpeedX += SIN(angle) * SPEED
    @pSprite.iSpeedY -= COS(angle) * SPEED

    Angle = internal_ATN2( @pSprite.iSpeedY, @pSprite.iSpeedX )+90.0
    IF Angle < 0.0 THEN
        Angle = Angle MOD 360.0 + 360.0
    ELSE
        IF Angle > 360.0 THEN
            Angle = Angle MOD 360.0
        END IF
    END IF
    @pSprite.iSpeedAngle = angle
    @pSprite.iSpeed = SQR(@pSprite.iSpeedX*@pSprite.iSpeedX+@pSprite.iSpeedY*@pSprite.iSpeedY)
    IF @pSprite.iChildCount THEN internal_ChildSpriteSetSpeed(pSprite,@pSprite.iSpeed,@pSprite.iSpeedAngle)
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetSpeed()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, espeed, eangle, SPEED,  angle AS EXT

    nParsed = thinBasic_ParseXNumbers(2,3, sp, eSpeed, eAngle)
    pSprite = sp

    IF nParsed = 2 THEN
        angle = @pSprite.iAngle
    ELSE
        angle = eangle
    END IF
    angle *= 0.0174532925
    SPEED = espeed
    @pSprite.iSpeedX = SIN(angle) * SPEED
    @pSprite.iSpeedY =- COS(angle) * SPEED

    Angle = internal_ATN2( @pSprite.iSpeedY, @pSprite.iSpeedX )+90.0
    IF Angle < 0.0 THEN
        Angle = Angle MOD 360.0 + 360.0
    ELSE
        IF Angle > 360.0 THEN
            Angle = Angle MOD 360.0
        END IF
    END IF
    @pSprite.iSpeedAngle = angle
    @pSprite.iSpeed = SPEED
    IF @pSprite.iChildCount THEN internal_ChildSpriteSetSpeed(pSprite,@pSprite.iSpeed,@pSprite.iSpeedAngle)
  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetTargetAngle() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite, pSpriteTarget AS TBGL_tSprite PTR
    LOCAL sp, spTarget, eflag AS EXT
    LOCAL xdiff, ydiff, angle AS SINGLE

    nParsed = thinBasic_ParseXNumbers(2,3, sp, spTarget, eflag)
    pSprite = sp
    pSpriteTarget = spTarget

    xdiff = @pSpriteTarget.xPos - @pSprite.xPos
    ydiff = @pSpriteTarget.yPos - @pSprite.yPos

    Angle = internal_ATN2( ydiff, xdiff )+90.0

    IF Angle < 0.0 THEN
        Angle = Angle MOD 360.0 + 360.0
    ELSE
        IF Angle > 360.0 THEN
            Angle = Angle MOD 360.0
        END IF
    END IF
    IF eflag = %TRUE THEN
        angle = angle - @pSprite.iAngle
        IF angle > 180.0 THEN
            angle = angle - 360.0
        ELSEIF angle < -180.0 THEN
            angle = angle + 360.0
        END IF
    END IF
    FUNCTION = angle

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetVectorAngle() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, xpos, ypos, eflag AS EXT
    LOCAL xdiff, ydiff, angle AS SINGLE

    nParsed = thinBasic_ParseXNumbers(3,4, sp, xpos, ypos, eflag)
    pSprite = sp

    xdiff = xPos - @pSprite.xPos
    ydiff = yPos - @pSprite.yPos

    Angle = internal_ATN2( ydiff, xdiff )+90.0

    IF Angle < 0.0 THEN
        Angle = Angle MOD 360.0 + 360.0
    ELSE
        IF Angle > 360.0 THEN
            Angle = Angle MOD 360.0
        END IF
    END IF
    IF eflag = %TRUE THEN
        angle = angle - @pSprite.iAngle
        IF angle > 180.0 THEN
            angle = angle - 360.0
        ELSEIF angle < -180.0 THEN
            angle = angle + 360.0
        END IF
    END IF
    FUNCTION = angle

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetTargetDist() AS EXT
  '*****************************************************************
    LOCAL pSprite, pSpriteTarget AS TBGL_tSprite PTR
    LOCAL sp, spTarget AS EXT
    LOCAL xdiff, ydiff, dist AS SINGLE

    thinBasic_Parse2Numbers(sp, spTarget)
    pSprite = sp
    pSpriteTarget = spTarget

    xdiff = @pSprite.xPos - @pSpriteTarget.xPos
    ydiff = @pSprite.yPos - @pSpriteTarget.yPos

    dist = SQR(xdiff*xdiff+ydiff*ydiff)

    FUNCTION = dist

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetVectorDist() AS EXT
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, xpos, ypos AS EXT
    LOCAL xdiff, ydiff, dist AS SINGLE

    nParsed = thinBasic_ParseXNumbers(2,3, sp, xpos, ypos)
    pSprite = sp

    xdiff = @pSprite.xPos - xPos
    ydiff = @pSprite.yPos - yPos

    dist = SQR(xdiff*xdiff+ydiff*ydiff)

    FUNCTION = dist

  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetSpeedAngle()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eangle AS EXT
    LOCAL SPEED, angle AS SINGLE

    nParsed = thinBasic_ParseXNumbers(1,2, sp, eAngle)
    pSprite = sp

    IF nParsed = 1 THEN
        angle = @pSprite.iAngle
    ELSE
        angle = eangle
    END IF
    angle *= 0.0174532925
    SPEED = @pSprite.iSpeed
    @pSprite.iSpeedX = SIN(angle) * SPEED
    @pSprite.iSpeedY =- COS(angle) * SPEED

    Angle = internal_ATN2( @pSprite.iSpeedY, @pSprite.iSpeedX )+90.0
    IF Angle < 0.0 THEN
        Angle = Angle MOD 360.0 + 360.0
    ELSE
        IF Angle > 360.0 THEN
            Angle = Angle MOD 360.0
        END IF
    END IF
    @pSprite.iSpeedAngle = angle
    IF @pSprite.iChildCount THEN internal_ChildSpriteSetSpeed(pSprite,@pSprite.iSpeed,@pSprite.iSpeedAngle)
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetSpeedXY()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, ex, ey, eflag AS EXT
    LOCAL angle,xx,xy,yx,yy AS SINGLE

    eflag = 0
    nParsed = thinBasic_ParseXNumbers(3,4, sp, ex, ey, eflag)
    pSprite = sp

    IF eFlag THEN
        IF ex THEN
            angle = (@pSprite.iAngle) * 0.0174532925
            xx = SIN(angle)*ex
            xy =- COS(angle)*ex
        END IF
        IF ey THEN
            angle = (@pSprite.iAngle+90) * 0.0174532925
            yx = SIN(angle)*ey
            yy =- COS(angle)*ey
        END IF
        @pSprite.iSpeedX = xx + yx
        @pSprite.iSpeedY = xy + yy
    ELSE
        @pSprite.iSpeedX = ex
        @pSprite.iSpeedY = ey
    END IF
    Angle = internal_ATN2( @pSprite.iSpeedY, @pSprite.iSpeedX )+90.0
    IF Angle < 0.0 THEN
        Angle = Angle MOD 360.0 + 360.0
    ELSE
        IF Angle > 360.0 THEN
            Angle = Angle MOD 360.0
        END IF
    END IF
    @pSprite.iSpeedAngle = angle
    @pSprite.iSpeed = SQR(@pSprite.iSpeedX*@pSprite.iSpeedX+@pSprite.iSpeedY*@pSprite.iSpeedY)
    IF @pSprite.iChildCount THEN internal_ChildSpriteSetSpeed(pSprite,@pSprite.iSpeed,@pSprite.iSpeedAngle)
  END SUB


  '*****************************************************************
  SUB internal_ChildSpriteSetScale(BYVAL pSprite AS TBGL_tSprite PTR, eScale AS EXT , erelative AS EXT)
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSpriteChild AS TBGL_tSprite PTR
    c = 0
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            IF ISFALSE(@pSpriteChild.iParentMode AND %TBGL_SCALE) THEN ITERATE FOR
            c += 1
            internal_SpriteSetOldScale(pSpriteChild)
            IF erelative = 0 THEN
                IF (@pSpriteChild.iParentMode AND %TBGL_ChildRel) THEN
                    @pSpriteChild.iScale += (@pSprite.iScale - @pSprite.oldIScale)
                ELSE
                    @pSpriteChild.iScale = eScale
                END IF
            ELSE
                @pSpriteChild.iScale += eScale
                IF @pSpriteChild.iScale <= 0.0 THEN @pSpriteChild.iScale = 0.001
            END IF
            internal_SizeSprite(pSpriteChild)
            IF @pSpriteChild.iChildCount THEN internal_ChildSpriteSetScale(pSpriteChild, eScale, erelative)
        END IF
        IF c = @pSprite.iChildCount THEN EXIT FOR
    NEXT
  END SUB



  '*****************************************************************
  SUB internal_ChildSpriteSetAngle( BYVAL pSprite AS TBGL_tSprite PTR, eAngle AS EXT , erelative AS EXT)
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSpriteChild AS TBGL_tSprite PTR
    c = 0
    'Rotating
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            IF ISFALSE(@pSpriteChild.iParentMode AND %TBGL_ROTATE) THEN ITERATE FOR
            c += 1
            internal_SpriteSetOldAngle(pSpriteChild)
            IF erelative = 0 THEN
                IF (@pSpriteChild.iParentMode AND %TBGL_ChildRel) THEN
                    @pSpriteChild.iAngle += (@pSprite.iAngle - @pSprite.oldIAngle)
                    IF @pSpriteChild.iAngle < 0.0 THEN
                        @pSpriteChild.iAngle = @pSpriteChild.iAngle MOD 360.0 + 360
                    ELSE
                        IF @pSpriteChild.iAngle > 360.0 THEN
                            @pSpriteChild.iAngle = @pSpriteChild.iAngle MOD 360.0
                        END IF
                    END IF
                ELSE
                    @pSpriteChild.iAngle = eangle
                END IF
            ELSE
                @pSpriteChild.iAngle += eangle
                IF @pSpriteChild.iAngle < 0.0 THEN
                    @pSpriteChild.iAngle = @pSpriteChild.iAngle MOD 360.0 + 360
                ELSE
                    IF @pSpriteChild.iAngle > 360.0 THEN
                        @pSpriteChild.iAngle = @pSpriteChild.iAngle MOD 360.0
                    END IF
                END IF
            END IF
            internal_RotateSprite(pSpriteChild)
            IF @pSpriteChild.iChildCount THEN internal_ChildSpriteSetAngle(pSpriteChild, eAngle, erelative)
        END IF

        IF c = @pSprite.iChildCount THEN EXIT FOR
    NEXT
  END SUB



  '*****************************************************************
  SUB internal_ChildSpriteSetOrbit( BYVAL pSprite AS TBGL_tSprite PTR, diffAngle AS SINGLE)
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSpriteChild AS TBGL_tSprite PTR
    LOCAL pSpriteParent AS TBGL_tSprite PTR
    LOCAL pDist, pAngle, xdiff, ydiff , angle, yv, xv AS SINGLE

    c = 0

    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            IF ISFALSE(@pSpriteChild.iParentMode AND %TBGL_ORBIT) THEN ITERATE FOR
            c += 1

            'Calculting the distance
            xdiff = @pSpriteChild.xPos - @pSprite.xPos
            ydiff = @pSpriteChild.yPos - @pSprite.yPos
            pDist = SQR(xdiff*xdiff+ydiff*ydiff)

            Angle = internal_ATN2( ydiff, xdiff )+ 90.0

            Angle += diffAngle

            IF Angle < 0.0 THEN
                Angle = Angle MOD 360.0 + 360.0
            ELSE
                IF Angle > 360.0 THEN
                    Angle = Angle MOD 360.0
                END IF
            END IF

            angle *= 0.0174532925
            xv = SIN(angle) * pDist
            yv = -COS(angle) * pDist

            @pSpriteChild.xPos = @pSprite.xPos + xv
            @pSpriteChild.YPos = @pSprite.yPos + Yv

            internal_RotateSprite(pSpriteChild)
            IF @pSpriteChild.iChildCount THEN internal_ChildSpriteSetOrbit(pSpriteChild, diffAngle)
        END IF

        IF c = @pSprite.iChildCount THEN EXIT FOR
    NEXT
  END SUB



  '*****************************************************************
  SUB internal_ChildSpriteDelete(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSpriteChild AS TBGL_tSprite PTR
    c = 0
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            IF ISFALSE(@pSpriteChild.iParentMode AND %TBGL_DELETE) THEN ITERATE FOR
            c += 1

            @pSpriteChild.isDeleted = %TRUE
            @pSpriteChild.isActive = %FALSE
            @pSpriteChild.isVisible = %FALSE
            spritesDeleted += 1


            IF @pSpriteChild.iChildCount THEN internal_ChildSpriteDelete(pSpriteChild)
        END IF
        IF c = @pSprite.iChildCount THEN EXIT FOR
    NEXT

  END SUB

  '*****************************************************************
  SUB internal_ChildSpriteSetSpin( BYVAL pSprite AS TBGL_tSprite PTR, spin AS SINGLE)
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSpriteChild AS TBGL_tSprite PTR
    c = 0
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            IF ISFALSE(@pSpriteChild.iParentMode AND %TBGL_SPIN) THEN ITERATE FOR
            c += 1
            @pSpriteChild.iSpeedSpin = spin
            IF @pSpriteChild.iChildCount THEN internal_ChildSpriteSetSpin(pSpriteChild, spin)
        END IF
        IF c = @pSprite.iChildCount THEN EXIT FOR
    NEXT
  END SUB


  '*****************************************************************
  SUB internal_ChildSpriteSetSpeed( BYVAL pSprite AS TBGL_tSprite PTR, SPEED AS SINGLE, angle AS SINGLE)
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSpriteChild AS TBGL_tSprite PTR
    c = 0
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            IF ISFALSE(@pSpriteChild.iParentMode AND %TBGL_SPEED) THEN ITERATE FOR
            c += 1
            @pSpriteChild.iSpeedAngle = angle
            @pSpriteChild.iSpeed = SPEED
            IF @pSpriteChild.iChildCount THEN internal_ChildSpriteSetSpeed(pSpriteChild, SPEED, angle)
        END IF
        IF c = @pSprite.iChildCount THEN EXIT FOR
    NEXT
  END SUB

  '*****************************************************************
  SUB internal_ChildSpriteSetPos( BYVAL pSprite AS TBGL_tSprite PTR, ex AS EXT, ey AS EXT , erelative AS EXT)
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSpriteChild AS TBGL_tSprite PTR
    c = 0
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            IF ISFALSE(@pSpriteChild.iParentMode AND %TBGL_POS) THEN ITERATE FOR
            c += 1
            internal_SpriteSetOldPos(pSpriteChild)
            IF erelative THEN
                @pSpriteChild.xPos += ex
                @pSpriteChild.yPos += ey
            ELSE
                IF (@pSpriteChild.iParentMode AND %TBGL_ChildRel) THEN
                    @pSpriteChild.xPos += (@pSprite.xPos - @pSprite.oldXPos)
                    @pSpriteChild.yPos += (@pSprite.yPos - @pSprite.oldYPos)
                ELSE
                    @pSpriteChild.xPos = ex
                    @pSpriteChild.yPos = ey
                END IF
            END IF
            IF @pSpriteChild.iChildCount THEN internal_ChildSpriteSetPos(pSpriteChild, ex, ey, erelative)
        END IF
        IF c = @pSprite.iChildCount THEN EXIT FOR
    NEXT
  END SUB


  '*****************************************************************
  SUB internal_ChildSpriteSlide( BYVAL pSprite AS TBGL_tSprite PTR, ex AS EXT, ey AS EXT, BYVAL pSpriteAngle AS TBGL_tSprite PTR )
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSpriteChild AS TBGL_tSprite PTR
    LOCAL angle AS EXT
    c = 0
    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            IF ISFALSE(@pSpriteChild.iParentMode AND %TBGL_POS) THEN ITERATE FOR
            c += 1
            internal_SpriteSetOldPos(pSpriteChild)
            IF ex THEN
                angle = (@pSpriteAngle.iAngle) * 0.0174533
                @pSpriteChild.xPos += SIN(angle)*ex
                @pSpriteChild.yPos -= COS(angle)*ex
            END IF
            IF ey THEN
                angle = (@pSpriteAngle.iAngle+90) * 0.0174533
                @pSpriteChild.xPos += SIN(angle)*ey
                @pSpriteChild.yPos -= COS(angle)*ey
            END IF
            IF @pSpriteChild.iChildCount THEN internal_ChildSpriteSlide(pSpriteChild, ex, ey, pSpriteAngle)
        END IF
        IF c = @pSprite.iChildCount THEN EXIT FOR
    NEXT
  END SUB





  '*****************************************************************
  SUB Exec_TBGL_SpriteSetPos()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, ex,ey,erelative AS EXT

    erelative = 0
    nParsed = thinBasic_ParseXNumbers(3,4, sp, ex, ey, erelative)
    pSprite = sp
    internal_SpriteSetOldPos(pSprite)
    IF erelative THEN
        @pSprite.xPos += ex
        @pSprite.yPos += ey
    ELSE
        @pSprite.xPos = ex
        @pSprite.yPos = ey
    END IF

    IF @pSprite.iChildCount THEN internal_ChildSpriteSetPos(pSprite, ex, ey, erelative)

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetTag()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eTag AS EXT

    thinBasic_Parse2Numbers(sp, eTag)
    pSprite = sp
    @pSprite.iTag = eTag
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetGroup()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, egroup AS EXT

    thinBasic_Parse2Numbers(sp, egroup)
    pSprite = sp
    @pSprite.iGroup = eGroup
  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetTag() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number()
    pSprite = sp
    FUNCTION = @pSprite.iTag
  END FUNCTION

  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetGroup() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number()
    pSprite = sp
    FUNCTION = @pSprite.iGroup
  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetParent()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eparent AS EXT

    thinBasic_Parse2Numbers(sp, eparent)
    pSprite = sp
    IF eParent THEN
        @pSprite.iParent = eparent
        pSprite = eParent
        @pSprite.iChildCount += 1
    ELSE
        IF @pSprite.iParent <> 0 THEN
            eparent = @pSprite.iParent
            @pSprite.iParent = 0
            pSprite = eParent
            @pSprite.iChildCount -= 1
        END IF

    END IF
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetParentMode()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, emode AS EXT

    thinBasic_Parse2Numbers(sp, emode)
    pSprite = sp
    @pSprite.iParentMode = emode
  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetParent() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number()
    pSprite = sp
    FUNCTION = @pSprite.iParent
  END FUNCTION

  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetChildCount() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number()
    pSprite = sp
    FUNCTION = @pSprite.iChildCount
  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetChildID() AS EXT
  '*****************************************************************
    LOCAL i AS LONG, c AS LONG
    LOCAL pSprite, pSpriteChild AS TBGL_tSprite PTR
    LOCAL sp, eindex AS EXT
    LOCAL index AS LONG
    c = 0

    thinBasic_Parse2Numbers(sp, eindex)
    pSprite = sp
    index = eindex

    FOR i = 1 TO maxsprites
        IF sprites(i) THEN
            pSpriteChild = sprites(i)
            IF @pSpriteChild.iParent <> pSprite THEN ITERATE FOR
            c += 1
            IF index = c THEN FUNCTION = pSpriteChild: EXIT FUNCTION
        END IF
    NEXT
    FUNCTION = 0
  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetScale()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eScale, erelative AS EXT

    erelative = 0
    nParsed = thinBasic_ParseXNumbers(2,3, sp, eScale, erelative)
    pSprite = sp
    internal_SpriteSetOldScale(pSprite)
    IF erelative = 0 THEN
        @pSprite.iScale = eScale
    ELSE
        @pSprite.iScale += eScale
        IF @pSprite.iScale <= 0.0 THEN @pSprite.iScale = 0.001
    END IF
    internal_SizeSprite(pSprite)

    IF @pSprite.iChildcount THEN internal_ChildSpriteSetScale(pSprite, eScale, eRelative)
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetHFlip()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eFlag AS EXT

    thinBasic_Parse2Numbers(sp, eFlag)
    pSprite = sp
    @pSprite.iHFlip = eflag
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetVFlip()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eFlag AS EXT

    thinBasic_Parse2Numbers(sp, eFlag)
    pSprite = sp
    @pSprite.iVFlip = eflag
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetLayer()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, elayer AS EXT

    thinBasic_Parse2Numbers(sp, elayer)
    pSprite = sp
    IF elayer > 256 THEN elayer = 256
    IF elayer < 1 THEN elayer = 1
    @pSprite.zPos = elayer/256.0
  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetLayer() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp

    FUNCTION = INT(256.0/@pSprite.zPos+0.99)
  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteAnimate()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eSpeed AS EXT
    LOCAL SPEED AS SINGLE

    thinBasic_Parse2Numbers(sp, eSpeed)
    pSprite = sp
    SPEED = espeed
    internal_AnimateSprite(pSprite, SPEED)
    internal_DeleteSprites()
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetAnim()
  '*****************************************************************
    LOCAL nParsed,group AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eStart, eEnd, egroup AS EXT
    eGroup = 1
    nParsed = thinBasic_ParseXNumbers(3,4, sp, eStart, eEnd, eGroup)
    pSprite = sp
    group = eGroup
    @pSprite.iStartFrame(group) = eStart
    @pSprite.iEndFrame(group) = eEnd
    @pSprite.iAnimLength(group) = @pSprite.iEndFrame(group) - @pSprite.iStartFrame(group) + 1
    @pSprite.iAnimTime(group) = eStart
    IF @pSprite.iAnimSpeed(group) = 0 THEN @pSprite.iAnimSpeed(group) = 1.0

  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetScale() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.iScale

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetSpin() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.iSpeedSpin

  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetAnimtime()
  '*****************************************************************
    LOCAL nParsed, group AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, egroup, etime AS EXT
    egroup = 1.0
    nParsed = thinBasic_ParseXNumbers(2,3, sp, etime, egroup)
    pSprite = sp
    group = egroup
    @pSprite.ianimTime(group) = etime

  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetAnimtime() AS EXT
  '*****************************************************************
    LOCAL nParsed, group AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, egroup AS EXT
    egroup = 1.0
    nParsed = thinBasic_ParseXNumbers(1,2, sp, egroup)
    pSprite = sp
    group = egroup
    FUNCTION = @pSprite.ianimTime(group)

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetAnimGroup() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT
    sp = thinBasic_Parse1Number
    pSprite = sp

    FUNCTION = @pSprite.ianimgroup

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetAnimLength() AS EXT
  '*****************************************************************
    LOCAL nParsed, group AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, egroup AS EXT
    egroup = 1.0

    nParsed = thinBasic_ParseXNumbers(1,2, sp, egroup)
    pSprite = sp
    group = egroup
    FUNCTION = @pSprite.ianimLength(group)

  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetAnimSpeed()
  '*****************************************************************
    LOCAL nParsed, group AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,espeed, egroup AS EXT
    egroup = 1.0
    nParsed = thinBasic_ParseXNumbers(2,3, sp,espeed, egroup)
    pSprite = sp
    group = egroup
    @pSprite.iAnimSpeed(group) = espeed
    IF espeed < 0 THEN
        @pSprite.ianimspin(@pSprite.iAnimGroup) = %TBGL_BACKWARD
    ELSE
        @pSprite.ianimspin(@pSprite.iAnimGroup) = %TBGL_FORWARD
    END IF

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetAnimGroup()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, egroup AS EXT
    thinBasic_Parse2Numbers(sp, egroup)
    pSprite = sp
    @pSprite.iAnimgroup = egroup

  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetAnimType()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp, eType, eGroup AS EXT
    eGroup = 1
    thinBasic_ParseXNumbers(2,3, sp, eType, eGroup)
    pSprite = sp
    @pSprite.iAnimType(eGroup) = eType

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetCollisionType()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,ect AS EXT

    thinBasic_Parse2Numbers(sp,ect)
    pSprite = sp
    @pSprite.icoltype = ect

  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetCollisionWith() AS EXT
  '*****************************************************************
    LOCAL w AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,ew AS EXT

    thinBasic_Parse2Numbers(sp,ew)
    pSprite = sp
    w = ew
    FUNCTION = @pSprite.icolwith(w)

  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetCollisionWith()
  '*****************************************************************
    LOCAL w AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,ew,eflag AS EXT

    thinBasic_Parse3Numbers(sp,ew,eflag)
    pSprite = sp
    w = ew
    @pSprite.icolwith(w) = eflag

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetCollisionMode()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,emode AS EXT

    thinBasic_Parse2Numbers(sp,emode)
    pSprite = sp
    @pSprite.icolmode = emode

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetCollisionGroup()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,ecg AS EXT

    thinBasic_Parse2Numbers(sp,ecg)
    pSprite = sp
    @pSprite.icolgroup = ecg

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetCollisionRadius()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,ecr AS EXT

    thinBasic_Parse2Numbers(sp,ecr)
    pSprite = sp
    @pSprite.iradius = ecr
    internal_SizeSprite(pSprite)
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetCollisionWidth()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,ew AS EXT

    thinBasic_Parse2Numbers(sp,ew)
    pSprite = sp
    @pSprite.icolwidth = ew
    internal_RotateSprite(pSprite)
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetCollisionHeight()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,eh AS EXT

    thinBasic_Parse2Numbers(sp,eh)
    pSprite = sp
    @pSprite.icolheight = eh
    internal_RotateSprite(pSprite)
  END SUB

  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetCollisionType() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.icoltype

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetCollisionMode() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.icolMode

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetCollisionRadius() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.icolrad

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetCollisionWidth() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.icolWidth

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetCollisionHeight() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.icolHeight

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetCollisionGroup() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.icolgroup

  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetActive()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,ea AS EXT

    thinBasic_Parse2Numbers(sp,ea)
    pSprite = sp
    @pSprite.isActive = ea

  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetVisible()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,ev AS EXT

    thinBasic_Parse2Numbers(sp,ev)
    pSprite = sp
    @pSprite.isVisible = ev

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetFriction()
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp,eFriction AS EXT

    thinBasic_Parse2Numbers(sp,eFriction)
    pSprite = sp
    @pSprite.iFriction = eFriction

  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetActive() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.isActive

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetVisible() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.isVisible

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetAngle() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.iAngle

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetAlpha() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.iAlpha

  END FUNCTION

  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetFriction() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number
    pSprite = sp
    FUNCTION = @pSprite.iFriction

  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteGetBaseSize()
  '*****************************************************************

    LOCAL pSprite AS TBGL_tSprite PTR
    'LOCAL x, y, z AS EXT
    LOCAL sp AS EXT

    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG


    'nParsed = thinBasic_ParseXNumbers(3,4, sp, x, y, z)
    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber sp
      pSprite = sp
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
          IF thinBasic_CheckCloseParens_Mandatory THEN
            thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @pSprite.iWidthBase)
            thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @pSprite.iHeightBase)
          END IF
        END IF
      END IF
    END IF


  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteGetVector()
  '*****************************************************************

    LOCAL pSprite AS TBGL_tSprite PTR

    LOCAL sp, espeed, eangle AS EXT
    LOCAL SPEED, angle,xv,yv AS SINGLE

    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG


    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber sp
      pSprite = sp
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_ParseNumber espeed
          SPEED = espeed
          IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
              thinBasic_ParseNumber eangle
              angle = eangle
              IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
                IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                    thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
                    IF thinBasic_CheckCloseParens_Mandatory THEN

                        angle *= 0.0174532925
                        xv = SIN(angle) * SPEED
                        yv = -COS(angle) * SPEED

                        thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @pSprite.xPos + xv)
                        thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @pSprite.yPos + yv)
                    END IF
                END IF
              END IF
          END IF
      END IF
    END IF


  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteGetOldPos()
  '*****************************************************************

    LOCAL pSprite AS TBGL_tSprite PTR

    LOCAL sp AS EXT

    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG


    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber sp
      pSprite = sp
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
            IF  thinBasic_CheckCloseParens_Mandatory THEN
                thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @pSprite.OldxPos)
                thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @pSprite.OldyPos)
            END IF
        END IF
      END IF
    END IF
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteGetPos()
  '*****************************************************************

    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG

    LOCAL lVariablePtr3     AS LONG
    LOCAL lVariableAbsPos3  AS LONG

    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber sp
      pSprite = sp
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
            IF thinBasic_CheckCloseParens_Optional THEN
                thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @pSprite.xPos)
                thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @pSprite.yPos)
            ELSE
                IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                    thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)

                    thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @pSprite.xPos)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @pSprite.yPos)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @pSprite.zPos)
                END IF
                thinBasic_CheckCloseParens_Mandatory
            END IF
        END IF
      END IF
    END IF


  END SUB



  '*****************************************************************
  SUB Exec_TBGL_SpriteGetSpeedXY()
  '*****************************************************************

    LOCAL pSprite AS TBGL_tSprite PTR

    LOCAL sp AS EXT

    LOCAL lVariablePtr1     AS LONG
    LOCAL lVariableAbsPos1  AS LONG

    LOCAL lVariablePtr2     AS LONG
    LOCAL lVariableAbsPos2  AS LONG

    LOCAL lVariablePtr3     AS LONG
    LOCAL lVariableAbsPos3  AS LONG

    IF thinBasic_CheckOpenParens_Mandatory THEN
      thinBasic_ParseNumber sp
      pSprite = sp
      IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
        thinBasic_VariableParse(lVariablePtr1, lVariableAbsPos1)
        IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
          thinBasic_VariableParse(lVariablePtr2, lVariableAbsPos2)
            IF thinBasic_CheckCloseParens_Optional THEN
                thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @pSprite.iSpeedX)
                thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @pSprite.iSpeedY)
            ELSE
                IF thinBasic_CheckComma_Mandatory AND thinBasic_ErrorFree THEN
                    thinBasic_VariableParse(lVariablePtr3, lVariableAbsPos3)

                    thinBasic_ChangeVariableNumberDirect (lVariablePtr1, lVariableAbsPos1, @pSprite.iSpeedx)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr2, lVariableAbsPos2, @pSprite.iSpeedy)
                    thinBasic_ChangeVariableNumberDirect (lVariablePtr3, lVariableAbsPos3, @pSprite.iSpeedSpin)
                END IF
                thinBasic_CheckCloseParens_Mandatory
            END IF
        END IF
      END IF
    END IF


  END SUB


  '*****************************************************************
  SUB internal_DrawSprite(BYVAL pSprite AS TBGL_tSprite PTR)
  '*****************************************************************
    LOCAL x1,y1,x2,y2,x3,y3,x4,y4,z,zs AS SINGLE
    LOCAL tx1, ty1, tx2, ty2, td AS SINGLE
    LOCAL x1s, y1s,x2s,y2s,x3s,y3s,x4s,y4s AS SINGLE
    LOCAL animframe AS LONG

    IF @pSprite.isVisible = %FALSE THEN EXIT SUB

    IF @pSprite.sevOnBeforeDraw THEN
        internal_CallSpriteEvent(pSprite,0,%TBGL_OnBeforeDraw)
    END IF

    IF @pSprite.textureID  < 0 OR @pSprite.textureID  > %LIMIT_MAX_TEXTUREINDEX THEN
        TBGLError "Statement:" + $TAB + "TBGL_DrawSprite2D" + $CRLF + "Problem:" + $TAB + $TAB + $ERR_BADTEXTUREINDEX + $CRLF + $CRLF + "Passed:" + $TAB + FORMAT$( @pSprite.textureID  )
        EXIT SUB
    END IF

    IF @pSprite.textureID <> LastBindedTexture THEN
        LastBindedTexture = @pSprite.textureID
        glBindTexture %GL_TEXTURE_2D, lpTexture( @pSprite.textureID )
    END IF

    x1 = @pSprite.xPos + @pSprite.x1
    x2 = @pSprite.xPos + @pSprite.x2
    x3 = @pSprite.xPos + @pSprite.x3
    x4 = @pSprite.xPos + @pSprite.x4
    y1 = @pSprite.yPos + @pSprite.y1
    y2 = @pSprite.yPos + @pSprite.y2
    y3 = @pSprite.yPos + @pSprite.y3
    y4 = @pSprite.yPos + @pSprite.y4
    z  = @pSprite.zPos


    IF @pSprite.isanimated = %True  THEN
        animframe = FIX(@pSprite.ianimtime(@pSprite.iAnimGroup))
    ELSE
        animframe = @pSprite.iCurrTexFrame
    END IF
    tx1 = @pSprite.tx1(animframe)
    tx2 = @pSprite.tx2(animframe)
    ty1 = @pSprite.ty1(animframe)
    ty2 = @pSprite.ty2(animframe)


    IF @pSprite.iVFlip = %TRUE THEN
        td = ty1
        ty1 = ty2
        ty2 = td
    END IF

    IF @pSprite.iHFlip = %TRUE THEN
        td = tx1
        tx1 = tx2
        tx2 = td
    END IF


    ' -- Draw shadow
    IF @pSprite.iShadow THEN

        x1s = @pSprite.xPos + @pSprite.iShadowXOff + @pSprite.x1s
        x2s = @pSprite.xPos + @pSprite.iShadowXOff + @pSprite.x2s
        x3s = @pSprite.xPos + @pSprite.iShadowXOff + @pSprite.x3s
        x4s = @pSprite.xPos + @pSprite.iShadowXOff + @pSprite.x4s
        y1s = @pSprite.yPos + @pSprite.iShadowYOff + @pSprite.y1s
        y2s = @pSprite.yPos + @pSprite.iShadowYOff + @pSprite.y2s
        y3s = @pSprite.yPos + @pSprite.iShadowYOff + @pSprite.y3s
        y4s = @pSprite.yPos + @pSprite.iShadowYOff + @pSprite.y4s
        zs = z - 0.002

        glcolor4f 0.0,0.0,0.0,@pSprite.iShadowAlpha
        glBegin %GL_QUADS

            glTexCoord2f tx1, ty1
            glVertex3f x1s , y1s, zs

            glTexCoord2f tx2, ty1
            glVertex3f x2s , y2s, zs

            glTexCoord2f tx2, ty2
            glVertex3f x3s , y3s, zs

            glTexCoord2f tx1, ty2
            glVertex3f x4s , y4s, zs
        glEnd

    END IF
    glcolor4f @pSprite.iRed, @pSprite.iGreen, @pSprite.iBlue, @pSprite.iAlpha
    glBegin %GL_QUADS
        glTexCoord2f tx1, ty1
        glVertex3f x1 , y1, z

        glTexCoord2f tx2, ty1
        glVertex3f x2 , y2, z

        glTexCoord2f tx2, ty2
        glVertex3f x3 , y3, z

        glTexCoord2f tx1, ty2
        glVertex3f x4 , y4, z
    glEnd

    IF @pSprite.sevOnAfterDraw THEN
        internal_CallSpriteEvent(pSprite,0,%TBGL_OnAfterDraw)
    END IF
  END SUB

  '*****************************************************************
  SUB Exec_TBGL_SpriteSetOrder()
  '*****************************************************************
    LOCAL i AS LONG
    LOCAL sp,eo AS EXT
    LOCAL isOrder,lOrder AS LONG
    LOCAL pSprite,pSprite2 AS TBGL_tSprite PTR
    LOCAL buffer AS STRING

    thinBasic_Parse2Numbers(sp, eo)
    pSprite = sp
    lOrder = eo
    IF lorder > maxsprites THEN lorder = maxsprites

    'Get current order(position in sprite list)
    FOR i = 1 TO maxsprites
        IF sprites(i) = pSprite THEN
            isOrder = i
            EXIT FOR
        END IF
    NEXT

    'New order is higher than current one
    buffer = SpriteUserdata(@pSprite.userpointer)
    IF isOrder < lOrder THEN
        FOR i = isOrder TO lOrder-1
            sprites(i) = sprites(i+1)
            SpriteUserdata(i) = SpriteUserdata(i+1)
            IF sprites(i) THEN
                pSprite2 = sprites(i)
                @pSprite2.userpointer = i
            END IF
        NEXT
        sprites(lOrder)=pSprite
        SpriteUserdata(lOrder)=buffer
    END IF

    'New order is lower than current one
    IF isOrder > lOrder THEN
        FOR i = isOrder TO lOrder+1 STEP - 1
            sprites(i) = sprites(i-1)
            SpriteUserdata(i) = SpriteUserdata(i-1)
            IF sprites(i) THEN
                pSprite2 = sprites(i)
                @pSprite2.userpointer = i
            END IF
        NEXT
        sprites(lOrder)=pSprite
        SpriteUserdata(lOrder)=buffer
    END IF
    @pSprite.userpointer = lOrder

  END SUB



  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetOrder() AS EXT
  '*****************************************************************
    LOCAL i AS LONG
    LOCAL sp AS EXT
    LOCAL order AS LONG

    sp = thinBasic_Parse1Number

    order = 0
    FOR i = 1 TO maxsprites
        IF sprites(i) = sp THEN
            order = i
            EXIT FOR
        END IF
    NEXT
    FUNCTION = order

  END FUNCTION


  '*****************************************************************
  SUB Exec_TBGL_SpriteDraw()
  '*****************************************************************
    LOCAL sp AS EXT
    LOCAL pSprite AS TBGL_tSprite PTR

    sp = thinBasic_Parse1Number
    pSprite = sp

    internal_DrawSprite(pSprite)
    internal_DeleteSprites()
  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteDrawAt()
  '*****************************************************************
    LOCAL nParsed AS LONG
    LOCAL ex, ey, elayer, sp AS EXT
    LOCAL pSprite AS TBGL_tSprite PTR

    nParsed = thinBasic_ParseXNumbers(3,4, sp, ex, ey, elayer)
    pSprite = sp
    internal_SpriteSetOldPos(pSprite)
    @pSprite.xPos = ex
    @pSprite.yPos = ey
    IF nParsed = 4 THEN
        IF elayer > 256.0 THEN elayer = 256.0
        IF elayer < 1.0 THEN elayer = 1.0
        @pSprite.zPos = elayer/256.0
    END IF

    internal_DrawSprite(pSprite)

  END SUB


  '*****************************************************************
  SUB Exec_TBGL_SpriteSetEvent()
  '*****************************************************************

    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL eid, sp AS EXT
    LOCAL eventID AS LONG
    LOCAL ScriptFuncPtr AS LONG
    LOCAL funcname AS STRING

    ScriptFuncPtr = 0
    IF thinBasic_CheckOpenParens THEN
      thinBasic_ParseNumber sp
      pSprite = sp
      IF thinBasic_CheckComma THEN
          thinBasic_ParseNumber eid
          eventID = eid
          IF thinBasic_CheckComma THEN
            'ScriptFuncPtr = thinBasic_FunctionParseAndGetPtr
            thinBasic_ParseStr funcname
            ScriptFuncPtr = thinBasic_FunctionGetPtr(funcname)
            IF thinBasic_CheckCloseParens THEN

                SELECT CASE AS LONG eventID
                CASE %TBGL_OnBeforeUpdate
                    @pSprite.sevOnBeforeUpdate = ScriptFuncPtr
                CASE %TBGL_OnAfterUpdate
                    @pSprite.sevOnAfterUpdate = ScriptFuncPtr
                CASE %TBGL_OnBeforeDraw
                    @pSprite.sevOnBeforeDraw = ScriptFuncPtr
                CASE %TBGL_OnAfterDraw
                    @pSprite.sevOnAfterDraw = ScriptFuncPtr
                CASE %TBGL_OnCollision
                    @pSprite.sevOnCollision = ScriptFuncPtr
                CASE %TBGL_OnBeforeAnimate
                    @pSprite.sevOnBeforeAnimate = ScriptFuncPtr
                CASE %TBGL_OnAfterAnimate
                    @pSprite.sevOnAfterAnimate = ScriptFuncPtr
                CASE %TBGL_OnDelete
                    @pSprite.sevOnDelete = ScriptFuncPtr
                CASE %TBGL_OnMouseOver
                    @pSprite.sevOnMouseOver = ScriptFuncPtr
                END SELECT
            END IF
          END IF
      END IF
    END IF

  END SUB


  '*****************************************************************
  FUNCTION internal_PointToPointDist( x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE ) AS SINGLE
  '*****************************************************************
    LOCAL dx,dy AS SINGLE
    dx = x1-x2
    dy = y1-y2

    FUNCTION = SQR(dx*dx + dy*dy)
  END FUNCTION


  '*****************************************************************
  FUNCTION internal_SpriteCollided_Line2Circle(x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE, px AS SINGLE, py AS SINGLE, r AS SINGLE) AS LONG
  '*****************************************************************
      LOCAL sx AS SINGLE
      LOCAL sy AS SINGLE
      LOCAL cx AS SINGLE
      LOCAL cy AS SINGLE


      LOCAL q AS SINGLE
      sx = x2-x1
      sy = y2-y1
      q = ((px-x1) * (x2-x1) + (py - y1) * (y2-y1)) / (sx*sx + sy*sy)

      IF q < 0.0 THEN q = 0.0
      IF q > 1.0 THEN q = 1.0

      cx =(1-q)*x1+q*x2
      cy =(1-q)*y1 + q*y2

      FUNCTION = (internal_PointToPointDist(px,py,cx,cy) < r)
  END FUNCTION


  '*****************************************************************
   FUNCTION internal_SpriteCollided_PointInsidePolygon(BYVAL pSprite AS TBGL_tSprite PTR, BYVAL px AS SINGLE, BYVAL py AS SINGLE) AS LONG
  '*****************************************************************
       LOCAL angle AS SINGLE
       LOCAL x1,x4,y1,y4 AS SINGLE


       LOCAL x,y AS SINGLE
       LOCAL SinVal, CosVal AS SINGLE

       angle = @pSprite.iAngle * 0.0174533
       SinVal = SIN(-angle)
       CosVal = COS(-angle)

       x = px-@pSprite.Xpos
       y = py-@pSprite.Ypos

       px=(x*CosVal)-(y*SinVal)+@pSprite.Xpos
       py=(y*CosVal)+(x*SinVal)+@pSprite.Ypos

       x1 =  @pSprite.x1c+@pSprite.Xpos
       x4 =  @pSprite.x4c+@pSprite.Xpos
       y1 =  @pSprite.y1c+@pSprite.Ypos
       y4 = -@pSprite.y4c+@pSprite.Ypos

        FUNCTION = ((px>x1 AND px<x4) AND (py>y4 AND py <y1))

   END FUNCTION


  '*****************************************************************
   FUNCTION internal_SpriteCollided_PointInsideCircle(BYVAL pSpriteA AS TBGL_tSprite PTR, BYVAL px AS SINGLE, BYVAL py AS SINGLE) AS LONG
  '*****************************************************************
      LOCAL xf, yf, rf AS SINGLE
      LOCAL retval AS LONG
      retval = %FALSE
      xf = (@pSpriteA.xPos - px)
      xf *= xf
      yf = (@pSpriteA.yPos - py)
      yf *= yf
      rf = @pSpriteA.iColRad
      rf *= rf
      IF (xf+yf) < rf THEN retval = %TRUE
      FUNCTION = retval

   END FUNCTION

  '*****************************************************************
  FUNCTION internal_SpriteCollided_Circle2Box(BYVAL pSpriteA AS TBGL_tSprite PTR , BYVAL pSpriteB AS TBGL_tSprite PTR) AS EXT
  '*****************************************************************
      LOCAL retval AS LONG
      LOCAL rf AS SINGLE
      LOCAL bp1, bp2, bp3, bp4 AS tPointS

      IF internal_SpriteCollided_PointInsidePolygon(pSpriteB,@pSpriteA.xPos,@pSpriteA.yPos) THEN ReturnAndExit(3)
      retval = 0

      rf = @pSpriteA.iColRad

      bp1.x = @pSpriteB.x1c + @pSpriteB.xPos
      bp1.y = @pSpriteB.y1c + @pSpriteB.yPos

      bp2.x = @pSpriteB.x2c + @pSpriteB.xPos
      bp2.y = @pSpriteB.y2c + @pSpriteB.yPos

      bp3.x = @pSpriteB.x3c + @pSpriteB.xPos
      bp3.y = @pSpriteB.y3c + @pSpriteB.yPos

      bp4.x = @pSpriteB.x4c + @pSpriteB.xPos
      bp4.y = @pSpriteB.y4c + @pSpriteB.yPos

      IF internal_SpriteCollided_Line2Circle(bp1.x,bp1.y,bp2.x,bp2.y,@pSpriteA.xpos,@pSpriteA.ypos,rf) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_Line2Circle(bp2.x,bp2.y,bp3.x,bp3.y,@pSpriteA.xpos,@pSpriteA.ypos,rf) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_Line2Circle(bp3.x,bp3.y,bp4.x,bp4.y,@pSpriteA.xpos,@pSpriteA.ypos,rf) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_Line2Circle(bp4.x,bp4.y,bp1.x,bp1.y,@pSpriteA.xpos,@pSpriteA.ypos,rf) THEN ReturnAndExit(2)
      FUNCTION = retval
  END FUNCTION


  '*****************************************************************
  FUNCTION internal_SpriteCollided_Box2Circle(BYVAL pSpriteB AS TBGL_tSprite PTR , BYVAL pSpriteA AS TBGL_tSprite PTR) AS EXT
  '*****************************************************************
      LOCAL retval AS LONG
      LOCAL rf AS SINGLE
      LOCAL bp1, bp2, bp3, bp4 AS tPointS

      IF internal_SpriteCollided_PointInsidePolygon(pSpriteB,@pSpriteA.xPos,@pSpriteA.yPos) THEN ReturnAndExit(3)
      retval = 0

      rf = @pSpriteA.iColRad


      bp1.x = @pSpriteB.x1c + @pSpriteB.xPos
      bp1.y = @pSpriteB.y1c + @pSpriteB.yPos

      bp2.x = @pSpriteB.x2c + @pSpriteB.xPos
      bp2.y = @pSpriteB.y2c + @pSpriteB.yPos

      bp3.x = @pSpriteB.x3c + @pSpriteB.xPos
      bp3.y = @pSpriteB.y3c + @pSpriteB.yPos

      bp4.x = @pSpriteB.x4c + @pSpriteB.xPos
      bp4.y = @pSpriteB.y4c + @pSpriteB.yPos

      IF internal_SpriteCollided_Line2Circle(bp1.x,bp1.y,bp2.x,bp2.y,@pSpriteA.xpos,@pSpriteA.ypos,rf) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_Line2Circle(bp2.x,bp2.y,bp3.x,bp3.y,@pSpriteA.xpos,@pSpriteA.ypos,rf) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_Line2Circle(bp3.x,bp3.y,bp4.x,bp4.y,@pSpriteA.xpos,@pSpriteA.ypos,rf) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_Line2Circle(bp4.x,bp4.y,bp1.x,bp1.y,@pSpriteA.xpos,@pSpriteA.ypos,rf) THEN ReturnAndExit(2)
      FUNCTION = retval
  END FUNCTION


  '*****************************************************************
  FUNCTION internal_SpriteCollided_Circle2Circle(BYVAL pSpriteA AS TBGL_tSprite PTR , BYVAL pSpriteB AS TBGL_tSprite PTR) AS EXT
  '*****************************************************************
      LOCAL xf, yf, rf AS SINGLE

      xf = (@pSpriteA.xPos - @pSpriteB.xPos)
      xf *= xf
      yf = (@pSpriteA.yPos - @pSpriteB.yPos)
      yf *= yf

      rf = @pSpriteA.iColRad + @pSpriteB.iColRad
      rf *= rf
      IF (xf+yf) < rf THEN ReturnAndExit(2)
      FUNCTION = 0
  END FUNCTION


  '*****************************************************************
  FUNCTION internal_SpriteCollided_Circle2CircleInBox(BYVAL pSpriteA AS TBGL_tSprite PTR , BYVAL pSpriteB AS TBGL_tSprite PTR) AS EXT
  '*****************************************************************
      LOCAL xf, yf, rf AS SINGLE

      xf = (@pSpriteA.xPos - @pSpriteB.xPos)
      xf *= xf
      yf = (@pSpriteA.yPos - @pSpriteB.yPos)
      yf *= yf

      rf = (@pSpriteA.iColRad + @pSpriteB.iColRad)*1.42
      rf *= rf
      IF (xf+yf) < rf THEN ReturnAndExit(2)
      FUNCTION = 0
  END FUNCTION


  '*****************************************************************
  FUNCTION internal_SpriteCollided_Bound2Bound(BYVAL pSpriteA AS TBGL_tSprite PTR , BYVAL pSpriteB AS TBGL_tSprite PTR) AS EXT
  '*****************************************************************
      LOCAL left1, left2, right1, right2, top1, top2, bottom1, bottom2,h,w AS SINGLE

      FUNCTION = %FALSE
      w = @pSpriteA.iWIDTH
      left1   = @pSpriteA.xPos - w/2
      right1  = left1 + w
      h = @pSpriteA.iHeight
      top1    = @pSpriteA.YPos - h/2
      bottom1 = top1 + h

      w = @pSpriteB.iWIDTH
      left2   = @pSpriteB.xPos - w/2
      right2  = left2 + w

      h = @pSpriteB.iHeight
      top2    = @pSpriteB.YPos - h/2
      bottom2 = top2 + h

      IF (bottom1 < top2) THEN EXIT FUNCTION
      IF (top1 > bottom2) THEN EXIT FUNCTION
      IF (right1 < left2) THEN EXIT FUNCTION
      IF (left1 > right2) THEN EXIT FUNCTION
      ReturnAndExit(2)
  END FUNCTION


  '*****************************************************************
  FUNCTION internal_SpriteCollided_Box2Box(BYVAL pSpriteA AS TBGL_tSprite PTR , BYVAL pSpriteB AS TBGL_tSprite PTR) AS EXT
  '*****************************************************************
      LOCAL i AS LONG, j AS LONG
      ' -- Corner points
      LOCAL ap1, ap2, ap3, ap4 AS tPointS
      LOCAL bp1, bp2, bp3, bp4 AS tPointS

      LOCAL a1p1, a1p2, a2p1, a2p2, a3p1, a3p2, a4p1, a4p2, _
            b1p1, b1p2, b2p1, b2p2, b3p1, b3p2, b4p1, b4p2 AS tPointS PTR

      ' -- Each rectangle defined by 4 lines - 0..3
      DIM a(3) AS tLine2D
      DIM b(3) AS tLine2D

      LOCAL la, lb AS tLine2D

      LOCAL retval AS LONG
      retval = 0

      IF internal_SpriteCollided_Circle2CircleInBox(pSpriteA  , pSpriteB)=0 THEN
        FUNCTION = 0
        EXIT FUNCTION
      END IF


      ' -- Sprite A
      ap1.x = @pSpriteA.x1c + @pSpriteA.xPos
      ap1.y = @pSpriteA.y1c + @pSpriteA.yPos

      ap2.x = @pSpriteA.x2c + @pSpriteA.xPos
      ap2.y = @pSpriteA.y2c + @pSpriteA.yPos

      ap3.x = @pSpriteA.x3c + @pSpriteA.xPos
      ap3.y = @pSpriteA.y3c + @pSpriteA.yPos

      ap4.x = @pSpriteA.x4c + @pSpriteA.xPos
      ap4.y = @pSpriteA.y4c + @pSpriteA.yPos


      ' -- Sprite B
      bp1.x = @pSpriteB.x1c + @pSpriteB.xPos
      bp1.y = @pSpriteB.y1c + @pSpriteB.yPos

      bp2.x = @pSpriteB.x2c + @pSpriteB.xPos
      bp2.y = @pSpriteB.y2c + @pSpriteB.yPos

      bp3.x = @pSpriteB.x3c + @pSpriteB.xPos
      bp3.y = @pSpriteB.y3c + @pSpriteB.yPos

      bp4.x = @pSpriteB.x4c + @pSpriteB.xPos
      bp4.y = @pSpriteB.y4c + @pSpriteB.yPos


      ' -- Test for corners inside first - faster
      IF internal_SpriteCollided_IsIn(bp1, ap1, ap2, ap4) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_IsIn(bp2, ap1, ap2, ap4) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_IsIn(bp3, ap1, ap2, ap4) THEN ReturnAndExit(2)
      IF internal_SpriteCollided_IsIn(bp4, ap1, ap2, ap4) THEN ReturnAndExit(2)

      IF internal_SpriteCollided_IsIn(ap1, bp1, bp2, bp4) THEN ReturnAndExit(3)
      IF internal_SpriteCollided_IsIn(ap2, bp1, bp2, bp4) THEN ReturnAndExit(3)
      IF internal_SpriteCollided_IsIn(ap3, bp1, bp2, bp4) THEN ReturnAndExit(3)
      IF internal_SpriteCollided_IsIn(ap4, bp1, bp2, bp4) THEN ReturnAndExit(3)


      ' -- Getting pointers
      a1p1 = VARPTR(a(0).p1)
      a1p2 = VARPTR(a(0).p2)

      a2p1 = VARPTR(a(1).p1)
      a2p2 = VARPTR(a(1).p2)

      a3p1 = VARPTR(a(2).p1)
      a3p2 = VARPTR(a(2).p2)

      a4p1 = VARPTR(a(3).p1)
      a4p2 = VARPTR(a(3).p2)

      b1p1 = VARPTR(b(0).p1)
      b1p2 = VARPTR(b(0).p2)

      b2p1 = VARPTR(b(1).p1)
      b2p2 = VARPTR(b(1).p2)

      b3p1 = VARPTR(b(2).p1)
      b3p2 = VARPTR(b(2).p2)

      b4p1 = VARPTR(b(3).p1)
      b4p2 = VARPTR(b(3).p2)

      ' -- Box A
      @a1p1.x = ap1.x
      @a1p1.y = ap1.y
      @a1p2.x = ap2.x
      @a1p2.y = ap2.y

      @a2p1.x = ap2.x
      @a2p1.y = ap2.y
      @a2p2.x = ap3.x
      @a2p2.y = ap3.y

      @a3p1.x = ap3.x
      @a3p1.y = ap3.y
      @a3p2.x = ap4.x
      @a3p2.y = ap4.y

      @a4p1.x = ap4.x
      @a4p1.y = ap4.y
      @a4p2.x = ap1.x
      @a4p2.y = ap1.y

      ' -- Box B
      @b1p1.x = bp1.x
      @b1p1.y = bp1.y
      @b1p2.x = bp2.x
      @b1p2.y = bp2.y

      @b2p1.x = bp2.x
      @b2p1.y = bp2.y
      @b2p2.x = bp3.x
      @b2p2.y = bp3.y

      @b3p1.x = bp3.x
      @b3p1.y = bp3.y
      @b3p2.x = bp4.x
      @b3p2.y = bp4.y

      @b4p1.x = bp4.x
      @b4p1.y = bp4.y
      @b4p2.x = bp1.x
      @b4p2.y = bp1.y

      ' -- Test for edge intersection

      FOR i = 0 TO 3
        FOR j = 0 TO 3
          la = a(i)
          lb = b(j)
          IF internal_SpriteCollided_CrossDistance(la, lb, 0) <> -1 THEN
            ReturnAndExit(4)
            EXIT FOR

          END IF
        NEXT
      NEXT

      FUNCTION = retval

  END FUNCTION


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteCheckMouse() AS EXT
  '*****************************************************************
      LOCAL nParsed AS LONG
      LOCAL pSpriteA AS TBGL_tSprite PTR
      LOCAL sp, ex, ey, ebutton AS EXT
      LOCAL retval AS EXT
      LOCAL ibutton AS LONG

      LOCAL mouse AS POINTAPI
      LOCAL Win AS RECT                     ' Canvas
      ebutton = 0
      nParsed = thinBasic_ParseXNumbers(1,2, sp, ebutton)
      pSpriteA = sp
      GetCursorPos mouse
      ibutton = ebutton

      IF g_Win.controlParent = 0 THEN         ' Window
          ScreenToClient( g_Win.HANDLE, mouse )
      ELSE
          GetWindowRect( g_Win.HANDLE, Win )
          mouse.y = mouse.y - win.ntop
          mouse.x = mouse.x - win.nLeft
      END IF

      retval = 0
      IF MouseIsInClient THEN
          ex = mouse.x
          ey = mouse.y
          IF @pSpriteA.isActive <> %FALSE THEN
              meSprite = 0
              youSprite = 0
              IF MouseIsInClient THEN
                ibutton = IIF( GetAsyncKeyState( ibutton ), ibutton, 0 )
              END IF
              IF @pSpriteA.iColType = %sctRect THEN
                IF internal_SpriteCollided_PointInsidePolygon(pSpriteA,ex,ey) THEN Mouse_ReturnAndExit(2)
              ELSEIF @pSpriteA.iColType = %sctBoundBox THEN
                IF internal_SpriteCollided_PointInsidePolygon(pSpriteA,ex,ey) THEN Mouse_ReturnAndExit(2)
              ELSEIF @pSpriteA.iColType = %sctCircle THEN
                IF internal_SpriteCollided_PointInsideCircle(pSpriteA, ex, ey) THEN Mouse_ReturnAndExit(2)
              END IF
          END IF
          internal_DeleteSprites()
      END IF
      FUNCTION = retval
  END FUNCTION

  '*****************************************************************
  FUNCTION Exec_TBGL_SpritesCheckMouseAll() AS EXT
  '*****************************************************************
      LOCAL i AS LONG
      LOCAL nParsed AS LONG
      LOCAL pSpriteA AS TBGL_tSprite PTR
      LOCAL ex, ey, ebutton AS EXT
      LOCAL retval AS EXT
      LOCAL ibutton AS LONG

      LOCAL mouse AS POINTAPI
      LOCAL Win AS RECT                     ' Canvas
      ebutton = 0
      nParsed = thinBasic_ParseXNumbers(0,1, ebutton)

      GetCursorPos mouse
      ibutton = ebutton

      IF g_Win.controlParent = 0 THEN         ' Window
          ScreenToClient( g_Win.HANDLE, mouse )
      ELSE
          GetWindowRect( g_Win.HANDLE, Win )
          mouse.y = mouse.y - win.ntop
          mouse.x = mouse.x - win.nLeft
      END IF

      retval = 0
      IF MouseIsInClient THEN
          ex = mouse.x
          ey = mouse.y
          IF MouseIsInClient AND ibutton <> 0 THEN
              ibutton = IIF( GetAsyncKeyState( ibutton ), ibutton, 0 )
          END IF
          FOR i = 1 TO maxsprites
                'IF sprites(i) <> 0 AND sprites(i) <> pSpriteA THEN
              IF sprites(i)  AND sprites(i) <> pSpriteA THEN
                  pSpriteA = sprites(i)
                  IF @pSpriteA.isActive = %FALSE THEN ITERATE FOR
                  meSprite = 0
                  youSprite = 0

                  IF @pSpriteA.iColType = %sctRect THEN
                    retval = internal_SpriteCollided_PointInsidePolygon(pSpriteA,ex,ey)
                  ELSEIF @pSpriteA.iColType = %sctBoundBox THEN
                    retval = internal_SpriteCollided_PointInsidePolygon(pSpriteA,ex,ey)
                  ELSEIF @pSpriteA.iColType = %sctCircle THEN
                    retval = internal_SpriteCollided_PointInsideCircle(pSpriteA, ex, ey)
                  END IF
                  IF retval <> 0 THEN internal_CallSpriteEvent(pSpriteA, ibutton, %TBGL_OnMouseOver)
              END IF
          NEXT
          internal_DeleteSprites()
      END IF
      FUNCTION = retval
  END FUNCTION

  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteCollided() AS EXT
  '*****************************************************************
      LOCAL i AS LONG
      LOCAL nParsed AS LONG
      LOCAL sp,sp2 AS EXT
      LOCAL pSpriteA AS TBGL_tSprite PTR
      LOCAL pSpriteB AS TBGL_tSprite PTR
      LOCAL retval AS EXT

      nParsed = thinBasic_ParseXNumbers(1,2, sp, sp2)

      pSpriteA = sp
      retval = 0



      IF @pSpriteA.isActive <> %FALSE THEN
          youSprite = 0
          IF nParsed = 2 THEN
            pSpriteB = sp2
            IF @pSpriteA.iColWith(@pSpriteB.iColGroup)=%False THEN EXIT FUNCTION

            IF @pSpriteB.isActive = %FALSE THEN
                retval = 0
            ELSE
                IF @pSpriteA.iColType = %sctRect THEN

                  IF @pSpriteB.iColType = %sctRect THEN
                    retval = internal_SpriteCollided_Box2Box(pSpriteA, pSpriteB)
                  ELSEIF @pSpriteB.iColType = %sctCircle THEN
                    retval = internal_SpriteCollided_Box2Circle(pSpriteA, pSpriteB)
                  ELSEIF @pSpriteB.iColType = %sctBoundBox THEN
                    retval = internal_SpriteCollided_Bound2Bound(pSpriteA, pSpriteB)
                  END IF

                ELSEIF @pSpriteA.iColType = %sctCircle THEN

                  IF @pSpriteB.iColType = %sctCircle THEN
                    retval = internal_SpriteCollided_Circle2Circle(pSpriteA, pSpriteB)
                  ELSEIF @pSpriteB.iColType = %sctRect THEN
                    retval = internal_SpriteCollided_Circle2Box(pSpriteA, pSpriteB)
                  ELSEIF @pSpriteB.iColType = %sctBoundBox THEN
                    retval = internal_SpriteCollided_Bound2Bound(pSpriteA, pSpriteB)
                  END IF

                ELSEIF @pSpriteA.iColType = %sctBoundBox THEN

                  retval = internal_SpriteCollided_Bound2Bound(pSpriteA, pSpriteB)

                END IF

            END IF
          ELSE
            FOR i = 1 TO maxsprites
                IF sprites(i)  AND sprites(i) <> pSpriteA THEN
                    pSpriteB = sprites(i)
                    IF @pSpriteA.iColWith(@pSpriteB.iColGroup)=%False THEN ITERATE FOR

                    IF @pSpriteB.isActive = %FALSE THEN ITERATE FOR

                  IF @pSpriteA.iColType = %sctRect THEN

                    IF @pSpriteB.iColType = %sctRect THEN
                      retval = internal_SpriteCollided_Box2Box(pSpriteA, pSpriteB)
                    ELSEIF @pSpriteB.iColType = %sctCircle THEN
                      retval = internal_SpriteCollided_Box2Circle(pSpriteA, pSpriteB)
                    ELSEIF @pSpriteB.iColType = %sctBoundBox THEN
                      retval = internal_SpriteCollided_Bound2Bound(pSpriteA, pSpriteB)
                    END IF

                  ELSEIF @pSpriteA.iColType = %sctCircle THEN

                    IF @pSpriteB.iColType = %sctCircle THEN
                      retval = internal_SpriteCollided_Circle2Circle(pSpriteA, pSpriteB)
                    ELSEIF @pSpriteB.iColType = %sctRect THEN
                      retval = internal_SpriteCollided_Circle2Box(pSpriteA, pSpriteB)
                    ELSEIF @pSpriteB.iColType = %sctBoundBox THEN
                      retval = internal_SpriteCollided_Bound2Bound(pSpriteA, pSpriteB)
                    END IF

                  ELSEIF @pSpriteA.iColType = %sctBoundBox THEN

                    retval = internal_SpriteCollided_Bound2Bound(pSpriteA, pSpriteB)
                  END IF
                END IF
                IF retval AND @pSpriteA.iColMode THEN EXIT FOR
            NEXT

          END IF
      END IF
      internal_DeleteSprites()
      FUNCTION = retval
  END FUNCTION



  '*****************************************************************
  SUB Exec_TBGL_SpriteSetUserData()
  '*****************************************************************

    LOCAL sp AS EXT
    LOCAL VariablePtr   AS LONG
    LOCAL MainType      AS LONG
    LOCAL SubType       AS LONG
    LOCAL ElementsAreFixed AS LONG
    LOCAL TotElements   AS LONG
    LOCAL ElementSize   AS LONG
    LOCAL DataPtr       AS LONG
    LOCAL AbsPos        AS LONG

    LOCAL sBuffer AS STRING
    LOCAL pSprite AS TBGL_tSprite PTR

    IF thinBasic_CheckOpenParens THEN
      thinBasic_ParseNumber sp
      pSprite = sp
      IF thinBasic_CheckComma_Mandatory THEN

        thinBasic_ParseVariableInfo( VariablePtr, MainType, SubType, ElementsAreFixed, TotElements, ElementSize, DataPtr, AbsPos )
        IF thinBasic_CheckCloseParens_Mandatory THEN

          ' -- We mark STRINGs differentely for STRPTR/VARPTR purposes
          IF ElementsAreFixed = %FALSE AND MainType = %VarMainType_IsString THEN
            LOCAL p AS STRING PTR
            p = DataPtr
            sBuffer = LEFT$(@p, LEN(@p))
          ELSE
            ' -- 1234 is magic number which helps to differentiate STRING for numbers and ASCIIZ
            sBuffer = PEEK$( DataPtr, ElementSize )+MKL$(1234)
          END IF
          SpriteUserdata(@pSprite.UserPointer) = sBuffer

        END IF
      END IF
    END IF

  END SUB


  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetUserDataPointer() AS EXT
  '*****************************************************************

      LOCAL sp AS EXT
      LOCAL pSprite AS TBGL_tSprite PTR
      sp = thinBasic_Parse1Number
      pSprite = sp


      IF RIGHT$(SpriteUserdata(@pSprite.userpointer), 4) = MKL$(1234) THEN
        FUNCTION = STRPTR(SpriteUserdata(@pSprite.userpointer))
      ELSE
        FUNCTION = VARPTR(SpriteUserdata(@pSprite.userpointer))
      END IF

  END FUNCTION

  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteGetUserDataID() AS EXT
  '*****************************************************************
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number()
    pSprite = sp
    FUNCTION = @pSprite.userpointer
  END FUNCTION

  '*****************************************************************
  FUNCTION Exec_TBGL_SpriteExists() AS EXT
  '*****************************************************************
    REGISTER i AS LONG
    LOCAL pSprite AS TBGL_tSprite PTR
    LOCAL sp AS EXT

    sp = thinBasic_Parse1Number()

    IF sp < lbound(sprites) or sp > ubound(sprites) THEN
      FUNCTION = %FALSE
      EXIT FUNCTION
    END IF

    ARRAY SCAN sprites(1), = sp, TO i

    FUNCTION = IIF(i > 0, %TRUE, %FALSE)

  END FUNCTION

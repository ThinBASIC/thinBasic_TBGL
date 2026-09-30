'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles materials
' =============================================================================
'
' -----------------------------------------------------------------------------

' >RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES
TYPE TBGL_tMaterial
  diffuse(3)        AS SINGLE
  ambient(3)        AS SINGLE
  specular(3)       AS SINGLE
  specularExponent  AS SINGLE
  emission(3)       AS SINGLE

  textureID(1 TO 4) AS LONG     ' -- Prepared for multitexturing

  ' -- Restoration flags
  rollBack_Texturing  AS LONG
END TYPE

GLOBAL g_MaterialHandles AS STRING
GLOBAL g_DefaultMaterial AS TBGL_tMaterial

SUB resource_Materials_Alloc()
  g_MaterialHandles = ""

  g_DefaultMaterial.diffuse(0) = 1!
  g_DefaultMaterial.diffuse(1) = 1!
  g_DefaultMaterial.diffuse(2) = 1!
  g_DefaultMaterial.diffuse(3) = 1!

  g_DefaultMaterial.ambient(0) = 1!
  g_DefaultMaterial.ambient(1) = 1!
  g_DefaultMaterial.ambient(2) = 1!
  g_DefaultMaterial.ambient(3) = 1!

  g_DefaultMaterial.specular(0)= 0!
  g_DefaultMaterial.specular(1)= 0!
  g_DefaultMaterial.specular(2)= 0!
  g_DefaultMaterial.specular(3)= 1!

  g_DefaultMaterial.specularExponent  = 0!

  g_DefaultMaterial.emission(0)= 0!
  g_DefaultMaterial.emission(1)= 0!
  g_DefaultMaterial.emission(2)= 0!
  g_DefaultMaterial.emission(3)= 1!

  g_DefaultMaterial.textureID(1)  = 0
  g_DefaultMaterial.textureID(2)  = 0
  g_DefaultMaterial.textureID(3)  = 0
  g_DefaultMaterial.textureID(4)  = 0
END SUB

SUB resource_Materials_Release(OPTIONAL BYVAL specific AS TBGL_HANDLE) ' -- IF TBGL_HANDLE = 0 -> all, otherwise just the one specified
  REGISTER i AS LONG

  LOCAL numMaterials AS LONG
  LOCAL hRes AS LONG

  numMaterials = LEN(g_MaterialHandles) / TBGL_HANDLE_SIZE

  DIM MaterialHandles(1 TO numMaterials) AS TBGL_HANDLE AT STRPTR(g_MaterialHandles)

  IF specific = 0 THEN

    FOR i = 1 TO numMaterials
      Mem_Free(MaterialHandles(i))
    NEXT

    g_MaterialHandles = ""
  ELSE

    FOR i = 1 TO numMaterials
      IF MaterialHandles(i) = specific THEN
        Mem_FREE(MaterialHandles(i))
        ARRAY DELETE MaterialHandles(i), 0
        g_MaterialHandles = LEFT$(g_MaterialHandles, LEN(g_MaterialHandles)-TBGL_HANDLE_SIZE)
        EXIT FOR
      END IF
    NEXT

  END IF

END SUB
' <RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES - RESOURCES

' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

FUNCTION tbgl_NewMaterial ALIAS "tbgl_NewMaterial" ( ) EXPORT AS LONG
  LOCAL mHandle AS TBGL_HANDLE

  mHandle = Mem_Alloc(SIZEOF(g_DefaultMaterial))

  g_MaterialHandles += MKL$(mHandle)

  DIM material(0) AS TBGL_tMaterial AT mHandle

  material(0).diffuse(0) = 1
  material(0).diffuse(1) = 1
  material(0).diffuse(2) = 1
  material(0).diffuse(3) = 1

  material(0).ambient(0) = 1
  material(0).ambient(1) = 1
  material(0).ambient(2) = 1
  material(0).ambient(3) = 1

  material(0).specular(0)= 0
  material(0).specular(1)= 0
  material(0).specular(2)= 0
  material(0).specular(3)= 1

  material(0).specularExponent  = 0

  material(0).emission(0)= 0
  material(0).emission(1)= 0
  material(0).emission(2)= 0
  material(0).emission(3)= 1

  material(0).textureID(1)  = 0
  material(0).textureID(2)  = 0
  material(0).textureID(3)  = 0
  material(0).textureID(4)  = 0

  FUNCTION = mHandle

END FUNCTION

SUB tbgl_DeleteMaterial ALIAS "tbgl_DeleteMaterial" ( BYVAL mHandle AS LONG ) EXPORT

  resource_Materials_Release(mHandle)

END SUB

' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

SUB tbgl_PushMaterial ALIAS "tbgl_PushMaterial" ( BYVAL mHandle AS LONG ) EXPORT
  glDisable(%GL_COLOR_MATERIAL)

  DIM material(0) AS TBGL_tMaterial AT mHandle

  glMaterialfv(%GL_FRONT_AND_BACK, %GL_DIFFUSE  , material(0).diffuse(0))
  glMaterialfv(%GL_FRONT_AND_BACK, %GL_AMBIENT  , material(0).ambient(0))
  glMaterialfv(%GL_FRONT_AND_BACK, %GL_SPECULAR , material(0).specular(0))
  glMaterialf (%GL_FRONT_AND_BACK, %GL_SHININESS, material(0).specularExponent)
  glMaterialfv(%GL_FRONT_AND_BACK, %GL_EMISSION , material(0).emission(0))

  IF material(0).textureID(1) THEN
    IF glIsEnabled(%GL_TEXTURE_2D) = 0 THEN
      material(0).rollBack_Texturing = %TRUE
      glEnable(%GL_TEXTURE_2D)
    END IF

    glBindTexture(%GL_TEXTURE_2D, lpTexture(material(0).textureID(1)))
  END IF
END SUB


' -- mHandle is not used, default material is restored
SUB tbgl_PopMaterial ALIAS "tbgl_PopMaterial" ( BYVAL mHandle AS LONG ) EXPORT

  DIM material(0) AS TBGL_tMaterial AT VARPTR(g_DefaultMaterial)

  glMaterialfv(%GL_FRONT_AND_BACK, %GL_DIFFUSE  , material(0).diffuse(0))
  glMaterialfv(%GL_FRONT_AND_BACK, %GL_AMBIENT  , material(0).ambient(0))
  glMaterialfv(%GL_FRONT_AND_BACK, %GL_SPECULAR , material(0).specular(0))
  glMaterialf (%GL_FRONT_AND_BACK, %GL_SHININESS, material(0).specularExponent)
  glMaterialfv(%GL_FRONT_AND_BACK, %GL_EMISSION , material(0).emission(0))

  glEnable(%GL_COLOR_MATERIAL)

  IF material(0).rollBack_Texturing = %TRUE THEN
    material(0).rollBack_Texturing = %FALSE
    glDisable(%GL_TEXTURE_2D)
  END IF
END SUB

' ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

' -- Which color of the material internal_SetMaterialColorF sets
%TBGL_MATCOLOR_DIFFUSE  = 0
%TBGL_MATCOLOR_AMBIENT  = 1
%TBGL_MATCOLOR_SPECULAR = 2
%TBGL_MATCOLOR_EMISSION = 3

' -- Components in 0..1, not clamped (thinBasic keywords pass script values / 255 unrounded)
SUB internal_SetMaterialColorF( BYVAL mHandle AS LONG, BYVAL component AS LONG, BYVAL r AS SINGLE, BYVAL g AS SINGLE, BYVAL b AS SINGLE, BYVAL a AS SINGLE )
  LOCAL pColor AS SINGLE PTR

  DIM material(0) AS TBGL_tMaterial AT mHandle

  SELECT CASE AS LONG component
    CASE %TBGL_MATCOLOR_DIFFUSE  : pColor = VARPTR(material(0).diffuse(0))
    CASE %TBGL_MATCOLOR_AMBIENT  : pColor = VARPTR(material(0).ambient(0))
    CASE %TBGL_MATCOLOR_SPECULAR : pColor = VARPTR(material(0).specular(0))
    CASE %TBGL_MATCOLOR_EMISSION : pColor = VARPTR(material(0).emission(0))
    CASE ELSE : EXIT SUB
  END SELECT

  @pColor[0] = r
  @pColor[1] = g
  @pColor[2] = b
  @pColor[3] = a

END SUB

SUB tbgl_SetMaterialDiffuse ALIAS "tbgl_SetMaterialDiffuse" ( BYVAL mHandle AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE ) EXPORT

  internal_SetMaterialColorF( mHandle, %TBGL_MATCOLOR_DIFFUSE, r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB tbgl_SetMaterialAmbient ALIAS "tbgl_SetMaterialAmbient" ( BYVAL mHandle AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE ) EXPORT

  internal_SetMaterialColorF( mHandle, %TBGL_MATCOLOR_AMBIENT, r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB tbgl_SetMaterialSpecular ALIAS "tbgl_SetMaterialSpecular" ( BYVAL mHandle AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE ) EXPORT

  internal_SetMaterialColorF( mHandle, %TBGL_MATCOLOR_SPECULAR, r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB tbgl_SetMaterialSpecularExponent ALIAS "tbgl_SetMaterialSpecularExponent" ( BYVAL mHandle AS LONG, BYVAL e AS SINGLE ) EXPORT

  DIM material(0) AS TBGL_tMaterial AT mHandle

  material(0).specularExponent = e

END SUB

SUB tbgl_SetMaterialEmission ALIAS "tbgl_SetMaterialEmission" ( BYVAL mHandle AS LONG, BYVAL r AS BYTE, BYVAL g AS BYTE, BYVAL b AS BYTE, BYVAL a AS BYTE ) EXPORT

  internal_SetMaterialColorF( mHandle, %TBGL_MATCOLOR_EMISSION, r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB tbgl_SetMaterialTexture ALIAS "tbgl_SetMaterialTexture" ( BYVAL mHandle AS LONG, BYVAL textureID AS LONG ) EXPORT  ' -- ToDo second parameter for multitexturing

  DIM material(0) AS TBGL_tMaterial AT mHandle

  material(0).textureID(1) = textureID

END SUB

' -----------------------------------------------------------------------------
' -- thinBasic keyword parsers
' -----------------------------------------------------------------------------
#IF %TBGL_THINBASIC_INTERPRETER_INTERFACE

FUNCTION Exec_TBGL_NewMaterial() AS EXT
  FUNCTION = tbgl_NewMaterial( )
END FUNCTION

SUB Exec_TBGL_DeleteMaterial()
  tbgl_DeleteMaterial( thinbasic_Parse1Number( ))
END SUB

SUB Exec_TBGL_PushMaterial()
  tbgl_PushMaterial( thinbasic_Parse1Number( ))
END SUB

SUB Exec_TBGL_PopMaterial()
  tbgl_PopMaterial( thinbasic_Parse1Number( ))
END SUB

' -- Alpha defaults to 0 when omitted
SUB Exec_TBGL_SetMaterialDiffuse()
  LOCAL eHandle, r, g, b, a AS EXT

  thinBasic_ParseXNumbers(4, 5, eHandle, r, g, b, a)
  internal_SetMaterialColorF( eHandle, %TBGL_MATCOLOR_DIFFUSE, r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB Exec_TBGL_SetMaterialAmbient()
  LOCAL eHandle, r, g, b, a AS EXT

  thinBasic_ParseXNumbers(4, 5, eHandle, r, g, b, a)
  internal_SetMaterialColorF( eHandle, %TBGL_MATCOLOR_AMBIENT, r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB Exec_TBGL_SetMaterialSpecular()
  LOCAL eHandle, r, g, b, a AS EXT

  thinBasic_ParseXNumbers(4, 5, eHandle, r, g, b, a)
  internal_SetMaterialColorF( eHandle, %TBGL_MATCOLOR_SPECULAR, r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB Exec_TBGL_SetMaterialSpecularExponent()
  LOCAL eHandle, e AS EXT

  thinBasic_Parse2Numbers(eHandle, e)
  tbgl_SetMaterialSpecularExponent( eHandle, e )

END SUB

SUB Exec_TBGL_SetMaterialEmission()
  LOCAL eHandle, r, g, b, a AS EXT

  thinBasic_ParseXNumbers(4, 5, eHandle, r, g, b, a)
  internal_SetMaterialColorF( eHandle, %TBGL_MATCOLOR_EMISSION, r / 255, g / 255, b / 255, a / 255 )

END SUB

SUB Exec_TBGL_SetMaterialTexture()
  LOCAL eHandle, e AS EXT

  thinBasic_Parse2Numbers(eHandle, e)
  tbgl_SetMaterialTexture( eHandle, e )

END SUB

#ENDIF

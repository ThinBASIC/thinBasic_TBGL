' =============================================================================
'  Part of     : thinBASIC_TBGL
'
'  Description : thinBasic module for 3D graphics
' =============================================================================
'  About part  : Class to handle M15 files
' =============================================================================
'
' -----------------------------------------------------------------------------

TYPE tModel_m15_VertexInfo
  TexXY(1)  AS SINGLE  ' [!] 0 to 1
  clr(2)    AS BYTE    ' [!] 0 to 2
  vtx(2)    AS SINGLE  ' [!] 0 to 2
  n(2)      AS SINGLE  ' [!] 0 TO 2

  pStop     AS BYTE    ' polygon stop flag
  TexN      AS LONG    ' texture number
  Layer     AS LONG    ' layer number
END TYPE

CLASS cModel_m15
  INSTANCE VertexData() AS tModel_m15_VertexInfo
  INSTANCE modelHandle AS DWORD
  INSTANCE flag_compileToDisplayList AS LONG
  INSTANCE flag_normalMethod AS LONG

  CLASS METHOD CREATE()
    DIM VertexData(1 TO 1)

    flag_compileToDisplayList = 0
    flag_normalMethod         = 3
  END METHOD

  INTERFACE iModel_m15
    INHERIT IUNKNOWN
    PROPERTY SET CompileToDisplayList( nValue AS LONG )
      flag_compileToDisplayList = IIF(nValue = 0, 0, -1)
    END PROPERTY

    METHOD LOAD( fName AS STRING, sDirTextures AS STRING, normalMethod AS LONG)
    END METHOD

    METHOD DRAW()
    END METHOD

    METHOD RecalcNormals( normalMethod AS LONG, layerID AS LONG )
    END METHOD



  END INTERFACE

END CLASS

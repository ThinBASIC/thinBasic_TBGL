'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include handles spheres, torus and so on
' =============================================================================
'
' [TODO]: Would be nice to make all primitives backface culling proof (box is not for sure)
' [TODO]: Would be nice to make all primitives vertex array way
'
' -----------------------------------------------------------------------------

' -- Useful macros
MACRO pi      = 3.14159265358##
MACRO twopi   = 6.28318530716##
MACRO halfpi  = 1.57079632679##

' -- Primitive quality, use internal_setPrimitiveQuality to set its value
GLOBAL g_primitiveQuality AS LONG

' -- Sphere is the only one managed via precalculated corrdinates, others should follow soon
GLOBAL auxSphere_NormalArray( ) AS SINGLE
GLOBAL auxSphere_UVArray( ) AS SINGLE

' -----------------------------------------------------------------------------

' -- Just assigns g_primitiveQuality in valid range
SUB Exec_TBGL_SetPrimitiveQuality( )
  LOCAL x AS EXT

  x = thinBasic_Parse1Number

  internal_SetPrimitiveQuality( x )

END SUB

' -- Renders box
SUB Exec_TBGL_Box( )
  LOCAL x, y, z AS EXT

  thinBasic_Parse3Numbers( x, y, z )
  internal_RenderBox( x, y, z )
END SUB

' -- Renders cylinder
SUB Exec_TBGL_Cylinder( )
  LOCAL lowR AS EXT, hiR AS EXT, height AS EXT
  thinBasic_Parse3Numbers( lowR, hiR, height )

  internal_RenderCylinder( lowR, hiR, height, 0 )

END SUB

' -- Renders ... sphere
SUB Exec_TBGL_Sphere( )
  LOCAL radius AS EXT
  radius = thinBasic_Parse1Number

  internal_RenderSphere( radius )

END SUB

' -- Torus rendering
SUB Exec_TBGL_Torus( )
  LOCAL minR, maxR AS EXT

  thinBasic_Parse2Numbers( minR, maxR )

  internal_RenderTorus( minR, maxR )

END SUB

' =============================================================================

' -- Sets primitive quality, and recalcs sphere data (it is really advantageous to do it here)
SUB internal_SetPrimitiveQuality( BYVAL PrimitiveQuality AS LONG )

  IF PrimitiveQuality < 3 THEN PrimitiveQuality = 3    ' -- To prevent fatal value ... ehm

  g_primitiveQuality = PrimitiveQuality

  ' -- NEW in 0.2.2 - precache data for primitives
  REGISTER i AS LONG, j AS LONG
  LOCAL n AS LONG
  n = g_primitiveQuality
  IF n MOD 2 <> 0 THEN INCR n

  ' [>] SYNC needed with RenderSphere
  REDIM auxSphere_NormalArray( 1 TO (( n \ 2 ) * ( n + 1 ) * 3 * 2 )) AS GLOBAL SINGLE
  REDIM auxSphere_UVArray( 1 TO (( n \ 2 ) * ( n + 1 ) * 3 )) AS GLOBAL SINGLE
  LOCAL theta1, theta2, theta3 AS SINGLE
  LOCAL IndexPointer, IndexPointerUV AS LONG
  LOCAL jn, jn1 AS SINGLE
  IndexPointer = 1
  IndexPointerUV = 1
  FOR j = 0 TO n \ 2 - 1
    theta1 = j * twopi / n - halfpi
    theta2 = ( j + 1 ) * twopi / n - halfpi
    jn = j / n
    jn1 = ( j + 1 ) / n
    FOR i = 0 TO n
      theta3 = i * twopi / n

      auxSphere_NormalArray( IndexPointer ) = - COS( theta2 ) * COS( theta3 )
      auxSphere_NormalArray( IndexPointer + 1 ) = - SIN( theta2 )
      auxSphere_NormalArray( IndexPointer + 2 ) = - COS( theta2 ) * SIN( theta3 )

      auxSphere_NormalArray( IndexPointer + 3 ) = - COS( theta1 ) * COS( theta3 )
      auxSphere_NormalArray( IndexPointer + 4 ) = - SIN( theta1 )
      auxSphere_NormalArray( IndexPointer + 5 ) = - COS( theta1 ) * SIN( theta3 )

      auxSphere_UVArray( IndexPointerUV ) = - i / n
      auxSphere_UVArray( IndexPointerUV + 1 ) = jn1
      auxSphere_UVArray( IndexPointerUV + 2 ) = jn

      IndexPointer = IndexPointer + 6
      IndexPointerUV = IndexPointerUV + 3

    NEXT i
  NEXT j

END SUB

' --
TYPE TBGL_tFaceParameters
  R       AS BYTE
  G       AS BYTE
  B       AS BYTE
  Texture AS LONG
END TYPE

' -- [!] Renders box via immediate mode, Vertex Array optimization would be nicer/faster
SUB internal_RenderBox( BYVAL x AS EXT, BYVAL y AS EXT, BYVAL z AS EXT )
  x /= 2
  y /= 2
  z /= 2
  #IF 1
    ' -- Immediate mode
    glBegin %GL_QUADS
      ' Front Face
      glNormal3f 0.0!, 0.0!, 1.0!
      glTexCoord2f 0.0!, 0.0! : glVertex3f -x, -y, z
      glTexCoord2f x   , 0.0! : glVertex3f  x, -y, z
      glTexCoord2f x   , y    : glVertex3f  x,  y, z
      glTexCoord2f 0.0!, y    : glVertex3f -x,  y, z
      ' Back Face
      glNormal3f 0.0!, 0.0!, -1.0!
      glTexCoord2f x   , 0.0! : glVertex3f -x, -y, -z
      glTexCoord2f x   , y    : glVertex3f -x,  y, -z
      glTexCoord2f 0.0!, y    : glVertex3f  x,  y, -z
      glTexCoord2f 0.0!, 0.0! : glVertex3f  x, -y, -z
      ' Top Face
      glNormal3f 0.0!, 1.0!, 0.0!
      glTexCoord2f 0.0!, z    : glVertex3f -x, y, -z
      glTexCoord2f 0.0!, 0.0! : glVertex3f -x, y,  z
      glTexCoord2f x   , 0.0! : glVertex3f  x, y,  z
      glTexCoord2f x   , z    : glVertex3f  x, y, -z
      ' Bottom Face
      glNormal3f 0.0!, -1.0!, 0.0!
      glTexCoord2f x   , z    : glVertex3f -x, -y, -z
      glTexCoord2f 0.0!, z    : glVertex3f  x, -y, -z
      glTexCoord2f 0.0!, 0.0! : glVertex3f  x, -y,  z
      glTexCoord2f x   , 0.0! : glVertex3f -x, -y,  z
      ' Right face
      glNormal3f 1.0!, 0.0!, 0.0!
      glTexCoord2f z   , 0.0! : glVertex3f x, -y, -z
      glTexCoord2f z   , y    : glVertex3f x,  y, -z
      glTexCoord2f 0.0!, y    : glVertex3f x,  y,  z
      glTexCoord2f 0.0!, 0.0! : glVertex3f x, -y,  z
      ' Left Face
      glNormal3f -1.0!, 0.0!, 0.0!
      glTexCoord2f 0.0!, 0.0! : glVertex3f -x, -y, -z
      glTexCoord2f z   , 0.0! : glVertex3f -x, -y,  z
      glTexCoord2f z   , y    : glVertex3f -x,  y,  z
      glTexCoord2f 0.0!, y    : glVertex3f -x,  y, -z
    glEnd

  #ELSE
    ' -- Vertex array ... terribly slow for now, must do it differentely, maybe I will leave it IM
    ' 72 = 24 * 3
    ' 48 = 24 * 2
    DIM auxBox_CoordArray(1 TO 72) AS SINGLE
    DIM auxBox_UVArray(1 TO 48) AS SINGLE
    DIM auxBox_NormalArray(1 TO 72) AS SINGLE

    ARRAY ASSIGN auxBox_CoordArray() =  -x, -y, z, _
                                            x, -y, z, _
                                            x,  y, z, _
                                           -x,  y, z, _
                                           -x, -y, -z, _
                                           -x,  y, -z, _
                                            x,  y, -z, _
                                            x, -y, -z, _
                                           -x, y, -z, _
                                           -x, y,  z, _
                                            x, y,  z, _
                                            x, y, -z, _
                                           -x, -y, -z, _
                                            x, -y, -z, _
                                            x, -y,  z, _
                                           -x, -y,  z, _
                                           x, -y, -z, _
                                           x,  y, -z, _
                                           x,  y,  z, _
                                           x, -y,  z, _
                                           -x, -y, -z, _
                                           -x, -y,  z, _
                                           -x,  y,  z, _
                                           -x,  y, -z


    ARRAY ASSIGN auxBox_UVArray() =   0.0!, 0.0! , _
                                      x   , 0.0! , _
                                      x   , y    , _
                                      0.0!, y    , _
                                      x   , 0.0! , _
                                      x   , y    , _
                                      0.0!, y    , _
                                      0.0!, 0.0! , _
                                      0.0!, z    , _
                                      0.0!, 0.0! , _
                                      x   , 0.0! , _
                                      x   , z    , _
                                      x   , z    , _
                                      0.0!, z    , _
                                      0.0!, 0.0! , _
                                      x   , 0.0! , _
                                      z   , 0.0! , _
                                      z   , y    , _
                                      0.0!, y    , _
                                      0.0!, 0.0! , _
                                      0.0!, 0.0! , _
                                      z   , 0.0! , _
                                      z   , y    , _
                                      0.0!, y

   ARRAY ASSIGN auxBox_NormalArray() =  0,0,1,0,0,1,0,0,1,0,0,1, _
                                        0,0,-1,0,0,-1,0,0,-1,0,0,-1, _
                                        0,1,0,0,1,0,0,1,0,0,1,0, _
                                        0,-1,0,0,-1,0,0,-1,0,0,-1,0, _
                                        1,0,0,1,0,0,1,0,0,1,0,0, _
                                        -1,0,0,-1,0,0,-1,0,0,-1,0,0



    glEnableClientState( %GL_NORMAL_ARRAY )
    glNormalPointer  (%GL_FLOAT, 0, auxBox_NormalArray(1)     )

    glEnableClientState( %GL_TEXTURE_COORD_ARRAY )
    glTexCoordPointer( 2, %GL_FLOAT, 0, auxBox_UVArray(1) )

    glEnableClientState( %GL_VERTEX_ARRAY )
    glVertexPointer  ( 3, %GL_FLOAT, 0, auxBox_CoordArray(1) )

    glDrawArrays(%GL_QUADS, 0, 24)

    glDisableClientState( %GL_VERTEX_ARRAY )
    glDisableClientState( %GL_TEXTURE_COORD_ARRAY )
    glDisableClientState( %GL_NORMAL_ARRAY )

  #ENDIF

END SUB

' -- [!] Renders cylinder via immediate mode, Vertex Array would mean significant boost
SUB internal_RenderCylinder( BYVAL lowR AS EXT, BYVAL hiR AS EXT, BYVAL height AS EXT, capped AS LONG )
  REGISTER i AS EXT
  LOCAL n AS LONG, px AS SINGLE, pz AS SINGLE, x AS SINGLE, y AS SINGLE, z AS SINGLE
  LOCAL j AS LONG
  LOCAL theta1, theta2, theta3 AS SINGLE

  n = g_primitiveQuality    '24

  j = (( n / 2 ) - 2 ) / 2

  theta1 = j * twopi / n - halfpi
  theta2 = ( j + 1 ) * twopi / n - halfpi
  glBegin %GL_QUAD_STRIP
  FOR i = 0 TO n

    theta3 = i * twopi / n

    x = COS( theta2 ) * COS( theta3 )
    y = SIN( theta2 )
    z = COS( theta2 ) * SIN( theta3 )
    px = lowR * x
    pz = lowR * z
    glNormal3f x, y, z
    glTexCoord2f - theta3 * lowR, 0.0!

    glVertex3f px, 0.0!, pz

    x = COS( theta2 ) * COS( theta3 )    ' theta1
    y = SIN( theta2 )
    z = COS( theta2 ) * SIN( theta3 )    ' theta1
    px = hiR * x
    pz = hiR * z
    glNormal3f x, y, z

    glTexCoord2f - theta3 * lowR, height
    glVertex3f px, height, pz
  NEXT i
  glEnd    'GL_QUAD_STRIP

  IF capped THEN
    theta1 = j * twopi / n - halfpi
    theta2 = ( j + 1 ) * twopi / n - halfpi
    glBegin %GL_TRIANGLE_FAN
    glNormal3f 0.0!, - 1.0!, 0.0!
    glTexCoord2f 0, 0
    glVertex3f 0.0!, 0.0!, 0.0!
    FOR i = 0 TO n

      theta3 = i * twopi / n

      x = COS( theta2 ) * COS( theta3 )
      y = SIN( theta2 )
      z = COS( theta2 ) * SIN( theta3 )
      px = lowR * x
      pz = lowR * z

      glTexCoord2f px, pz
      glVertex3f px, 0.0!, pz

    NEXT i
    glEnd

    theta1 = j * twopi / n - halfpi
    theta2 = ( j + 1 ) * twopi / n - halfpi
    glBegin %GL_TRIANGLE_FAN
    glNormal3f 0.0!, 1.0!, 0.0!
    glTexCoord2f 0, 0
    glVertex3f 0.0!, height, 0.0!
    FOR i = n TO 0 STEP - 1

      theta3 = i * twopi / n

      x = COS( theta2 ) * COS( theta3 )
      y = SIN( theta2 )
      z = COS( theta2 ) * SIN( theta3 )
      px = hiR * x
      pz = hiR * z

      glTexCoord2f px, pz
      glVertex3f px, height, pz

    NEXT i
    glEnd

  END IF

END SUB

%RENDER_SPHERE = 1 ' 1 = faster, 2 = should be faster, it is not :D
' -- Rendering sphere via vectorised immediate mode,
SUB internal_RenderSphere( BYVAL radius AS EXT )

  #IF ( %RENDER_SPHERE = 1 )
    REGISTER i AS LONG, j AS LONG
    LOCAL theta1, theta2, theta3 AS SINGLE

    LOCAL n AS LONG
    LOCAL Diameter AS SINGLE
    LOCAL IndexPointer, IndexPointerUV AS LONG
    IndexPointer = 1
    IndexPointerUV = 1
    Diameter = Radius * 2


    DIM t( 0 TO 1 ) AS LOCAL SINGLE
    DIM v( 0 TO 2 ) AS LOCAL SINGLE
    n = g_primitiveQuality
    IF n MOD 2 <> 0 THEN INCR n
    'Draw Quad-Strip part of sphere
    FOR j = 0 TO n \ 2 - 1
      glBegin %GL_QUAD_STRIP

      FOR i = 0 TO n
        ARRAY ASSIGN t( ) = auxSphere_UVArray( IndexPointerUV ) * Radius, auxSphere_UVArray( IndexPointerUV + 1 ) * Diameter
        ARRAY ASSIGN v( ) = - Radius * auxSphere_NormalArray( IndexPointer ), - Radius * auxSphere_NormalArray( IndexPointer + 1 ), - Radius * auxSphere_NormalArray( IndexPointer + 2 )
        glNormal3fV( auxSphere_NormalArray( IndexPointer ))
        glTexCoord2fV( t( 0 ))
        glVertex3fV( v( 0 ))

        ' -- Because 0 stays the same
        t( 1 ) = auxSphere_UVArray( IndexPointerUV + 2 ) * Diameter
        ARRAY ASSIGN v( ) = - Radius * auxSphere_NormalArray( IndexPointer + 3 ), - Radius * auxSphere_NormalArray( IndexPointer + 4 ), - Radius * auxSphere_NormalArray( IndexPointer + 5 )
        glNormal3fV( auxSphere_NormalArray( IndexPointer + 3 ))
        glTexCoord2fV( t( 0 ))
        glVertex3fV( v( 0 ))

        IndexPointer = IndexPointer + 6
        IndexPointerUV = IndexPointerUV + 3
      NEXT i
      glEnd    'GL_QUAD_STRIP
    NEXT j

  #ELSEIF ( %RENDER_SPHERE = 2 )
    REGISTER i AS LONG, j AS LONG
    LOCAL theta1, theta2, theta3 AS SINGLE
    LOCAL n AS LONG
    LOCAL Diameter AS SINGLE
    Diameter = Radius * 2

    DIM n( 0 TO 4 ) AS LOCAL SINGLE
    DIM t( 0 TO 2 ) AS LOCAL SINGLE
    DIM v( 0 TO 2 ) AS LOCAL SINGLE

    n = g_primitiveQuality
    IF n MOD 2 <> 0 THEN INCR n
    'Draw Quad-Strip part of sphere
    FOR j = 0 TO n \ 2 - 1
      theta1 = j * twopi / n - halfpi
      theta2 = ( j + 1 ) * twopi / n - halfpi
      glBegin %GL_QUAD_STRIP
      FOR i = 0 TO n
        theta3 = i * twopi / n

        ARRAY ASSIGN n( ) = - COS( theta2 ) * COS( theta3 ), - SIN( theta2 ), - COS( theta2 ) * SIN( theta3 )
        ARRAY ASSIGN t( ) = - i / n * Radius,( j + 1 ) / n * Diameter
        ARRAY ASSIGN v( ) = - Radius * n( 0 ), - Radius * n( 1 ), - Radius * n( 2 )
        glNormal3fV( n( 0 ))
        glTexCoord2fV( t( 0 ))
        glVertex3fV( v( 0 ))

        ARRAY ASSIGN n( ) = - COS( theta1 ) * COS( theta3 ), - SIN( theta1 ), - COS( theta1 ) * SIN( theta3 )
        ARRAY ASSIGN t( ) = - i / n * Radius, j / n * Diameter
        ARRAY ASSIGN v( ) = - Radius * n( 0 ), - Radius * n( 1 ), - Radius * n( 2 )

        glNormal3fV( n( 0 ))
        glTexCoord2fV( t( 0 ))
        glVertex3fV( v( 0 ))
      NEXT i
      glEnd    'GL_QUAD_STRIP
    NEXT j
  #ENDIF
END SUB

' -- [!] Torus, again it would deserve optimization, also UVs are little odd now

SUB internal_RenderTorus( BYVAL minR AS EXT, BYVAL maxR AS EXT )
  REGISTER stacks AS LONG, slices AS LONG

  LOCAL wrapFrac, phi, stMod, stMod1, sinphi, cosphi, r AS SINGLE
  LOCAL n AS LONG

  LOCAL MaxStacks AS LONG, MaxSlices AS LONG

  n = g_primitiveQuality
  IF n MOD 2 <> 0 THEN INCR n

  MaxStacks = n
  MaxSlices = n - 1

  LOCAL CenterLength AS SINGLE
  LOCAL CyLength AS SINGLE
  CenterLength = twopi * maxR
  CyLength = twopi * minR

  'Start Quad-Strip
  glBegin %GL_QUAD_STRIP
  'Stacks
  FOR stacks = 0 TO MaxStacks
    'Slices
    FOR slices = 0 TO MaxSlices
      wrapFrac = ( slices MOD MaxStacks ) / MaxStacks
      phi = twopi * wrapFrac
      sinphi = SIN( phi )
      cosphi = COS( phi )
      r = maxR + ( minR * cosphi )
      stMod = twopi * ( stacks MOD MaxStacks + wrapFrac ) / MaxStacks
      stMod1 = twopi * (( stacks + 1 ) MOD MaxStacks + wrapFrac ) / MaxStacks

      glNormal3f( SIN( stMod ) * cosphi, sinphi, COS( stMod ) * cosphi )
      glTexCoord2f SIN( stMod ) * r, COS( stMod ) * r
      glVertex3f( SIN( stMod ) * r, minR * sinphi, COS( stMod ) * r )

      glNormal3f( SIN( stMod1 ) * cosphi, sinphi, COS( stMod1 ) * cosphi )
      '
      glTexCoord2f SIN( stMod1 ) * r, COS( stMod1 ) * r
      glVertex3f( SIN( stMod1 ) * r, minR * sinphi, COS( stMod1 ) * r )

    NEXT slices

  NEXT stacks
  glEnd    'GL_QUAD_STRIP

END SUB

'/*++ BUILD Version: 0004    // Increment this if a change has global effects

'Copyright (c) 1985-95, Microsoft Corporation

'Module Name:

'    glu.h

'Abstract:

'    Procedure declarations, constant definitions and macros for the OpenGL
'    Utility Library.

'--*/

#IF NOT %DEF(%GLU_INC)
    %GLU_INC = 1

#INCLUDE ONCE "GL.INC"

'/*
'** Copyright 1991-1993, Silicon Graphics, Inc.
'** All Rights Reserved.
'**
'** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
'** the contents of this file may not be disclosed to third parties, copied or
'** duplicated in any form, in whole or in part, without the prior written
'** permission of Silicon Graphics, Inc.
'**
'** RESTRICTED RIGHTS LEGEND:
'** Use, duplication or disclosure by the Government is subject to restrictions
'** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
'** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
'** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
'** rights reserved under the Copyright Laws of the United States.
'*/

'/*
'** Return the error string associated with a particular error code.
'** This will return 0 for an invalid error code.
'**
'** The generic function prototype that can be compiled for ANSI or Unicode
'** is defined as follows:
'**
'** LPCTSTR APIENTRY gluErrorStringWIN (GLenum errCode);
'*/

' ========================================================================================
' const GLubyte* APIENTRY gluErrorString (GLenum errCode);
' The gluErrorString function produces an error string from an OpenGL or GLU error code.
' The error string is ANSI only.
' ========================================================================================
DECLARE FUNCTION gluErrorString LIB "GLU32.DLL" ALIAS "gluErrorString" ( _
   BYVAL errCode AS DWORD _                             ' GLEnum errCode
 ) AS DWORD                                             ' *GLubyte

' ========================================================================================
' const wchar_t* APIENTRY gluErrorUnicodeStringEXT (GLenum errCode);
' The gluErrorString function produces an error string from an OpenGL or GLU error code.
' The error string is Unicode.
' ========================================================================================
DECLARE FUNCTION gluErrorUnicodeStringEXT LIB "GLU32.DLL" ALIAS "gluErrorUnicodeStringEXT" ( _
   BYVAL errCode AS DWORD _                             ' GLEnum errCode
 ) AS DWORD                                             ' *wchar_t

' ========================================================================================
' const GLubyte* APIENTRY gluGetString (GLenum name);
' Gets a string that describes the GLU version number or supported GLU extension calls.
' ========================================================================================
DECLARE FUNCTION gluGetString LIB "GLU32.DLL" ALIAS "gluGetString" ( _
   BYVAL name AS DWORD _                                ' GLEnum name
 ) AS DWORD                                             ' *GLubyte

' ========================================================================================
' void APIENTRY gluOrtho2D (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top);
' Defines a 2-D orthographic projection matrix.
' ========================================================================================
DECLARE SUB gluOrtho2D LIB "GLU32.DLL" ALIAS "gluOrtho2D" ( _
   BYVAL left AS DOUBLE _                               ' GLdouble left
 , BYVAL right AS DOUBLE _                              ' GLdouble right
 , BYVAL bottom AS DOUBLE _                             ' GLdouble bottom
 , BYVAL top AS DOUBLE _                                ' GLdouble top
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluPerspective (GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar);
' ========================================================================================
DECLARE SUB gluPerspective LIB "GLU32.DLL" ALIAS "gluPerspective" ( _
   BYVAL fovy AS DOUBLE _                               ' GLdouble fovy
 , BYVAL aspect AS DOUBLE _                             ' GLdouble aspect
 , BYVAL zNear AS DOUBLE _                              ' GLdouble zNear
 , BYVAL zFar AS DOUBLE _                               ' GLdouble zFar
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluPickMatrix (GLdouble x, GLdouble y, GLdouble width, GLdouble height,
' GLint viewport[4]);
' Defines a picking region.
' ========================================================================================
DECLARE SUB gluPickMatrix LIB "GLU32.DLL" ALIAS "gluPickMatrix" ( _
   BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 , BYVAL width AS DOUBLE _                              ' GLdouble width
 , BYVAL height AS DOUBLE _                             ' GLdouble height
 , BYREF viewport AS LONG _                             ' GLint viewport[4]
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluLookAt (GLdouble eyex, GLdouble eyey, GLdouble eyez, GLdouble centerx,
' GLdouble centery, GLdouble centerz, GLdouble upx, GLdouble upy, GLdouble upz);
' Defines a viewing transformation.
' ========================================================================================
DECLARE SUB gluLookAt LIB "GLU32.DLL" ALIAS "gluLookAt" ( _
   BYVAL eyex AS DOUBLE _                               ' GLdouble eyex
 , BYVAL eyey AS DOUBLE _                               ' GLdouble eyey
 , BYVAL eyez AS DOUBLE _                               ' GLdouble eyez
 , BYVAL centerx AS DOUBLE _                            ' GLdouble centerx
 , BYVAL centery AS DOUBLE _                            ' GLdouble centery
 , BYVAL centerz AS DOUBLE _                            ' GLdouble centerz
 , BYVAL upx AS DOUBLE _                                ' GLdouble upx
 , BYVAL upy AS DOUBLE _                                ' GLdouble upy
 , BYVAL upz AS DOUBLE _                                ' GLdouble upz
 )                                                      ' void

' ========================================================================================
' int APIENTRY gluProject (GLdouble objx, GLdouble objy, GLdouble objz,
' const GLdouble modelMatrix[16], const GLdouble projMatrix[16], const GLint viewport[4],
' GLdouble *winx, GLdouble *winy, GLdouble *winz);
' The gluProject function maps object coordinates to window coordinates.
' ========================================================================================
DECLARE FUNCTION gluProject LIB "GLU32.DLL" ALIAS "gluProject" ( _
   BYVAL objx AS DOUBLE _                               ' GLdouble objx
 , BYVAL objy AS DOUBLE _                               ' GLdouble objy
 , BYVAL objz AS DOUBLE _                               ' GLdouble objz
 , BYREF modelMatrix AS DOUBLE _                        ' const GLdouble modelMatrix[16]
 , BYREF projMatrix AS DOUBLE _                         ' const GLdouble projMatrix[16]
 , BYREF viewport AS LONG _                             ' const GLint viewport[4]
 , BYREF winx AS DOUBLE _                               ' GLdouble *winx
 , BYREF winy AS DOUBLE _                               ' GLdouble *winy
 , BYREF winz AS DOUBLE _                               ' GLdouble *winz
 ) AS LONG                                              ' int

' ========================================================================================
' int APIENTRY gluUnProject (GLdouble winx, GLdouble winy, GLdouble winz,
' const GLdouble modelMatrix[16], const GLdouble projMatrix[16], const GLint viewport[4],
' GLdouble *objx, GLdouble *objy, GLdouble *objz);
' Maps window coordinates to object coordinates.
' ========================================================================================
DECLARE FUNCTION gluUnProject LIB "GLU32.DLL" ALIAS "gluUnProject" ( _
   BYVAL winx AS DOUBLE _                               ' GLdouble winx
 , BYVAL winy AS DOUBLE _                               ' GLdouble winy
 , BYVAL winz AS DOUBLE _                               ' GLdouble winz
 , BYREF modelMatrix AS DOUBLE _                        ' const GLdouble modelMatrix[16]
 , BYREF projMatrix AS DOUBLE _                         ' const GLdouble projMatrix[16]
 , BYREF viewport AS LONG _                             ' const GLint viewport[4]
 , BYREF objx AS DOUBLE _                               ' GLdouble *objx
 , BYREF objy AS DOUBLE _                               ' GLdouble *objy
 , BYREF objz AS DOUBLE _                               ' GLdouble *objz
 ) AS LONG                                              ' int

' ========================================================================================
' int APIENTRY gluScaleImage (GLenum format, GLint widthin, GLint heightin, GLenum typein,
' const void *datain, GLint widthout, GLint heightout, GLenum typeout, void *dataout);
' Scales an image to an arbitrary size.
' ========================================================================================
DECLARE FUNCTION gluScaleImage LIB "GLU32.DLL" ALIAS "gluScaleImage" ( _
   BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL widthin AS LONG _                              ' GLint widthin
 , BYVAL heightin AS LONG _                             ' GLint heightin
 , BYVAL typein AS DWORD _                              ' GLEnum typein
 , BYREF datain AS ANY _                                ' const void *datain
 , BYVAL widthout AS LONG _                             ' GLint widthout
 , BYVAL heightout AS LONG _                            ' GLint heightout
 , BYVAL typeout AS DWORD _                             ' GLEnum typeout
 , BYREF dataout AS ANY _                               ' void *dataout
 ) AS LONG                                              ' int

' ========================================================================================
' int APIENTRY gluBuild1DMipmaps (GLenum target, GLint components, GLint width,
' GLEnum format, GLenum type, const void *data);
' Creates 1-D mipmaps.
' ========================================================================================
DECLARE FUNCTION gluBuild1DMipmaps LIB "GLU32.DLL" ALIAS "gluBuild1DMipmaps" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL components AS LONG _                           ' GLint components
 , BYVAL width AS LONG _                                ' GLint width
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF data AS ANY _                                  ' const void *data
 ) AS LONG                                              ' int

' ========================================================================================
' int APIENTRY gluBuild2DMipmaps (GLenum target, GLint components, GLint width,
' GLint height, GLenum format, GLenum type, const void *data);
' Creates 2-D mipmaps.
' ========================================================================================
DECLARE FUNCTION gluBuild2DMipmaps LIB "GLU32.DLL" ALIAS "gluBuild2DMipmaps" ( _
   BYVAL target AS DWORD _                              ' GLEnum target
 , BYVAL components AS LONG _                           ' GLint components
 , BYVAL width AS LONG _                                ' GLint width
 , BYVAL height AS LONG _                               ' GLint height
 , BYVAL format AS DWORD _                              ' GLEnum format
 , BYVAL type AS DWORD _                                ' GLEnum type
 , BYREF data AS ANY _                                  ' const void *data
 ) AS LONG                                              ' int

' ========================================================================================
' GLUquadric* APIENTRY gluNewQuadric (void);
' Creates a quadric object.
' ========================================================================================
DECLARE FUNCTION gluNewQuadric LIB "GLU32.DLL" ALIAS "gluNewQuadric" ( _
 ) AS DWORD                                             ' *GLUquadric

' ========================================================================================
' void APIENTRY gluDeleteQuadric (GLUquadric *state);
' Destroys a quadric object.
' ========================================================================================
DECLARE SUB gluDeleteQuadric LIB "GLU32.DLL" ALIAS "gluDeleteQuadric" ( _
   BYVAL state AS DWORD _                               ' GLUquadric *state
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluQuadricNormals (GLUquadric *quadObject, GLenum normals);
' Specifies what kind of normals are to be used for quadrics.
' ========================================================================================
DECLARE SUB gluQuadricNormals LIB "GLU32.DLL" ALIAS "gluQuadricNormals" ( _
   BYVAL quadObject AS DWORD _                          ' GLUquadric *quadObject
 , BYVAL normals AS DWORD _                             ' GLEnum normals
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluQuadricTexture (GLUquadric *quadObject, GLboolean textureCoords);
' Specifies whether quadrics are to be textured.
' ========================================================================================
DECLARE SUB gluQuadricTexture LIB "GLU32.DLL" ALIAS "gluQuadricTexture" ( _
   BYVAL quadObject AS DWORD _                          ' GLUquadric *quadObject
 , BYVAL textureCoords AS BYTE _                        ' GLboolean textureCoords
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluQuadricOrientation (GLUquadric *quadObject, GLenum orientation);
' Specifies inside or outside orientation for quadrics.
' ========================================================================================
DECLARE SUB gluQuadricOrientation LIB "GLU32.DLL" ALIAS "gluQuadricOrientation" ( _
   BYVAL quadObject AS DWORD _                          ' GLUquadric *quadObject
 , BYVAL orientation AS DWORD _                         ' GLEnum orientation
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluQuadricDrawStyle (GLUquadric *quadObject, GLenum drawStyle);
' Specifies the draw style desired for quadrics.
' ========================================================================================
DECLARE SUB gluQuadricDrawStyle LIB "GLU32.DLL" ALIAS "gluQuadricDrawStyle" ( _
   BYVAL quadObject AS DWORD _                          ' GLUquadric *quadObject
 , BYVAL drawStyle AS DWORD _                           ' GLEnum drawStyle
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluCylinder (GLUquadric *qobj, GLdouble baseRadius, GLdouble topRadius,
' GLdouble height, GLint slices, GLint stacks);
' Draws a cylinder.
' ========================================================================================
DECLARE SUB gluCylinder LIB "GLU32.DLL" ALIAS "gluCylinder" ( _
   BYVAL qobj AS DWORD _                                ' GLUquadric *qobj
 , BYVAL baseRadius AS DOUBLE _                         ' GLdouble baseRadius
 , BYVAL topRadius AS DOUBLE _                          ' GLdouble topRadius
 , BYVAL height AS DOUBLE _                             ' GLdouble height
 , BYVAL slices AS LONG _                               ' GLint slices
 , BYVAL stacks AS LONG _                               ' GLint stacks
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluDisk (GLUquadric *qobj, GLdouble innerRadius, GLdouble outerRadius,
' GLint slices, GLint loops);
' Draws a disk.
' ========================================================================================
DECLARE SUB gluDisk LIB "GLU32.DLL" ALIAS "gluDisk" ( _
   BYVAL qobj AS DWORD _                                ' GLUquadric *qobj
 , BYVAL innerRadius AS DOUBLE _                        ' GLdouble innerRadius
 , BYVAL outerRadius AS DOUBLE _                        ' GLdouble outerRadius
 , BYVAL slices AS LONG _                               ' GLint slices
 , BYVAL loops AS LONG _                                ' GLint loops
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluPartialDisk (GLUquadric *qobj, GLdouble innerRadius, GLdouble outerRadius,
' GLint slices, GLint loops, GLdouble startAngle, GLdouble sweepAngle);
' Draws an arc of a disk.
' ========================================================================================
DECLARE SUB gluPartialDisk LIB "GLU32.DLL" ALIAS "gluPartialDisk" ( _
   BYVAL qobj AS DWORD _                                ' GLUquadric *qobj
 , BYVAL innerRadius AS DOUBLE _                        ' GLdouble innerRadius
 , BYVAL outerRadius AS DOUBLE _                        ' GLdouble outerRadius
 , BYVAL slices AS LONG _                               ' GLint slices
 , BYVAL loops AS LONG _                                ' GLint loops
 , BYVAL startAngle AS DOUBLE _                         ' GLdouble startAngle
 , BYVAL sweepAngle AS DOUBLE _                         ' GLdouble sweepAngle
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluSphere (GLUquadric *qobj, GLdouble radius, GLint slices, GLint stacks);
' Draws a sphere.
' ========================================================================================
DECLARE SUB gluSphere LIB "GLU32.DLL" ALIAS "gluSphere" ( _
   BYVAL qobj AS DWORD _                                ' GLUquadric *qobj
 , BYVAL radius AS DOUBLE _                             ' GLdouble radius
 , BYVAL slices AS LONG _                               ' GLint slices
 , BYVAL satcks AS LONG _                               ' GLint satcks
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluQuadricCallback (GLUquadric *qobj, GLenum which, void (CALLBACK* fn)());
' Defines a callback for a quadric object.
' ========================================================================================
'SUB gluQuadricCallback ( _
'   BYVAL qobj AS DWORD _                                ' GLUquadric *qobj
' , BYVAL which AS DWORD _                               ' GLEnum which
' , BYVAL fn AS DWORD _                                  ' void (CALLBACK* fn)()
' )                                                      ' void

' ========================================================================================
' GLUtesselator* APIENTRY gluNewTess(void );
' Creates a tessellation object.
' ========================================================================================
DECLARE FUNCTION gluNewTess LIB "GLU32.DLL" ALIAS "gluNewTess" ( _
 ) AS DWORD                                             ' *GLUtesselator

' ========================================================================================
' void APIENTRY gluDeleteTess(GLUtesselator *tess );
' Destroys a tessellation object.
' ========================================================================================
DECLARE SUB gluDeleteTess LIB "GLU32.DLL" ALIAS "gluDeleteTess" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluTessBeginPolygon(GLUtesselator *tess, void *polygon_data );
' Delimit a polygon description.
' ========================================================================================
DECLARE SUB gluTessBeginPolygon LIB "GLU32.DLL" ALIAS "gluTessBeginPolygon" ( _
   BYVAL tess AS DWORD  _                               ' GLUtesselator *tess
 , BYREF polygon_data AS ANY _                          ' void *polygon_data
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluTessBeginContour(GLUtesselator *tess );
' Delimits a contour description.
' ========================================================================================
DECLARE SUB gluTessBeginContour LIB "GLU32.DLL" ALIAS "gluTessBeginContour" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluTessVertex(GLUtesselator *tess, GLdouble coords[3], void *data);
' Specifies a vertex on a polygon.
' ========================================================================================
DECLARE SUB gluTessVertex LIB "GLU32.DLL" ALIAS "gluTessVertex" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 , BYREF coords AS DOUBLE _                             ' GLdouble coords[3]
 , BYREF data AS ANY _                                  ' void *data
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluTessEndContour(GLUtesselator *tess );
' Delimits a contour description.
' ========================================================================================
DECLARE SUB gluTessEndContour LIB "GLU32.DLL" ALIAS "gluTessEndContour" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator * tess
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluTessEndPolygon(GLUtesselator *tess );
' Delimits a polygon description.
' ========================================================================================
DECLARE SUB gluTessEndPolygon LIB "GLU32.DLL" ALIAS "gluTessEndPolygon" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluTessProperty(GLUtesselator *tess, GLenum which, GLdouble value);
' Sets the property of a tessellation object.
' ========================================================================================
DECLARE SUB gluTessProperty LIB "GLU32.DLL" ALIAS "gluTessProperty" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 , BYVAL which AS DWORD _                               ' GLEnum which
 , BYVAL value AS DOUBLE _                              ' GLdouble value
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluTessNormal(GLUtesselator *tess, GLdouble x, GLdouble y, GLdouble z);
' Specifies a normal for a polygon.
' ========================================================================================
DECLARE SUB gluTessNormal LIB "GLU32.DLL" ALIAS "gluTessNormal" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 , BYVAL x AS DOUBLE _                                  ' GLdouble x
 , BYVAL y AS DOUBLE _                                  ' GLdouble y
 , BYVAL z AS DOUBLE _                                  ' GLdouble z
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluTessCallback(GLUtesselator *tess, GLenum which, void (CALLBACK *fn)());
' Defines a callback for a tessellation object.
' ========================================================================================
'SUB gluTessCallback ( _
'   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
' , BYVAL which AS DWORD  _                              ' GLEnum which
' , BYVAL fn AS DWORD _                                  ' void (CALLBACK *fn)()
' )                                                      ' void

' ========================================================================================
' void APIENTRY gluGetTessProperty(GLUtesselator *tess, GLenum which, GLdouble *value );
' Sets the property of a tessellation object.
' ========================================================================================
DECLARE SUB gluGetTessProperty LIB "GLU32.DLL" ALIAS "gluGetTessProperty" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 , BYVAL which AS DWORD _                               ' GLEnum which
 , BYREF value AS DOUBLE _                              ' GLdouble *value
 )                                                      ' void

' ========================================================================================
' GLUnurbs* APIENTRY gluNewNurbsRenderer (void);
' Creates a NURBS object.
' ========================================================================================
DECLARE FUNCTION gluNewNurbsRenderer LIB "GLU32.DLL" ALIAS "gluNewNurbsRenderer" ( _
 ) AS DWORD                                             ' *GLUnurbs

' ========================================================================================
' void APIENTRY gluDeleteNurbsRenderer (GLUnurbs *nobj);
' Destroys a NURBS object.
' ========================================================================================
DECLARE SUB gluDeleteNurbsRenderer LIB "GLU32.DLL" ALIAS "gluDeleteNurbsRenderer" ( _
   BYVAL nObj AS DWORD _                                ' GLUnurbs *nObj
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluBeginSurface (GLUnurbs *nobj);
' Delimits a NURBS surface definition.
' ========================================================================================
DECLARE SUB gluBeginSurface LIB "GLU32.DLL" ALIAS "gluBeginSurface" ( _
   BYVAL nObj AS DWORD _                                ' GLUnurbs *nObj
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluBeginCurve (GLUnurbs *nobj);
' Delimits a Non-Uniform Rational B-Spline (NURBS) curve definition.
' ========================================================================================
DECLARE SUB gluBeginCurve LIB "GLU32.DLL" ALIAS "gluBeginCurve" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluEndCurve (GLUnurbs *nobj);
' Delimits a Non-Uniform Rational B-Spline (NURBS) curve definition.
' ========================================================================================
DECLARE SUB gluEndCurve LIB "GLU32.DLL" ALIAS "gluEndCurve" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluEndSurface (GLUnurbs *nobj);
' Delimits a NURBS surface definition.
' ========================================================================================
DECLARE SUB gluEndSurface LIB "GLU32.DLL" ALIAS "gluEndSurface" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluBeginTrim (GLUnurbs *nobj);
' Delimits a NURBS trimming loop definition.
' ========================================================================================
DECLARE SUB gluBeginTrim LIB "GLU32.DLL" ALIAS "gluBeginTrim" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluEndTrim (GLUnurbs *nobj);
' Delimits a NURBS trimming loop definition.
' ========================================================================================
DECLARE SUB gluEndTrim LIB "GLU32.DLL" ALIAS "gluEndTrim" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluPwlCurve (GLUnurbs *nobj, GLint count, GLfloat *array, GLint stride, GLenum type);
' Describes a piecewise linear NURBS trimming curve.
' ========================================================================================
DECLARE SUB gluPwlCurve LIB "GLU32.DLL" ALIAS "gluPwlCurve" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 , BYVAL count AS LONG _                                ' GLint count
 , BYREF array AS SINGLE _                              ' GLfloat *array
 , BYVAL stride AS LONG _                               ' GLint stride
 , BYVAL type AS DWORD _                                ' GLEnum type
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluNurbsCurve (GLUnurbs *nobj, GLint nknots, GLfloat *knot, GLint stride,
' GLfloat *ctlarray, GLint order, GLenum type);
' Defines the shape of a NURBS curve.
' ========================================================================================
DECLARE SUB gluNurbsCurve LIB "GLU32.DLL" ALIAS "gluNurbsCurve" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 , BYVAL nknots AS LONG _                               ' GLint nknots
 , BYREF knot AS SINGLE _                               ' GLfloat *knot
 , BYVAL stride AS LONG _                               ' GLint stride
 , BYREF ctlarray AS SINGLE _                           ' GLfloat *ctlarray
 , BYVAL order AS LONG _                                ' GLint order
 , BYVAL type AS DWORD _                                ' GLEnum type
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluNurbsSurface(GLUnurbs *nobj, GLint sknot_count, GLfloat *sknot,
' GLint tknot_count, GLfloat *tknot, GLint s_stride, GLint t_stride, GLfloat *ctlarray,
' GLint sorder, GLint torder, GLenum type);
' Defines the shape of a NURBS surface.
' ========================================================================================
DECLARE SUB gluNurbsSurface LIB "GLU32.DLL" ALIAS "gluNurbsSurface" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 , BYVAL sknot_count AS LONG _                          ' GLint sknot_count
 , BYREF sknot AS SINGLE _                              ' GLfloat *sknot
 , BYVAL tknot_count AS LONG _                          ' GLint tknot_count
 , BYREF tknot AS SINGLE _                              ' GLfloat tknot
 , BYVAL s_stride AS LONG _                             ' GLint s_stride
 , BYVAL t_stride AS LONG _                             ' GLint t_stride
 , BYREF ctlarray AS SINGLE _                           ' GLfloat ctlarray
 , BYVAL sorder AS LONG _                               ' GLint sorder
 , BYVAL torder AS LONG _                               ' GLint torder
 , BYVAL type AS DWORD _                                ' GLEnum type
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluLoadSamplingMatrices (GLUnurbs *nobj, const GLfloat modelMatrix[16],
' const GLfloat projMatrix[16], const GLint viewport[4] );
' Loads NURBS sampling and culling matrices.
' ========================================================================================
DECLARE SUB gluLoadSamplingMatrices LIB "GLU32.DLL" ALIAS "gluLoadSamplingMatrices" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 , BYREF modelMatrix AS SINGLE _                        ' const GLfloat modelMatrix[16]
 , BYREF projMatrix AS SINGLE _                         ' const GLfloat projMatrix[16]
 , BYREF viewport AS LONG _                             ' const GLint viewport[4]
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluNurbsProperty (GLUnurbs *nobj, GLenum property, GLfloat value );
' Sets a NURBS property.
' ========================================================================================
DECLARE SUB gluNurbsProperty LIB "GLU32.DLL" ALIAS "gluNurbsProperty" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 , BYVAL property AS DWORD _                            ' GLEnum property
 , BYVAL value AS SINGLE _                              ' GLfloat value
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluGetNurbsProperty (GLUnurbs *nobj, GLenum property, GLfloat *value );
' Gets a NURBS property.
' ========================================================================================
DECLARE SUB gluGetNurbsProperty LIB "GLU32.DLL" ALIAS "gluGetNurbsProperty" ( _
   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
 , BYVAL property AS DWORD _                            ' GLEnum property
 , BYREF value AS SINGLE _                              ' GLfloat *value
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluNurbsCallback (GLUnurbs *nobj, GLenum which, void (CALLBACK* fn)() );
' Defines a callback for a NURBS object.
' ========================================================================================
'SUB gluNurbsCallback ( _
'   BYVAL nobj AS DWORD _                                ' GLUnurbs *nobj
' , BYVAL which AS DWORD _                               ' GLEnum which
' , BYVAL fn AS DWORD _                                  ' void (CALLBACK* fn)()
' )                                                      ' void

'/****           Callback function prototypes    ****/

'/* gluQuadricCallback */
'typedef void (CALLBACK* GLUquadricErrorProc) (GLenum);
'SUB GLUquadricErrorProc(BYVAL DWORD)

'/* gluTessCallback */
'typedef void (CALLBACK* GLUtessBeginProc)        (GLenum);
' SUB GLUtessBeginProc (BYVAL DWORD)

'typedef void (CALLBACK* GLUtessEdgeFlagProc)     (GLboolean);
'SUB GLUtessEdgeFlagProc (BYVAL BYTE)

'typedef void (CALLBACK* GLUtessVertexProc)       (void *);
'SUB GLUtessVertexProc (BYVAL DWORD)

'typedef void (CALLBACK* GLUtessEndProc)          (void);
'SUB GLUtessEndProc (BYVAL DWORD)

'typedef void (CALLBACK* GLUtessErrorProc)        (GLenum);
'SUB GLUtessErrorProc (BYVAL DWORD)

'typedef void (CALLBACK* GLUtessCombineProc)      (GLdouble[3],
'                                                  void*[4],
'                                                  GLfloat[4],
'                                                  void** );
' SUB GLUtessCombineProc (BYREF DOUBLE, BYREF DWORD, BYREF SINGLE, BYREF DWORD)

'typedef void (CALLBACK* GLUtessBeginDataProc)    (GLenum, void *);
'SUB GLUtessBeginDataProc (BYVAL DWORD, BYVAL DWORD)

'typedef void (CALLBACK* GLUtessEdgeFlagDataProc) (GLboolean, void *);
'SUB GLUtessEdgeFlagDataProc (BYVAL BYTE, BYVAL DWORD)

'typedef void (CALLBACK* GLUtessVertexDataProc)   (void *, void *);
'SUB GLUtessVertexDataProc (BYVAL DWORD, BYVAL DWORD)

'typedef void (CALLBACK* GLUtessEndDataProc)      (void *);
'SUB GLUtessEndDataProc (BYVAL DWORD)

'typedef void (CALLBACK* GLUtessErrorDataProc)    (GLenum, void *);
'SUB GLUtessErrorDataProc (BYVAL DWORD, BYVAL DWORD)

'typedef void (CALLBACK* GLUtessCombineDataProc)  (GLdouble[3],
'                                                  void*[4],
'                                                  GLfloat[4],
'                                                  void**,
'                                                  void* );
'SUB GLUtessCombineDataProc (BYREF DOUBLE, BYREF DWORD, BYREF SINGLE, BYREF DWORD, BYVAL DWORD)

'/* gluNurbsCallback */
'typedef void (CALLBACK* GLUnurbsErrorProc)   (GLenum);
' SUB GLUnurbsErrorProc (BYVAL DWORD)


'/****           Generic constants               ****/

'/* Version */
%GLU_VERSION_1_1             = 1
%GLU_VERSION_1_2             = 1

'/* Errors: (return value 0 = no error) */
%GLU_INVALID_ENUM            = 100900
%GLU_INVALID_VALUE           = 100901
%GLU_OUT_OF_MEMORY           = 100902
%GLU_INCOMPATIBLE_GL_VERSION = 100903

'/* StringName */
%GLU_VERSION                 = 100800
%GLU_EXTENSIONS              = 100801

'/* Boolean */
%GLU_TRUE                    = %GL_TRUE
%GLU_FALSE                   = %GL_FALSE


'/****           Quadric constants               ****/

'/* QuadricNormal */
%GLU_SMOOTH             = 100000
%GLU_FLAT               = 100001
%GLU_NONE               = 100002

'/* QuadricDrawStyle */
%GLU_POINT              = 100010
%GLU_LINE               = 100011
%GLU_FILL               = 100012
%GLU_SILHOUETTE         = 100013

'/* QuadricOrientation */
%GLU_OUTSIDE            = 100020
%GLU_INSIDE             = 100021

'/* Callback types: */
'/*      GLU_ERROR               100103 */


'/****           Tesselation constants           ****/

MACRO GLU_TESS_MAX_COORD        = 1.0e150

'/* TessProperty */
%GLU_TESS_WINDING_RULE          = 100140
%GLU_TESS_BOUNDARY_ONLY         = 100141
%GLU_TESS_TOLERANCE             = 100142

'/* TessWinding */
%GLU_TESS_WINDING_ODD           = 100130
%GLU_TESS_WINDING_NONZERO       = 100131
%GLU_TESS_WINDING_POSITIVE      = 100132
%GLU_TESS_WINDING_NEGATIVE      = 100133
%GLU_TESS_WINDING_ABS_GEQ_TWO   = 100134

'/* TessCallback */
%GLU_TESS_BEGIN          = 100100  '/* void (CALLBACK*)(GLenum    type)  */
%GLU_TESS_VERTEX         = 100101  '/* void (CALLBACK*)(void      *data) */
%GLU_TESS_END            = 100102  '/* void (CALLBACK*)(void)            */
%GLU_TESS_ERROR          = 100103  '/* void (CALLBACK*)(GLenum    errno) */
%GLU_TESS_EDGE_FLAG      = 100104  '/* void (CALLBACK*)(GLboolean boundaryEdge)  */
%GLU_TESS_COMBINE        = 100105  '/* void (CALLBACK*)(GLdouble  coords[3],
                                   '                    void      *data[4],
                                   '                    GLfloat   weight[4],
                                   '                    void      **dataOut) */
%GLU_TESS_BEGIN_DATA     = 100106  '/* void (CALLBACK*)(GLenum    type,
                                   '                    void      *polygon_data) */
%GLU_TESS_VERTEX_DATA    = 100107  '/* void (CALLBACK*)(void      *data,
                                   '                    void      *polygon_data) */
%GLU_TESS_END_DATA       = 100108  '/* void (CALLBACK*)(void      *polygon_data) */
%GLU_TESS_ERROR_DATA     = 100109  '/* void (CALLBACK*)(GLenum    errno,
                                   '                    void      *polygon_data) */
%GLU_TESS_EDGE_FLAG_DATA = 100110  '/* void (CALLBACK*)(GLboolean boundaryEdge,
                                   '                    void      *polygon_data) */
%GLU_TESS_COMBINE_DATA   = 100111  '/* void (CALLBACK*)(GLdouble  coords[3],
                                   '                    void      *data[4],
                                   '                    GLfloat   weight[4],
                                   '                    void      **dataOut,
                                   '                    void      *polygon_data) */

'/* TessError */
%GLU_TESS_ERROR1    = 100151
%GLU_TESS_ERROR2    = 100152
%GLU_TESS_ERROR3    = 100153
%GLU_TESS_ERROR4    = 100154
%GLU_TESS_ERROR5    = 100155
%GLU_TESS_ERROR6    = 100156
%GLU_TESS_ERROR7    = 100157
%GLU_TESS_ERROR8    = 100158

%GLU_TESS_MISSING_BEGIN_POLYGON  = %GLU_TESS_ERROR1
%GLU_TESS_MISSING_BEGIN_CONTOUR  = %GLU_TESS_ERROR2
%GLU_TESS_MISSING_END_POLYGON    = %GLU_TESS_ERROR3
%GLU_TESS_MISSING_END_CONTOUR    = %GLU_TESS_ERROR4
%GLU_TESS_COORD_TOO_LARGE        = %GLU_TESS_ERROR5
%GLU_TESS_NEED_COMBINE_CALLBACK  = %GLU_TESS_ERROR6

'/****           NURBS constants                 ****/

'/* NurbsProperty */
%GLU_AUTO_LOAD_MATRIX     = 100200
%GLU_CULLING              = 100201
%GLU_SAMPLING_TOLERANCE   = 100203
%GLU_DISPLAY_MODE         = 100204
%GLU_PARAMETRIC_TOLERANCE = 100202
%GLU_SAMPLING_METHOD      = 100205
%GLU_U_STEP               = 100206
%GLU_V_STEP               = 100207

'/* NurbsSampling */
%GLU_PATH_LENGTH          = 100215
%GLU_PARAMETRIC_ERROR     = 100216
%GLU_DOMAIN_DISTANCE      = 100217

'/* NurbsTrim */
%GLU_MAP1_TRIM_2          = 100210
%GLU_MAP1_TRIM_3          = 100211

'/* NurbsDisplay */
'/*      GLU_FILL                100012 */
%GLU_OUTLINE_POLYGON      = 100240
%GLU_OUTLINE_PATCH        = 100241

'/* NurbsCallback */
'/*      GLU_ERROR               100103 */

'/* NurbsErrors */
%GLU_NURBS_ERROR1        = 100251
%GLU_NURBS_ERROR2        = 100252
%GLU_NURBS_ERROR3        = 100253
%GLU_NURBS_ERROR4        = 100254
%GLU_NURBS_ERROR5        = 100255
%GLU_NURBS_ERROR6        = 100256
%GLU_NURBS_ERROR7        = 100257
%GLU_NURBS_ERROR8        = 100258
%GLU_NURBS_ERROR9        = 100259
%GLU_NURBS_ERROR10       = 100260
%GLU_NURBS_ERROR11       = 100261
%GLU_NURBS_ERROR12       = 100262
%GLU_NURBS_ERROR13       = 100263
%GLU_NURBS_ERROR14       = 100264
%GLU_NURBS_ERROR15       = 100265
%GLU_NURBS_ERROR16       = 100266
%GLU_NURBS_ERROR17       = 100267
%GLU_NURBS_ERROR18       = 100268
%GLU_NURBS_ERROR19       = 100269
%GLU_NURBS_ERROR20       = 100270
%GLU_NURBS_ERROR21       = 100271
%GLU_NURBS_ERROR22       = 100272
%GLU_NURBS_ERROR23       = 100273
%GLU_NURBS_ERROR24       = 100274
%GLU_NURBS_ERROR25       = 100275
%GLU_NURBS_ERROR26       = 100276
%GLU_NURBS_ERROR27       = 100277
%GLU_NURBS_ERROR28       = 100278
%GLU_NURBS_ERROR29       = 100279
%GLU_NURBS_ERROR30       = 100280
%GLU_NURBS_ERROR31       = 100281
%GLU_NURBS_ERROR32       = 100282
%GLU_NURBS_ERROR33       = 100283
%GLU_NURBS_ERROR34       = 100284
%GLU_NURBS_ERROR35       = 100285
%GLU_NURBS_ERROR36       = 100286
%GLU_NURBS_ERROR37       = 100287

'/****           Backwards compatibility for old tesselator           ****/

' ========================================================================================
' void APIENTRY gluBeginPolygon(GLUtesselator *tess);
' Delimits a polygon description.
' ========================================================================================
DECLARE SUB gluBeginPolygon LIB "GLU32.DLL" ALIAS "gluBeginPolygon" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluNextContour(GLUtesselator *tess, GLenum type );
' Marks the beginning of another contour.
' ========================================================================================
DECLARE SUB gluNextContour LIB "GLU32.DLL" ALIAS "gluNextContour" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 , BYVAL type AS DWORD _                                ' GLEnum type
 )                                                      ' void

' ========================================================================================
' void APIENTRY gluEndPolygon(GLUtesselator *tess);
' ========================================================================================
DECLARE SUB gluEndPolygon LIB "GLU32.DLL" ALIAS "gluEndPolygon" ( _
   BYVAL tess AS DWORD _                                ' GLUtesselator *tess
 )                                                      ' void

'/* Contours types -- obsolete! */
%GLU_CW          = 100120
%GLU_CCW         = 100121
%GLU_INTERIOR    = 100122
%GLU_EXTERIOR    = 100123
%GLU_UNKNOWN     = 100124

'/* Names without "TESS_" prefix */
%GLU_BEGIN       = %GLU_TESS_BEGIN
%GLU_VERTEX      = %GLU_TESS_VERTEX
%GLU_END         = %GLU_TESS_END
%GLU_ERROR       = %GLU_TESS_ERROR
%GLU_EDGE_FLAG   = %GLU_TESS_EDGE_FLAG


#ENDIF   ' #IF NOT %DEF(%GLU_INC)

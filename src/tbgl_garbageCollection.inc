'  =============================================================================
'   Part of     : thinBASIC_TBGL
'
'   Description : thinBasic module for 3D graphics
' =============================================================================
'   About part  : This include provides functions for all garbage collections
'                 CURRENTLY JUST FOR TESTING, in future used for VBO, DL, Textures...
' =============================================================================


' -----------------------------------------------------------------------------
#IF 0
TYPE TGarbagePair
  userID  AS LONG
  glID    AS LONG
END TYPE

CLASS CgcIDArray

  INSTANCE Items() AS TGarbagePair, ItemsCount AS LONG

  INTERFACE IgcIDArray
    INHERIT IUNKNOWN

  END INTERFACE

END CLASS

' -----------------------------------------------------------------------------
' -- Display Lists
' -----------------------------------------------------------------------------

CLASS CcgDisplayList
  ' USAGE:
  ' DIM displayList AS IgcDisplayList
  ' displayList = CLASS "CgcDisplayList"
  ' displayList.New(1)
  ' - GL code here -
  ' displayList.End()
  ' ...
  ' displayList.DeleteAll()

  ' -- Contains pairs of IDs, 0th item not used
  INSTANCE Items() AS TGarbagePair, ItemsCount AS LONG

  ' -- Constructor
  CLASS METHOD CREATE()
    DIM Items(0)
    ItemsCount = 0
  END METHOD

  ' -- Destructor
  CLASS METHOD DESTROY()
    IF ItemsCount THEN
      me.p_deleteAll()
    END IF
  END METHOD

  ' -- "PRIVATE" method to prepare empty slot
  CLASS METHOD p_allocNew( listID AS LONG ) AS LONG

    REGISTER i AS LONG

    ' -- Did we used that ID already?
    LOCAL Index AS LONG
    FOR i = 1 TO ItemsCount
      IF Items(i).userID = listID THEN
        Index = i
        EXIT FOR
      END IF
    NEXT

    ' -- Yes, so lets delete resources and reuse slot
    IF Index  THEN
      ' -- Delete just on OpenGL side
      glDeleteLists(Items(i).glID, 1)

    ELSE
      ItemsCount += 1
      IF ItemsCount > UBOUND(Items) THEN REDIM PRESERVE Items(ItemsCount*2)
      Index       = ItemsCount

    END IF

    ' -- Assign
    Items(Index).userID = listID
    Items(Index).glID   = glGenLists(1)

    METHOD = index

  END METHOD

  ' -- "PRIVATE" method to clear all display lists
  CLASS METHOD p_deleteAll()
    REGISTER i AS LONG

    FOR i = 1 TO ItemsCount
      glDeleteLists(Items(i).glID, 1)
    NEXT

    REDIM Items(0)
    ItemsCount = 0
  END METHOD

  INTERFACE IgcDisplayList
    INHERIT IUNKNOWN

    METHOD AllocOne( listID AS LONG ) AS LONG
      METHOD = me.p_allocNew(listID)
    END METHOD

    METHOD AllocMultiple( listID AS LONG )
      me.p_allocNew(listID)
    END METHOD

    ' -- Begin new list
    METHOD NEW( listID AS LONG ) AS LONG
      LOCAL Index AS LONG
      Index = me.p_allocNew(listID)

      ' -- Begin creation
      glNewList Items(Index).glID, %GL_COMPILE
    END METHOD

    ' -- End definition
    METHOD END()
      glEndList()
    END METHOD

    ' -- Call existing list
    METHOD CALL( listID AS LONG )
      REGISTER i AS LONG

      FOR i = 1 TO ItemsCount
        IF Items(i).userID = listID THEN
          glCallList Items(i).glID
          EXIT FOR
        END IF
      NEXT

    END METHOD

    ' -- Delete existing list
    METHOD DELETE( listID AS LONG )
      REGISTER i AS LONG
      LOCAL Index AS LONG

      FOR i = 1 TO ItemsCount
        IF Items(i).userID = listID THEN
          Index = i
          EXIT FOR
        END IF
      NEXT

      IF Index THEN
        ' -- Delete from OpenGL
        glDeleteLists(Items(Index).userID, 1)

        ' -- Delete from our registry
        ItemsCount -= 1
        ARRAY DELETE Items(Index)
      END IF
    END METHOD

    ' -- Clear all lists
    METHOD DeleteAll()
      ' -- Calling private method which can be used from here and in destructor
      me.p_deleteAll()
    END METHOD

  END INTERFACE

END CLASS

' -----------------------------------------------------------------------------
' -- Textures
' -----------------------------------------------------------------------------
#ENDIF

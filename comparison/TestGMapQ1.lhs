> import BinTreeDatatype
> import GShow(gapplyShowList)

Here we test higher order generic functions. This is what we do:
* a generic show function is received as argument and then it is
  applied to all the fields of a constructor.
* the function that applies the gen. show is defined in the GShow
  module for each approach.

The function definition should roughly  be defined as follows:

 gapplyShowList = gmapQ gshow'
 gapplyShowBinTree = gmapQ gshow'

here gmapQ takes the generic function, applies it to every field
of dept (which have different type),
collects the results and returns them in a list.

Note that gmapQ is higher order because it takes gshow', which is
a generic function itself, and it can be specialized to any datatype.

List is probably represented in most libraries, so no extensibility would
be needed here. If it would be needed, some libraries would not be
testable for first-class generic functions.

> main = print $ (gapplyShowList [1,2]
>                )

/** 
Display collapsed error stack - any adjacent stack elements with the 
same location are combined on one line. For the bottommost error, we 
display the full line, with a caret pointing to the column of the error. 
Example: 

1.1 file 'companies.json'; array
5.1 object
5.2 key-value
5.10 ':'

{ "MSFT" ; 24, 
         ^
If a level contains nonempty `otherFailures`, these are placed on the 
next line, indented.  
*/
override def toString =
  if (stack.isEmpty) "empty error message"
  else {
    val flat: List[(Int,(Location,String))] = 
      allMsgs(0).groupBy(_._2._1).toList.
                 sortBy(_._1.offset).
                 flatMap(_._2)
    val context = flat.map { 
      case (lvl,(loc,msg)) => ("  " * lvl) + formatLoc(loc) + " " + msg 
    } mkString "\n"
    val errorPointer = flat.filter(_._1 == 0).last match {
      case (_,(loc,_)) => loc.currentLine + "\n" + (" " * (loc.col-1)) + "^"
    }
    context + "\n\n" + errorPointer
  }

def allMsgs(level: Int): List[(Int,(Location,String))] = 
  collapseStack(stack).map((level,_)) ++ otherFailures.flatMap(_.allMsgs(level+1))

/* Builds a collapsed version of the given error stack - 
 * messages at the same location have their messages merged, 
 * separated by semicolons */
def collapseStack(s: List[(Location,String)]): List[(Location,String)] = 
  s.groupBy(_._1).
    mapValues(_.map(_._2).mkString("; ")).
    toList.sortBy(_._1.offset)

def formatLoc(l: Location): String = l.line + "." + l.col
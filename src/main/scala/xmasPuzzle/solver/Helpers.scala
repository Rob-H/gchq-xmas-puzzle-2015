package xmasPuzzle.solver

object Helpers {
    def singleToSpec(single: Seq[Boolean]):String = {
        val upToFirst = single.dropWhile(x => !x)
        val afterFirst = upToFirst.dropWhile(x => x)
        val sizeOfBlock = (upToFirst.length - afterFirst.length)
        if(sizeOfBlock > 0) (sizeOfBlock.toString + " " + singleToSpec(afterFirst)).trim
        else ""
    }

    def createBoard(size:Int, existingSquareSpec:Seq[String]) = {
        def extractPairs(input:String) = {
            val Array(x, theRest) = input.split(":")
            theRest.split(" ").map(y => (x.toInt, y.toInt))
        }

        val squares = existingSquareSpec.flatMap(extractPairs)
        val board = Array.tabulate(size,size)((x,y) => squares.contains((x,y)))
        new Board(board.toList.map(_.toList))
    }
}


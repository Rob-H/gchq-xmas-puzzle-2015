package xmasPuzzle

import scala.io.Source

object Main {
    def main(args: Array[String]) {
        val input = Source.fromInputStream(getClass.getResourceAsStream("/input.txt")).getLines.toList;
        val columnSpec = input take 25 
        val rowSpec = input drop 26 take 25
        val blocks = input drop 52
        println(new Board(25, blocks.toList))
    }
}

class Board(size:Int, existingSquareSpec:Seq[String]) {
    private def extractPairs(input:String) = {
        val Array(x, theRest) = input.split(":")
        theRest.split(" ").map(y => (x.toInt, y.toInt))
    }

    private val squares = existingSquareSpec.flatMap(extractPairs)
    private val board = Array.tabulate(size,size)((x,y) => squares.contains((x,y)))
    val rows = board.toList
    val columns = board.transpose.toList
    def cell(x:Int, y:Int) = board(x)(y)

    def isValidFor(rowSpec: List[String], columnSpec: List[String]) = {
        def toSpec(actual: List[Array[Boolean]]) = {
            def singleToSpec(single: Array[Boolean]):String = {
                val upToFirst = single.dropWhile(x => !x)
                val afterFirst = upToFirst.dropWhile(x => x)
                val sizeOfBlock = (upToFirst.length - afterFirst.length);
                if(sizeOfBlock > 0) sizeOfBlock.toString + " " + singleToSpec(afterFirst)
                else ""
            }
            actual.map(x => singleToSpec(x).trim).toList
        }
        rowSpec == toSpec(rows) && columnSpec == toSpec(columns)

    }

    override lazy val toString = rows.map(row => ("|" :: row.map(if(_) "*|" else " |").toList).mkString("")).mkString("\n")
}

package xmasPuzzle

import scala.io.Source

object Main {
    def and(a:Seq[Boolean], b:Seq[Boolean]) = {
        a zip b map {case (aval, bval) => aval & bval}
    }

    def andOverSeq(seqs: Seq[Seq[Boolean]]): Seq[Boolean] = {
        seqs.transpose.map(_.forall(x => x))
    }

    def allPossiblePermutationsOf(spec: String, currentDefiniteBlocks: Seq[Boolean]): Seq[Seq[Boolean]] = {
        def minSize(specs: List[Int]) = specs.sum + specs.length - 1

        if (Helpers.singleToSpec(currentDefiniteBlocks) == spec) List(currentDefiniteBlocks)
        else if (currentDefiniteBlocks.length == 0 && spec.length == 0) List(Nil)
        else if (spec.length == 0) List(List.fill(currentDefiniteBlocks.length)(false))
        else {
            val specs = spec.split(" ").map(_.toInt).toList
            if (currentDefiniteBlocks.length < minSize(specs)) Nil
            else {
                val firstSpec :: theRest = specs
                val mySpace = if(theRest == Nil) currentDefiniteBlocks.length else currentDefiniteBlocks.length - minSize(theRest) - 1
                val theRestSpec = theRest.mkString(" ")

                val myBlock = List.fill(firstSpec)(true)

                val myPossibilities = for(prefix <- 0 to mySpace - firstSpec) yield List.fill(prefix)(false) ++ myBlock

                val everything = for {
                    possiblityPrefix <- myPossibilities
                    suffixPossibility <- allPossiblePermutationsOf(theRestSpec, currentDefiniteBlocks.drop(possiblityPrefix.length + 1))
                } yield { 
                    if(suffixPossibility.length > 0) possiblityPrefix ++ List(false) ++ suffixPossibility 
                    else possiblityPrefix ++ List.fill(mySpace - possiblityPrefix.length)(false)
                }


                val allValid = for {
                    possiblity <- everything
                    if(and(possiblity, currentDefiniteBlocks).toList == currentDefiniteBlocks.toList)
                } yield possiblity

                allValid
            }
        }
    }

    def solverStep(board: Board, rowSpec:Seq[String], columnSpec: Seq[String]) = {
        val newRows = board.rows zip rowSpec map {case (row, spec) => {
            val all = allPossiblePermutationsOf(spec, row)
            if(all.length == 0) throw new Exception("oh no..")
            andOverSeq(all)
        }}
        val newColumns = newRows.transpose zip columnSpec map {case (column, spec) => {
            val all = allPossiblePermutationsOf(spec, column)
            if(all.length == 0) throw new Exception("oh no..")
            andOverSeq(all)
        }}
        new Board(newColumns.transpose)
    }

    def solve(board:Board, rowSpec:Seq[String], columnSpec: Seq[String]):Unit = {
       if (board.isValidFor(rowSpec, columnSpec)) {
           println("Finished: ") 
           println(board)
           return;
       }
       val newBoard = solverStep(board, rowSpec, columnSpec)
       if (newBoard.toString == board.toString) {
           println("got as far as we can: ")
           println(board)
           val listOfPossibilities = newBoard.rows zip rowSpec map {case (row, spec) => {
               val all = allPossiblePermutationsOf(spec, row)
               println(all.length)
               all.length
           }}
           print("possibilities:")
           println(listOfPossibilities.reduceLeft(_*_))
           return;
       }
       println("*" * 50)
       println(board)
       solve(newBoard, rowSpec, columnSpec)
    }


    def main(args: Array[String]) {
        val input = Source.fromInputStream(getClass.getResourceAsStream("/input.txt")).getLines.toList;
        val columnSpec = (input take 25).toList
        val rowSpec = (input drop 26 take 25).toList
        val blocks = input drop 52

        var board = Helpers.createBoard(25, blocks.toList)

        solve(board, rowSpec, columnSpec)
    }
}

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

class Board(board: Seq[Seq[Boolean]]) {
    val rows = board.toList
    val columns = board.transpose.toList
    def cell(x:Int, y:Int) = board(x)(y)

    def isValidFor(rowSpec: Seq[String], columnSpec: Seq[String]) = {
        def toSpec(actual: List[Seq[Boolean]]) = {
            actual.map(Helpers.singleToSpec).toList
        }
        rowSpec == toSpec(rows) && columnSpec == toSpec(columns)
    }

    override lazy val toString = rows.map(row => ("|" :: row.map(if(_) "*|" else " |").toList).mkString("")).mkString("\n")
}

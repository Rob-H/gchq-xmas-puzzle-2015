package xmasPuzzle

import java.awt.image.BufferedImage
import scala.io.Source
import xmasPuzzle.solver._
import xmasPuzzle.ui.UI

object Main {
    val blockSize = 10;

    def saveImageOf(board: Board) = {
        val size = board.rows.length
        val canvas = new BufferedImage(size * blockSize, size * blockSize, BufferedImage.TYPE_INT_RGB)         

        val g = canvas.createGraphics()

        board.draw(g, blockSize)

        javax.imageio.ImageIO.write(canvas, "png", new java.io.File("result.png"))
    }

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

                val myBlock = List.fill(firstSpec)(true)

                val myPossibilities = for(prefix <- 0 to mySpace - firstSpec) yield List.fill(prefix)(false) ++ myBlock

                val everything = for {
                    possiblityPrefix <- myPossibilities
                    suffixPossibility <- allPossiblePermutationsOf(theRest.mkString(" "), currentDefiniteBlocks.drop(possiblityPrefix.length + 1))
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

        val newBoard = new Board(newColumns.transpose)
        if(newBoard.toString == board.toString) {
            val deffoNotFromRows = board.rows zip rowSpec map {case (row, spec) => {
                val all = allPossiblePermutationsOf(spec, row)
                val inverse = all.map(_.map(x => !x))
                andOverSeq(inverse)
            }}

            val newFilteredColumns = newBoard.columns zip columnSpec zip deffoNotFromRows.transpose map {case ((column, spec), deffoNot) => {
                val all = allPossiblePermutationsOf(spec, column)
                val filtered = all.filter(perm => perm zip deffoNot forall {case (poss, deffoNot) => !(poss && deffoNot) })
                andOverSeq(filtered)
            }}

            new Board(newFilteredColumns.transpose)

        } else newBoard
    }

    def solve(boardPrinter: (Board) => Unit)(board:Board, rowSpec:Seq[String], columnSpec: Seq[String]):Unit = {
       if (board.isValidFor(rowSpec, columnSpec)) {
           println("Finished: ") 
           boardPrinter(board)
           saveImageOf(board)
           return;
       }
       val newBoard = solverStep(board, rowSpec, columnSpec)
       if (newBoard.toString == board.toString) {
           println("got as far as we can: ")
           boardPrinter(board)
           return;
       }
       boardPrinter(board)
       solve(boardPrinter)(newBoard, rowSpec, columnSpec)
    }


    def main(args: Array[String]) {
        val input = Source.fromInputStream(getClass.getResourceAsStream("/input.txt")).getLines.toList;
        val columnSpec = (input take 25).toList
        val rowSpec = (input drop 26 take 25).toList
        val blocks = input drop 52

        var board = Helpers.createBoard(25, blocks.toList)

        val ui = new UI(blockSize, board)
        ui.visible = true

        val uiPrinter = (x: Board) => {ui.setBoard(x)}

        solve(uiPrinter)(board, rowSpec, columnSpec)
    }
}


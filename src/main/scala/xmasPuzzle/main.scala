package xmasPuzzle

import scala.io.Source

import scala.swing._

import java.awt.geom._
import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}

object boardDrawer {
    val blockSize = 10
    def drawBoard(g: Graphics2D)(board: Board) = {
        val size = board.rows.length
        // clear background
        g.setColor(Color.WHITE)
        g.fillRect(0, 0, size * blockSize, size * blockSize)    

        for{
            row <- 0 until size
            col <- 0 until size
        } {
            if(board.cell(row, col)) g.setColor(Color.BLACK)  
            else g.setColor(Color.WHITE)
            g.fillRect(col * blockSize, row * blockSize, blockSize, blockSize)
        }
    }
}
class QrCanvas(board: Board) extends Panel {
    var currentBoard = board;

    override def paintComponent(g: Graphics2D) {
        boardDrawer.drawBoard(g)(currentBoard);    
    }

    def setBoard(board: Board) = {
        currentBoard = board
        repaint()
    }
}

class UI(blockSize:Int, board:Board) extends MainFrame {
    title = "GCHQ puzzle solver"
    peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    preferredSize = new Dimension((25 * blockSize) + 20, (25 * blockSize) + 40)
    val panel = new QrCanvas(board) 
    contents = panel
    def setBoard(board:Board) = {
        panel.setBoard(board)
    }

}

object Main {
    val blockSize = 10;

    def saveImageOf(board: Board) = {
        val size = board.rows.length
        val canvas = new BufferedImage(size * blockSize, size * blockSize, BufferedImage.TYPE_INT_RGB)         

        val g = canvas.createGraphics()

        boardDrawer.drawBoard(g)(board)

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

        val jPanelPrinter = (x: Board) => {ui.setBoard(x)}

        solve(jPanelPrinter)(board, rowSpec, columnSpec)
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

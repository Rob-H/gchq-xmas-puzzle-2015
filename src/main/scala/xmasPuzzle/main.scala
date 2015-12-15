package xmasPuzzle

import java.awt.image.BufferedImage
import scala.io.Source
import xmasPuzzle.solver._
import xmasPuzzle.ui.UI

object Main {
    val blockSize = 10;

    def saveImageOf(board: Board) = {
        val size = board.size
        val canvas = new BufferedImage(size * blockSize, size * blockSize, BufferedImage.TYPE_INT_RGB)         

        val g = canvas.createGraphics()

        board.draw(g, blockSize)

        javax.imageio.ImageIO.write(canvas, "png", new java.io.File("result.png"))
    }


    def main(args: Array[String]) {
        val input = Source.fromInputStream(getClass.getResourceAsStream("/input.txt")).getLines.toList;
        val columnSpec = (input take 25).map(spec => spec.split(" ").toList.map(_.toInt)).toList
        val rowSpec = (input drop 26 take 25).map(spec => spec.split(" ").toList.map(_.toInt)).toList        
        val blocks = input drop 52

        var board = Helpers.createBoard(25, blocks.toList)

        val ui = new UI(blockSize, board)
        ui.visible = true

        val uiPrinter = (x: Board) => {ui.setBoard(x)}

        val solved = new Solver(uiPrinter).solve(board, rowSpec, columnSpec)
        saveImageOf(solved)
    }
}


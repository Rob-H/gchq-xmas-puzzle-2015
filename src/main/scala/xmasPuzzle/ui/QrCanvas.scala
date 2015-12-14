package xmasPuzzle.ui

import java.awt.Graphics2D
import scala.swing.Panel
import xmasPuzzle.solver.Board

class QrCanvas(board: Board, blockSize: Int) extends Panel {
    var currentBoard = board

    override def paintComponent(g: Graphics2D) {
        currentBoard.draw(g, blockSize)    
    }

    def setBoard(board: Board) = {
        currentBoard = board
        repaint()
    }
}


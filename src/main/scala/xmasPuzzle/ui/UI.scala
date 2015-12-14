package xmasPuzzle.ui

import scala.swing.MainFrame
import xmasPuzzle.solver.Board

class UI(blockSize:Int, board:Board) extends MainFrame {
    title = "GCHQ puzzle solver"
    peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    preferredSize = new java.awt.Dimension((25 * blockSize) + 20, (25 * blockSize) + 40)
    val panel = new QrCanvas(board, blockSize) 
    contents = panel
    def setBoard(board:Board) = {
        panel.setBoard(board)
    }
}

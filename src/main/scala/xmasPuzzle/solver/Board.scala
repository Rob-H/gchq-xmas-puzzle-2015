package xmasPuzzle.solver

import java.awt.{Graphics2D,Color}

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

    def draw(g: Graphics2D, blockSize: Int) = {
        val size = rows.length
        // clear background
        g.setColor(Color.WHITE)
        g.fillRect(0, 0, size * blockSize, size * blockSize)    

        for{
            row <- 0 until size
            col <- 0 until size
        } {
            if(cell(row, col)) g.setColor(Color.BLACK)  
            else g.setColor(Color.WHITE)
            g.fillRect(col * blockSize, row * blockSize, blockSize, blockSize)
        }
    }

    override lazy val toString = rows.map(row => ("|" :: row.map(if(_) "*|" else " |").toList).mkString("")).mkString("\n")
}

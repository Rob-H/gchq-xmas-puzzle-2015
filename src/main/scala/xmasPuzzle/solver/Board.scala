package xmasPuzzle.solver

import java.awt.{Graphics2D,Color}

class Board(board: Seq[Seq[Boolean]]) {
    val rows = board.toList
    val columns = board.transpose.toList
    def cell(x:Int, y:Int) = board(x)(y)
    val size = rows.length

    def isValidFor(rowSpec: Seq[Seq[Int]], columnSpec: Seq[Seq[Int]]) = {
        def toSpec(actual: List[Seq[Boolean]]) = {
            actual.map(Helpers.singleToSpec).toList
        }
        rowSpec == toSpec(rows) && columnSpec == toSpec(columns)
    }

    def draw(g: Graphics2D, blockSize: Int) = {
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

    override def equals(o: Any) = o match {
        case that: Board => that.toString == toString
        case _ => false
    }

    override lazy val toString = rows.map(row => ("|" :: row.map(if(_) "*|" else " |").toList).mkString("")).mkString("\n")
}

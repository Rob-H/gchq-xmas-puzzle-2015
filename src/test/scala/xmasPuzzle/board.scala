import org.scalatest._
import xmasPuzzle._

class BoardSpec extends FunSpec with Matchers with Inspectors{
    it("creates an empty board of correct size with no specification") {
        val board = new Board(25, List())

        val rows = board.rows;

        rows should have length 25

        all (rows) should have length 25

        forAll (rows) { cellsInRow => all (cellsInRow) should be (false) }
    }

    it("creates a populated board of correct size with specification") {
        val board = new Board(5, List(
            "1:1 4",
            "3:0 5"
        ))
        val rows = board.rows;

        rows should have length 5

        all (rows) should have length 5

        for {
            x <- 0 until 5
            y <- 0 until 5
        } {
            if((x == 1 && (y == 1 || y == 4)) || (x == 3 && (y == 0 || y == 5))) 
                board.cell(x, y) should be (true)
            else 
                board.cell(x, y) should be (false)
        }
    }

    it("is valid for simple square") {
        val board = new Board(3, List(
            "0:0",
            "1:1",
            "2:2"
        ))

        board.isValidFor(List("1", "1", "1"), List("1", "1", "1")) should be (true)
    }

    it("is not valid for non-valid simple square") {
        val board = new Board(3, List(
            "0:0",
            "1:1",
            "2:0"
        ))

        board.isValidFor(List("1", "1", "1"), List("1", "1", "1")) should be (false)
    }

    it("is valid for valid simple square") {
        val board = new Board(3, List(
            "0:0 2",
            "1:1",
            "2:0"
        ))

        board.isValidFor(List("1 1", "1", "1"), List("1 1", "1", "1")) should be (true)
    }
}


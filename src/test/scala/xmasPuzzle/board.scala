import org.scalatest._
import xmasPuzzle.solver._

class BoardSpec extends FunSpec with Matchers with Inspectors{
    it("creates an empty board of correct size with no specification") {
        val board = Helpers.createBoard(25, List())

        val rows = board.rows;

        rows should have length 25

        all (rows) should have length 25

        forAll (rows) { cellsInRow => all (cellsInRow) should be (false) }
    }

    it("creates a populated board of correct size with specification") {
        val board = Helpers.createBoard(5, List(
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
        val board = Helpers.createBoard(3, List(
            "0:0",
            "1:1",
            "2:2"
        ))

        board.isValidFor(List(List(1), List(1), List(1)), List(List(1), List(1), List(1))) should be (true)
    }

    it("is not valid for non-valid simple square") {
        val board = Helpers.createBoard(3, List(
            "0:0",
            "1:1",
            "2:0"
        ))

        board.isValidFor(List(List(1),List(1),List(1)), List(List(1),List(1),List(1))) should be (false)
    }

    it("is valid for valid simple square") {
        val board = Helpers.createBoard(3, List(
            "0:0 2",
            "1:1",
            "2:0"
        ))

        board.isValidFor(List(List(1, 1),List(1),List(1)), List(List(1, 1),List(1),List(1))) should be (true)
    }

    it("can find permuatations when it's already in the configuration") {
        val currentDefiniteBlocks = Array(true, true, false, true)
        val spec = List(2, 1)

        Solver.allPossiblePermutationsOf(spec, currentDefiniteBlocks).toList should equal (List(currentDefiniteBlocks.toList))
    }

    it("can find permuatations when it is not already in the configuration") {
        val currentDefiniteBlocks = Array(true, true, false, false, false)
        val spec = List(2, 1)

        Solver.allPossiblePermutationsOf(spec, currentDefiniteBlocks).toList.sortBy(_.toString) should equal (
            List(
                List(true, true, false, true, false), 
                List(true, true, false, false, true)
            ).sortBy(_.toString)
        )
    }

    it("can find permutations when it is not already in the configuration, with less to go off") {
        val currentDefiniteBlocks = Array(false, true, false, false, false)
        val spec = List(2, 1)

        Solver.allPossiblePermutationsOf(spec, currentDefiniteBlocks).toList.sortBy(_.toString) should equal (
            List(
                List(true, true, false, true, false), 
                List(true, true, false, false, true),
                List(false, true, true, false, true)
            ).sortBy(_.toString)
        )
        
    }

    it("can and stuff") {
        Solver.andOverSeq(List(List(true, true, true), List(true, false, true), List(true, true, false))).toList should equal (
            List(true, false, false)
        )
    }
}


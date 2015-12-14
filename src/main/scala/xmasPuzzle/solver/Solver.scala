package xmasPuzzle.solver;

object Solver {
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

}

class Solver(progressReporter: (Board) => Unit) {
    def solverStep(board: Board, rowSpec:Seq[String], columnSpec: Seq[String]) = {
        val newRows = board.rows zip rowSpec map {case (row, spec) => {
            val all = Solver.allPossiblePermutationsOf(spec, row)
            if(all.length == 0) throw new Exception("oh no..")
            Solver.andOverSeq(all)
        }}
        val newColumns = newRows.transpose zip columnSpec map {case (column, spec) => {
            val all = Solver.allPossiblePermutationsOf(spec, column)
            if(all.length == 0) throw new Exception("oh no..")
            Solver.andOverSeq(all)
        }}

        val newBoard = new Board(newColumns.transpose)

        val deffoNotFromRows = board.rows zip rowSpec map {case (row, spec) => {
            val all = Solver.allPossiblePermutationsOf(spec, row)
            val inverse = all.map(_.map(x => !x))
            Solver.andOverSeq(inverse)
        }}

        val newFilteredColumns = newBoard.columns zip columnSpec zip deffoNotFromRows.transpose map {case ((column, spec), deffoNot) => {
            val all = Solver.allPossiblePermutationsOf(spec, column)
            val filtered = all.filter(perm => perm zip deffoNot forall {case (poss, deffoNot) => !(poss && deffoNot) })
            Solver.andOverSeq(filtered)
        }}

        new Board(newFilteredColumns.transpose)
    }

    def solve(board:Board, rowSpec:Seq[String], columnSpec: Seq[String]): Board = {
       progressReporter(board)
       if (board.isValidFor(rowSpec, columnSpec)) board
       else {
           val newBoard = solverStep(board, rowSpec, columnSpec)
           if (newBoard.toString == board.toString) throw new Exception("cannot solve sorry")
           else solve(newBoard, rowSpec, columnSpec)
       }
    } 
}

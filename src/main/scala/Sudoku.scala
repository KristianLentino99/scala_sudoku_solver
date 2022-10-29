
object Sudoku{

  type SudokuNumber = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  type SudokuBoard = Array[Array[SudokuNumber]]
  def prettyString(sudoku: SudokuBoard): String =
    sudoku.grouped(3).map{ bigGroup =>
      bigGroup.map { row =>
        row.grouped(3).map { smallGroup =>
          smallGroup.mkString(" ", " ", " ")
        }.mkString("|", "|", "|")
      }.mkString("\n")
    }.mkString("-------------------------\n", "\n-------------------------\n","\n-------------------------")

  def validate(sudoku: SudokuBoard,x: Int, y: Int, value: Int): Boolean = {
    val row = sudoku(y)
    val column = sudoku.map(row => row.apply(x))
    val boxX = x / 3
    val boxY = y / 3
    val box: Seq[Int] = for{
      yb <- (boxY * 3) until (boxY * 3 + 3)
      xb <- (boxX * 3) until (boxX * 3 + 3)
    } yield sudoku(yb)(xb)
    row.count(_ == value) == 0 && column.count(_ == value) == 0 && box.count(_== value) == 0
  }

  def solve(sudoku: SudokuBoard, x: Int = 0, y: Int = 0): Unit =
    if(y >= 9) println(prettyString(sudoku)) //print the final solution
    /**
     * if x >= 9 then we've finished the cells in the row, so we're moving forward to the next row
     */
    else if(x >= 9) solve(sudoku, 0, y + 1)
    /**
     * if the cell at Y & X coordinates are filled with a number != 0 this means that
     * we can skip forward to the next cell of the same row
     */
    else if(sudoku(y)(x) > 0) solve(sudoku, x + 1, y)
    /**
     * if all the previous conditions are false then we iterate from 1 to 9 , filter this sequence of numbers
     * with our validate function so that we have only the numbers which are valid for the given x and y
     * and then we move forward to the next cell
     */
    else (1 to 9).filter(value => {
      validate(sudoku, x, y, value)
    }).foreach{ value =>
      sudoku(y)(x) = value.asInstanceOf[SudokuNumber]
      solve(sudoku, x + 1, y)
      sudoku(y)(x) = 0
    }

  @main
  def main(): Unit = {

    val problem = Array(
      Array[SudokuNumber](5,3,0, 0,7,0, 0,0,0),
      Array[SudokuNumber](6,0,0, 1,9,5, 0,0,0),
      Array[SudokuNumber](0,9,8, 0,0,0, 0,6,0),

      Array[SudokuNumber](8,0,0, 0,6,0, 0,0,3),
      Array[SudokuNumber](4,0,0, 8,0,3, 0,0,1),
      Array[SudokuNumber](7,0,0, 0,2,0, 0,0,6),

      Array[SudokuNumber](0,6,0, 0,0,0, 2,8,0),
      Array[SudokuNumber](0,0,0, 4,1,9, 0,0,5),
      Array[SudokuNumber](0,0,0, 0,8,0, 0,7,9)
    )


    solve(problem)
  }
}
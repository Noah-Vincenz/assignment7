// Part 2 about finding a single tour for a board
//================================================

// copy any function you need from file knight1.scala

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions
def is_legal(dim: Int, path: Path)(x: Pos): Boolean = {
  if (x._1 >= 0 && x._1 < dim && x._2 >= 0 && x._2 < dim && !path.contains(x)) true
  else false
}
def is_legal(dim: Int, path: Path)(x: Pos): Boolean = {
  if (x._1 >= 0 && x._1 < dim && x._2 >= 0 && x._2 < dim && !path.contains(x)) true
  else false
}
def legal_moves(dim: Int, path: Path, x: Pos): List[Pos] = {
  List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2)).filter(x => is_legal(dim, path)(x))
}
import scala.annotation.tailrec

//(2a) Implement a first-function that finds the first 
// element, say x, in the list xs where f is not None. 
// In that case return f(x), otherwise none.

def first(xs: List[Pos], f: Pos => Option[Path]): Option[Path] = xs match {
  case Nil => None
  case x :: xs => {
    val result = f(x)
    if (result.isDefined) result
    else first(xs, f)
  }
}

//use third argument accumulator where solutions are stored (not yet real solutions) and match to acc
//x is head of acc list, xs is the rest
//if it is already bigger than total for example, discharge it by recursive call of xs instead of x
//else still small and partial -> first(total, coins, coins.filter(_ > 0)/*filter out junk coins*/.map(_::x) ::: xs)

//(2b) Implement a function that uses the first-function for
// trying out onward moves, and searches recursively for an 
// *open* tour on a dim * dim-board.

def first_tour(dim: Int, path: Path): Option[Path] = {
  if (path.size == dim * dim) Some(path)
  else first(legal_moves(dim, path, path(0)), x => first_tour(dim, x :: path))
}
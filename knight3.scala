// Part 3 about finding a single tour using the Warnsdorf Rule
//=============================================================

// copy any function you need from files knight1.scala and
// knight2.scala

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions
def is_legal(dim: Int, path: Path)(x: Pos): Boolean = {
  if (x._1 >= 0 && x._1 < dim && x._2 >= 0 && x._2 < dim && !path.contains(x)) true
  else false
}
def legal_moves(dim: Int, path: Path, x: Pos): List[Pos] = {
  List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2)).filter(x => is_legal(dim, path)(x))
}
def first(xs: List[Pos], f: Pos => Option[Path]): Option[Path] = xs match {
  case Nil => None
  case x :: xs => {
    val result = f(x)
    if (result.isDefined) result
    else first(xs, f)
  }
}
import scala.annotation.tailrec

//(3a) Complete the function that calculates a list of onward
// moves like in (1b) but orders them according to the Warnsdorf’s 
// rule. That means moves with the fewest legal onward moves 
// should come first.

def ordered_moves(dim: Int, path: Path, x: Pos): List[Pos] = {
  List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2)).filter(x => is_legal(dim, path)(x)).sortWith((legal_moves(dim, path, _).size < legal_moves(dim, path, _).size))
}

//(3b) Complete the function that searches for a single *closed* 
// tour using the ordered moves function.
def isInReach(a: Pos, b: Pos): Boolean = {
  if (b == (a._1 + 1, a._2 + 2) ||
    b == (a._1 + 2, a._2 + 1) ||
    b == (a._1 + 2, a._2 - 1) ||
    b == (a._1 + 1, a._2 - 2) ||
    b == (a._1 - 1, a._2 - 2) ||
    b == (a._1 - 2, a._2 - 1) ||
    b == (a._1 - 2, a._2 + 1) ||
    b == (a._1 - 1, a._2 + 2)
  ) true
  else false
}
def first_closed_tour_heuristic(dim: Int, path: Path): Option[Path] = {
  if (path.size == dim * dim && isInReach(path.head, path(path.size - 1)) == true) Some(path)
  else first(ordered_moves(dim, path, path(0)), x => first_closed_tour_heuristic(dim, x :: path))
}
/*
def first_closed_tour_heuristic(dim: Int, path: Path): Option[Path] = {
  val head = path.head
  val tail = path(path.size - 1)
  if (path.size == dim * dim && (
      head == (tail._1 + 1, tail._2 + 2) ||
      head == (tail._1 + 2, tail._2 + 1) ||
      head == (tail._1 + 2, tail._2 + 1) ||
      head == (tail._1 + 1, tail._2 + 2) ||
      head == (tail._1 + 1, tail._2 + 2) ||
      head == (tail._1 + 2, tail._2 + 1) ||
      head == (tail._1 + 2, tail._2 + 1) ||
      head == (tail._1 + 1, tail._2 + 2)
    )) Some(path)
  else first(ordered_moves(dim, path, path(0)), x => first_closed_tour_heuristic(dim, x :: path))
}
*/
//(3c) Same as (3b) but searches for *open* tours.

@tailrec
def first_tour_heuristicT(dim: Int, path: Path, accPath: List[Path]): Option[Path] = accPath match {
  case Nil => None
  case x::xs =>
    if (x.size == dim * dim) Some(x)
    else first_tour_heuristicT(dim, path, ordered_moves(dim, path, accPath(0)(0)).map(_::x) ::: xs)
}
def first_tour_heuristic(dim: Int, path: Path): Option[Path] = {
  first_tour_heuristicT(dim, path, path :: ordered_moves(dim, path, path(0)).map(List(_)))
}
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

//(3c) Same as (3b) but searches for *open* tours.
/*
@tailrec
def first_tour_heuristic(dim: Int, path: Path): Option[Path] = {
  if (path.size == dim * dim) Some(path)
  else first(ordered_moves(dim, path, path(0)), x => first_closed_tour_heuristic(dim, x :: path))
}
*/
@tailrec
def first_tour_heuristicT(dim: Int, path: Path, accPath: List[Path]): Option[Path] = accPath match {
  case Nil => None
  case x::xs =>
    //if (dim * dim < x.size) first_tour_heuristicT(dim, path, xs)
    if (x.size == dim * dim) Some(x)
    else first_tour_heuristicT(dim, path, ordered_moves(dim, path, accPath(0)(0)).map(_::x) ::: xs)
    //else first_tour_heuristicT(dim, path, List(ordered_moves(dim, path, path(0)).head).map(_::x) ::: xs)
    //else first_tour_heuristicT(dim, path, ordered_moves(dim, path, path(0)).map(_::x) ::: xs)
    //else first_tour_heuristicT(dim, path, path.map(_::x) ::: xs)
    //ordered_moves(dim, path, path(0)), x => first_closed_tour_heuristicT(dim, x :: path)
}
def first_tour_heuristic(dim: Int, path: Path): Option[Path] = {
  //first_tour_heuristicT(dim, path, ordered_moves(dim, path, path(0)).map(List(_)))
  first_tour_heuristicT(dim, path, path.map(List(_)))
}

//try with legal moves

//total = dim: INT INT
//coins = path: List[Int] List[Pos](Path)
//cs = path?
def search(total: Int, coins: List[Int], cs: List[Int]): Option[List[Int]] = {
  if (total < cs.sum) None
  else if (cs.sum == total) Some(cs)
  else first_positive(coins, (c: Int) => search(total, coins, c::cs))
}
@tailrec
def searchT(total: Int, coins: List[Int],
            acc_cs: List[List[Int]]): Option[List[Int]] = acc_cs match {
  case Nil => None
  case x::xs =>
    if (total < x.sum) searchT(total, coins, xs)
    else if (x.sum == total) Some(x)
    else searchT(total, coins, coins.filter(_ > 0).map(_::x) ::: xs)
}
val start_acc = coins.filter(_ > 0).map(List(_))
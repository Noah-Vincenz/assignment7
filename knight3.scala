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

//(3a) Complete the function that calculates a list of onward
// moves like in (1b) but orders them according to the Warnsdorf’s 
// rule. That means moves with the fewest legal onward moves 
// should come first.

def ordered_moves(dim: Int, path: Path, x: Pos): List[Pos] = {
  List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2)).filter(x => is_legal(dim, path)(x)).sortWith((legal_moves(dim, path, _).size < legal_moves(dim, path, _).size))
}

sort by the number of onward legal moves

def lessOnwardMoves(p1: Pos, p2: Pos, dim: Int, path: Path): Boolean = (p1, p2) match {
  case (legal_moves(dim, path, p1).size < legal_moves(dim, path, p2).size) => true
  case (legal_moves(dim, path, p1).size > legal_moves(dim, path, p2).size) => false
}


//(3b) Complete the function that searches for a single *closed* 
// tour using the ordered moves function.

def first_closed_tour_heuristic(dim: Int, path: Path): Option[Path] = ...

//(3c) Same as (3b) but searches for *open* tours.

def first_tour_heuristic(dim: Int, path: Path): Option[Path] = ...

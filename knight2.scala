// Part 2 about finding a single tour for a board
//================================================

// copy any function you need from file knight1.scala

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions
def is_legal(dim: Int, path: Path)(x: Pos): Boolean = {
  if (x._1 >= 0 && x._1 < dim && x._2 >= 0 && x._2 < dim && !path.contains(x)) true
  else false
}
def legal_moves(dim: Int, path: Path, x: Pos): List[Pos] = {
  val list = List((x._2 + 1, x._1 + 2), (x._2 + 2, x._1 + 1), (x._2 + 2, x._1 - 1), (x._2 + 1, x._1 - 2), (x._2 - 1, x._1 - 2), (x._2 - 2, x._1 - 1), (x._2 - 2, x._1 + 1), (x._2 - 1, x._1 + 2))
  list.filter(x => is_legal(dim, path)(x))
}


//(2a) Implement a first-function that finds the first 
// element, say x, in the list xs where f is not None. 
// In that case return f(x), otherwise none.

def first(xs: List[Pos], f: Pos => Option[Path]): Option[Path] = {
  if (xs.isEmpty) None
  else if (f(xs.head) != None) f(xs.head)
  else first(xs.drop(1), f)
}

//(2b) Implement a function that uses the first-function for
// trying out onward moves, and searches recursively for an 
// *open* tour on a dim * dim-board.

def first_tour(dim: Int, path: Path): Option[Path] = {
  //use legalmoves -- how do we copy? immport???
  //backtrack if tour is not complete
  if (path.size == dim * dim ) Some[path]
  else for (x <- legal_moves(dim, path, path(0))) yield first(path, Some[x])
}

def first_tour(dim: Int, path: Path): Option[Path] = {
  for ((x,y) <- legal_moves(dim, path, path(0))) yield first(path, Some[(x,y)])
}

def first_tour(dim: Int, path: Path): Option[Path] = {
  for ((x,y) <- legal_moves(dim, path, path(0))) yield first(legal_moves(dim, path, path(0)), Some[(x,y)])
}



/**
 * User: bar0n
 * Date: 21.10.2014
 * Time: 23:31
 */
class Pouring(capacity: Vector[Int]) {
  type State = Vector[Int]
  val initialState = capacity map (x => 0)
  val glasses = 0 until capacity.length
  val moves =
    (for (glass <- glasses) yield Empty(glass)) ++
      (for (glass <- glasses) yield Fill(glass)) ++
      (for {
        glass <- glasses
        glass2 <- glasses
        if glass != glass2
      } yield Pour(glass, glass2))
  val initialPath = new Path(Nil)
  val pathsSet = from(Set(initialPath), Set(initialState))

  def from(paths: Set[Path], explore: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves.map(x => path.extend(x))
        if !explore.contains(next.endState)
      } yield next
      paths #:: from(more, explore ++ more.map(x => x.endState))
    }

  def solution(target: Int): Stream[Path] = for {
    pathSet <- pathsSet
    path <- pathSet
    if path.endState.contains(target)
  } yield path

  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  /*  def from(paths: Set[Path]): Stream[Set[Path]] =
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          next <- moves.map(x => path.extend(x))
} yield next
paths #:: from(more)
}

val pathsSet = from(Set(initialPath))*/

  class Path(history: List[Move]) {
    def extend(move: Move) = new Path(move :: history)

    override def toString: String = history.reverse.mkString(" ") + "-->" + endState

    def endState = history.foldRight(initialState)((move, acum) => move.change(acum))
  }

}


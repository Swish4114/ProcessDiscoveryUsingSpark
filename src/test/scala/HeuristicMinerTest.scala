import de.bachelorarbeit.geneticminer.{Edges, EventLog, HeuristicMiner, Trace, CausalMatrix, Places, Place}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class HeuristicMinerTest extends FunSuite {
  test("heuristicMiner.1") {
    val autoKauf = EventLog(
      Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "i", "a", "i", "a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "d", "c", "e", "h", "d", "c", "e", "f", "g")
    )
    val miner = new HeuristicMiner(autoKauf, 0.2, 0.1, 0.4)
    val individual = miner.getIndividual
    individual.startTask should be ("a")
    individual.endTask should be ("g")
    individual.causalMatrix should be (CausalMatrix("e" -> Edges(Places(Place("d"), Place("c")),Places(Place("f", "h"))), "h" -> Edges(Places(Place("e")),Places(Place("d"), Place("c"))), "b" -> Edges(Places(Place("a")),Places(Place("d"), Place("c"))), "d" -> Edges(Places(Place("h", "b")),Places(Place("e"))), "g" -> Edges(Places(Place("f")),Places()), "a" -> Edges(Places(Place("i")),Places(Place("i", "b"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "c" -> Edges(Places(Place("h", "b")),Places(Place("e"))), "f" -> Edges(Places(Place("e")),Places(Place("g")))))
  }

  test("heuristicMiner.2") {
    val proDiGenEventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y")
    )
    val miner = new HeuristicMiner(proDiGenEventLog, 0.7, 0.1, 0.05)
    val individual = miner.getIndividual
    individual.causalMatrix should be (Map("e" -> Edges(Places(Place("c")),Places(Place("y"))), "b" -> Edges(Places(Place("x")),Places(Place("c"))), "d" -> Edges(Places(Place("c")),Places(Place("y"))), "y" -> Edges(Places(Place("d", "e")),Places()), "a" -> Edges(Places(Place("x")),Places(Place("c"))), "x" -> Edges(Places(),Places(Place("a", "b"))), "c" -> Edges(Places(Place("a", "b")),Places(Place("d", "e")))))
    individual.startTask should be ("x")
    individual.endTask should be ("y")
  }
}

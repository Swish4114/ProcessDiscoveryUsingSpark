import de.bachelorarbeit.geneticminer.EventLog
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class EventLogTest extends FunSuite {
  test("EventLog.1") {
    val trace1 = Vector("a", "b")
    val trace2 = Vector("x", "y", "z")
    val trace3 = Vector("t", "z", "u", "k")

    val eventLog = EventLog(trace1, trace2, trace3)

    eventLog.size should be (3)
    eventLog.head should be (trace1)
    eventLog(1) should be (trace2)
    eventLog(2) should be (trace3)
  }
}

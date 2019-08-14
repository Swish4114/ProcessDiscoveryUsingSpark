package de.bachelorarbeit.geneticminer

import scala.collection.immutable
import scala.collection.parallel.{ParMap, ParSeq, SeqSplitter}
import scala.collection.parallel.immutable.ParVector
class EventLog(traces: Trace*) extends ParSeq[Trace] {

  private final val log: ParVector[Trace] = ParVector(traces: _*)

  final val activities: Vector[Task] = log.flatten.distinct.toVector
  final val numAllActivities = log.map(trace => trace.size).sum

  final val numberAB: ParMap[(Task, Task), Int] = log.flatMap(trace => trace.sliding(2))
                                                  .map(pair => (pair.head, pair(1)))
                                                  .groupBy(identity)
                                                  .mapValues(_.size)
  final val numberABA: ParMap[(Task, Task, Task), Int] = log.flatMap(trace => trace.sliding(3))
                                                         .filter(triple => triple.size > 2 && triple.head == triple(2))
                                                         .map(triple => (triple.head, triple(1), triple(2)))
                                                         .groupBy(identity)
                                                         .mapValues(_.size)

  final val inputDependenciesForMutation: immutable.Map[Task, IndexedSeq[Task]] =
    activities.map(task => (task,
      log.flatMap(trace => trace.take(trace.lastIndexOf(task)).filter(t => t != task)).distinct.toIndexedSeq)).toMap

  final val outputDependenciesForMutation: immutable.Map[Task, IndexedSeq[Task]] =
    activities.map(task => (task,
      log.flatMap(trace => trace.drop({trace.indexOf(task) match {
        case -1 => trace.size
        case x => x
      }}).filter(t => t != task)).distinct.toIndexedSeq)).toMap

  def getDependencyMeasure(a: Task, b: Task): Double = {
    if (!a.equals(b)) {
      val numABA: Int = numberABA.getOrElse((a, b, a), 0)
      if (numABA > 0) {
        val numBAB: Int = numberABA.getOrElse((b, a, b), 0)
        (numABA + numBAB) / (numABA + numBAB + 1).toDouble
      } else {
        val numAB: Int = numberAB.getOrElse((a, b), 0)
        val numBA: Int = numberAB.getOrElse((b, a), 0)
        (numAB - numBA) / (numAB + numBA + 1).toDouble
      }
    } else {
      val numAB: Int = numberAB.getOrElse((a, b), 0)
      numAB / (numAB + 1).toDouble
    }
  }

  def apply(idx: Int): Trace = log.apply(idx)

  override def splitter: SeqSplitter[Trace] = log.splitter

  override def seq: Seq[Trace] = log.seq

  override def length: Int = log.length
}


object EventLog {
  def apply(traces: Trace*): EventLog = new EventLog(traces: _*)
}

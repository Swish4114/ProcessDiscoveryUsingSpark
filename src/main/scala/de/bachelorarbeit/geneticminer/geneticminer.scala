package de.bachelorarbeit

import scala.collection.mutable

package object geneticminer {
  type Task = String

  type Trace = Vector[Task]
  def Trace(tasks: Task*): Vector[Task] = Vector(tasks: _*)

  type Individuals = IndexedSeq[Individual]

  type Place = mutable.Set[Task]
  def Place(tasks: Task*): mutable.Set[Task] = mutable.Set(tasks: _*)

  type Places = mutable.ArrayBuffer[Place]
  def Places(places: Place*): mutable.ArrayBuffer[Place] = mutable.ArrayBuffer(places: _*)

  case class Edges(var inputs: Places, var outputs: Places)
  type CausalMatrix = mutable.Map[Task, Edges]
  def CausalMatrix(pairs: (Task, Edges)*): mutable.Map[Task, Edges] = mutable.Map(pairs: _*)

  type RawIndividual = (CausalMatrix, Task, Task, Double, Double, Double)
}

package ch.bharanya

object PrioQueueTest extends App{
  case class Elem(var priority: Int, i: Int)

  def MyOrdering = new Ordering[Elem] {
    def compare(a : Elem, b : Elem) = a.priority.compare(b.priority)
  }

  val pq = new scala.collection.mutable.PriorityQueue[Elem]()(MyOrdering)  ++ List(Elem(1,1), Elem(0,0), Elem(2,2))

  val pq2 = pq.clone

  pq2.find(x => x.priority == 0) match {
    case Some(elem: Elem) => elem.priority = 3
    case None => println("Not found")
  }

  println(pq)
  println(pq.dequeue())
  println(pq.dequeue())
  println(pq.dequeue())


  println(pq2)
  println(pq2.dequeue())
  println(pq2.dequeue())
  println(pq2.dequeue())
}

// since scala 2.12
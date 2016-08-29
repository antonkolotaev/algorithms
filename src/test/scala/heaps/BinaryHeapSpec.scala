package heaps

import org.scalatest.{Matchers, FunSpec}

import scala.util.Random

class BinaryHeapSpec extends FunSpec with Matchers {

    for (i <- 0 to 100) {
        val lst = sorting.randomInts(i).toList
        it (s"heapify [${lst mkString ","}]") {
            val heap = new BinaryHeap(lst)
            heap.verify()
        }
    }

    it ("playground") {
        val heap = new BinaryHeap(List(1,44))
        heap.verify()
    }

    val toInsert = new BinaryHeap(sorting.randomInts(10, 3))

    val rnd = new Random()

    for (i <- -5 to 5) {
        it (s"insert $i into $toInsert") {
            toInsert insert i
            toInsert.verify()
        }
    }

}

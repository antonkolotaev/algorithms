package heaps

import org.scalatest.{Matchers, FunSpec}

class BinaryHeapSpec extends FunSpec with Matchers {

 /*   for (i <- 0 to 100) {
        val lst = sorting.randomInts(i).toList
        it (s"checking [${lst mkString ","}]") {
            val heap = new BinaryHeap(lst)
            heap.verify()
        }
    } */

    it ("playground") {
        val heap = new BinaryHeap(List(1,44))
        heap.verify()
    }



}

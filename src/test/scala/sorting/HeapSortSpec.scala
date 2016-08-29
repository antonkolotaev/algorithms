package sorting

import heaps.BinaryHeap
import org.scalatest.{FunSpec, Matchers, FlatSpec}

import scala.collection.mutable.ArrayBuffer

class HeapSortSpec extends FunSpec with Matchers {

    def checkHeapSort[T : Ordering](input : List[T]) = {

        val heap = new BinaryHeap(input)

        val output = new ArrayBuffer[T]()

        while (heap.nonEmpty)
            output append heap.pop()

        output should contain theSameElementsAs input

        if (output.nonEmpty)
            output zip output.tail foreach { case (x,y) => x should be <= y }
    }


    for (i <- 0 to 100) {
        val lst = randomInts(i).toList
        it (s"checking [${lst mkString ","}]") {
            checkHeapSort(lst)
        }
    }

}

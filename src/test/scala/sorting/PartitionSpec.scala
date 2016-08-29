package sorting

import org.scalatest._

class PartitionSpec extends FunSpec with Matchers  {

    def checkPartition[T : Ordering](partition : (Array[T], Int, Int, Ordering[T]) => Int)(array : Array[T]) =
    {
        val ordering = implicitly[Ordering[T]]
        val input = array.clone()

        val p = partition(array, 0, array.length, ordering)

        if (array.nonEmpty && p < array.length) {
            val (left, right) = array.splitAt(p)
            if (left.nonEmpty) {
                val leftMax = left.max
                if (right.nonEmpty) {
                    val rightMin = right.min
                    assert(ordering.lt(leftMax, rightMin))
                }
            }
        }

        array should contain theSameElementsAs input

        p
    }

    for (i <- 0 to 100) {
        val array = randomInts(i).toArray
        it (s"checking [${array mkString ","}]") {
            checkPartition(QuickSort.leftmostPartition[Int])(array)
        }
    }

    it ("playground") {
        checkPartition(QuickSort.leftmostPartition[Int])(Array(-31))
    }


}

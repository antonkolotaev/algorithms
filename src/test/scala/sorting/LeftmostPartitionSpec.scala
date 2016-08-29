package sorting

import org.scalatest._

class LeftmostPartitionSpec extends FunSpec with Base  {


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

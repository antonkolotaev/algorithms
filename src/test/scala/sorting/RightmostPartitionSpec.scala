package sorting

import org.scalatest._

class RightmostPartitionSpec extends FunSpec with Base  {


    for (i <- 0 to 100) {
        val array = randomInts(i).toArray
        it (s"checking [${array mkString ","}]") {
            checkPartition(QuickSort.rightmostPartition[Int])(array)
        }
    }

    it ("playground") {
        checkPartition(QuickSort.rightmostPartition[Int])(Array(-31))
    }


}

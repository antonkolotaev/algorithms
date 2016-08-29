package sorting.partition

import org.scalatest._
import sorting._

class RandomSpec extends FunSpec with Base  {

    for (i <- 0 to 100) {
        val array = randomInts(i).toArray
        it (s"checking [${array mkString ","}]") {
            checkPartition(QuickSort.randomPartition[Int])(array)
        }
    }

    it ("playground") {
        checkPartition(QuickSort.leftmostPartition[Int])(Array(-31))
    }


}

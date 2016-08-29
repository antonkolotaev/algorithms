import org.scalatest.Matchers

package object sorting {

    val random = new scala.util.Random

    def randomInts(n: Int, upperBound : Int = 50) =
        Stream.continually(random.nextInt % upperBound).take(n)

    trait Base extends Matchers {
        def checkPartition[T: Ordering](partition: (Array[T], Int, Int) => Int)(array: Array[T]) = {
            val ordering = implicitly[Ordering[T]]
            val input = array.clone()

            val p = partition(array, 0, array.length)

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
    }
}

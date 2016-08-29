package heaps

import scala.collection.mutable.ArrayBuffer

class BinaryHeap[T : Ordering](elements : Seq[T]) {

    val ordering = implicitly[Ordering[T]]

    def left(idx : Int) = 2 * idx + 1  // 0 -> (1,2); 1 -> (3,4), 2 -> (5,6)
    def right(idx : Int) = 2 * idx + 2

    def parent(idx : Int) = (idx - 1) / 2 // 0 <- (1,2); 1 <- (3,4), 2 <- (5,6)

    private val xs = new ArrayBuffer[T]
    xs.appendAll(elements)

    private def swap(i : Int, j : Int) = {
        val t = xs(i)
        xs(i) = xs(j)
        xs(j) = t
    }

    def heapify(i : Int) : Unit = {

        val l = left(i)
        val r = right(i)

        var smallest = i

        if (l < xs.length && ordering.lt(xs(l), xs(smallest)))
            smallest = l
        if (r < xs.length && ordering.lt(xs(r), xs(smallest)))
            smallest = r

        if (i != smallest) {
            swap(i, smallest)
            heapify(smallest)
        }
    }

    if (xs.length > 1)
        for (i <- parent(xs.length - 1) to (0, -1))
            heapify(i)


    def verify() = {
        if (xs.length > 1)
            for (i <- 0 to parent(xs.length - 1))
            {
                assert(ordering.lteq(xs(i), xs(left(i))))
                if (right(i) < xs.length)
                    assert(ordering.lteq(xs(i), xs(right(i))))
            }
    }
}

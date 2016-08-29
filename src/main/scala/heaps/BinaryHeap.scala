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

    private def bubbleDown(i : Int) : Unit = {

        val l = left(i)
        val r = right(i)

        var smallest = i

        if (l < xs.length && ordering.lt(xs(l), xs(smallest)))
            smallest = l
        if (r < xs.length && ordering.lt(xs(r), xs(smallest)))
            smallest = r

        if (i != smallest) {
            swap(i, smallest)
            bubbleDown(smallest)
        }
    }

    if (xs.length > 1)
        for (i <- parent(xs.length - 1) to (0, -1))
            bubbleDown(i)


    def verify() = {
        if (xs.length > 1)
            for (i <- 0 to parent(xs.length - 1))
            {
                assert(ordering.lteq(xs(i), xs(left(i))))
                if (right(i) < xs.length)
                    assert(ordering.lteq(xs(i), xs(right(i))))
            }
    }

    private def bubbleUp(i : Int) : Unit = {
        if (i > 0) {
            val p = parent(i)
            if (!ordering.lteq(xs(p), xs(i))) {
                swap(p, i)
                bubbleUp(p)
            }
        }
    }

    def insert(x : T) = {
        xs append x
        bubbleUp(xs.length - 1)
    }

    def pop() : T = {
        if (xs.isEmpty)
            throw new Exception(s"popping from empty heap")
        val ret = xs.head
        xs(0) = xs(xs.length - 1)
        xs.remove(xs.length - 1) // or more idiomatic way to say xs.pop_back()
        bubbleDown(0)
        ret
    }

    def nonEmpty = xs.nonEmpty

    override def toString = xs mkString ("[",",","]")
}

package sorting

object QuickSort {


    /**
     * Quick sort strategy
     * @param partition -- function that accepts a subrange in an array (array, lo, hi) and partitions it somehow
     *                     it returns index 'p' that forms two ranges:
     *                      [lo, p) is less than some pivot
     *                      [p, hi) is greater or equal to the pivot
     * @param array - an array to be sorted in-place
     * @tparam T - a type that supports comparisons
     * @return - nothing since the array is sorted in place
     */
    def apply[T : Ordering](partition : (Array[T], Int, Int, Ordering[T]) => (Int,Int))
                           (array : Array[T]) : Unit =
    {
        def impl(lo : Int, hi : Int) : Unit =
        {
            if (lo < hi) {
                val (lo_to, hi_from) = partition(array, lo, hi, implicitly[Ordering[T]])
                impl(lo, lo_to)
                impl(hi_from, hi)
            }
        }

        impl(0, array.length)
    }

    /**
     * Partitions somehow array[lo until hi] on two parts: the left one is less than 'p' and the right one is greater or equal to 'p'
     * @param array - array to be partitioned
     * @param lo - low inclusive index of the subarray in 'array' to be partitioned
     * @param hi - high exclusive index of the subarray in 'array' to be partitioned
     * @param p - pivot
     * @return position of the partition k:
     *              all lo <= i < k, k <= j < hi: array[i] < array[j]
     */
    def partition[T : Ordering](array: Array[T], lo : Int, hi : Int, p : T) : Int =
    {
        val ordering = implicitly[Ordering[T]]

        var i = lo
        var j = hi - 1
        while (i < j) {
            while (i < j &&  ordering.lt(array(i), p)) i += 1
            while (i < j && !ordering.lt(array(j), p)) j -= 1
            if (i < j)
            {
                val t = array(i)
                array(i) = array(j)
                array(j) = t
            }
        }
        if (!ordering.lt(array(i), p))
            i
        else
            j + 1
    }

    object Pivot {

        def leftmost[T](array : Array[T], lo : Int, hi : Int) = array(lo)

        private val rnd = new scala.util.Random

        def random[T](array : Array[T], lo : Int, hi : Int) = array(lo + rnd.nextInt(hi - lo))

    }

    def leftmostPartition[T : Ordering](array: Array[T], lo : Int, hi : Int) =
        if (lo < hi)
            partition(array, lo, hi, Pivot.leftmost[T](array, lo, hi))
        else
            0

    def randomPartition[T : Ordering](array: Array[T], lo : Int, hi : Int) =
        if (lo < hi)
            partition(array, lo, hi, Pivot.random[T](array, lo, hi))
        else
            0
}

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

    def partition[T](pivot : (Array[T], Int, Int, Ordering[T]) => T)
                    (array: Array[T], lo : Int, hi : Int, ordering : Ordering[T]) : Int =
    {
        val p = pivot(array, lo, hi, ordering)
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

        def leftmost[T](array : Array[T], lo : Int, hi : Int, ordering : Ordering[T]) = array(lo)

    }

    def leftmostPartition[T](array: Array[T], lo : Int, hi : Int, ordering : Ordering[T]) =
        if (lo < hi)
            partition(Pivot.leftmost[T])(array, lo, hi, ordering)
        else
            0
}

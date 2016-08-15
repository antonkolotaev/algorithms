package sorting

object MergeSort {

    /**
     * Merges two sorted lists into a new sorted list
     * @param xs -- first sorted list to merge
     * @param ys -- second sorted list to merge
     * @param acc -- accumulator keeping reversed result
     * @tparam T -- element type for which an Ordering[T] should be available
     * @return
     */
    def mergeSortedLists[T : Ordering](xs : List[T], ys : List[T], acc : List[T]) : List[T] = {
        (xs, ys) match {
            case (Nil, _) => (ys.reverse ++ acc).reverse
            case (_, Nil) => (xs.reverse ++ acc).reverse
            case (x :: xtail, y :: ytail) =>
                val (e, xs1, ys1) =
                    if (implicitly[Ordering[T]].lt(x,y))
                        (x, xtail, y :: ytail)
                    else
                        (y, x :: xtail, ytail)
                mergeSortedLists(xs1, ys1, e :: acc)
        }
    }
    /**
     * Merges input into output
     * @param output -- contains lists of size power(2, N) or Nil
     * @param input -- list to be merged in (precondition: its size is equal to the size of the first element of output)
     * @return 'output' with 'input' merged into it
     */
    private def mergeImpl[T : Ordering](output : List[List[T]], input : List[T]) : List[List[T]] =
    {
        val ordering = implicitly[Ordering[T]]

        if (output.head == Nil)
            input :: output.tail
        else {
            Nil :: mergeImpl(output.tail, mergeSortedLists(output.head, input, List.empty[T]))
        }
    }

    /**
     * Merge sort for immutable lists
     */
    def apply[T : Ordering](xs : Seq[T]) : List[T] =
    {
        val built = xs.foldLeft(List.empty[List[T]]){ (acc,x) => mergeImpl[T](acc, x :: Nil) }
        
        built.foldLeft(List.empty[T]){ (acc, x) => mergeSortedLists(acc, x, List.empty[T]) }
    }


}

package sorting

import org.scalatest.{Matchers, FlatSpec}

class QuickSortSpec extends FlatSpec with Matchers {

/*    def checkQuickSort[T : Ordering](input : Array[T]) = {

        val output = input.clone()

        QuickSort(QuickSort.pivotLeft[T])(output)

        output should contain theSameElementsAs input

        if (output.nonEmpty)
            output zip output.tail foreach { case (x,y) => x should be <= y }
    }

    "empty list" should "be sorted" in {

        checkQuickSort(Array.empty[Int])

    }

    "sorted list" should "be left as is" in {

        checkQuickSort(Array(1))

    }

    "unsorted list" should "be sorted" in {

        checkQuickSort(Array(5,2,1,9,7,0,7))

    }

    */

}

package sorting

import org.scalatest.{Matchers, FlatSpec}

class MergeSortSpec extends FlatSpec with Matchers {

    def checkMergeSort[T : Ordering](input : List[T]) = {

        val output = MergeSort(input)

        output should contain theSameElementsAs input

        if (output.nonEmpty)
            output zip output.tail foreach { case (x,y) => x should be <= y }
    }


    "empty list" should "be sorted" in {

        checkMergeSort(List.empty[Int])

    }

    "sorted list" should "be left as is" in {

        checkMergeSort(List(1,2,3))

    }

    "unsorted list" should "be sorted" in {

        checkMergeSort(List(5,2,1,9,7,0,7))

    }

}

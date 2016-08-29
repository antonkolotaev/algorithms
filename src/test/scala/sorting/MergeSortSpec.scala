package sorting

import org.scalatest.{FunSpec, Matchers, FlatSpec}

class MergeSortSpec extends FunSpec with Matchers {

    def checkMergeSort[T : Ordering](input : List[T]) = {

        val output = MergeSort(input)

        output should contain theSameElementsAs input

        if (output.nonEmpty)
            output zip output.tail foreach { case (x,y) => x should be <= y }
    }


    for (i <- 0 to 100) {
        val lst = randomInts(i).toList
        it (s"checking [${lst mkString ","}]") {
            checkMergeSort(lst)
        }
    }

}

import scala.io.Source

object Problems {

  def main(args: Array[String]) = {


    def runProblems = {
      selectionSortProblem
      bubbleSortProblem
      mergeSortProblem
      quickSortProblem
      heapSortProblem
    }
    runProblems

    def selectionSortProblem = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)
      def selectionSort(input: Array[Int]): Array[Int] = {
        if (input.size == 1){
          input 
        } else {
          val biggest = input.foldLeft(0)(math.max(_,_))
          Array(biggest) ++ selectionSort(input diff Array(biggest))
        }
      }
      val result = run(selectionSort(input))
      println(result.mkString(" "))
      result
    }

    def bubbleSortProblem = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)
      def bubbleSort(input: Array[Int]) = {
        var inputCopy = input.clone
        for(i <- inputCopy.size-1 to 1 by -1){
          for(j <- 0 to i){
            if (inputCopy(i) > inputCopy(j)) swap(inputCopy, i, j)
          }
        }
        inputCopy 
      }
      def swap(input:Array[Int], a:Int, b:Int) = {
        val temp = input(a)
        input(a) = input(b)
        input(b) = temp
      }
      val result = run(bubbleSort(input))
      println(result.mkString(" "))
      result
    }

    def mergeSortProblem = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)
      def mergeSort(input: Array[Int]): Array[Int] = {
        val last = input.size
        val middle = last/2 + last%2
        if(input.size > 1){
          val split = input.splitAt(middle)
          val left = mergeSort(split._1)
          val right = mergeSort(split._2)
          merge(left, right)
        } else {
          input
        }
      }

      def merge(left: Array[Int], right: Array[Int]): Array[Int] = {
        if(left.isEmpty || right.isEmpty) {
          left ++ right
        } else if(left.head > right.head){
          Array(left.head) ++ merge(left.tail, right)
        } else {
          Array(right.head) ++ merge(left, right.tail)
        }
      }
      val result = run(mergeSort(input))
      println(result.mkString(" "))
      result
    }

    def quickSortProblem = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)
      def quickSort(input:Array[Int]): Array[Int] = {
        if(input.size == 0) {
          Array()
        } else {
          val splitNumber = input(0)
          val (less, more) = input.tail.partition{_ > splitNumber}
          quickSort(less) ++ Array(splitNumber) ++ quickSort(more)
        }
      }
      val result = run(quickSort(input))
      println(result.mkString(" "))
      result
    }

    def heapSortProblem = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)
      def heapSort(input:Array[Int]): Array[Int] = {
        class PriorityQueue(val heap: Array[Int]) {
          def size = heap.size
          def parent(node:Int): Int = {
            if (node == 1) {
              -1
            } else  {
              node / 2
            }
          }
          def youngestChild(node:Int): Int = {
            2 * node 
          }
          def add(node:Int): PriorityQueue = {
            bubbleUp( new PriorityQueue( this.heap ++ Array(node)), size+1)
          }
          def bubbleUp(queue: PriorityQueue, node:Int): PriorityQueue = {
            if(queue.parent(node) == -1) {
              queue 
            } else {
              val parentVal = queue.heap(queue.parent(node)) 
              val nodeVal = queue.heap(node) 
              if(parentVal > nodeVal) {
                val result = swap(queue.heap, queue.parent(node), node) 
                bubbleUp( new PriorityQueue(result), queue.parent(node))
              } else {
                queue 
              } 
            } 
          }
          def swap(input:Array[Int], a:Int, b:Int) = {
            val tempArray = input.clone
            var temp = tempArray(a)
            tempArray(a) = tempArray(b)
            tempArray(b) = temp
            val returnArray = tempArray.clone 
            returnArray
          }
        }
        input
      }
      val result = run(heapSort(input))
      println(result.mkString(" "))
      result
    }

    def run[R](block: => R): R = {
      val result = time(block)
      println(result)
      result
    }

    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result
    }
  }
}

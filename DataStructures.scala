import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object DataStructures{

  def main(args: Array[String]) = {


    def runProblems = {
      heapSortProblem
      stackImplementation
    }
    runProblems

    def stackImplementation = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)
      class Stack(input: ArrayBuffer[Int]){

        def push(item:Int) = {
          input = Array(item) ++ input
        }

        def pop() = {
          val result = input.head
          input = input.tail
          result
        }

        def peek() = {
          input.head
        }

        def isEmpty() = {
          input.isEmpty 
        }
      }
      val stack = new Stack(input)
      println(stack.pop)
      stack.push(3)
      println(stack.pop)
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

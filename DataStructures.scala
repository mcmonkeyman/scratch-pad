import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object DataStructures{

  def main(args: Array[String]) = {


    def runProblems = {
      //heapSortProblem
      //stackImplementation
      //queueImplementation
      treeImplementation
    }
    runProblems

    def stackImplementation = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)
      class Stack(inputArray: Array[Int]){
        var input = inputArray.clone

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

        override def toString() = {
          input.mkString(" ")
        }
      }
      val stack = new Stack(input)

      val operations = List(
        funcOf(stack.push(3)),
        funcOf(stack.push(55)),
        funcOf(stack.push(33)),
        funcOf(stack.pop()),
        funcOf(stack.pop()),
        funcOf(stack.peek()),
        funcOf(stack.pop())
      )

      operations.map(operation => performOperation(operation, stack))
    }

    def queueImplementation = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)

      class Queue(inputArray: Array[Int]){
        var input = inputArray.clone

        def enqueue(item:Int) = {
          input = input ++ Array(item)
        }

        def dequeue() = {
          val result = input.head
          input = input.tail
          result
        }

        def front = {
          input.head
        }

        def isEmpty = {
          size == 0 
        }

        def size = {
          input.size 
        }

        override def toString() = {
          input.mkString(" ")
        }
      }
      val queue = new Queue(input)

      val operations = List(
        funcOf(queue.enqueue(666)),
        funcOf(queue.front),
        funcOf(queue.isEmpty),
        funcOf(queue.size),
        funcOf(queue.dequeue())
      )

      operations.map(operation => performOperation(operation, queue))
    }

    def treeImplementation = {
      val input = Array(1,4,5,6,7,1,23,0,45,5,6)

      class Tree(inputArray: Array[Int]){
        var input = inputArray.clone

        def enqueue(item:Int) = {
          input = input ++ Array(item)
        }

        def dequeue() = {
          val result = input.head
          input = input.tail
          result
        }

        def front = {
          input.head
        }

        def isEmpty = {
          size == 0 
        }

        def size = {
          input.size 
        }

        override def toString() = {
          input.mkString(" ")
        }
      }
      val tree = new Tree(input)

      val operations = List(
        funcOf(tree.enqueue(666)),
        funcOf(tree.front),
        funcOf(tree.isEmpty),
        funcOf(tree.size),
        funcOf(tree.dequeue())
      )

      operations.map(operation => performOperation(operation, tree))
    }


    def funcOf(param: => Any) = () => param

    def performOperation(operation: () => Any, dataStructure: Any) = {
      println("DS Before: " + dataStructure)
      val result = operation()
      println("Result of operation: " + result)
      println("DS After: " + dataStructure)
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
  }
}

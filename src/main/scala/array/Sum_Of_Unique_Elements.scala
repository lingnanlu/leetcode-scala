package array

import scala.collection.mutable

/**
 * 本题就是为了练习各种map, list的操作
 *
 * 方法一:
 * 过程式
 *
 * 方法二:
 * 过程式
 */
object Sum_Of_Unique_Elements {

  object First {
    def sumOfUnique(nums: Array[Int]): Int = {
      val counts = mutable.Map[Int, Int]()
      nums.foreach(n => {
        if (counts.contains(n)) {
          counts += (n -> (counts(n) + 1))
        } else {
          counts += (n -> 1)
        }
      })
      counts.filterInPlace((_, v) => v == 1)
      //这是一个parameterless function, 不用加括号
      counts.keys.sum
    }
  }

  object Second {
    def sumOfUnique(nums: Array[Int]): Int = {
      val counts = mutable.Map[Int, Int]()
      for (n <- nums) {
        if (counts.contains(n)) {
          counts += (n -> (counts(n) + 1))
        } else {
          counts += (n -> 1)
        }
      }

      var sum = 0
      for ((k, v) <- counts) {
        if (v == 1) {
          sum += k
        }
      }
      sum
    }
  }

  object Third {
    def sumOfUnique(nums: Array[Int]): Int = {

      // 这个函数有一个参数, 这个参数是一个元组类型
      val f =  (pair:(Int, Array[Int])) => (pair._1, pair._2.length)
      nums
        .groupBy(identity)
        .map(f)
        .filter(_._2 == 1)
        .keys
        .sum
    }
  }

  // 匿名函数版本
  object Fourth {
    def sumOfUnique(nums: Array[Int]): Int = {
      nums
        .groupBy(identity)
        .map(pair => (pair._1, pair._2.length))
        .filter(_._2 == 1)
        .keys
        .sum
    }
  }

  // 使用case解构版本, 解构必须使用case, 而不能直接
  // map((k, v) => ... ) 这样是无法解构的, 感觉不太方便
  object Fifth {
    def sumOfUnique(nums: Array[Int]): Int = {
      nums
        .groupBy(identity)
        .map({
          case (k, v) => (k, v.length)
        })
        .filter(_._2 == 1)
        .keys
        .sum
    }
  }

  // 桶排序
  object Sixth {
    def sumOfUnique(nums: Array[Int]): Int = {

      // 这是创建初始化一个元素的数组类似于Array(100, 200)
      // val counts = Array[Int](100)

      val counts = Array.fill(100)(0)
      nums.foreach(n => counts(n - 1) += 1)
      counts
        .zipWithIndex
        .filter {case (count, _) => count == 1}
        .map(_._2)
        .sum
    }
  }

}

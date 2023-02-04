package string

import scala.collection.mutable

/**
 * 这题看起来不难, 遇到?时替换一个字母, 并且不与前面和后面的字母相同.
 * 我觉得最关键的是, 如何快速找出一个字母, 不与前面和后面的字母相同
 *
 * 其实就是在26个字母中, 找这么一个元素, 它不等于其它两个字母, 这就是一个查找问题.
 *
 *
 *
 * 方法一:
 * 从a-z按个检查, 注意, 有可能有多个连续的???所以要
 *
 * 这题提交了3次, 总是错误, 看来还是要使用不变式, 不要忘记了不变式
 *
 * 启示:
 * 在多个if分支中, 一定要仔细考察其是否不重不漏, 对于只有一个字符的情况, 本题其实就是漏了这种情况
 *
 * 方法二:
 * 方法一是从a-z中找, 能不能缩小查找范围呢?
 * 其实只要在a, b, c三个字符中找就可以了, 这就缩小的范围.
 * 其实在a-z中找时, 也是只检查了a, b, c三个, 不会再往后找了.
 *
 * 方法三:
 *
 * 方法一要求同时看两边, 所以很复杂(各种边界条件), 所以很容易出错, 能不能只看一边?
 * 其实就是要求s[i] 和s[i - 1]不等, 那么, 我们能不能不找了, 直接让s[i] = s[i - 1] + 1?
 * 这样可能会导致s[i] 和 s[i + 1]相同, 那怎么办? 此时, 就是要修改s[i], 再让它 + 1
 * 但 + 1可能会超过z, 这个模26就好.
 *
 * 启示:
 * 由看两边感觉复杂, 想到只看一边, 代码瞬间变的简单多了.
 * 就是像二维的要看四个方向, 但如果转成一维的, 只看一个方向就行.
 *
 */
object Replace_All_to_Avoid_Consecutive_Repeating_Characters {

  object First {
    def modifyString(s: String): String = {

      // 这是错误了, StringBuilder是一个对象, 这相当于使用sb引用另一个对象, 并没有创建StringBuilder的实例
      // val sb = StringBuilder

      // 只有一个元素的情况特殊处理, 否则在后面不好处理
      if (s.length == 1) {
        if (s(0) == '?') "a" else s
      } else {
        // sb中的是已处理[0, i)之间的, i = 0, 所以不变式为真
        val sb = new StringBuilder()

        for (i <- 0 until s.length) {
          if (s(i) != '?') {
            sb += s(i)
          } else {
            // 排序了单个元素的情况, 剩下的就好分类了.
            if (i == 0) { //第一个
              ('a' to 'z').find(_ != s(i + 1)) match {
                case Some(c) => sb += c
                case None =>
              }
            } else if (i == s.length - 1) { // 最后一个
              ('a' to 'z').find(c => c != s(i - 1) && c != sb(i - 1)) match {
                case Some(c) => sb += c
                case None =>
              }
            } else {      // 中间的
              ('a' to 'z').find(c => c != s(i - 1) && c != s(i + 1) && c != sb(i - 1)) match {
                case Some(c) => sb += c
                case None =>
              }
            }
          }
        }
        sb.toString()
      }

    }
  }

  object Third {
    def modifyString(s: String): String = {

      // 因为每一次都要看前一位, 所以在数组开头放一个a, 会让代码简洁很多.
      val arr = 'a' +: s.toArray

      for (i <- 1 until arr.length) {

        val preChar = arr(i - 1)
        if (arr(i) == '?') {
          // 注意字符加1的过程, 先转成从0开始的整数, 再取模, 再转字符
          arr(i) = ((preChar - 'a' + 1) % 26 + 'a').toChar
        } else if (arr(i) == preChar) {
          // 此时肯定是前一位+1导致的与该位相同, 所以要再修改前一位
          arr(i - 1) = ((preChar - 'a' + 1) % 26 + 'a').toChar
        }
      }

      new String(arr.drop(1))
    }
  }
}

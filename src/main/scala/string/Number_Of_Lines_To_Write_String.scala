package string

/**
 *
 * 方法一:(错误的)
 * 这题读着, 有整除与取余的味道.
 * 其实就是一个整数/100的问题
 * 最关键的是求出这个整数是多少
 *
 * 以上这个方法是错误的, 比如一行是10个单位, 可能每个字母占据6个单位, 这样第一行只使用6个单位, 第二行也是6个单位.
 * 这里6个单位是不能再分的.
 *
 * 方法二:
 * 没什么好方法, 使用人工模拟来做, 这就需要考察不变式功力了
 */
object Number_Of_Lines_To_Write_String {

  // 错误的
  object First {
    def numberOfLines(widths: Array[Int], s: String): Array[Int] = {
      var num = 0
      for (c <- s) {
        num += widths(c - 'a')
      }
      Array((num.toDouble / 100).ceil, num % 100)
    }
  }

  object Second {
    def numberOfLines(widths: Array[Int], s: String): Array[Int] = {
      // 下一个字符要写的行号
      var line = 0
      // 下一个字符要写的行剩余的单位
      var remain = 100

      for (c <- s) {
        // 如果剩余的不够, 写到下一行
        if (remain < widths(c - 'a')) {
          line += 1 // 移动到下一行
          remain = 100 // 重置
          remain -= widths(c - 'a') // 更新
        } else {
          remain -= widths(c - 'a')
        }
      }
      // 100 - remain是使用了的
      Array(line + 1, 100 - remain)



    }
  }


}

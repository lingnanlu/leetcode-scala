package math

/**
 * 方法一:
 * 这题其实就是查表
 *
 * 方法二:
 * 既然每个月都确定了下来, 那么, 累积其实也确定了下来, 可以省去累积的一步
 *
 * 启示:
 * 对于一个固定的数组, 其实还有很多隐含的条件
 * 如累加和也是固定的.
 * 如果在算法中要使用到累加和之类的, 都可以提前计算出来
 *
 */
object Day_of_the_Year {

  /**
   * 注意这里有13个元素, 是为了使用方便
   * array(i) 表示的是什么含义呢?
   * i表示真实月份, array(i)表示1-i的天数累积(包括i)
   * 如array(1) 表示1月天数
   * array(3) 表示1, 2, 3三个月的天数
   */
  val monthsAccu = Array(0, 31, 31, 62, 92, 123, 153, 184, 215, 245, 276, 306, 337)

  def dayOfYear(date: String): Int = {
    val splits = date.split('-').map(_.toInt)
    val (year, month, day) = (splits(0), splits(1), splits(2))

    // 这题最难的就是判断闰年的逻辑
    val leap: Int => Boolean = (year: Int) => year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)
    var days = 0

    days += monthsAccu(month - 1)

    // 处理一下闰年情况
    if (month > 2) {
      if (leap(year)) {
        days += 29
      } else {
        days += 28
      }
    }
    days += day
    days
  }
}

package tree

import scala.collection.mutable.ListBuffer

/**
 * 树的结果不容易使用scala的集合来做(因为集合其实都是线性的).
 * 所以还是乖乖使用dfs
 *
 * 方法一:
 * 使用dfs收集所有的路径, 然后再把所有路径的值求和. 但因为是scala, 要写的FP一些, 不要有副作用.fh
 *
 * 方法二:
 * 可以不记录路径, 因为在dfs过程中, 直到叶子结点时, 会得到一个完整路径, 在遍历这个路径过程中, 可以进行移位操作
 */
object Sum_of_Root_To_Leaf_Binary_Numbers {

  object First {

    // 访问一个树, 返回其路径
    // Option表示可能有路径, 也可能没有路径
    private def dfs(node:TreeNode): Option[List[String]] = {
      if (node == null) {
        None // 没有路径
      } else {
        // 非空结点
        (dfs(node.left), dfs(node.right)) match {
          case (None, None) => { // 左右子树都没有路径
            Some(List(s"${node.value}"))
          }
          case (Some(path), None) => {  // 左子树有路径, 将其添加到各个路径头
            Some(path.map(node.value + _))
          }
          case (None, Some(path)) => {
            Some(path.map(node.value + _))
          }
          case (Some(leftPath), Some(rightPath)) => {
            Some(leftPath.map(node.value + _) ::: rightPath.map(node.value + _))
          }
        }
      }
    }

    def sumRootToLeaf(root: TreeNode): Int = {
      dfs(root) match {
        case None => 0
        case Some(paths) => paths.map (Integer.parseInt(_, 2)).sum
      }
    }
  }

  // 在dfs过程中, 每深入一步, 其实就是将已知的进行位移, 然后再加, fh
  object Second {

    def sumRootToLeaf(root: TreeNode): Int = {

      var sum = 0
      /**
       * @param node 要进入的结点
       * @param cur  进入结点之前, 路径上的已计算出来的值
       */
      def dfs(node: TreeNode, cur: Int): Unit = {

        // 叶子结点了
        if (node.left == null && node.right == null) {
          sum += (cur << 1) + node.value
        } else {

          if (node.left != null) {
            dfs(node.left, (cur << 1) + node.value)
          }

          if (node.right != null) {
            dfs(node.right, (cur << 1) + node.value)
          }
        }
      }

      if (root == null) 0
      else {
        dfs(root, 0)
        sum
      }
    }
  }

}

package tree

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  // 这里好像比kotlin麻烦, 因为_left, _value, _right不是成员, kotlin中就已经是成员了.
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

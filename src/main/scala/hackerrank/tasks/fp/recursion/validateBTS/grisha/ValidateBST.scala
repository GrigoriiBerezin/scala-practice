package hackerrank.tasks.fp.recursion.validateBTS.grisha

object ValidateBST extends App {

//  Definition for a binary tree root.
  class Treeroot(_value: Int = 0, _left: Treeroot = null, _right: Treeroot = null) {
    var value: Int = _value
    var left: Treeroot = _left
    var right: Treeroot = _right
  }

  def isValidBST(root: Treeroot): Boolean =
    if (root == null) true
    else if (root.left != null && root.left.value >= root.value) false
    else if (root.right != null && root.right.value <= root.value) false
    else if (!isValidBST(root.left) || !isValidBST(root.right)) false
    else true

}

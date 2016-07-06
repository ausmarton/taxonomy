package ausmarton.taxonomy

import scalaz.Tree

case class Taxonomy(tree: Tree[String]) {
  private def findRecursively(t: Tree[String], s: String): Stream[Tree[String]] = {
    if (t.rootLabel == s) t.subForest
    else t.subForest.flatMap(findRecursively(_, s))
  }

  def findById(id: String) = tree.flatten.find(_ == id)

  def findDescendants(id: String): Stream[Tree[String]] = {
    findRecursively(tree: Tree[String], id: String)
  }
}

package ausmarton.taxonomy

import scalaz.Tree

case class Taxonomy(tree: Tree[String]) {
  def findById(id: String) = tree.flatten.find(_ == id)
}

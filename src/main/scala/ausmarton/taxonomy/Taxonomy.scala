package ausmarton.taxonomy

import scalaz.Tree

case class Category(name: String)

case class Taxonomy(categories: Tree[Category]) {
  private def findRecursively(t: Tree[Category], c: Category): Stream[Tree[Category]] = {
    if (t.rootLabel == c) t.subForest
    else t.subForest.flatMap(findRecursively(_, c))
  }

  def findById(name: String) = categories.flatten.find(_.name == name)

  def findDescendants(c: Category): Stream[Tree[Category]] = {
    findRecursively(categories: Tree[Category], c)
  }
}

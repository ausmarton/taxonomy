package ausmarton.taxonomy

import scalaz.Tree

case class Category(name: String, tags: Seq[Tag] = Nil)
case class Tag(name: String, translations: Map[String, String] = Map.empty)

case class Taxonomy(categories: Tree[Category]) {

  def findById(name: String) = categories.flatten.find(_.name == name)

  def findDescendants(category: Category, tree: Tree[Category] = categories): Stream[Tree[Category]] =
    if (tree.rootLabel == category) tree.subForest
    else tree.subForest.flatMap(findDescendants(category,_))

  // returns the tagged category as well as it's descendants
  def findByTag(tag: Tag): Stream[Category] = {
    val taggedCategories = categories.flatten.filter(_.tags.contains(tag))
    taggedCategories
      .flatMap(findDescendants(_)
      .flatMap(_.flatten)) #::: taggedCategories
  }
}

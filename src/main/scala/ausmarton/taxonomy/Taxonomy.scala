package ausmarton.taxonomy

import scalaz.Tree

case class Category(name: String, tags: Seq[Tag] = Nil)
case class Tag(name: String, translations: Map[String, String] = Map.empty)

case class Taxonomy(categories: Tree[Category]) {

  def find(toStream: Tree[Category] => Stream[Tree[Category]], criteria: (Category) => Boolean, tree: Tree[Category] = categories): Stream[Tree[Category]] = {
    if (criteria(tree.rootLabel)) toStream(tree)
    else tree.subForest.flatMap(find(toStream,criteria,_))
  }

  def findById(name: String) = find(Stream(_), _.name == name).headOption

  def findDescendants(category: Category) = find(_.subForest, _ == category)

  // returns the tagged category as well as it's descendants
  def findByTag(tag: Tag) = find(Stream(_), _.tags.contains(tag)).flatMap(_.flatten)
}

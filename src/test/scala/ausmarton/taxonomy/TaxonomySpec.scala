package ausmarton.taxonomy

import org.scalatest.{Matchers, WordSpecLike}
import scalaz.Scalaz._
import scalaz.Tree

class TaxonomySpec extends WordSpecLike with Matchers {
  val music = Category("music").node(
    Category("jazz").leaf,
    Category("pop").leaf,
    Category("rock").leaf
  )
  val musicSubForest = music.subForest
  private val comedyShow = Category("comedy").leaf
  private val frenchRestaurant = Category("french").leaf

  val categoryTaxonomy = Taxonomy(Category("categories").node(
    Category("shows").node(
      Category("theatre").leaf,
      Category("films").node(
        Category("chinese").leaf,
        comedyShow,
        Category("action").leaf
      )
    ),
    music,
    Category("restaurants").node(
      Category("chinese").leaf,
      frenchRestaurant,
      Category("italian").leaf
    )
  ))

  "Taxonomy::findById" should {
    "return None for non-existent ids" in {
      Taxonomy(comedyShow).findById("non-existent") shouldBe None
    }

    "return Node for id with one node if present" in {
      Taxonomy(comedyShow).findById("comedy") shouldBe Some(comedyShow.rootLabel)
    }

    "return Node for id when present" in {
      categoryTaxonomy.findById("french") shouldBe Some(frenchRestaurant.rootLabel)
      categoryTaxonomy.findById("music") shouldBe Some(music.rootLabel)
    }
  }

  "Taxonomy::findDescendants" should {
    "return no descendants for leaf nodes" in {
      categoryTaxonomy.findDescendants(comedyShow.rootLabel) shouldBe Stream[Tree[Category]]()
    }

    "return descendants for nodes" in {
      categoryTaxonomy.findDescendants(music.rootLabel) shouldBe musicSubForest
    }
  }
}

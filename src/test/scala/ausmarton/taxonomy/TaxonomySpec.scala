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

  val restaurantTag: Tag = Tag("restaurant", Map("en_GB" -> "Restaurants", "fr_FR" -> "Restaurants", "es_ES" -> "Restaurantes"))
  val chineseTag: Tag = Tag("chinese", Map("en_GB" -> "Chinese", "fr_FR" -> "Chinois", "it_IT" -> "Cinese"))

  val comedyShow = Category("comedy").leaf

  val frenchRestaurant = Category("french").leaf
  val chineseRestaurant = Category("chinese", List(chineseTag)).leaf
  val italianRestaurant = Category("italian").leaf

  val restaurantsCategory = Category("restaurants", List(restaurantTag)).node(
    chineseRestaurant,
    frenchRestaurant,
    italianRestaurant
  )
  val chineseFilms = Category("chinese", List(chineseTag)).leaf

  val categoryTaxonomy = Taxonomy(Category("categories").node(
    Category("shows").node(
      Category("theatre").leaf,
      Category("films").node(
        chineseFilms,
        comedyShow,
        Category("action").leaf
      )
    ),
    music,
    restaurantsCategory
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

  "Taxonomy::findByTag" should {
    "return tagged categories" in {
      val taggedCategories = categoryTaxonomy.findByTag(restaurantTag)

      taggedCategories should contain(restaurantsCategory.rootLabel)
      taggedCategories should contain(chineseRestaurant.rootLabel)
      taggedCategories should contain(italianRestaurant.rootLabel)
      taggedCategories should contain(frenchRestaurant.rootLabel)
    }

    "return tagged categories under different paths" in {
      val taggedCategories = categoryTaxonomy.findByTag(chineseTag)

      taggedCategories should contain(chineseRestaurant.rootLabel)
      taggedCategories should contain(chineseFilms.rootLabel)
    }
  }
}

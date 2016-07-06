package ausmarton.taxonomy

import org.scalatest.{Matchers, WordSpecLike}
import scalaz.Scalaz._

class TaxonomySpec extends WordSpecLike with Matchers {
  val categoryTaxonomy = Taxonomy("categories".node(
    "shows".node(
      "theatre".leaf,
      "films".node(
        "chinese".leaf,
        "comedy".leaf,
        "action".leaf
      )
    ),
    "music".node(
      "jazz".leaf,
      "pop".leaf,
      "rock".leaf
    ),
    "restaurants".node(
      "chinese".leaf,
      "french".leaf,
      "italian".leaf
    )
  ))

  "Taxonomy::findById" should {
    "return None for non-existent ids" in {
      Taxonomy("comedy".leaf).findById("non-existent") shouldBe None
    }

    "return Node for id with one node if present" in {
      Taxonomy("comedy".leaf).findById("comedy") shouldBe Some("comedy")
    }

    "return Node for id when present" in {
      categoryTaxonomy.findById("french") shouldBe Some("french")
      categoryTaxonomy.findById("music") shouldBe Some("music")
    }
  }
}

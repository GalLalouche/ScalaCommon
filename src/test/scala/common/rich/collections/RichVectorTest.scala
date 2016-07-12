package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichVector._
import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class RichVectorTest extends PropSpec with PropertyChecks with AuxSpecs {
	def randomNumber() = (Random.nextDouble * 1000) - 1000

	def vectors = for (n <- Gen.choose(2, 5)) yield {
		Vector.fill(n)(randomNumber)
	}
	def vectorPairs = for (n <- Gen.choose(2, 5)) yield {
		Vector.fill(n)(randomNumber) -> Vector.fill(n)(randomNumber)
	}
	property("v should have unique entries") {
		forAll(vectors) { v =>
			v.toSet.size shouldReturn v.size
		}
	}

	property("self dot should equal magnitude squared") {
		forAll(vectors) { v =>
			val mag = v.magnitude
			v.dot(v) shouldBeApproximately mag * mag
		}
	}

	property("A vector should have a cosine similarity of 1.0 to itself") {
		forAll(vectors) { v =>
			v.cosineSimilarityTo(v) shouldBeApproximately 1.0
		}
	}

	property("a unit vector's magnitude should be 1") {
		forAll(vectors) { v =>
			v.toUnit.magnitude shouldBeApproximately 1.0
		}
	}

	property("a unit vector's cosine similarity to its original vector should be 1") {
		forAll(vectors) { v =>
			v.toUnit.cosineSimilarityTo(v) shouldBeApproximately 1.0
		}
	}

	property("dot should be symmetric") {
		forAll(vectorPairs) { case (u, v) =>
			v.dot(u) shouldBeApproximately u.dot(v)
		}
	}

	property("Cosine similarity should be symmetric") {
		forAll(vectorPairs) { case (u, v) =>
			v.cosineSimilarityTo(u) shouldBeApproximately u.cosineSimilarityTo(v)
		}
	}

	property("projection should be of a lesser magnitude than the original vector") {
		forAll(vectorPairs) { case (u, v) =>
			v.projectionOn(u).magnitude should be < v.magnitude
		}
	}


	property("the projection of a vector should have a Cosine similarity of 1 with vector") {
		forAll(vectorPairs) {
			case (u, v) =>
				v.projectionOn(u).cosineSimilarityTo(u) shouldBeApproximately 1.0
		}
	}

	property("The anti vector should be perpendicular to the vector") {
		forAll(vectorPairs) {
			case (u, v) =>
				(v.antiVector(u)).cosineSimilarityTo(u) shouldBeApproximately 0.0
		}
	}

	property("Cosine similarity should be non-negative") {
		forAll(vectorPairs) {
			case (u, v) =>
				v.cosineSimilarityTo(u) should be >= 0.0
		}
	}
}

import Population.Person
import zio.test._
import zio.test.Assertion._

object PopulationTest
    extends DefaultRunnableSpec(
      suite("Population Exercise")(
        test("Nobody") {
          val countries = Population
            .getCountries(Iterator.empty)
          assert(countries, isEmpty)
        },
        test("All people living at the same country") {
          val countries = Population
            .getCountries(
              Iterator(Person("A") -> Person("B"),
                       Person("C") -> Person("D"),
                       Person("D") -> Person("A")))
          assert(countries, hasSize(equalTo(1)))
        },
        test("each couple lives at a unique country") {
          val countries = Population
            .getCountries(
              Iterator(Person("A") -> Person("B"),
                       Person("C") -> Person("D"),
                       Person("E") -> Person("F")))
          assert(countries, hasSize(equalTo(3)))
        },
        test("Few people are living at the same country") {
          val countries = Population
            .getCountries(
              Iterator(Person("A") -> Person("B"),
                       Person("C") -> Person("B"),
                       Person("E") -> Person("F")))
          assert(countries, hasSize(equalTo(2)))
        }
      )
    )

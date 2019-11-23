object Population extends App {

  final case class Person(id: String)
  final case class Country(id: Long, population: Set[Person])

  type Result = Vector[Set[Person]]
  object Result {
    def empty: Result = Vector.empty
    def update(distinctCombination: Result,
               group1: Set[Person],
               group2: Set[Person],
               newPeople: Set[Person]): Result = {
      val combine = group1 ++ group2 ++ newPeople
      distinctCombination.filterNot(e => e == group1 || e == group2) :+ combine
    }
  }

  def getCountries(personIt: Iterator[(Person, Person)]): Vector[Country] = {
    def loop(result: Result, personIt: Iterator[(Person, Person)]): Result = {
      if (personIt.hasNext) {
        val (person1, person2) = personIt.next()
        val set1 =
          result.find(_.contains(person1)).fold(Set.empty[Person])(identity)
        val set2 =
          result.find(_.contains(person2)).fold(Set.empty[Person])(identity)
        loop(Result.update(result, set1, set2, Set(person1, person2)), personIt)
      } else result
    }
    val result = loop(Result.empty, personIt)
    result.zipWithIndex.map { case (r, i) => Country(i, r) }
  }

  val country1 = getCountries(
    List(Person("A") -> Person("B"),
         Person("A") -> Person("C"),
         Person("B") -> Person("D"),
         Person("K") -> Person("L"),
         Person("Z") -> Person("W"),
         Person("L") -> Person("A")).iterator)

  val country2 = getCountries(
    List(Person("A") -> Person("B"),
         Person("D") -> Person("E"),
         Person("B") -> Person("K"),
         Person("L") -> Person("A")).iterator)

  val country3 = getCountries(
    List(Person("A") -> Person("B"),
         Person("D") -> Person("E"),
         Person("W") -> Person("K")).iterator)

  println(s"the country 1 are: $country1")
  println(s"the country 2 are: $country2")
  println(s"the country 2 are: $country3")

}

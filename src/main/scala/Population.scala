object Population extends App {

  final case class Person(id: String)
  final case class City(id: Long, population: Set[Person])

  type Result = List[Set[Person]]
  object Result {
    def update(distinctCombination: Result,
               group1: Set[Person],
               group2: Set[Person],
               newPeople: Set[Person]): Result = {
      val combine = group1 ++ group2 ++ newPeople
      distinctCombination.filterNot(e => e == group1 || e == group2) :+ combine
    }
  }

  def getCities(personIt: Iterator[(Person, Person)]): List[City] = {
    var result: Result = List.empty
    while (personIt.hasNext) {
      val (person1, person2) = personIt.next()
      val set1 =
        result.find(_.contains(person1)).fold(Set.empty[Person])(identity)
      val set2 =
        result.find(_.contains(person2)).fold(Set.empty[Person])(identity)
      result = Result.update(result, set1, set2, Set(person1, person2))
    }
    result.zipWithIndex.map { case (r, i) => City(i, r) }
  }

  val cities1 = getCities(
    List(Person("A") -> Person("B"),
      Person("A") -> Person("C"),
      Person("B") -> Person("D"),
      Person("K") -> Person("L"),
      Person("Z") -> Person("W"),
      Person("L") -> Person("A")).iterator)

  val cities2 = getCities(
    List(Person("A") -> Person("B"),
      Person("D") -> Person("E"),
      Person("B") -> Person("K"),
      Person("L") -> Person("A")).iterator)

  val cities3 = getCities(
    List(Person("A") -> Person("B"),
      Person("D") -> Person("E"),
      Person("W") -> Person("K")).iterator)

  println(s"the cities 1 are: $cities1")
  println(s"the cities 2 are: $cities2")
  println(s"the cities 2 are: $cities3")

}

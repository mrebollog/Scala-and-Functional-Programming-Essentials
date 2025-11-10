package lectures.part3fp

import scala.annotation.tailrec

object TuplesAndMaps extends App {
  /*
    1.  What would happen if I had two original entries "Jim" -> 555 and "JIM" -> 900

        !!! careful with mapping keys.

    2.  Overly simplified social network based on maps
        Person = String
        - add a person to the network
        - remove
        - friend (mutual)
        - unfriend

        - number of friends of a person
        - person with most friends
        - how many people have NO friends
        - if there is a social connection between two people (direct or not)
   */

  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    network + (person -> Set())

  def friend(network: Map[String, Set[String]], friendNameA: String, friendNameB: String): Map[String, Set[String]] = {
    val friendsA = network(friendNameA)
    val friendsB = network(friendNameB)

    // we replace the friendNameA and friendNameB keys content
    network + (friendNameA -> (friendsA + friendNameB)) + (friendNameB -> (friendsB + friendNameA))
  }

  def unfriend(network: Map[String, Set[String]], friendNameA: String, friendNameB: String): Map[String, Set[String]] = {
    val friendsA = network(friendNameA)
    val friendsB = network(friendNameB)

    network + (friendNameA -> (friendsA - friendNameB)) + (friendNameB -> (friendsB - friendNameA))
  }

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    // we eliminate person from the friends of the person's friends
    @tailrec
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]): Map[String, Set[String]] =
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(networkAcc, person, friends.head))

    val unfriended = removeAux(network(person), network)
    unfriended - person
  }

  val initialNetwork: Map[String, Set[String]] = Map()
  val people = add(add(add(initialNetwork, "Bob"), "Mary"), "Jim")
  val jimBob = friend(people, "Bob", "Jim")
  val testNet = friend(jimBob, "Bob", "Mary")

  def nFriends(network: Map[String, Set[String]], person: String): Int =
    if (!network.contains(person)) 0
    else network(person).size

  println(nFriends(testNet, "Bob"))

  def mostFriends(network: Map[String, Set[String]]): String =
    network.maxBy(pair => pair._2.size)._1

  println(mostFriends(testNet))

  def nPeopleWithNoFriends(network: Map[String, Set[String]]): Int =
    network.count(_._2.isEmpty)

  println(nPeopleWithNoFriends(testNet))

  def socialConnection(network: Map[String, Set[String]], a: String, b: String): Boolean = {
    @tailrec
    def bfs(target: String, consideredPeople: Set[String], discoveredPeople: Set[String]): Boolean = {
      if (discoveredPeople.isEmpty) false
      else {
        val person = discoveredPeople.head
        if (person == target) true
        else if (consideredPeople.contains(person)) bfs(target, consideredPeople, discoveredPeople.tail)
        else bfs(target, consideredPeople + person, discoveredPeople.tail ++ network(person))
      }
    }

    bfs(b, Set(), network(a) + a)
  }

  println(socialConnection(testNet, "Mary", "Jim"))
}

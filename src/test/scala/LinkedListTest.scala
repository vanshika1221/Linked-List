package com.knoldus

import org.scalatest.funsuite.AnyFunSuite

class LinkedListTest extends AnyFunSuite {

  test("Linked List should have size zero if the list is empty"){
    val emptyList = new LinkedList[Int]()
    assert(emptyList.calculateSize() == 0)
  }
  test("Linked List should have size greater than zero if the list is non-empty") {
    val nonEmptyList = new LinkedList[Int]()
      .insertElementAtEnd(12)
      .insertElementAtEnd(13)
      .insertElementAtEnd(14)
    assert(nonEmptyList.calculateSize() == 3)
  }
  test("An element should be inserted at beginning of the Linked List") {
    val nonEmptyList = new LinkedList[Int]()
      .insertElementAtBeginning(12)
      .insertElementAtBeginning(13)
      .insertElementAtBeginning(14)
    val newList = nonEmptyList.insertElementAtBeginning(29)
    assert(newList.traversal() == Node(29,Node(14,Node(13,Node(12,EmptyNode)))))
  }
  test ("An element should be inserted at end of the Linked List") {
    val nonEmptyList = new LinkedList[Int]()
      .insertElementAtEnd(10)
      .insertElementAtEnd(20)
      .insertElementAtEnd(30)
    val newList = nonEmptyList.insertElementAtEnd(40)
    assert(newList.traversal() == Node(10, Node(20, Node(30, Node(40, EmptyNode)))))
  }
  test("An element should be inserted at any given index of the Linked List") {
    val nonEmptyList = new LinkedList[Int]()
      .insertElementAtEnd(25)
      .insertElementAtEnd(24)
      .insertElementAtEnd(23)
    val newList = nonEmptyList.insertElementAtIndex(22,1)
    assert(newList.traversal() == Node(25, Node(22, Node(24, Node(23, EmptyNode)))))
  }
  test("An element should be deleted from the Linked List") {
    val nonEmptyList = new LinkedList[Int]()
      .insertElementAtEnd(25)
      .insertElementAtEnd(24)
      .insertElementAtEnd(23)
    val newList = nonEmptyList.deleteElement(24)
    assert(newList.traversal() == Node(25, Node(23, EmptyNode)))
  }
  test("An element should not be deleted if it is not present in the Linked List") {
    val nonEmptyList = new LinkedList[Int]()
      .insertElementAtEnd(25)
      .insertElementAtEnd(24)
      .insertElementAtEnd(23)
    val newList = nonEmptyList.deleteElement(45)
    assert(newList.traversal() == Node(25, Node(24, Node(23,EmptyNode))))
  }
  test("To delete element from an empty linked list") {
    val nonEmptyList = new LinkedList[Int]()
    val newList = nonEmptyList.deleteElement(45)
    assert(newList.traversal() == EmptyNode)
  }
  test("An element should be searched in the Linked List") {
    val nonEmptyList = new LinkedList[Int]()
      .insertElementAtEnd(25)
      .insertElementAtEnd(24)
      .insertElementAtEnd(23)
    assert(nonEmptyList.searchElement(23) == Some(23))
  }
  test("An element should not be found in the Linked List if it is not present") {
    val nonEmptyList = new LinkedList[Int]()
      .insertElementAtEnd(25)
      .insertElementAtEnd(24)
      .insertElementAtEnd(23)
    assert(nonEmptyList.searchElement(89) == None)
  }
}

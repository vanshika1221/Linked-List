package com.knoldus

import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

sealed trait MyList[+A]
case class Node[+A](element: A, nextNode: MyList[A]) extends MyList[A]
case object EmptyNode extends MyList[Nothing]

class LinkedList[+A](private val list: MyList[A] = EmptyNode) {
    private val loggerObject = Logger(getClass)
    def insertElementAtBeginning[B >: A](elementToInsert: B): LinkedList[B] = {
        new LinkedList[B](Node(elementToInsert, list))
    }

    def insertElementAtEnd[B >: A](elementToInsert: B): LinkedList[B] = {
        def insertHelper(currentNode: MyList[B]): MyList[B] = {
            currentNode match {
                case EmptyNode => Node(elementToInsert, EmptyNode)
                case Node(element, EmptyNode) => Node(element,Node(elementToInsert,EmptyNode))
                case Node(element, nextNode) => Node(element, insertHelper(nextNode))
            }
        }
        new LinkedList[B](insertHelper(list))
    }

    def insertElementAtIndex[B >: A](elementToInsert: B, index: Int): LinkedList[B] = {
        def insertHelper(currentNode: MyList[B], currentIndex: Int): MyList[B] = {
                currentNode match {
                    case EmptyNode if currentIndex == 0 => Node(elementToInsert, EmptyNode)
                    case Node(element, nextNode) if currentIndex == 0 => Node(elementToInsert, Node(element, nextNode))
                    case Node(element, nextNode) => Node(element, insertHelper(nextNode, currentIndex - 1))
                    case EmptyNode => {
                        loggerObject.info("Index out of range")
                        EmptyNode
                    }
                }
        }
        new LinkedList[B](insertHelper(list, index))
    }
    def deleteElement[B >: A](elementToDelete: B): LinkedList[B] = {
        def deleteHelper(currentNode: MyList[B],currentIndex: Int): MyList[B] = {
            currentNode match {
                case EmptyNode => EmptyNode
                case Node(element, nextNode) => {
                    if (element == elementToDelete) nextNode
                    else Node(element, deleteHelper(nextNode,currentIndex + 1))
                }
            }
        }
        val result = deleteHelper(list,0)
        if(result == list) loggerObject.info(s"$elementToDelete is not found in the Linked List")
        else loggerObject.info(s"$elementToDelete deleted successfully")

        new LinkedList[B](result)
    }
    def traversal(): MyList[A] = {
        @tailrec
        def traverseHelper(currentNode: MyList[A]): MyList[A] = {
            currentNode match {
                case EmptyNode => EmptyNode
                case Node(_, nextNode) => traverseHelper(nextNode)
            }
        }
        traverseHelper(list)
        list
    }
    def searchElement[B >: A](elementToSearch: B): Option[B] = {
        def searchHelper(currentNode: MyList[A], currentIndex: Int): Option[B] = {
            currentNode match {
                case EmptyNode => None
                case Node(element, nextNode) =>
                    if (element == elementToSearch) {
                        loggerObject.info(s"$elementToSearch found at position $currentIndex")
                        Some(elementToSearch)
                    } else searchHelper(nextNode, currentIndex + 1)
            }
        }
        searchHelper(list, 0) match {
            case Some(element) => Some(element)
            case None => {
                loggerObject.info("Data not found")
                None
            }
        }
    }
    def calculateSize(): Int = {
        @tailrec
        def sizeHelper(currentNode: MyList[A], count: Int): Int = {
            currentNode match {
                case EmptyNode => count
                case Node(_, nextNode) => sizeHelper(nextNode, count + 1)
            }
        }
        sizeHelper(list, 0)
    }
}


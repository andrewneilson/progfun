package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times in a string") {
    assert(times(string2Chars("hello, world")) === List(('h',1),('e',1),('l',3),('o',2),(',',1),(' ',1),('w',1),('r',1),('d',1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafList for empty table") {
    assert(makeOrderedLeafList(List()) === List())
  }

  test("makeOrderedLeafList for single element table") {
    assert(makeOrderedLeafList(List(('t', 2))) === List(Leaf('t',2)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("encode a very short text") {
    new TestTrees {
      val encoding = encode(t1)("ab".toList)
      assert(encoding  === List(0,1))
      assert(decode(t1, encoding) === "ab".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val encoding = encode(t1)("ab".toList)
      assert(encoding  === List(0,1))
      assert(decode(t1, encoding) === "ab".toList)
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      val encoding = quickEncode(t1)("ab".toList)
      assert(encoding  === List(0,1))
      assert(decode(t1, encoding) === "ab".toList)
    }
  }
}

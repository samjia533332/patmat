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

  // unit tests of singleton
  test("singleton empty list") {
    assert(singleton(List()) === false)
  }

  test("singleton one element") {
    val tmp1 = new Leaf('a', 1)
    val tmp2 = List(tmp1)
    assert(singleton(tmp2) === true)
  }

  test("singleton two or more element") {
    val tmp1 = new Leaf('a', 1)
    val tmp2 = tmp1 :: tmp1 :: Nil
    assert(singleton(tmp2) === false)
  }
  // unit tests of times
  test("times of empty list") {
    val tmp = List()
    assert(times(tmp) === Nil)
  }

  test("times of one element list") {
    val tmp = 'a' :: Nil
    assert(times(tmp) === List(('a',1)))
  }

  test("times of two identical elements list") {
    val tmp = 'a':: 'a' :: Nil
    assert(times(tmp) === List(('a',2)))
  }

  test("inList test positive case") {
    val list1 = ('a', 1) :: Nil
    assert(inList('a', list1) === true)
  }

  test("inList test negative case") {
    val list1 = ('b', 1) :: Nil
    assert(inList('a', list1) === false)
  }

  test("inList test negative case Nil") {
    val list1 = Nil
    assert(inList('a', list1) === false)
  }

  test("count element in a list") {
    val tmp = 'a' :: 'b' :: 'c' :: 'a' :: 'b' :: Nil
    assert(countChar('a', tmp) === 2)
  }

  test("times of multipule elements test") {
    val tmp = 'a' :: 'b' :: 'c' :: 'a' :: 'b' :: Nil
    assert(times(tmp) === List(('c',1), ('b',2), ('a',2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  // unit test for decoding
  test("decode a very short code 0101 on t1") {
    new TestTrees {
      assert(decode(t1, List(0,1,0,1)) === "abab".toList)
    }
  }

  test("decode a long code on t2") {
    new TestTrees {
      assert(decode(t2, List(1,0,0,0,1,1)) === "dabd".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}

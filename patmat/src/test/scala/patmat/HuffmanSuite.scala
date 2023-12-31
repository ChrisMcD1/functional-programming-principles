package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(
      Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5),
      Leaf('d', 4),
      List('a', 'b', 'd'),
      9
    )
  }

  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }

  test("Basic times") {
    val basicTimes = times(List('a', 'a', 'a'))

    assertEquals(basicTimes, List(('a', 3)))
  }

  test("Times with multiple characters") {
    val basicTimes = times(List('a', 'b', 'a', 'c'))

    assertEquals(basicTimes, List(('a', 2), ('b', 1), ('c', 1)))
  }

  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a', 'b', 'd'))
  }

  test("string2chars hello world") {
    assertEquals(
      string2Chars("hello, world"),
      List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
    )
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(
      makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))),
      List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3))
    )
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(
      combine(leaflist),
      List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))
    )
  }

  test("basic convert to codetable") {
    new TestTrees:
      // val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
      val codeTable1 = convert(t1);
      assertEquals(codeTable1, List(('a', List(0)), ('b', List(1))))

  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

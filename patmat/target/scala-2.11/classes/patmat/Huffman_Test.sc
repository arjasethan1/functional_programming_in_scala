import patmat.Huffman._

object Main extends App {


  val t1 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  val enc1 = encode(t1)(string2Chars("abd"))
  println( enc1 )
  println( quickEncode(t1)(string2Chars("abd")) )

  println( decodedSecret )
}
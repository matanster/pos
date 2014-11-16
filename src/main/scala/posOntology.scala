package com.articlio.pos

object posOntology {
  import scala.io.Source  
  val rawOntologyIterator  = Source.fromFile("data/pos-ontology").getLines.toList.iterator
  object Name {
    val builder = scala.collection.mutable.Map.newBuilder[Char, String]
    while (rawOntologyIterator.hasNext) { val row = rawOntologyIterator.next;  if (!row.startsWith("//")) builder += ((row.takeRight(1).head, row.dropRight(1).trim)) }
    val translationMap = builder.result
    
    def translate(shortNameSeq: String) = {
      shortNameSeq.toSeq.map(s =>  s match {
      case '?' => "Unknown"
      case _ => translationMap(s)
      })
    }
  }
}

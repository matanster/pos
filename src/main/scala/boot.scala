/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.articlio.pos

object Dict {
  import scala.io.Source
  val dictRaw = Source.fromFile("data/moby").getLines.toList
  val dict = dictRaw.map(row => row.split('×')).map(result => (result(0), result(1))).toMap
}

object Boot extends App {
  import com.github.tototoshi.csv._

  def wordList = {
    val reader = CSVReader.open("data/words.csv")
    val iterator = reader.iterator
    val words = scala.collection.mutable.Seq.newBuilder[String]
    while (iterator.hasNext) { val asArray = iterator.next.toArray; val word = asArray(1); words+=word}
    words
  }

  val dict = Dict.dict
  val words = wordList  

  var found = 0
  words.result.foreach(word => if (dict.isDefinedAt(word)) found += 1)
  
  println(s"found part/s of speech for $found words out of ${words.result.length} words (${found.toFloat / words.result.length}).")
  
  val posMap = words.result.map(word => (word, posOntology.Name.translate(dict.getOrElse(word, "?"))))
  
  import java.io.File
  val fileName = "output/out.csv"
  val f = new File(fileName)    
  val writer = CSVWriter.open(f)
  writer.writeAll(posMap.map(i => List(i._1, (i._2).mkString(" or "))))
    
  val indirect = words.result.filter(word => !dict.isDefinedAt(word))
 
  import epic.features.{PorterStemmer}
  val stemmer = PorterStemmer
  //indirect.foreach(word => println(stemmer(word) + " stems for " + word))
  //val posMapStemmed = indirect.map(word => (word, posOntology.Name.translate(dict.getOrElse(word, "?"))))
  
  var found2 = 0
  indirect.foreach(word => if (dict.isDefinedAt(stemmer(word))) found2 += 1)
  println(found2)
  
  val indirectResolved = indirect.map(word => inflectionAnalyzer.pos(word))
  //println(indirectResolved)
  
  println(indirectResolved.count(_ != Set("Unknown")))
  println(indirectResolved.count(_ == Set("Unknown")))
  
  //println(indirectResolved.filter(_ == Set("Unknown")))
  
}

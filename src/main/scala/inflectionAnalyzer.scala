package com.articlio.pos

object inflectionAnalyzer {
  val dict = Dict.dict
  import epic.features.{PorterStemmer}
  val stemmer = PorterStemmer
  
  implicit class MyStringOps(val word: String) {
    def endsWithAny(suffixes: Seq[String]) : Boolean = 
      if (suffixes.isEmpty) false
      else word.endsWith(suffixes.head) || word.endsWithAny(suffixes.tail)
  }
  
  def pos(word: String) : Set[String] = {
    val stemPos = posOntology.Name.translate(dict.getOrElse(stemmer(word), "?"))

    val pos : Seq[String] = word match {
      
      case w if w.endsWith("es") => stemPos.map(_ match {
          case "Noun" => "Plural Noun"
          case p if p.startsWith("Verb") => "Verb"
          case _ => "Unknown"
      })
    
      case w if w.endsWith("s") => stemPos.map(_ match {
          case "Noun" => "Plural Noun"
          case p if p.startsWith("Verb") => "Verb"
          case _ => "Unknown"
      })
    
      case w if w.endsWith("ly") => stemPos.map(_ match {
          case "Adjective" => "Adverb"
          case p if p.startsWith("Verb") => "Verb"
          case _ => "Unknown"
      })
    
      case w if w.endsWith("ed") => stemPos.map(_ match {
          case p if p.startsWith("Verb") => "Verb"
          case p if p.startsWith("Noun") => "Adjective"
          case _ => "Unknown"
      })
    
      case w if w.endsWithAny(Seq("er","est")) => stemPos.map(_ match {
          case p if p.startsWith("Adjective") => "Adjective"
          case _ => "Unknown"
      })
    
      case w if w.endsWithAny(Seq("ous", "ness", "like")) => Seq("Adjective")
    
      case w if w.endsWith("ing") => Seq("Verb") 
        
      case _ => Seq("Unknown")
    }
    
    val collapsed = pos.toSet match {
      case c if c.exists(_ == "Unknown") && c.exists(_ != "Unknown") => c.filter(_ != "Unknown")
      case c => c
    }
    
    if (collapsed == Set("Unknown")) println(word + " stemmed as " + stemmer(word) + " is " + collapsed.mkString(" or "))
    
    collapsed
  }
}

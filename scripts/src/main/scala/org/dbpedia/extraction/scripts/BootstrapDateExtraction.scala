package org.dbpedia.extraction.scripts

import scala.collection.mutable.ListBuffer

/**
 * Helper script to bootstrap date parsing from a DBpedia (3.8+) dump.
 * 
 */
object DateExtractorHelper {
  val monthsen = Map("January"   -> 1,
                     "February"  -> 2,
                     "March"     -> 3,
                     "April"     -> 4,
                     "May"       -> 5,
                     "June"      -> 6,
                     "July"      -> 7,
                     "August"    -> 8,
                     "September" -> 9,
                     "October"   -> 10,
                     "November"  -> 11,
                     "December"  -> 12)

  def farsiCharToInt(c: Char): Int = {
    val f0 = '\u06F0'.toInt
    val f9 = '\u06F9'.toInt
    val ci = c.toInt
    if ((ci >= f0) || (ci <= f9))
      return ci - f0
    else
      throw new NumberFormatException("Not a Farsi number: " + c)
  }
  def farsiToInt(s: String): Int = {
    s.toList.map{farsiCharToInt}.reduceLeft(_ * 10 + _)
  }


  private val prefix = "^<http://dbpedia.org/resource/"
  private val inner = "> <http://www.w3.org/2000/01/rdf-schema#label> \""
  private val months = "(" + monthsen.keySet.mkString("|") + ")"

  private val monthlinestr = prefix + months + inner + "([^\"]*)\".*"
  private val MonthLine = monthlinestr.r

  private val monthdaylinestr = prefix + months + "_([0-9]*)" + inner + "([^\"]*)\".*"
  private val MonthDayLine = monthdaylinestr.r

  def mkmonthinner(in: Map[String, Int]) = {
    in.toList.sortBy{_._2}.map{e => "\"" + e._1 + "\" -> " + e._2}.mkString(", ")
  }

  def main(args: Array[String]) {
    val file = new File(args(0))
    var foundmonths:Map[String,Int] = Map()
    var foundmonthdays:Map[String,(String,String)] = Map()
    var ords:ListBuffer[String] = ListBuffer()
    // add in to regexes
    //var langs:ListBuffer[String] = ListBuffer()

    val lines = io.Source.fromFile(file).getLines.toList
    for(line <- lines) line match {
      case MonthLine(e, f) => foundmonths += (f -> monthsen(e)) //; langs += l
      case MonthDayLine(m, d, f) => val x = (m, d); foundmonthdays += (f -> x) //; langs += l
      case _ =>
    }

    if(foundmonths < 12) {
      println("Warning: fewer than 12 months found!")
    }
    //val langset = langs.toList.toSet
    //if(langset.size != 1) {
    //  println("Warning: found more than one language in dataset")
    //  println("Cannot continue: results will be useless")
    //  exit(-1)
    //}

    println("Add to ...")
    println("     \"" + lang + "\" -> Map(" + mkmonthinner(foundmonths) + "),")
  }
}

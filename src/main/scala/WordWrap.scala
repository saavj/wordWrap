package com.saavj.wordwrap

import scala.annotation.tailrec

object WordWrap {

  implicit class StringImprovements(string: String) {

    def wordWrap(maxLength: Int): String = {

      @tailrec
      def go(words: List[String], list: List[String]): List[String] = words match {
        case word1 :: tail          if word1.length            >= maxLength => go(tail                    , list :+ word1)
        case word1 :: word2 :: tail if s"$word1 $word2".length <  maxLength => go(s"$word1 $word2" :: tail, list)
        case word1 :: word2 :: tail if s"$word1 $word2".length == maxLength => go(tail                    , list :+ s"$word1 $word2")
        case word1 :: word2 :: _    if s"$word1 $word2".length >  maxLength => go(words.tail              , list :+ word1)
        case _ => list
      }

      go(string.split(" ").toList, List.empty).mkString("n")
    }
  }
}
/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017 Michael Haywood
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package cobalt.parser

import cobalt.parser.WsApi._
import fastparse.core.Parsed
import fastparse.noApi._

class Test {

  // this is an example
  def sentenceParser: P[(String, String)] = P("this" ~ "is".! ~ "an".! ~ "example").map(x => (x._1, x._2))

}

object Main {

  def main(args: Array[String]): Unit = {
    val sentence = "this is an example"
    val result = new Test().sentenceParser.parse(sentence)

    result match {
      case Parsed.Success(value, _) => println(value)
      case Parsed.Failure(_,_,_) => println("Failed parsing")
    }
  }

}

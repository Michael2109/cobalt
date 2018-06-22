package cobalt.parser.statement

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class MatchParserTest extends FunSpec with Matchers
{
  /* TODO
      let codeMatch = unlines [ "match obj with"
                            , "    ClassName1 -> i"
                            , "    ClassName2 -> j"
                            , "    (_)        -> k"
                            ]
   */

  /* TODO
      let codeMatchDoBlock = unlines [ "match obj with"
                                   , "    ClassName1 -> do"
                                   , "        i"
                                   , "        j"
                                   , "    ClassName2 -> j"
                                   , "    (_)        -> do"
                                   , "        k"
                                   , "        z"
                                   ]
   */
}

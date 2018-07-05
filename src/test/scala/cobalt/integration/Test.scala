package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil

object Test {

  def main(args: Array[String]): Unit = {
    CompilerUtil.executeJava(Paths.get("test/Test"))
  }

}

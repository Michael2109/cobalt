package cobalt.integration.snake

import java.nio.file.Paths

import cobalt.utils.CompilerUtil

object Snake {

  def main(args: Array[String]): Unit = {
    CompilerUtil.executeJava(Paths.get("snake/Snake"))
  }

}

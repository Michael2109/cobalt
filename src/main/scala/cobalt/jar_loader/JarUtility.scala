package cobalt.jar_loader

import java.io.{File, FileFilter}
import java.nio.file.{Path, Paths}
import java.util.jar.JarFile
import java.util.zip.ZipEntry

import scala.collection.mutable.ListBuffer

object JarUtility {

  val JavaHome = Paths.get(System.getenv().get("JAVA_HOME"))

  def main(args: Array[String]): Unit = {

    println(findJar(JavaHome.toFile, "java/lang/Double.class"))

  }

  def findJar(start: File, className: String): Path ={

    val filter = new FileFilter() {
      def accept(pathname: File): Boolean = {
        return pathname.getName().endsWith(".jar") || pathname.isDirectory()
      }
    }

    var path: Path = null

    for (f <- start.listFiles(filter)) {
      if(path == null) {
        if (f.isDirectory()) {
          val foundPath = findJar(f, className)
          if (foundPath != null) {
            path = foundPath
          }
        } else {
          val foundPath = searchJar(f, className)
          if (foundPath != null) {
            path = foundPath
          }
        }
      }
    }

    return path
  }

  private def searchJar(f: File, className: String): Path ={

    val jar = new JarFile(f)
    var e: ZipEntry = jar.getEntry(className)
    if (e == null) {
      e = jar.getJarEntry(className)
      if(e != null) {
        f.toPath
      }else{
        null
      }
    } else {
      f.toPath
    }
  }

}

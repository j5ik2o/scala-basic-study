package example.filesearch

import java.io.File

object SearchFileName extends App with FileSearchSupport {

  override protected def contains(file: File, keyword: String): Boolean = {
    file.toString.contains(keyword)
  }

  find("activator", new java.io.File(".")).foreach(println)
}

package example.filesearch

import java.io.File
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait FileSearchSupport {

  protected def getPaths(dir: Path): List[Path] = {
    @tailrec
    def getPaths0(itr: java.util.Iterator[Path], lb: ListBuffer[Path]): List[Path] = {
      if (!itr.hasNext) lb.result()
      else {
        lb.append(itr.next())
        getPaths0(itr, lb)
      }
    }
    val lb = ListBuffer.empty[Path]
    val ds = Files.newDirectoryStream(dir)
    val itr = ds.iterator()
    getPaths0(itr, lb)
  }

  protected def find(keyword: String, dir: File): Seq[File] = {
    @tailrec
    def find0(result: List[Path], files: List[Path], keyword: String): List[Path] = {
      files match {
        case Nil => result
        case head :: tail =>
          if (Files.isDirectory(head)) {
            val childFiles = getPaths(head)
            find0(result, tail ++ childFiles, keyword)
          } else {
            if (contains(head.toFile, keyword)) {
              find0(head :: result, tail, keyword)
            } else {
              find0(result, tail, keyword)
            }
          }
      }
    }
    val files = getPaths(dir.toPath)
    find0(List.empty, files, keyword).map(_.toFile)
  }

  protected def contains(file: File, keyword: String): Boolean

  protected def using[A, R <: {def close()}](r: R)(f: R => A): A =
    try {
      f(r)
    } finally {
      r.close()
    }

}

package example.filesearch

import java.io.{File, FileInputStream}
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import java.nio.{ByteBuffer, CharBuffer}
import java.util
import java.util.regex.{Matcher, Pattern}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SearchFileNameWithBody extends App with FileSearchSupport {

  private val DefaultCharset = Charset.forName("ISO-8859-15")
  private val Decoder = DefaultCharset.newDecoder()
  private val LinePattern = Pattern.compile(".*\r?\n")

  private def grep1(file: File, charBuffer: CharBuffer, pattern: Pattern): Boolean = {
    @tailrec
    def grep0(result: Boolean, lineMatcher: Matcher, patternMatcher: Option[Matcher]): Boolean = {
      if (result) result
      else {
        if (!lineMatcher.find()) {
          result
        } else {
          val line = lineMatcher.group()
          val matcher = patternMatcher.map(_.reset(line)).getOrElse(pattern.matcher(line))
          if (matcher.find()) {
            println(s"$file:$line")
            grep0(result = true, lineMatcher, None)
          } else {
            grep0(result = false, lineMatcher, Some(matcher))
          }
        }
      }
    }
    val lineMatcher = LinePattern.matcher(charBuffer)
    grep0(result = false, lineMatcher, None)
  }

  def grep3(file: File, keyword: String, byteBuffer: ByteBuffer): Boolean = {
    @tailrec
    def grep0(result: Boolean, keyword: Array[Byte], byteBuffer: ByteBuffer): Boolean = {
      if (result) result
      else {
        val lb = new ListBuffer[Byte]
        val offset = byteBuffer.limit() - byteBuffer.position()
        val pos = if (offset < keyword.size) offset else keyword.size
        (1 to pos).foreach(_ => lb.append(byteBuffer.get()))
        if (byteBuffer.position() < byteBuffer.limit()) {
          byteBuffer.position(byteBuffer.position() - keyword.size + 1)
        }
        val sub = lb.result().toArray
        //println(new String(sub, "ISO-8859-15"), new String(b, "ISO-8859-15"))
        if (util.Arrays.equals(sub, keyword)) {
          grep0(result = true, keyword, byteBuffer)
        } else {
          grep0(result = byteBuffer.position() != byteBuffer.limit(), keyword, byteBuffer)
        }
      }
    }
    grep0(result = false, keyword.getBytes, byteBuffer)
  }

  def grep2(file: File, keyword: String, byteBuffer: ByteBuffer): Boolean = {
    val b = keyword.getBytes
    while (byteBuffer.position() < byteBuffer.limit()) {
      //println("pos = " + byteBuffer.position())
      val lb = new ListBuffer[Byte]
      val offset = byteBuffer.limit() - byteBuffer.position()
      val pos = if (offset < b.size) offset else b.size
      (1 to pos).foreach(_ => lb.append(byteBuffer.get()))
      if (byteBuffer.position() < byteBuffer.limit()) {
        byteBuffer.position(byteBuffer.position() - b.size + 1)
      }
      val sub = lb.result().toArray
      println(new String(sub, "ISO-8859-15"), new String(b, "ISO-8859-15"))
      if (util.Arrays.equals(sub, b)) {
        return true
      }
    }
    false
  }

  override def contains(file: File, keyword: String): Boolean = {
    file.toString.contains(keyword) || using(new FileInputStream(file)) { fis =>
      val channel = fis.getChannel
      val byteBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size())
      val charBuffer = Decoder.decode(byteBuffer)
      grep1(file, charBuffer, Pattern.compile(keyword))
    }
  }

  find("kato", new File(".")).foreach(println)
}

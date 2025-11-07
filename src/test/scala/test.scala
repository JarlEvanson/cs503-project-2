import java.io.ByteArrayOutputStream
import java.io.File
import java.io.PrintStream
import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source
import slox.Lox

class SLoxSpec extends AnyFlatSpec {
  val testDir = new File("src/test/resources/")

  testDir.listFiles().foreach { folder =>
    behavior of s"${folder.getName}"

    folder.listFiles().filter(_.getName.endsWith(".lox")).foreach { file => {
      val content = Source.fromFile(file).mkString
      val description = {
        val line = content.split("\n").head
        assert(line.startsWith("//"))
        line.stripPrefix("//")
      }

      it should s"${description}" in {
        val buffer = ByteArrayOutputStream()
        val stream = PrintStream(buffer)

        Console.withOut(stream) {
          Console.withErr(stream) {
            Lox.run(content)
            Lox.hadError = false;
            Lox.hadRuntimeError = false;
          }
        }

        val bufferLines = buffer.toString("UTF-8").split("\n").toList
        var outputLines = content.split("\n")
          .tail
          .filter(s => s.indexOf("//") != -1)
          .filter(s => s.substring(s.indexOf("//") + 2).startsWith(" expect: "))
          .map(s => s.substring(s.indexOf("//") + 11)).toList

        assert(bufferLines.size == outputLines.size, s"-> in ${file.getName}")
        for (((output, expected), index) <- bufferLines.zip(outputLines).zipWithIndex) {
          assert(output.contains(expected), s"-> lines failed to match on expectation ${index} in ${file.getName}")
        }
      }
    }}
  }
}

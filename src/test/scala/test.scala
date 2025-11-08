import java.io.ByteArrayOutputStream
import java.io.File
import java.io.PrintStream
import org.scalatest.funspec.AnyFunSpec
import scala.io.Source
import slox.Lox
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

class SLoxSpec extends AnyFunSpec {
  val testDir = File("src/test/resources/")

  describe("Crafting Interpreters") {
    (File(testDir.getPath + "/crafting-interpreters")).listFiles().foreach { folder =>
      describe(s"${folder.getName}") {
        folder.listFiles().filter(_.getName.endsWith(".lox")).foreach { inputFile => {
          val strippedName = {
            val fileName = inputFile.getName
            val index = fileName.indexOf(".")
            fileName.substring(0, index)
          }

          val input = Source.fromFile(inputFile).mkString
          val output = {
            val filePath = inputFile.getPath
            val index = filePath.lastIndexOf(".")
            val outputFilePath = filePath.substring(0, index) + ".output"
            Source.fromFile(outputFilePath).mkString
          }

          it(s"${strippedName}") {
            val buffer = ByteArrayOutputStream()
            val stream = PrintStream(buffer)

            Console.withOut(stream) {
              Console.withErr(stream) {
                Lox.run(input)
                Lox.hadError = false;
                Lox.hadRuntimeError = false;
              }
            }

            val bufferLines = buffer.toString("UTF-8").split("\n")
            val outputLines = output.split("\n")

            assert(bufferLines.size == outputLines.size)
            for ((line, expected) <- bufferLines.zip(outputLines)) {
              assert(line.equals(expected), s"${line} was not ${expected}")
            }
          }
        }}
      }
    }
  }

  describe("General") {
    testDir.listFiles().filter(_.getName.endsWith(".lox")).foreach { inputFile => {
        val strippedName = {
          val fileName = inputFile.getName
          val index = fileName.indexOf(".")
          fileName.substring(0, index)
        }

        val input = Source.fromFile(inputFile).mkString
        val output = {
          val filePath = inputFile.getPath
          val index = filePath.lastIndexOf(".")
          val outputFilePath = filePath.substring(0, index) + ".output"
          Source.fromFile(outputFilePath).mkString
        }

        it(s"${strippedName}") {
          val buffer = ByteArrayOutputStream()
          val stream = PrintStream(buffer)

          Console.withOut(stream) {
            Console.withErr(stream) {
              Lox.run(input)
              Lox.hadError = false;
              Lox.hadRuntimeError = false;
            }
          }

          val bufferLines = buffer.toString("UTF-8").split("\n")
          val outputLines = output.split("\n")

          assert(bufferLines.size == outputLines.size)
          for ((line, expected) <- bufferLines.zip(outputLines)) {
            assert(line.equals(expected), s"${line} was not ${expected}")
          }
        }
    }}
  }
}

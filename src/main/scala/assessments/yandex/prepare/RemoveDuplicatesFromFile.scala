package assessments.yandex.prepare

import java.io.{BufferedReader, BufferedWriter, File, FileWriter}
import scala.io.Source

object RemoveDuplicatesFromFile extends App {
  val input = "input.txt"
  val output = "output.txt"

  val reader: BufferedReader = Source.fromFile(input).bufferedReader()
  val writer: BufferedWriter = new BufferedWriter(new FileWriter(new File(output)))

  def calculate(reader: BufferedReader, writer: BufferedWriter): Unit = {
    val n = reader.readLine().toInt
    if (n < 1) ()
    else {
      var l = reader.readLine()
      var i = 1

      while (i < n) {
        val line = reader.readLine()
        if (!line.equals(l)) {
          writer.write(line)
          l = line
        }
        i += 1
      }
    }
  }

  reader.close()
  writer.close()

}

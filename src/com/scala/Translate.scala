/**
 * User: soren
 */


package com.scala

import io.Source
import java.io.{FileWriter, File}
import java.net.{URL, URLEncoder}

class Translate {

  def translatePropertyFiles(propertyFiles: List[File]) {

    propertyFiles.foreach(dir => {

      dir.listFiles().filterNot(f => !f.getName.endsWith("_en.properties")).foreach(f => f.delete())

      val files = dir.listFiles().filterNot(f => !f.getName.endsWith(".properties"))
      files.foreach(file => {

        println("Processing file " + file.getCanonicalPath)
        val lines = Source.fromFile(file, "latin1").getLines()
        val translatedLines = lines.toList.par.map(line => {
          if (line.startsWith("#") || line.trim.isEmpty || line.indexOf("=") < 0)
            line
          else {
            val key = line.substring(0, line.indexOf("="))
            val value = line.substring(line.indexOf("=") + 1, line.length)
            val translatedValue = if (value.isEmpty) "" else new Translator().translate("da", "en", value).replaceAll("\\{ ", "\\{")
            key + "=" + translatedValue
          }
        })
        val parentPath = file.getParentFile
        val writer = new FileWriter(new File(parentPath, file.getName.replace(".", "_en.")))
        writer.write(translatedLines.mkString("\n"))
        writer.close()
      })
    })
  }

  def translateJavaClass(javaFiles: Array[File]) {
    val writer = new FileWriter(new File("generated_en.properties"))
    val result = javaFiles.par.map(
      file => {
        println("Processing file " + file.getCanonicalPath)
        val lines = Source.fromFile(file, "utf8").getLines()
        val translatedLines = lines.toList.map(line => {
          if (line.contains(",")) {

            val newLine = line.split(",")
            val value = newLine(1).replaceAll("\"", "").replace(");", "")
            val translatedValue = if (value.isEmpty) "" else new Translator().translate("da", "en", value).replaceAll("\\{ ", "\\{")
            val key = newLine(0).substring(newLine(0).indexOf("\""), newLine(0).length).replaceAll("\"", "")
            val capValue = capitalize(translatedValue)
            key + "=" + capValue
          } else
            "" //line
        })
        translatedLines
      }).flatMap(f => f)
    writer.write(result.mkString("\n"))
    writer.close()
  }

  def capitalize(s: String) = {
    s(0).toUpper + s.substring(1, s.length)
  }
}


object Translate {
  def main(args: Array[String]) {
    val start = System.currentTimeMillis()
    val javaAndProperties = args.map(s => new File(s)).partition(p => p.getName.endsWith(".properties"))
    new Translate().translateJavaClass(javaAndProperties._1)
    val stop = System.currentTimeMillis()
    println("took " + (stop - start) + " ms.")
  }
}


class Translator {

  val googleTranslate = "http://translate.google.com/#"

  def translate(from: String, to: String, item: String) = {
    val encodedValue = URLEncoder.encode(item)
    val url = "http://translate.google.dk/translate_a/t?client=t&text=" + encodedValue + "&hl=da&sl=da&tl=en&ie=UTF-8&oe=UTF-8&multires=1&prev=btn&ssel=0&tsel=0&sc=1"

    val translationURL = new URL(url)

    val conn = translationURL.openConnection()
    // fake request coming from browser
    conn.setRequestProperty("User-Agent", "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-GB;     rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13 (.NET CLR 3.5.30729)")

    val fullPage = Source.fromInputStream(conn.getInputStream, "utf-8").mkString
    fullPage.substring(0, fullPage.indexOf(",")).replaceAll("\\[", "").tail.dropRight(1)
  }

}

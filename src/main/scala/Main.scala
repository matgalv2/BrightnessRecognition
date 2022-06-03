import java.io.File

object Main {

  def main(args: Array[String]): Unit = {

    val inputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\BrightnessRecognition\\resources\\photos"
    val outputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\output"


    val paths = Image.getImagesPaths(inputDirectory)


    val images = paths.map(path => new Image(path)).map(image => image.brightness = Image.getImageBrightness(image.path))
//      .map(image => image.brightness)

    println(images.head)

    images.foreach(image => image.classify(outputDirectory, 20))

    println(Image.loadImage())

  }

}

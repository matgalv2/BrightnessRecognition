import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object Main {

  def main(args: Array[String]): Unit = {

    val inputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\BrightnessRecognition\\resources\\photos"
//    val inputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\test_images"
    val outputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\output"
    val cutOffPoint = 20



    val paths = Image.getImagesPaths(inputDirectory)
    var start = System.nanoTime()
    val images = paths.par.map(path => new Image(path)).par.map(image => image.brightness = Image.getImageBrightness(image.path))


    images.par.foreach(image => image.classify(outputDirectory, cutOffPoint))

    println(System.nanoTime() - start, " <- równolegle")

    start = System.nanoTime()
    val images2 = paths.map(path => new Image(path)).map(image => image.brightness = Image.getImageBrightness(image.path))
    images2.par.foreach(image => image.classify(outputDirectory, cutOffPoint))
    println(System.nanoTime() - start, " <- sekwencyjnie")

    /* TODO:
        1. Resistance to errors like file not found
        2. Added weighted brightness based on distance to center of image

     */

  }

}

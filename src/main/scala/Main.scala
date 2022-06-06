import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object Main {

  def main(args: Array[String]): Unit = {

    val inputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\BrightnessRecognition\\resources\\photos"
//    val inputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\test_images"
    val outputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\output"
    val cutOffPoint = 80

    val paths = ImageAnalyser.getImagesPaths(inputDirectory)
    val images = paths.par.map(path => ImageAnalyser(path)).par.map(image => image.brightness = ImageAnalyser.getImageBrightness(image.path))
    images.par.foreach(image => image.classify(outputDirectory, cutOffPoint))



    /* TODO:
        1. Weighted brightness based on distance to center of image
    */




  }

}

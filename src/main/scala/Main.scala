import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object Main {

  def main(args: Array[String]): Unit = {

    val inputDirectory = "src\\main\\resources\\photos"
//    val outputDirectory = "src\\main\\resources\\output"
    val outputDirectory = "X:\\Dokumenty\\Praca\\Scalac\\output"
    val cutOffPoint = 80

    val paths = ImageAnalyser.getImagesPaths(inputDirectory)
    val images = paths.par.map(path => ImageAnalyser(path)).par
      .map(image => image.brightness = ImageAnalyser.getImageBrightness(image.path))
//      .map(image => image.brightness = ImageAnalyser.getImageBrightness(image.path,0.3f,(1,0)))
    images.par.foreach(image => image.classify(outputDirectory, cutOffPoint))

  }

}

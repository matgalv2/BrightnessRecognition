import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object Main {

  def main(args: Array[String]): Unit = {

    val inputDirectory = "resources\\photos"
    val outputDirectory = "resources\\output"
    val cutOffPoint = 80

    val paths = ImageAnalyser.getImagesPaths(inputDirectory)
    val images = paths.par.map(path => ImageAnalyser(path)).par
      .map(image => image.brightness = ImageAnalyser.getImageBrightness(image.path))
    images.par.foreach(image => image.classify(outputDirectory, cutOffPoint))

//     2nd option
//       .map(image => image.brightness = ImageAnalyser.getImageWeightedBrightness(image.path,0.3f,(1,0)))
//    images.par.foreach(image => image.classify(outputDirectory, cutOffPoint, avoidCollisionName=true))

  }
}

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import javax.imageio.ImageIO
import javax.management.InvalidAttributeValueException


/**
 * An abstract representation of features of image file.
 *
 *
 *
 * @author Mateusz Galicki
 * */

class ImageAnalyser(private val _path: String){

  /**
   * Represents the brightness of an image.
   */
  private var _brightness: Int = ImageAnalyser.NotCheckedYet

/**
 * A pathname to image file.
 * */
  def path: String = _path

  /**
   * Brightness getter.
   */
  def brightness: Int = _brightness
  /**
   * Brightness setter. The return type of method is <emp>this.type</emp>, because class may be extended in the future.
   * @param value  New value of brightness.
   * @return Pointer to object, which called this method.
   */
  def brightness_=(value: Int): this.type = {
    _brightness = value
    this
  }

  /**
   * Copy file at object's pathname to outputLocation with added
   * brightness class and darkness value to filename. If object
   * path's or outputPath points at invalid place method has no
   * effect, just like in case in which object has brightness below 0.
   *
   *
   * @param outputPath - path to folder, where result will be copied.
   * @param cutOffPoint - value from which brightness are considered as dark.
   */
  def classify(outputPath: String, cutOffPoint: Int): Unit = {
    if(ImageAnalyser.folderExists(outputPath) && (brightness != ImageAnalyser.NotCheckedYet || brightness == ImageAnalyser.UndefinedBrightness)){
      if(ImageAnalyser.fileExists(this.path)){
        val darknessLevel = 100 - brightness
        val brightnessClass = if(darknessLevel < cutOffPoint) ImageAnalyser.BrightLabel else ImageAnalyser.DarkLabel
        val (filename, extension) = ImageAnalyser.getFileNameWithExtension(path)

        try {
          Files.copy(Paths.get(path),
            Paths.get(s"$outputPath\\${filename}_${brightnessClass}_$darknessLevel$extension"),
            StandardCopyOption.REPLACE_EXISTING)
        }
        catch {
          case e: Exception => e.printStackTrace()
        }
      }
    }
  }

  override def toString: String = {
    super.toString + ", brightness :" + brightness
  }
}

object ImageAnalyser{

  private final val NotCheckedYet = -1
  private final val UndefinedBrightness = -2
  private final val BrightLabel = "bright"
  private final val DarkLabel = "dark"


  def apply(path: String): ImageAnalyser = new ImageAnalyser(path)


  def getImageBrightness(path: String): Int = {

    val file = new File(path)

    if(!file.canRead)
      return UndefinedBrightness

    val image = ImageIO.read(file)

    if(image == null)
      return UndefinedBrightness

    var red = 0
    var green = 0
    var blue = 0

    for(i <- 0 until image.getWidth; j <- 0 until image.getHeight){
      val colour = image.getRGB(i, j) // 32 bits - alpha red green blue
      // &0xFF - values are over 255, because we don't extract them as single values so we must use binary AND

      red += (colour >>> 16) & 0xFF // out: green blue
      green += (colour >>> 8) & 0xFF // out: blue
      blue += (colour >>> 0) & 0xFF // blue is last

    }

    red /= image.getWidth * image.getHeight
    green /= image.getWidth * image.getHeight
    blue /= image.getWidth * image.getHeight

    // calc luminance in range 0 to 100; using SRGB luminance constants
    val luminance = (((red * 0.2126f + green * 0.7152f + blue * 0.0722f) / 255)*100).round

    luminance
  }

  def getImageBrightness(path: String, radius: Float = 1, weights: (Int,Int) = (1,0)): Int = {
    def checkArguments(): Unit = {
      if (weights._1 < 0 || weights._2 < 0)
        throw new InvalidAttributeValueException("Weights must be non negative.")
      else if( weights._1 + weights._2 == 0)
        throw new InvalidAttributeValueException("At least one weight most be positive.")
      else if(radius < 0 || radius > 1)
        throw new InvalidAttributeValueException("Radius must be from range [0,1].")
      else if(radius == 1 && weights._2 != 0)
        throw new InvalidAttributeValueException("If radius == 1, then weights._2 should equals 0.")
      else if(radius == 0 && weights._1 != 0)
        throw new InvalidAttributeValueException("If radius == 0, then weights._1 should equals 0.")
    }

    val file = new File(path)

    if(!file.canRead)
      return UndefinedBrightness

    val image = ImageIO.read(file)

    if(image == null)
      return UndefinedBrightness

    checkArguments()



    // RGB for central part
    var red_center = 0
    var green_center = 0
    var blue_center = 0

    // RGB for edges
    var red_edges = 0
    var green_edges = 0
    var blue_edges = 0

    val heightLimits = (image.getHeight/2 - image.getHeight*radius/2,image.getHeight/2 + image.getHeight*radius/2)
    val widthLimits = (image.getWidth/2 - image.getWidth*radius/2,image.getWidth/2 + image.getWidth*radius/2)

    var n, m = 0
    for(i <- 0 until image.getWidth; j <- 0 until image.getHeight){
      val colour = image.getRGB(i, j)

      if(i >= widthLimits._1 && i <= widthLimits._2 && j >= heightLimits._1 && j <= heightLimits._2) {
        red_center += (colour >>> 16) & 0xFF
        green_center += (colour >>> 8) & 0xFF
        blue_center += (colour >>> 0) & 0xFF
        n += 1
      }
      else{
        red_edges += (colour >>> 16) & 0xFF
        green_edges += (colour >>> 8) & 0xFF
        blue_edges += (colour >>> 0) & 0xFF
      }
    }

    n = if(n != 0) n else 1
    m = if(m != 0) image.getHeight*image.getWidth - n else 1

    red_center /= n
    green_center /= n
    blue_center /= n


    red_edges /= m
    green_edges /= m
    blue_edges /= m



    val luminance_center = (((red_center * 0.2126f + green_center * 0.7152f + blue_center * 0.0722f) / 255)*100).round
    val luminance_edges = (((red_edges * 0.2126f + green_edges * 0.7152f + blue_edges * 0.0722f) / 255)*100).round

    (luminance_center * weights._1 + luminance_edges * weights._2)/(weights._1 + weights._2)
  }


  private def getPaths(path: String, extensions: String): List[String] = {
    def getPathsRec(list: List[String], path: String): List[String] =
      list match {
        case Nil => Nil
        case head::tail =>  if(new File(path + "\\" + head).isDirectory) getPathsRec(tail, path) ::: getPathsRec(new File(path + "\\" + head).list().toList, path + "\\" + head)
        else if(head.matches(".*\\.(" + extensions + ")$")) (path + "\\" + head) :: getPathsRec(tail, path)
        else getPathsRec(tail, path)
      }
    getPathsRec(new File(path).list().toList, path)
  }

  def getImagesPaths(path: String, extensions: String = "(jpg)|(png)|(jpeg)"): List[String] = {
    val file = new File(path)
    if(!file.exists() || !file.isDirectory)
      List()
    else
      getPaths(path, extensions)
  }

  private def folderExists(path: String): Boolean = {
    val destination = new File(path)
    destination.exists && destination.isDirectory
  }

  private def fileExists(path: String): Boolean = {
    val destination = new File(path)
    destination.exists && destination.isFile
  }

  private def getFileNameWithExtension(path: String): (String, String) = {
    // Only for files! That's why it's private. If the path points to folder error will be thrown.
    val pattern = "(^.*\\\\.*\\\\)(.*)?(\\.[A-z]+$)".r
    val pattern(_,filename, extension) = path
    (filename, extension)
  }

}
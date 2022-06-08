import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
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
   * Copies file at object's pathname to outputLocation with added
   * brightness class and darkness value to filename. If object
   * path's or outputPath points at invalid place method has no
   * effect, just like in case in which object has brightness below 0.
   *
   *
   * @param outputPath path to folder, where result will be copied.
   * @param cutOffPoint value from which brightness are considered as dark.
   * @param avoidCollisionName indicates if filenames collision should be solved.
   */
  def classify(outputPath: String, cutOffPoint: Int, avoidCollisionName: Boolean = false): Unit = {
    if(ImageAnalyser.folderExists(outputPath) && (brightness != ImageAnalyser.NotCheckedYet || brightness == ImageAnalyser.UndefinedBrightness)){
      if(ImageAnalyser.fileExists(this.path)){
        val darknessLevel = 100 - brightness
        val brightnessClass = if(darknessLevel < cutOffPoint) ImageAnalyser.BrightLabel else ImageAnalyser.DarkLabel
        var (filename, extension) = ImageAnalyser.getFileNameWithExtension(path)

        if(avoidCollisionName) {
          val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss.SSS")
          LocalDateTime.now().format(formatter)
          filename = LocalDateTime.now().format(formatter) + "__" + filename
        }

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

  /**
   * Values of brightness, which inform that object can not be classified.
   */
  private final val NotCheckedYet = -1
  private final val UndefinedBrightness = -2

  /**
   * Possible labels for image after classification.
   */
  private final val BrightLabel = "bright"
  private final val DarkLabel = "dark"


  def apply(path: String): ImageAnalyser = new ImageAnalyser(path)


  /**
   * Checks if file pointed by the path can be read.
   * @param path path to file.
   * @return true if file can be read.
   */
  private def isReadable(path: String): BufferedImage = {
    val file = new File(path)
    if(!file.canRead)
      null
    else
      ImageIO.read(file)
  }

  /**
   * Calculate image brightness in scale 0 to 100, where 100 is perfectly white image.
   * @param path path to image.
   * @return image brightness in scale 0 to 100.
   */
  def getImageBrightness(path: String): Int = {

    val image = isReadable(path)

    if (image == null)
      return UndefinedBrightness

    var rgb = RGB()

    for (i <- 0 until image.getWidth; j <- 0 until image.getHeight)
      rgb.addRGBValue(image.getRGB(i, j))

    rgb /= image.getWidth * image.getHeight

    RGB.luminance(rgb)
  }

  /**
   * Calculate weighted image brightness in scale 0 to 100, where zones (central, edges) have different weight.
   * @param path path to image.
   * @param radius determines size of central zone, 1 - whole image is central zone; 0 - no central zone; 0,5 - central zone equal [0.25 , 0.75] * size of image.
   * @param weights weight of every zone - (central, edges) - central zone should have larger weight, because usually faces are right there.
   * @return image brightness from 0 to 100.
   * @throws InvalidAttributeValueException in 5 cases:
   * <ol>
   * <li> Both values of weights are negative.
   * <li> Both values of weights are equal 0.
   * <li> Radius is out of range [0,1].
   * <li> Radius equals 1 and weights._2 does not equal 0.
   * <li> Radius equals 0 and weights._1 does not equal 0.
   * </ol>
   */
  def getImageWeightedBrightness(path: String, radius: Float = 1, weights: (Int,Int) = (1,0)): Int = {
    def checkArguments(): Unit = {
      if (weights._1 < 0 || weights._2 < 0)
        throw new InvalidAttributeValueException("Weights must be non negative.")
      else if( weights._1 + weights._2 == 0)
        throw new InvalidAttributeValueException("At least one weight most be positive.")
      else if(radius < 0 || radius > 1)
        throw new InvalidAttributeValueException("Radius must be from range [0,1].")
      else if(radius == 1 && weights._2 != 0)
        throw new InvalidAttributeValueException("If radius == 1, then weights._2 should equal 0.")
      else if(radius == 0 && weights._1 != 0)
        throw new InvalidAttributeValueException("If radius == 0, then weights._1 should equal 0.")
    }

    val image = isReadable(path)

    if(image == null)
      return UndefinedBrightness

    checkArguments()

    var rgb_central, rgb_edges = RGB()

    val heightLimits = (image.getHeight/2 - image.getHeight*radius/2,image.getHeight/2 + image.getHeight*radius/2)
    val widthLimits = (image.getWidth/2 - image.getWidth*radius/2,image.getWidth/2 + image.getWidth*radius/2)

    var pixels_center = 0

    for(i <- 0 until image.getWidth; j <- 0 until image.getHeight){
      val colour = image.getRGB(i, j)

      if(i >= widthLimits._1 && i <= widthLimits._2 && j >= heightLimits._1 && j <= heightLimits._2) {
        rgb_central.addRGBValue(colour)
        pixels_center += 1
      }
      else
        rgb_edges.addRGBValue(colour)
    }

    var pixels_edges = image.getHeight*image.getWidth - pixels_center

    pixels_center = if(pixels_center != 0) pixels_center else 1
    pixels_edges = if(pixels_edges != 0) pixels_edges else 1

    rgb_central /= pixels_center
    rgb_edges /= pixels_edges

    val luminance_center = RGB.luminance(rgb_central)
    val luminance_edges = RGB.luminance(rgb_edges)

    (luminance_center * weights._1 + luminance_edges * weights._2)/(weights._1 + weights._2)
  }


  /**
   * Get all files from folder and its every subfolder.
   * @param path path to folder.
   * @param extensions group of extensions as regular expression, which decides, which files are added to the list.
   */

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

  /**
   * Get all files from folder and its every subfolder. If path is incorrect returns empty List.
   * @param path path to folder.
   * @param extensions group of extensions as regular expression, which decide which file will be added to the list.
   * @return list of all paths to files, which extension matched any of <emp>extensions</emp>
   */

  def getImagesPaths(path: String, extensions: String = "(jpg)|(png)|(jpeg)"): List[String] = {
    val file = new File(path)
    if(!file.exists() || !file.isDirectory)
      List()
    else
      getPaths(path, extensions)
  }

  /**
   * Checks if folder exists.
   * @param path pathname to folder.
   * @return True if pathname points to valid folder.
   */
  private def folderExists(path: String): Boolean = {
    val destination = new File(path)
    destination.exists && destination.isDirectory
  }


  /**
   * Checks if file exists.
   * @param path path to file.
   * @return True if pathname points to valid file.
   */
  private def fileExists(path: String): Boolean = {
    val destination = new File(path)
    destination.exists && destination.isFile
  }

  /**
   * Get filename and its extension without the rest of path.
   * @param path path to file.
   * @return Tuple consists of filename and its extension.
   */
  private def getFileNameWithExtension(path: String): (String, String) = {
    val pattern = "(^.*\\\\.*\\\\)(.*)?(\\.[A-z]+$)".r
    val pattern(_,filename, extension) = path
    (filename, extension)
  }

}
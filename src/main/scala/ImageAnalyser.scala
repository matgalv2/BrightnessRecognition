import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import javax.imageio.ImageIO


class ImageAnalyser(private var _path: String, private var _brightness: Int = ImageAnalyser.NotCheckedYet){

  def path: String = _path

  def brightness: Int = _brightness
  def brightness_=(value: Int): this.type = {
    _brightness = value
    this
  }

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
    brightness + " " + path
  }
}

object ImageAnalyser{

  private val NotCheckedYet = -1
  private val UndefinedBrightness = -2
  private val BrightLabel = "bright"
  private val DarkLabel = "dark"

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

  def getImagesPaths(path: String, extensions: String = "(jpg)|(png)|(jpeg)"): List[String] = {
    // must check firstly if path exists!
    def getPathsRec(list: List[String], path: String): List[String] =
      list match {
        case Nil => Nil
        case head::tail =>  if(new File(path + "\\" + head).isDirectory) getPathsRec(tail, path) ::: getPathsRec(new File(path + "\\" + head).list().toList, path + "\\" + head)
        else if(head.matches(".*\\.(" + extensions + ")$")) (path + "\\" + head) :: getPathsRec(tail, path)
        else getPathsRec(tail, path)
      }
    getPathsRec(new File(path).list().toList, path)
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
    /* Only for files! That's why it's private. If the path points to folder error will be thrown. */
//    val pattern = "(^.*\\\\.*\\\\([a-z]|[A-Z]|[0-9])+)(\\.[A-z]+$)".r
//    val pattern = "(.*[A-z]\\\\[A-z]+)(\\.[A-z]+$)".r
//    val pattern = "(^.*\\\\.*\\\\)([a-z]|[A-Z]|[0-9])+(\\.[A-z]+$)".r
    val pattern = "(^.*\\\\.*\\\\)(.*)?(\\.[A-z]+$)".r
    val pattern(_,filename, extension) = path
    (filename, extension)
  }

}
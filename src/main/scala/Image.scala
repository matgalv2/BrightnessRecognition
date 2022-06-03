import com.ibm.jvm.dtfjview.tools.utils.FileUtils

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import javax.imageio.ImageIO


class Image (private var _path: String, private var _brightness: Int = Image.NotCheckedYet){



  def path: String = _path

  def brightness: Int = _brightness
  def brightness_=(value: Int): this.type = {
    _brightness = value
    this
  }

  def classify(outputPath: String, cutOffPoint: Int): Unit = {
    if(!Image.folderExists(outputPath))
      return

    if(_brightness == Image.NotCheckedYet) {
    }
    else {

      //      if(Image.folderExists(this.path)){
      if(true){
        try {
          val brightnessClass = if(brightness > cutOffPoint) Image.BrightLabel else Image.DarkLabel
          val (filename, extension) = Image.getFileNameWithExtension(path)
          Files.copy(Paths.get(path),Paths.get(s"${outputPath}\\${filename}_${brightnessClass}_${brightness.toString + extension}"), StandardCopyOption.REPLACE_EXISTING)
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

object Image{

  private val NotCheckedYet = -1
  private val BrightLabel = "bright"
  private val DarkLabel = "dark"


  def loadImage(): Int = {
    //    val image = ImageIO.read(new File("X:\\Dokumenty\\Praca\\Scalac\\BrightnessRecognition\\photos\\bright\\a.jpg"))
    val image = ImageIO.read(new File("X:\\Dokumenty\\Praca\\Scalac\\BrightnessRecognition\\resources\\photos\\bright\\c.jpg"))

    var red = 0
    var green = 0
    var blue = 0

    for(i <- 0 until image.getWidth; j <- 0 until image.getHeight){
      val colour = image.getRGB(i, j)
      red += (colour >>> 16) & 0xFF
      green += (colour >>> 8) & 0xFF
      blue += (colour >>> 0) & 0xFF

    }
    red /= image.getWidth * image.getHeight
    green /= image.getWidth * image.getHeight
    blue /= image.getWidth * image.getHeight


//    println((red * 0.2126f + green * 0.7152f + blue * 0.0722f) / 255)

    image.getRGB(0, 0)

    //scale image to 1x1 pixel or iterate through every single pixel and add its rgb to (r,g,b) and divide by pixel's number
    /*
        //
        val x, y = 0 //?

        val color = image.getRGB(x, y)

        val luminance = pixelBrightness(color)

        // choose brightness threshold as appropriate:
        if (luminance >= 0.5f) {
          // bright color
        }
        else {
          // dark color
        }

     */
  }

  def pixelBrightness(color: Int): Float = {

    // extract each color component
    val red = (color >>> 16) & 0xFF
    val green = (color >>> 8) & 0xFF
    val blue = (color >>> 0) & 0xFF

    // calc luminance in range 0.0 to 1.0; using SRGB luminance constants
    val luminance = (red * 0.2126f + green * 0.7152f + blue * 0.0722f) / 255

    luminance
  }

  def getImageBrightness(path: String): Int = {


    val image = ImageIO.read(new File(path))

    var red = 0
    var green = 0
    var blue = 0

    for(i <- 0 until image.getWidth; j <- 0 until image.getHeight){
      val colour = image.getRGB(i, j)

      red += (colour >>> 16) & 0xFF
      green += (colour >>> 8) & 0xFF
      blue += (colour >>> 0) & 0xFF

    }

    red /= image.getWidth * image.getHeight
    green /= image.getWidth * image.getHeight
    blue /= image.getWidth * image.getHeight


    (((red * 0.2126f + green * 0.7152f + blue * 0.0722f) / 255)*100).toInt

    val luminance = (((red * 0.2126f + green * 0.7152f + blue * 0.0722f) / 255)*100).round

    luminance
  }

  def getImagesPaths(path: String): List[String] = {
    // must check firstly if path exists!
    def getPathsRec(list: List[String], path: String): List[String] =
      list match {
        case Nil => Nil
        case head::tail =>  if(new File(path + "\\" + head).isDirectory) getPathsRec(tail, path) ::: getPathsRec(new File(path + "\\" + head).list().toList, path + "\\" + head)
        else if(head.matches(".*\\.((jpg)||(png))$")) (path + "\\" + head) :: getPathsRec(tail, path)
        else getPathsRec(tail, path)
      }
    getPathsRec(new File(path).list().toList, path)
  }

  //  def decide()

  def folderExists(path: String): Boolean = {
    val destination = new File(path)

    destination.exists && destination.isDirectory
  }

  private def getFileNameWithExtension(path: String): (String, String) = {
    /* Only for files. If the path points to folder error will be thrown. */
//    val pattern = "(^.*\\\\.*\\\\([a-z]|[A-Z]|[0-9])+)(\\.[A-z]+$)".r
//    val pattern = "(.*[A-z]\\\\[A-z]+)(\\.[A-z]+$)".r
    val pattern = "(^.*\\\\.*\\\\)([a-z]|[A-Z]|[0-9])+(\\.[A-z]+$)".r
    val pattern(_,filename, extension) = path
    (filename, extension)
  }


}
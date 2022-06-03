import java.io.File

object Main {

  def main(args: Array[String]): Unit = {

    val inputDirectory = ""
    val outputDirectory = ""

    //get all paths

    //map them as Image


    val paths = Image.getImagesPaths("X:\\Dokumenty\\Praca\\Scalac\\BrightnessRecognition\\resources\\photos")


    val brightnesses = paths.map(path => new Image(path)).map(image => image.brightness = Image.getImageBrightness(image.path)).map(image => image.brightness)

    //

    println(brightnesses)

    println(Image.folderExists("X:\\Dokumenty\\Praca\\Scalac\\BrightnessRecognition\\src\\main\\scala\\Image.scala"))

    //    brightnesses.foreach(image => print(image.brightness))



    val x,y = 0


  }

}

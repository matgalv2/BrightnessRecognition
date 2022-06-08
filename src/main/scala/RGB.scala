/**
 * Representation of colour in rgb.
 *
 * @author Mateusz Galicki
 */
class RGB(private var _red: Int =0, private var _green: Int =0, private var _blue: Int =0) {
  def red: Int = _red
  def red_=(value: Int): Unit = _red = value

  def green: Int = _green
  def green_=(value: Int): Unit = _green = value

  def blue: Int = _blue
  def blue_=(value: Int): Unit = _blue = value


  /**
   * Update current colours.
   * @param colour rgb value represented by Int.
   */
  def addRGBValue(colour: Int): Unit = {
    // &0xFF - values are over 255, because we don't extract them as single values so we must use binary AND
    _red += (colour >>> 16) & 0xFF
    _green += (colour >>> 8) & 0xFF
    _blue += (colour >>> 0) & 0xFF
  }

  /**
   * Divides current colours by given value.
   * @param value value to divide colour by.
   * @return new RGB object with colours, which have divided values compared to current object.
   * @throws ArithmeticException if value equals 0.
   */
  def / (value: Int):RGB = {
    if(value == 0)
      throw new ArithmeticException("Cannot divide RGB object by 0")
    new RGB(red/value, green/value, blue/value)
  }

  override def toString: String = f"$red, $green, $blue"
}
object RGB{

  def apply(): RGB = new RGB()

  /**
   * Calculates luminance, using SRGB luminance constants.
   * @param rgb representation of colour in rgb object.
   * @return Value of luminance in range 0 to 100.
   */
  def luminance(rgb: RGB): Int =
    (((rgb.red * 0.2126f + rgb.green * 0.7152f + rgb.blue * 0.0722f) / 255)*100).round

}




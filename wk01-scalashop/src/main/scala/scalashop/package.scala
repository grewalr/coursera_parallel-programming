

package object scalashop
{

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int =
  {
    if(v < min) min
    else if(v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA])
  {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA =
  {
    // Sum up surrounding pixels RGBA vals then get average
    // store average in the output pixel RGBA
    var iter = 0
    var iterX = clamp(x - radius, 0, src.width -1)
    var r, g, b, a = 0

    while (iterX <= clamp(x + radius, 0, src.width -1))
    {
      var iterY = clamp(y - radius, 0, src.height -1)
      while (iterY <= clamp(y + radius, 0, src.height -1))
      {
        val pixel = src(iterX, iterY)
        r += red(pixel)
        g += green(pixel)
        b += blue(pixel)
        a += alpha(pixel)

        iterY += 1
        iter += 1
      }

      iterX +=1
    }

    rgba(r / iter, g / iter, b  / iter, a / iter)
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernelForLoop(src: Img, x: Int, y: Int, radius: Int): RGBA =
  {
    // Sum up surrounding pixels then get average
    // store average in the output pixel
    var rgbaSum = 0
    var iter = 0
    var r, g, b, a = 0

    for (i <- clamp(x - radius, 0, src.width -1) to clamp(x + radius, 0, src.width -1))
    {
      for (j <- clamp(y - radius, 0, src.height -1) to clamp(y + radius, 0, src.height -1))
      {
        val rgbaVal = src(i, j)
        r += red(rgbaVal)
        g += green(rgbaVal)
        b += blue(rgbaVal)
        a += alpha(rgbaVal)

        iter += 1
      }
    }

    rgba(r / iter, g / iter, b  / iter, a / iter)
  }
}

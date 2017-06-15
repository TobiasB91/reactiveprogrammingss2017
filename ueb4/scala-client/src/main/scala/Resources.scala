import java.util.concurrent.TimeoutException

import org.scalajs.dom.{CanvasRenderingContext2D, document, html, window}
import org.scalajs.dom.html.Canvas
import pixijs.Texture
import pixijs.extras.TilingSprite
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.concurrent.{Future, Promise}
import scala.util.Random

/**
  * Created by martin on 5/31/17.
  */
object Resources {
  def loadImage(src: String, scale: Double = 1.0, timeout: Int = 2000): Future[html.Image] = {
    val p = Promise[html.Image]
    val t = Promise[html.Image]
    window.setTimeout(
      () => t.failure(new TimeoutException(s"could not load '$src' within $timeout ms")),
      timeout)
    val img = document.createElement("img").asInstanceOf[html.Image]
    img.onload = e => {
      img.width = (img.width * scale).toInt
      img.height = (img.height * scale).toInt
      p.success(img)
    }
    img.src = src
    Future.firstCompletedOf(Seq(p.future, t.future))
  }

  def generateStarTexture(z: Int) = {
    val size = 1024
    val margin = 64
    val canvas = document.createElement("canvas").asInstanceOf[Canvas]
    canvas.width = size + 2 * margin
    canvas.height = canvas.width
    val context = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    context.globalAlpha = 0.7

    for (i <- 1 to (10000 / Math.pow(z,3)).toInt) {
      context.beginPath()
      val x = margin + Random.nextInt(size)
      val y = margin + Random.nextInt(size)
      val r = 1.5 * z * List.fill(z + 1)(Random.nextDouble()).product
      context.arc(x, y, r, 0, 2 * Math.PI)
      val hue = (180 + Random.nextInt(240)) % 360
      val saturation = Random.nextDouble() * 100
      val lightness = Random.nextInt(20) + 80
      context.fillStyle = s"hsl($hue,$saturation%,$lightness%)"
      context.shadowColor = s"hsl($hue,$saturation%,$lightness%)"
      context.shadowBlur = 10
      context.fill()
    }

    // 1 2 3
    // 4   5
    // 6 7 8
    context.drawImage(canvas, 0, 0, margin, margin, size, size, margin, margin)
    context.drawImage(canvas, margin, 0, size, margin, margin, size, size, margin)
    context.drawImage(canvas, margin + size, 0, margin, margin, margin, size, margin, margin)

    context.drawImage(canvas, 0, margin, margin, size, size, margin, margin, size)
    context.drawImage(canvas, margin + size, margin, margin, size, margin, margin, margin, size)

    context.drawImage(canvas, 0, margin + size, margin, margin, size, margin, margin, margin)
    context.drawImage(canvas, margin, margin + size, size, margin, margin, margin, size, margin)
    context.drawImage(canvas, margin + size, margin + size, margin, margin, margin, margin, margin, margin)

    val target = document.createElement("canvas").asInstanceOf[Canvas]
    target.width = size
    target.height = size
    val targetContext = target.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    targetContext.drawImage(canvas, margin, margin, size, size, 0, 0, size, size)
    loadImage(target.toDataURL("image/png"))
  }

  // Load background layers in parallel
  def loadBackgrounds() = Future.sequence(
    //loadImage("images/background.jpg") +:
      (1 to 4).map(Resources.generateStarTexture)
  ).map { imgs =>
    imgs.map { img =>
      val texture = Texture.from(img)
      new TilingSprite(texture, texture.width, texture.height)
    }
  }

  def loadSpaceTextures() = loadImage("images/sheet.png").map(new SpaceTextures(_))

  class SpaceTextures(val sheet: html.Image) {
    import Color._

    def clip(x: Int, y: Int, width: Int, height: Int) = {
      val texture = Texture.from(sheet)
      texture.frame = new pixijs.Rectangle(x, y, width, height)
      texture
    }

    val beams = Array(
      clip(x = 143, y = 377, width = 43, height = 31),
      clip(x = 327, y = 644, width = 40, height = 20),
      clip(x = 262, y = 907, width = 38, height = 31),
      clip(x = 396, y = 384, width = 29, height = 29),
      clip(x = 177, y = 496, width = 41, height = 17),
      clip(x = 186, y = 377, width = 40, height = 25),
      clip(x = 120, y = 688, width = 43, height = 23)
    )

    val longBeams = Array(
      clip(x = 828, y = 943, width = 15, height = 67),
      clip(x = 307, y = 309, width = 25, height = 64)
    )

    val enemies = Map[Color.Enemy, Array[Texture]](
      Black -> Array(
        clip(x = 423, y = 728, width = 93, height = 84),
        clip(x = 120, y = 604, width = 104, height = 84),
        clip(x = 144, y = 156, width = 103, height = 84),
        clip(x = 518, y = 325, width = 82, height = 84),
        clip(x = 346, y = 150, width = 97, height = 84)
      ),
      Blue -> Array(
        clip(x = 425, y = 468, width = 93, height = 84),
        clip(x = 143, y = 293, width = 104, height = 84),
        clip(x = 222, y = 0, width = 103, height = 84),
        clip(x = 518, y = 409, width = 82, height = 84),
        clip(x = 421, y = 814, width = 97, height = 84)

      ),
      Green -> Array(
        clip(x = 425, y = 552, width = 93, height = 84),
        clip(x = 133, y = 412, width = 104, height = 84),
        clip(x = 224, y = 496, width = 103, height = 84),
        clip(x = 518, y = 493, width = 82, height = 84),
        clip(x = 408, y = 907, width = 97, height = 84)
      ),
      Red -> Array(
        clip(x = 425, y = 384, width = 93, height = 84),
        clip(x = 120, y = 520, width = 104, height = 84),
        clip(x = 224, y = 580, width = 103, height = 84),
        clip(x = 520, y = 577, width = 82, height = 84),
        clip(x = 423, y = 644, width = 97, height = 84)
      )
    )


    val engines = Array(
      clip(x = 224, y = 907, width = 38, height = 23),
      clip(x = 163, y = 688, width = 42, height = 28),
      clip(x = 644, y = 1002, width = 27, height = 22),
      clip(x = 144, y = 240, width = 49, height = 45),
      clip(x = 133, y = 496, width = 44, height = 24)
    )

    val fires = Array(
      clip(x = 827, y = 125, width = 16, height = 40),
      clip(x = 828, y = 206, width = 14, height = 31),
      clip(x = 827, y = 663, width = 14, height = 32),
      clip(x = 829, y = 437, width = 14, height = 34),
      clip(x = 831, y = 0, width = 14, height = 31),
      clip(x = 834, y = 299, width = 14, height = 31),
      clip(x = 835, y = 502, width = 14, height = 31),
      clip(x = 835, y = 330, width = 14, height = 31),
      clip(x = 827, y = 867, width = 16, height = 40),
      clip(x = 811, y = 663, width = 16, height = 40),
      clip(x = 812, y = 206, width = 16, height = 40),
      clip(x = 835, y = 395, width = 14, height = 31),
      clip(x = 835, y = 533, width = 14, height = 32),
      clip(x = 835, y = 361, width = 14, height = 34),
      clip(x = 831, y = 31, width = 14, height = 31),
      clip(x = 829, y = 471, width = 14, height = 31),
      clip(x = 828, y = 268, width = 14, height = 31),
      clip(x = 828, y = 237, width = 14, height = 31),
      clip(x = 827, y = 165, width = 16, height = 41),
      clip(x = 812, y = 246, width = 16, height = 41)
    )

    val guns = Array(
      clip(x = 827, y = 907, width = 16, height = 36),
      clip(x = 810, y = 867, width = 17, height = 33),
      clip(x = 829, y = 611, width = 14, height = 36),
      clip(x = 809, y = 796, width = 20, height = 41),
      clip(x = 827, y = 84, width = 16, height = 41),
      clip(x = 423, y = 0, width = 21, height = 41),
      clip(x = 810, y = 900, width = 17, height = 38),
      clip(x = 829, y = 796, width = 14, height = 41),
      clip(x = 848, y = 263, width = 10, height = 47),
      clip(x = 809, y = 611, width = 20, height = 52),
      clip(x = 808, y = 961, width = 20, height = 52)
    )

    val lasers = Map[Color.Laser, Array[Texture]](
      Blue -> Array(
        clip(x = 856, y = 421, width = 9, height = 54),
        clip(x = 841, y = 647, width = 13, height = 37),
        clip(x = 856, y = 57, width = 9, height = 37),
        clip(x = 835, y = 565, width = 13, height = 37),
        clip(x = 858, y = 475, width = 9, height = 37),
        clip(x = 835, y = 752, width = 13, height = 37),
        clip(x = 856, y = 775, width = 9, height = 37),
        clip(x = 596, y = 961, width = 48, height = 46),
        clip(x = 434, y = 325, width = 48, height = 46),
        clip(x = 740, y = 724, width = 37, height = 37),
        clip(x = 698, y = 795, width = 38, height = 37),
        clip(x = 835, y = 695, width = 13, height = 57),
        clip(x = 856, y = 869, width = 9, height = 57),
        clip(x = 842, y = 206, width = 13, height = 57),
        clip(x = 849, y = 480, width = 9, height = 57),
        clip(x = 843, y = 62, width = 13, height = 54)
      ),
      Green -> Array(
        clip(x = 740, y = 686, width = 37, height = 38),
        clip(x = 843, y = 116, width = 13, height = 57),
        clip(x = 855, y = 173, width = 9, height = 57),
        clip(x = 848, y = 565, width = 13, height = 37),
        clip(x = 854, y = 639, width = 9, height = 37),
        clip(x = 845, y = 0, width = 13, height = 57),
        clip(x = 849, y = 364, width = 9, height = 57),
        clip(x = 848, y = 738, width = 13, height = 37),
        clip(x = 856, y = 94, width = 9, height = 37),
        clip(x = 843, y = 426, width = 13, height = 54),
        clip(x = 849, y = 310, width = 9, height = 54),
        clip(x = 843, y = 602, width = 13, height = 37),
        clip(x = 858, y = 0, width = 9, height = 37),
        clip(x = 193, y = 240, width = 48, height = 46),
        clip(x = 443, y = 182, width = 48, height = 46),
        clip(x = 760, y = 192, width = 37, height = 37)
      ),
      Red -> Array(
        clip(x = 858, y = 230, width = 9, height = 54),
        clip(x = 843, y = 977, width = 13, height = 37),
        clip(x = 856, y = 602, width = 9, height = 37),
        clip(x = 843, y = 940, width = 13, height = 37),
        clip(x = 856, y = 983, width = 9, height = 37),
        clip(x = 843, y = 903, width = 13, height = 37),
        clip(x = 856, y = 131, width = 9, height = 37),
        clip(x = 580, y = 661, width = 48, height = 46),
        clip(x = 602, y = 600, width = 48, height = 46),
        clip(x = 738, y = 650, width = 37, height = 36),
        clip(x = 737, y = 613, width = 37, height = 37),
        clip(x = 843, y = 846, width = 13, height = 57),
        clip(x = 856, y = 812, width = 9, height = 57),
        clip(x = 843, y = 789, width = 13, height = 57),
        clip(x = 856, y = 926, width = 9, height = 57),
        clip(x = 848, y = 684, width = 13, height = 54)
      )
    )

    import Size._

    val meteors = Map[Color.Meteor, Map[Size.Meteor, Array[Texture]]](
      Brown -> Map(
        Big -> Array(
          clip(x = 224, y = 664, width = 101, height = 84),
          clip(x = 0, y = 520, width = 120, height = 98),
          clip(x = 518, y = 810, width = 89, height = 82),
          clip(x = 327, y = 452, width = 98, height = 96)
        ),
        Medium -> Array(
          clip(x = 651, y = 447, width = 43, height = 43),
          clip(x = 237, y = 452, width = 45, height = 40)
        ),
        Small -> Array(
          clip(x = 406, y = 234, width = 28, height = 28),
          clip(x = 778, y = 587, width = 29, height = 26)
        ),
        Tiny -> Array(
          clip(x = 346, y = 814, width = 18, height = 18),
          clip(x = 399, y = 814, width = 16, height = 15)
        )
      ),
      Grey -> Map(
        Big -> Array(
          clip(x = 224, y = 748, width = 101, height = 84),
          clip(x = 0, y = 618, width = 120, height = 98),
          clip(x = 516, y = 728, width = 89, height = 82),
          clip(x = 327, y = 548, width = 98, height = 96)
        ),
        Medium -> Array(
          clip(x = 674, y = 219, width = 43, height = 43),
          clip(x = 282, y = 452, width = 45, height = 40)
        ),
        Small -> Array(
          clip(x = 406, y = 262, width = 28, height = 28),
          clip(x = 396, y = 413, width = 29, height = 26)
        ),
        Tiny -> Array(
          clip(x = 364, y = 814, width = 18, height = 18),
          clip(x = 602, y = 646, width = 16, height = 15)
        )
      )
    )

    val numerals = Array(
      clip(x = 367, y = 644, width = 19, height = 19),
      clip(x = 205, y = 688, width = 19, height = 19),
      clip(x = 406, y = 290, width = 19, height = 19),
      clip(x = 580, y = 707, width = 19, height = 19),
      clip(x = 386, y = 644, width = 19, height = 19),
      clip(x = 628, y = 646, width = 19, height = 19),
      clip(x = 671, y = 1002, width = 19, height = 19),
      clip(x = 690, y = 1004, width = 19, height = 19),
      clip(x = 709, y = 1004, width = 19, height = 19),
      clip(x = 491, y = 215, width = 19, height = 19),
      clip(x = 382, y = 814, width = 17, height = 17)
    )

    object players {
      val ships = Map[Color.Player, Array[Texture]](
        Blue -> Array(
          clip(x = 211, y = 941, width = 99, height = 75),
          clip(x = 112, y = 791, width = 112, height = 75),
          clip(x = 325, y = 739, width = 98, height = 75)
        ),
        Green -> Array(
          clip(x = 237, y = 377, width = 99, height = 75),
          clip(x = 112, y = 866, width = 112, height = 75),
          clip(x = 346, y = 75, width = 98, height = 75)
        ),
        Orange -> Array(
          clip(x = 247, y = 84, width = 99, height = 75),
          clip(x = 112, y = 716, width = 112, height = 75),
          clip(x = 336, y = 309, width = 98, height = 75)
        ),
        Red -> Array(
          clip(x = 224, y = 832, width = 99, height = 75),
          clip(x = 0, y = 941, width = 112, height = 75),
          clip(x = 325, y = 0, width = 98, height = 75)
        )
      )

      val damage = Array[Array[Texture]](
        Array(
          clip(x = 112, y = 941, width = 99, height = 75),
          clip(x = 247, y = 234, width = 99, height = 75),
          clip(x = 247, y = 159, width = 99, height = 75)
        ),
        Array(
          clip(x = 0, y = 866, width = 112, height = 75),
          clip(x = 0, y = 791, width = 112, height = 75),
          clip(x = 0, y = 716, width = 112, height = 75)
        ),
        Array(
          clip(x = 323, y = 832, width = 98, height = 75),
          clip(x = 310, y = 907, width = 98, height = 75),
          clip(x = 325, y = 664, width = 98, height = 75)
        )
      )

      val lifes = Map[Color.Player, Array[Texture]](
        Blue -> Array(
          clip(x = 482, y = 358, width = 33, height = 26),
          clip(x = 465, y = 991, width = 37, height = 26),
          clip(x = 777, y = 385, width = 32, height = 26)
        ),
        Green -> Array(
          clip(x = 535, y = 150, width = 33, height = 26),
          clip(x = 391, y = 991, width = 37, height = 26),
          clip(x = 778, y = 469, width = 32, height = 26)
        ),
        Orange -> Array(
          clip(x = 777, y = 327, width = 33, height = 26),
          clip(x = 428, y = 991, width = 37, height = 26),
          clip(x = 777, y = 712, width = 32, height = 26)
        ),
        Red -> Array(
          clip(x = 775, y = 301, width = 33, height = 26),
          clip(x = 502, y = 991, width = 37, height = 26),
          clip(x = 777, y = 443, width = 32, height = 26)
        )
      )
    }

    val pills = Map[Color.Pill, Texture](
      Blue -> clip(x = 674, y = 262, width = 22, height = 21),
      Green -> clip(x = 573, y = 989, width = 22, height = 21),
      Red -> clip(x = 222, y = 108, width = 22, height = 21),
      Yellow -> clip(x = 222, y = 129, width = 22, height = 21)
    )

    import PowerupType._

    val powerups = Map[Color.Powerup, Map[PowerupType, Texture]](
      Blue -> Map(
        Plain -> clip(x = 696, y = 329, width = 34, height = 33),
        Bolt -> clip(x = 539, y = 989, width = 34, height = 33),
        Shield -> clip(x = 777, y = 679, width = 34, height = 33),
        Star -> clip(x = 776, y = 895, width = 34, height = 33)
      ),
      Green -> Map(
        Plain -> clip(x = 774, y = 613, width = 34, height = 33),
        Bolt -> clip(x = 766, y = 80, width = 34, height = 33),
        Shield -> clip(x = 776, y = 862, width = 34, height = 33),
        Star -> clip(x = 651, y = 490, width = 34, height = 33)
      ),
      Red -> Map(
        Plain -> clip(x = 491, y = 182, width = 34, height = 33),
        Bolt -> clip(x = 775, y = 646, width = 34, height = 33),
        Shield -> clip(x = 776, y = 928, width = 34, height = 33),
        Star -> clip(x = 774, y = 977, width = 34, height = 33)
      ),
      Yellow -> Map(
        Plain -> clip(x = 774, y = 761, width = 34, height = 33),
        Bolt -> clip(x = 740, y = 761, width = 34, height = 33),
        Shield -> clip(x = 482, y = 325, width = 34, height = 33),
        Star -> clip(x = 607, y = 857, width = 34, height = 33)
      )
    )

    val scratches = Array(
      clip(x = 325, y = 814, width = 21, height = 16),
      clip(x = 423, y = 41, width = 21, height = 16),
      clip(x = 346, y = 295, width = 16, height = 12)
    )

    val shields = Array(
      clip(x = 0, y = 412, width = 133, height = 108),
      clip(x = 0, y = 293, width = 143, height = 119),
      clip(x = 0, y = 156, width = 144, height = 137)
    )

    val metalShields = Map[Color.Metal,Texture](
      Bronze -> clip(x = 797, y = 143, width = 30, height = 30),
      Gold -> clip(x = 797, y = 113, width = 30, height = 30),
      Silver -> clip(x = 778, y = 824, width = 30, height = 30)
    )

    val speed = clip(x = 858, y = 284, width = 7, height = 108)

    val stars = Array(
      clip(x = 628, y = 681, width = 25, height = 24),
      clip(x = 222, y = 84, width = 25, height = 24),
      clip(x = 576, y = 300, width = 24, height = 24)
    )

    val metalStars = Map[Color.Metal,Texture](
      Bronze -> clip(x = 778, y = 794, width = 31, height = 30),
      Gold -> clip(x = 778, y = 557, width = 31, height = 30),
      Silver -> clip(x = 778, y = 527, width = 31, height = 30)
    )

    val things = Map[Color.Metal,Texture](
      Bronze -> clip(x = 778, y = 495, width = 32, height = 32),
      Gold -> clip(x = 777, y = 411, width = 32, height = 32),
      Silver -> clip(x = 777, y = 353, width = 32, height = 32)
    )

    val turretBase_big = clip(x = 310, y = 982, width = 41, height = 41)
    val turretBase_small = clip(x = 808, y = 301, width = 26, height = 26)

    val ufos = Map[Color.UFO,Texture](
      Blue -> clip(x = 444, y = 91, width = 91, height = 91),
      Green -> clip(x = 434, y = 234, width = 91, height = 91),
      Red -> clip(x = 444, y = 0, width = 91, height = 91),
      Yellow -> clip(x = 505, y = 898, width = 91, height = 91)
    )


    val wings = Map[Color.Wing,Array[Texture]](
      Blue -> Array(
        clip(x = 647, y = 924, width = 45, height = 78),
        clip(x = 738, y = 253, width = 37, height = 72),
        clip(x = 805, y = 0, width = 26, height = 84),
        clip(x = 600, y = 450, width = 51, height = 75),
        clip(x = 692, y = 924, width = 42, height = 80),
        clip(x = 596, y = 892, width = 51, height = 69),
        clip(x = 694, y = 847, width = 42, height = 74),
        clip(x = 675, y = 134, width = 43, height = 83)
      ),
      Green -> Array(
        clip(x = 650, y = 525, width = 45, height = 78),
        clip(x = 775, y = 229, width = 37, height = 72),
        clip(x = 809, y = 527, width = 26, height = 84),
        clip(x = 535, y = 0, width = 51, height = 75),
        clip(x = 694, y = 431, width = 42, height = 80),
        clip(x = 525, y = 251, width = 51, height = 69),
        clip(x = 695, y = 511, width = 42, height = 74),
        clip(x = 655, y = 764, width = 43, height = 83)
      ),
      Red -> Array(
        clip(x = 809, y = 712, width = 26, height = 84),
        clip(x = 768, y = 0, width = 37, height = 72),
        clip(x = 600, y = 300, width = 51, height = 75),
        clip(x = 698, y = 715, width = 42, height = 80),
        clip(x = 586, y = 75, width = 51, height = 69),
        clip(x = 718, y = 123, width = 42, height = 74),
        clip(x = 653, y = 681, width = 43, height = 83),
        clip(x = 651, y = 286, width = 45, height = 78)
      ),
      Yellow -> Array(
        clip(x = 650, y = 603, width = 45, height = 78),
        clip(x = 760, y = 120, width = 37, height = 72),
        clip(x = 809, y = 353, width = 26, height = 84),
        clip(x = 576, y = 150, width = 51, height = 75),
        clip(x = 726, y = 0, width = 42, height = 80),
        clip(x = 525, y = 182, width = 51, height = 69),
        clip(x = 695, y = 585, width = 42, height = 74),
        clip(x = 651, y = 364, width = 43, height = 83)
      )
    )

    val bolt_silver = clip(x = 810, y = 837, width = 19, height = 30)
    val bolt_bronze = clip(x = 810, y = 467, width = 19, height = 30)
    val bolt_gold = clip(x = 809, y = 437, width = 19, height = 30)

    val buttons = Map[Color.Button,Texture](
      Blue -> clip(x = 0, y = 78, width = 222, height = 39),
      Green -> clip(x = 0, y = 117, width = 222, height = 39),
      Red -> clip(x = 0, y = 0, width = 222, height = 39),
      Yellow -> clip(x = 0, y = 39, width = 222, height = 39)
    )

    val cockpits = Map[Color.Cockpit,Array[Texture]](
      Blue -> Array(
        clip(x = 586, y = 0, width = 51, height = 75),
        clip(x = 736, y = 862, width = 40, height = 40),
        clip(x = 684, y = 67, width = 42, height = 56),
        clip(x = 336, y = 384, width = 60, height = 61),
        clip(x = 637, y = 0, width = 47, height = 67),
        clip(x = 627, y = 144, width = 48, height = 75),
        clip(x = 684, y = 0, width = 42, height = 67),
        clip(x = 737, y = 542, width = 41, height = 71)
      ),
      Green -> Array(
        clip(x = 576, y = 225, width = 51, height = 75),
        clip(x = 734, y = 977, width = 40, height = 40),
        clip(x = 696, y = 659, width = 42, height = 56),
        clip(x = 346, y = 234, width = 60, height = 61),
        clip(x = 627, y = 219, width = 47, height = 67),
        clip(x = 694, y = 364, width = 42, height = 67),
        clip(x = 737, y = 471, width = 41, height = 71),
        clip(x = 602, y = 525, width = 48, height = 75)
      ),
      Red -> Array(
        clip(x = 535, y = 75, width = 51, height = 75),
        clip(x = 351, y = 982, width = 40, height = 40),
        clip(x = 718, y = 197, width = 42, height = 56),
        clip(x = 520, y = 661, width = 60, height = 61),
        clip(x = 647, y = 857, width = 47, height = 67),
        clip(x = 605, y = 707, width = 48, height = 75),
        clip(x = 736, y = 795, width = 42, height = 67),
        clip(x = 736, y = 329, width = 41, height = 71)
      ),
      Yellow -> Array(
        clip(x = 726, y = 80, width = 40, height = 40),
        clip(x = 247, y = 309, width = 60, height = 61),
        clip(x = 637, y = 67, width = 47, height = 67),
        clip(x = 607, y = 782, width = 48, height = 75),
        clip(x = 696, y = 262, width = 42, height = 67),
        clip(x = 736, y = 400, width = 41, height = 71),
        clip(x = 734, y = 921, width = 42, height = 56),
        clip(x = 600, y = 375, width = 51, height = 75)
      )
    )

    val cursor = clip(x = 797, y = 173, width = 30, height = 33)

  }

}
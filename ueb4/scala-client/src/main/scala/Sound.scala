import org.scalajs.dom.{html, window}

object Sound {
  /**
    * Plays a sound file
    */
  def play(file: String) = {
    val elem = window.document.createElement("audio").asInstanceOf[html.Audio]
    elem.src = file
    elem.load()
    elem.play()
  }

  def shoot() = play("sounds/sfx_laser1.ogg")
}

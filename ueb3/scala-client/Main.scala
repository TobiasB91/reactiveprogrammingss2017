import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import io.circe.parser._
import io.circe.syntax._
import scala.util.Try

/**
 * Die JavaScript Client Anwendung
 */
object Main extends JSApp {
  def main(): Unit = {
    // Wir bauen eine Websocket Verbindung zum Server Auf
    val socket = new WebSocket("ws://" + window.location.host)
    socket.onopen = (d) => {
      renderSheet(upd => socket.send(upd.asJson.noSpaces))
    }

    // Hier reagieren wir auf Nachrichten die wir vom Server bekommen
    socket.onmessage = (e) => {
      println(e.data.toString)
      // Die dekodierte JSON Nachricht
      val msg = decode[Message](e.data.toString)
      // Wenn ein Fehler beim dekodieren aufgetreten ist, werfe ihn auf der Konsole
      // Sonst verarbeite die Nachricht
      msg.fold(throw _, {
        case CellUpdate(x,y,Right(value)) =>
          renderCell(x,y,value.map(_.toString).getOrElse(""))
        case CellUpdate(x,y,Left(error)) =>
          renderCell(x,y,error,true)
        case other =>
          console.error("unhandled message", other.toString)
      })
    }

    // Wenn die Verbindung geschlossen wird, zeigen wir eine Fehlermeldung an.
    socket.onclose = (e) => {
      document.getElementById("sheet").innerHTML =
        "<span class='error'>Die Verbindung zum Server wurde geschlossen</span>"
    }
  }

  def renderCell(x: Char, y: Int, content: String, error: Boolean = false) = {
    Try(document.getElementById(s"cell-$x-$y").asInstanceOf[html.TableCell]).foreach { cell =>
      cell.innerHTML = content
      cell.className = ""
      if (error) cell.classList.add("error")
    }
  }

  def renderSheet(send: Message => Unit) = {
    val sheet = document.getElementById("sheet").asInstanceOf[html.Div]
    sheet.innerHTML = (0 to 100).map { y =>
      ('@' to 'Z').map {
        case '@' => s"<th id='head-$y'>$y</th>"
        case x => y match {
          case 0 => s"<th id='head-$x'>$x</th>"
          case y => s"<td id='cell-${x}-$y'></td>"
        }
      }.mkString("<tr>","\n","</tr>")
    }.mkString("<table>","\n","</table>")

    val CellId = "cell-([A-Z])-([0-9]*[1-9])".r

    sheet.onclick = (e) => {
      Try(e.target.asInstanceOf[html.TableDataCell]).filter(_.tagName == "TD").foreach { cell =>
        val CellId(x,y) = cell.id
        val formula = Option(cell.getAttribute("data-formula")).getOrElse("")
        val beforeClass = cell.className
        val beforeText = cell.innerHTML
        val form = document.createElement("form").asInstanceOf[html.Form]
        val input = document.createElement("input").asInstanceOf[html.Input]
        val submit = document.createElement("input").asInstanceOf[html.Input]
        input.`type` = "text"
        input.value = formula
        submit.`type` = "submit"
        submit.style.visibility = "hidden"
        form.appendChild(input)
        form.appendChild(submit)
        cell.innerHTML = ""
        cell.appendChild(form)
        cell.className = "editing"
        input.focus()
        input.onblur = (e: Event) => {
          cell.className = beforeClass
          cell.innerHTML = beforeText
        }
        form.onsubmit = (e: Event) => {
          val text = input.value.trim
          cell.setAttribute("data-formula",text)
          val payload = if (text.isEmpty) None else Some(text)
          cell.classList.remove("editing")
          cell.classList.add("loading")
          cell.innerHTML = "loading"
          send(FormulaUpdate(x.head, y.toInt, payload))
          false
        }
      }
    }
  }
}
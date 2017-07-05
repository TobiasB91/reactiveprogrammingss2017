import org.scalajs.dom.{document, html, console}

object Editor {
  type Cursor = Int
  def empty = Editor("",0,List())
}

case class Editor(content: String, cursor: Editor.Cursor, cursors: List[Editor.Cursor]) {
  def applyOperation(operation: TextOperation.Operation): Editor = copy(TextOperation.applyOp(content,operation))

  def insert(c: Char) = {
    val operation = if (cursor == 0 && content.length == 0) {
      List(Insert(c.toString()))
    } else if(cursor == 0) {
      List(Insert(c.toString()),Retain(content.length))
    } else if(content.length == 0) {
      List(Insert(c.toString()))
    } else if(content.length == cursor) {
      List(Retain(cursor),Insert(c.toString()))
    } else {
      List(Retain(cursor),Insert(c.toString()),Retain(content.length - cursor))
    }
    (Option(operation),Editor(TextOperation.applyOp(content,operation),cursor + 1,
      cursors.map(c => TextOperation.transformCursor(operation, c))))
  }

  def backspace = if (cursor > 0) {
    val operation = List(Retain(cursor - 1), Delete(1) , Retain(content.length - cursor)).filter(op => op!=Retain(0))
    (Option(operation),Editor(TextOperation.applyOp(content,operation),cursor - 1,
      cursors.map(c => TextOperation.transformCursor(operation, c))))
  } else (None,this)

  def moveLeft = {
    val operation = List(Retain(content.length)) 
    (Option(operation), Editor(content, Math.max(0,cursor - 1),cursors)) 
  }

  def moveRight = {
    val operation = List(Retain(content.length))
    (Option(operation), Editor(content, Math.min(content.length, cursor + 1),cursors))
  }

  /**
    * Measure the size of a rendered string
    * @param text the string to render
    * @return width and height of the rendered text
    */
  private def measure(text: String): (Int,Int) = {
    val measure = document.getElementById("measure")
    val width = if (text.isEmpty || text.last == '\n') 0
    else {
      measure.textContent = text.lines.toStream.last
      measure.clientWidth
    }
    measure.textContent = if (text.isEmpty || text.last == '\n') text + " " else text
    (width,measure.clientHeight)
  }

  def render(elem: html.Div) = {
    elem.textContent = content
    val (w,h) = measure(content.take(cursor))
    val ownCursor = document.createElement("div").asInstanceOf[html.Div]
    ownCursor.style.left = w + "px"
    ownCursor.style.top = h + "px"
    ownCursor.classList.add("ownCaret")
    elem.appendChild(ownCursor)
    
    cursors.foldRight(elem)((c,e) => {
      val (w,h) = measure(content.take(c))
      val cursor = document.createElement("div").asInstanceOf[html.Div]
      cursor.style.left = w + "px"
      cursor.style.top = h + "px"
      cursor.classList.add("caret")
      e.appendChild(cursor)
      e 
    })
  }
}

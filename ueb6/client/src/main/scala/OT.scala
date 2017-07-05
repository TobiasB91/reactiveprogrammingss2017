import org.scalajs.dom.{document, html, console}
trait Operation {
  type Operation
  type Document

  def noop(doc: Document): Operation
  def compose(a: Operation, b: Operation): Operation
  def transform(a: Operation, b: Operation): (Operation,Operation)  
  def applyOp(doc: Document, op: Operation): Document
  def transformCursor(a: Operation, cursor: Int) : Int
  def reduce(op: Operation) : Operation
}

sealed trait TextAction
case class Retain(n: Int) extends TextAction
case class Insert(cs: String) extends TextAction
case class Delete(n: Int) extends TextAction

object TextOperation extends Operation {
  type Operation = List[TextAction]
  type Document = String

  def reduce(op: Operation) : Operation = op match {
    case (Insert(a)::Insert(b)::as) => reduce(Insert(a+b) :: as)
    case (a :: as) => a :: reduce(as)
    case Nil => Nil
  }

  def retain(n: Int): Operation = List(Retain(n))
  def delete(n: Int): Operation = List(Delete(n))
  def insert(s: String): Operation = List(Insert(s))

  def noop(doc: Document): Operation = List(Retain(doc.length))

  def compose(a: Operation, b: Operation): Operation = (a,b) match {
    case (Nil, Nil)       => Nil
    case (Delete(n)::as, bs) => Delete(n) :: compose(as, bs)
    case (as, Insert(cs)::bs) => 
      Insert(cs) :: compose(as, bs)
    case (Retain(n)::as, Retain(n2)::bs) => 
      if (n > n2) Retain(n2)::compose(Retain(n-n2)::as,bs)
      else if (n < n2) Retain(n)::compose(as,Retain(n2-n)::bs)
      else Retain(n)::compose(as, bs)
    case (Retain(n):: as, Delete(n2)::bs) =>
      if(n > n2) Delete(n2)::compose(Retain(n-n2)::as,bs)
      else if(n < n2) Delete(n)::compose(as,Retain(n2-n)::bs)
      else Delete(n)::compose(as,bs)
    case (Insert(cs)::as,Retain(n)::bs) =>
      if(n > cs.length) Insert(cs)::compose(as,Retain(n-cs.length)::bs)
      else if(n < cs.length) Insert(cs.take(n))::compose(Insert(cs.drop(n))::as, bs)
      else Insert(cs) :: compose(as, bs)
    case (Insert(cs)::as, Delete(n)::bs) =>
      if(n > cs.length) compose(as,Delete(n-cs.length)::bs)
      else if(n < cs.length) compose(Insert(cs.drop(n))::as,bs)
      else compose(as, bs)
  }

  def transform(a: Operation, b: Operation): (Operation,Operation) = (a,b) match {      
    case (Nil,Nil) => (Nil,Nil)
    case (Insert(cs)::as, bs) => 
      val (as_,bs_) = transform(as,bs)
      (Insert(cs)::as_,Retain(cs.length)::bs_)
    case (as, Insert(cs) :: bs) =>
      val (as_,bs_) = transform(as,bs)
      (Retain(cs.length)::as_, Insert(cs)::bs_)
    case (Retain(n):: as, Retain(n2):: bs) =>
      if (n > n2) {
        val (as_,bs_) = transform(Retain(n-n2)::as,bs)
        (Retain(n2)::as_,Retain(n2)::bs_)
      }
      else if (n < n2) {
        val (as_,bs_) = transform(as,Retain(n2-n)::bs)
        (Retain(n)::as_, Retain(n)::bs_)
      }
      else { 
        val (as_,bs_) = transform(as,bs)
        (Retain(n):: as_, Retain(n2):: bs_)
      }
    case (Delete(n)::as, Delete(n2)::bs) =>
      if(n > n2) {
        transform(Delete(n-n2)::as,bs)
      }
      else if(n > n2) {
        transform(as,Delete(n2-n)::bs)
      }
      else {
        transform(as,bs)
      }
    case (Retain(n):: as, Delete(n2)::bs) =>
      if(n > n2) {
        val (as_,bs_) = transform(Retain(n-n2)::as,bs)
        (as_,Delete(n2)::bs_)
      } 
      else if(n < n2) {
        val (as_,bs_) = transform(as,Delete(n2-n)::bs)
        (as_,Delete(n)::bs_)
      }
      else {
        val (as_,bs_) = transform(as,bs)
        (as_, Delete(n)::bs_)
      }
    case (Delete(n):: as, Retain(n2)::bs) =>
      if (n > n2) {
        val (as_,bs_) = transform(Delete(n-n2)::as, bs)
        (Delete(n2)::as_,bs_)
      }
      else if(n < n2) {
        val (as_,bs_) = transform(as,Retain(n2-n)::bs)
        (Delete(n)::as_,bs_)
      }
      else {
        val (as_,bs_) = transform(as,bs)
        (Delete(n)::as_,bs_)
      }
  }

  def transformCursor(op: Operation, cursor: Int) : Int = 
    TextOperation.split(op,cursor).foldRight(cursor) { 
      (o, acc) => o match {
        case Retain(n) => acc
        case Delete(n) => acc - n
        case Insert(cs) => acc + cs.length
      }
    }
  
  def split(op: Operation, c : Int) : Operation = {
    op match {
      case Nil => Nil 
      case (x::xs) => 
        if (c <= 0) Nil 
        else x match {
          case Retain(n) => 
            if (n <= c) x :: split(xs,c-n) 
            else List(Retain(c))
          case Delete(n) =>
            if (n <= c) x :: split(xs,c-n)
            else List(Delete(c))
          case Insert(cs) =>
            if (cs.length <= c) x :: split(xs,c-cs.length) 
            else List(Insert(cs.take(c)))
        }
    }
  }
    def applyOp(doc: Document, op: Operation): Document = (doc,op) match {
    case ("",Nil) => ""
    case (d,Retain(0)::as) => applyOp(d,as)
    case (d,Retain(n)::as) if d.nonEmpty => d.head + applyOp(d.tail,Retain(n-1)::as)
    case (d,Insert(cs)::as) => cs ++ applyOp(d,as)
    case (d,Delete(0)::as) => applyOp(d,as)
    case (d,Delete(n)::as) if d.nonEmpty => applyOp(d.tail, Delete(n-1)::as)
  }
}

trait Client[T <: Operation] { self =>
  val ot: T
  val revision: Int
  val pending: Option[ot.Operation]
  val buffer: Option[ot.Operation]

  private def copy(
    revision: Int = this.revision,
    pending: Option[ot.Operation] = this.pending,
    buffer: Option[ot.Operation] = this.buffer
  ): Client[T] = {
    val (r,p,b) = (revision,pending,buffer)
    new Client[T] {
      val ot = self.ot
      val revision = r
      val pending = p.asInstanceOf[Option[ot.Operation]]
      val buffer = b.asInstanceOf[Option[ot.Operation]]
    }
  }

  def localEdit(op: ot.Operation): (Boolean, Client[T]) = {
    val newPending = pending orElse Some(op)
    val newBuffer = for {
      pending <- pending
    } yield buffer.map(ot.compose(_,op)).map(ot.reduce(_)).getOrElse(op)
    (pending.isEmpty, copy(pending = newPending, buffer = newBuffer))
  }

  def remoteEdit(op: ot.Operation): (ot.Operation, Client[T]) = (pending,buffer) match {
    case (None,None) => (op, copy(revision = revision + 1))
    case (Some(p),None) => 
      val (a,b) = ot.transform (p,op)
      (b, copy(revision = revision + 1, pending = Some(a)))
    case (Some(p),Some(b)) =>
      val (a1,b1) = ot.transform(p,op)
      val (a2,b2) = ot.transform(b,b1)
      (b2, copy(revision + 1, Some(a1), Some(a2)))
  }

  def ack: (Option[ot.Operation], Client[T]) = (buffer, copy(revision + 1, buffer, None))

  def isSynchronized: Boolean = pending.isEmpty

  override def toString = s"Client(\n  revision = $revision,\n  pending = $pending,\n  buffer = $buffer\n)"
}

object Client {
  def empty[T <: Operation](implicit evidence: T): Client[T] = new Client[T] {
    val ot = evidence
    val revision = 0
    val pending = Option.empty[ot.Operation]
    val buffer = Option.empty[ot.Operation]    
  }  
}

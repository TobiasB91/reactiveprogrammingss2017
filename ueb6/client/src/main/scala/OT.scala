trait Operation {
  type Operation
  type Document

  def noop(doc: Document): Operation
  def compose(a: Operation, b: Operation): Operation
  def transform(a: Operation, b: Operation): (Operation,Operation)  
  def applyOp(doc: Document, op: Operation): Document
  def transformCursor(a: Operation, cursor: Int) : Int
}

sealed trait TextAction
case object Retain extends TextAction
case class Insert(c: Char) extends TextAction
case object Delete extends TextAction

object TextOperation extends Operation {
  type Operation = List[TextAction]
  type Document = String

  def retain(n: Int): Operation = List.fill(n)(Retain)
  def delete(n: Int): Operation = List.fill(n)(Delete)
  def insert(s: String): Operation = s.map(Insert).toList

  def noop(doc: Document): Operation = doc.map(_ => Retain).toList

  def compose(a: Operation, b: Operation): Operation = (a,b) match {
    case (Nil, Nil)       => Nil
    case (Delete::as, bs) => Delete :: compose(as, bs)
    case (as, Insert(c)::bs) => Insert(c) :: compose(as, bs)
    case (Retain::as, Retain::bs) => Retain :: compose(as, bs)
    case (Retain :: as, Delete::bs)   => Delete :: compose(as, bs)
    case (Insert(c) :: as, Retain::bs) => Insert(c) :: compose(as, bs)
    case (Insert(_) :: as, Delete::bs) => compose(as, bs)
  }

  def transform(a: Operation, b: Operation): (Operation,Operation) = (a,b) match {      
    case (Nil,Nil) => (Nil,Nil)
    case (Insert(c) :: as, bs) => 
      val (as_,bs_) = transform(as,bs)
      (Insert(c) :: as_, Retain :: bs_)
    case (as, Insert(c) :: bs) =>
      val (as_,bs_) = transform(as,bs)
      (Retain :: as_, Insert(c) :: bs_)
    case (Retain :: as, Retain :: bs) =>
      val (as_,bs_) = transform(as,bs)
      (Retain :: as_, Retain :: bs_)
    case (Delete :: as, Delete :: bs) => 
      transform(as,bs)
    case (Retain :: as, Delete :: bs) =>
      val (as_,bs_) = transform(as,bs)
      (as_, Delete :: bs_)
    case (Delete :: as, Retain :: bs) =>
      val (as_,bs_) = transform(as,bs)
      (Delete :: as_, bs_)
  }

  def transformCursor(op: Operation, cursor: Int) : Int = 
    op.take(cursor).foldRight(cursor) { 
      (o, acc) => o match {
        case Retain => acc
        case Delete => acc - 1
        case Insert(_) => acc + 1
      }
    }

  def applyOp(doc: Document, op: Operation): Document = (doc,op) match {
    case ("",Nil) => ""
    case (d,Retain::as) if d.nonEmpty => d.head + applyOp(d.tail,as)
    case (d,Insert(c)::as) => c + applyOp(d,as)
    case (d,Delete::as) if d.nonEmpty => applyOp(d.tail,as)
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
    } yield buffer.map(ot.compose(_,op)).getOrElse(op)
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

package de.ag.jrlang.core

case class StylesMap(
    predefined: Map[Style.Internal, Style.External],
    styles : Map[Style.Internal, Style.External]) {
  def lookup(style: Style.Internal): (Style.External, StylesMap) = {
    val pre = predefined.get(style);
    if (pre.isDefined)
      (pre.get, this)
    else {
      val e = styles.getOrElse(style, Style.External(StylesMap.newName(styles)));
      (e, copy(styles = styles.updated(style, e)))
    }
  }
  
  def list: Seq[(String, Style.Internal)] =
    styles.map({ case(i,e) => (e.reference, i) }) toSeq
}
object StylesMap {
  def apply(predefined: Map[String, Style.Internal]) =
    new StylesMap(predefined map { case(n, i) => (i, Style.External(n)) }, Map.empty)    
  
  private def newName(styles : Map[Style.Internal, Style.External]) : String =
    "autodef" + (styles.size) // could be a little more intelligent
}

trait StyleFoldable[T] {
  def foldStyles(st: StylesMap): (T, StylesMap)
}
object StyleFoldable {
  
  def foldAll[T <: StyleFoldable[T]](coll : Seq[T], st: StylesMap) : (Vector[T], StylesMap) =
    coll.foldLeft((Vector.empty:Vector[T], st)) { case((c, st), v) => {
      val (v_, st_) = v.foldStyles(st);
      (c :+ v_, st_)
    }}
  
  def foldOption[T <: StyleFoldable[T]](v: Option[T], st: StylesMap) =
    if (v.isDefined) {
      val (v_, st_) = v.get.foldStyles(st)
      (Some(v_), st_)
    }
    else (None, st)
      
}

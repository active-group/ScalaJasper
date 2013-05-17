package de.ag.jrlang.core

sealed case class Band (
    height : Int,
    printWhenExpression : Option[Expression],
    splitType : net.sf.jasperreports.engine.`type`.SplitTypeEnum,
    children : Seq[Element] // elements + groups
    // origin: probably useless (set automatically, after Band is used somewhere)
    ) extends StyleFoldable[Band] with EnvCollector
{
  def foldStyles(st0: StylesMap) = {
    val (children_, st1) = Element.foldAllStyles(children, st0);
    (copy(children = children_),
        st1)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    printWhenExpression.collectEnv(
      children.collectEnv(e0))
};

object Band {
  val empty = new Band(
      height = 0,
      printWhenExpression = None,
      splitType = net.sf.jasperreports.engine.`type`.SplitTypeEnum.IMMEDIATE, //??
      children = Vector.empty
      )
  
  implicit def compileBand(o: Band) : net.sf.jasperreports.engine.design.JRDesignBand = {
    if (o == null)
      // this is only to simplify code, where an implicit conversion from Option[Band] => jasper.Band is handy.
      null
    else {
      val r = new net.sf.jasperreports.engine.design.JRDesignBand();
      r.setHeight(o.height);
      r.setPrintWhenExpression(o.printWhenExpression);
      r.setSplitType(o.splitType);
      ElementUtils.addChildren(o.children, r.addElement(_), r.addElementGroup(_));
      r
    }
  }
}

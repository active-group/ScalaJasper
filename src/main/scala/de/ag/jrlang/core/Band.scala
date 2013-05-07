package de.ag.jrlang.core

sealed case class Band (
    height : Int,
    printWhenExpression : Expression,
    splitType : net.sf.jasperreports.engine.`type`.SplitTypeEnum,
    children : Seq[Element] // elements + groups
    // origin: probably useless (set automatically, after Band is used somewhere)
    ) extends StyleFoldable[Band]
{
  def foldStyles(st0: StylesMap) = {
    val (children_, st1) = Element.foldAllStyles(children, st0);
    (copy(children = children_),
        st1)
  }
};

object Band {
  val empty = new Band(
      height = 0,
      printWhenExpression = "",
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
      // obj will 'own' the created child objects (like in DOM)
      for (c <- o.children) {
        // although elements and groups end up in the same children list,
        // there is no add method for children, but only for the two
        // classes of children, elements and element groups -
        // that API crime should be healed here
        val co : net.sf.jasperreports.engine.JRChild = c;
        co match {
          case g : net.sf.jasperreports.engine.design.JRDesignElementGroup => r.addElementGroup(g)
          case e : net.sf.jasperreports.engine.design.JRDesignElement => r.addElement(e)
          case _ => throw new RuntimeException("Unexpected type of child: " + co.getClass())
        }
      }
      r
    }
  }
}

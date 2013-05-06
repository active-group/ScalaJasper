package de.ag.jrlang.core

// JRDesignElementGroup, JRBaseElementGroup

import de.ag.jrlang.core.JRDesignBand;
import scala.collection.JavaConversions._

sealed case class JRDesignBand (
    height : Int,
    printWhenExpression : JRDesignExpression,
    splitType : net.sf.jasperreports.engine.`type`.SplitTypeEnum,
    children : Seq[JRDesignChild] // elements + groups
    // origin: probably useless
    );

object JRDesignBand {
  val empty = new JRDesignBand(
      height = 0,
      printWhenExpression = null,
      splitType = net.sf.jasperreports.engine.`type`.SplitTypeEnum.IMMEDIATE, //??
      children = Vector.empty
      )
  
  implicit def dropBand(o: JRDesignBand) : net.sf.jasperreports.engine.design.JRDesignBand = {
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

/*
object JRDesignBand extends CompanionAdapter[JRDesignBand, net.sf.jasperreports.engine.design.JRDesignBand] {
  def apply() : JRDesignBand = apply(new net.sf.jasperreports.engine.design.JRDesignBand())
  
  def apply(o: net.sf.jasperreports.engine.design.JRDesignBand) =
    new JRDesignBand(
        height = o.getHeight(),
        printWhenExpression = o.getPrintWhenExpression().asInstanceOf[net.sf.jasperreports.engine.design.JRDesignExpression],
        splitType = o.getSplitTypeValue(),
        children = o.getChildren().asInstanceOf[java.util.List[net.sf.jasperreports.engine.JRChild]]
    )

}
*/
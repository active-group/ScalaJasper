package de.ag.jrlang.core

import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{TraversableLike, IterableLike, GenTraversableOnce}
import net.sf.jasperreports.engine.design.JRDesignBand



import Transformer._
import net.sf.jasperreports.engine.`type`.SplitTypeEnum

sealed case class Band (
    splitType : SplitTypeEnum,
    content : Seq[Element], // elements + groups
    height : Int = 0, // calculate from content, under some circumstances?
    printWhenExpression : Option[Expression[Boolean]] = None
    // origin: probably useless (set automatically, after Band is used somewhere)
    ) extends Transformable[JRDesignBand] //FilterMonadic[Band, Iterable[Band]]
{
  def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignBand()
    ret(r.setHeight(height)) >>
    drop(orNull(printWhenExpression map {_.transform})) { r.setPrintWhenExpression(_) } >>
    ret(r.setSplitType(splitType)) >>
    ElementUtils.contentTransformer(content, r.addElement(_), r.addElementGroup(_)) >>
    ret(r)
  }
}

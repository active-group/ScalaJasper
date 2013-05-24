package de.ag.jrlang.core

import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{TraversableLike, IterableLike, GenTraversableOnce}
import net.sf.jasperreports.engine.design.JRDesignBand



import Transformer._

sealed case class Band (
    height : Int, // calculate from content, under some circumstances?
    printWhenExpression : Option[Expression[Boolean]],
    splitType : net.sf.jasperreports.engine.`type`.SplitTypeEnum,
    content : Seq[Element] // elements + groups
    // origin: probably useless (set automatically, after Band is used somewhere)
    ) extends Transformable[JRDesignBand] //FilterMonadic[Band, Iterable[Band]]
{
  def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignBand()
    ret(r.setHeight(height)) >>
    drop(printWhenExpression map {_.transform}) { r.setPrintWhenExpression(_) } >>
    ret(r.setSplitType(splitType)) >>
    ElementUtils.contentTransformer(content, r.addElement(_), r.addElementGroup(_)) >>
    ret(r)
  }
}

object Band {
  val empty = new Band(
      height = 0,
      printWhenExpression = None,
      splitType = net.sf.jasperreports.engine.`type`.SplitTypeEnum.IMMEDIATE, //??
      content = Vector.empty
      )
}

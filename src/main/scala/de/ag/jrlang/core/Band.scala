package de.ag.jrlang.core

import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.{TraversableLike, IterableLike, GenTraversableOnce}
import net.sf.jasperreports.engine.design.JRDesignBand



import Transformer._
import net.sf.jasperreports.engine.`type`.SplitTypeEnum

abstract sealed class BandHeight

object BandHeight {
  /** Defined, fixed height */
  sealed case class Fixed(height: Int) extends BandHeight
  /** Automatically calculate needed height */
  case object Auto extends BandHeight
  /** Automatically calculate needed height, but add some margin */
  sealed case class AutoPlus(margin: Int) extends BandHeight

  implicit def fixed(height: Int) = Fixed(height)

  private[core] def calc(h: BandHeight)(setter: Int=>Unit) = { contentHeight:Int =>
    val r = h match {
      case BandHeight.Fixed(v) => v
      case BandHeight.Auto => contentHeight
      case BandHeight.AutoPlus(v) => contentHeight + v
    }
    setter(r)
    ret()
  }
}

sealed case class Band (
    splitType : SplitTypeEnum,
    content : Seq[Element], // elements + groups
    height : BandHeight = BandHeight.Auto, // calculate from content
    printWhenExpression : Option[Expression[Boolean]] = None
    // origin: probably useless (set automatically, after Band is used somewhere)
    ) extends Transformable[JRDesignBand] //FilterMonadic[Band, Iterable[Band]]
{
  def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignBand()
    drop(orNull(printWhenExpression map {_.transform})) { r.setPrintWhenExpression(_) } >>
    ret(r.setSplitType(splitType)) >>
    (ElementUtils.contentTransformer(content, r.addElement(_), r.addElementGroup(_)) >>=
      BandHeight.calc(height){r.setHeight(_)}) >>
    ret(r)
  }
}

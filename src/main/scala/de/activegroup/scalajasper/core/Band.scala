package de.activegroup.scalajasper.core


import Transformer._
import net.sf.jasperreports.engine.`type`.SplitTypeEnum
import de.activegroup.scalajasper.core.Dimensions.Length

abstract sealed class BandHeight

object BandHeight {
  /** Defined, fixed height */
  sealed case class Fixed(height: Length) extends BandHeight
  /** Automatically calculate needed height */
  case object Auto extends BandHeight
  /** Automatically calculate needed height, but add some margin */
  sealed case class AutoPlus(margin: Length) extends BandHeight

  implicit def fixed(height: Length): Fixed = Fixed(height)

  private[core] def calc(h: BandHeight, contentHeight: Length)(setter: Int=>Unit): Unit = {
    val r = h match {
      case BandHeight.Fixed(v) => v
      case BandHeight.Auto => contentHeight
      case BandHeight.AutoPlus(v) => contentHeight + v
    }
    setter(r.inAbsolutePixels)
  }
}

sealed case class Band (
    splitType : SplitTypeEnum, // TODO add reasonable default?
    content : Element, // elements + groups
    height : BandHeight = BandHeight.Auto, // calculate from content
    printWhenExpression : Option[Expression[Boolean]] = None
    // origin: probably useless (set automatically, after Band is used somewhere)
    )
{
  private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignBand()
    BandHeight.calc(height, content.verticalExtent)(r.setHeight)
    drop(orNull(printWhenExpression map {_.transform})) { r.setPrintWhenExpression(_) } >>
    ret(r.setSplitType(splitType)) >>
    ElementUtils.contentTransformer(content.seq, r.addElement, r.addElementGroup) >>
    ret(r)
  }
}

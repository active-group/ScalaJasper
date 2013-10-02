package de.activegroup.scalajasper.core

import net.sf.jasperreports.engine.{JRHyperlinkParameter, JRExpression}
import net.sf.jasperreports.engine.design.JRDesignHyperlinkParameter
import net.sf.jasperreports.engine.`type`.{HyperlinkTypeEnum, HyperlinkTargetEnum}

import Transformer._

/** When the user clicks a hyperlink, he or she is redirected to a local destination within the current document or
  * to an external resource. */
abstract class Hyperlink
object Hyperlink {
  case object None extends Hyperlink

  /** point to an external resource, specified by the given expression */
  sealed case class Reference(url: Expression[String]) extends Hyperlink

  /** point to a local anchor, whose name (?) is specified by the given expression */
  sealed case class LocalAnchor(anchor: Expression[String]) extends Hyperlink

  /** point to a local page, whose 1-based index is specified with the given expression */
  sealed case class LocalPage(index: Expression[Int]) extends Hyperlink

  /** point to an anchor within an external document (url?) */
  sealed case class RemoteAnchor(reference: Expression[String], anchor: Expression[String]) extends Hyperlink

  /** point to a page within an external document (url?) */
  sealed case class RemotePage(reference: Expression[String], index: Expression[Int]) extends Hyperlink

  /** @see net.sf.jasperreports.engine.export.JRHyperlinkProducer */
  sealed case class Custom(typeName: String, arguments: Map[String, Expression[Any]]) extends Hyperlink

  private[core] def put(o: Hyperlink,
                        setHyperlinkType : HyperlinkTypeEnum => Unit,
                        setHyperlinkReferenceExpression : JRExpression => Unit,
                        setHyperlinkWhenExpression : JRExpression => Unit,
                        setHyperlinkAnchorExpression : JRExpression => Unit,
                        setHyperlinkPageExpression : JRExpression => Unit,
                        setLinkType : String => Unit,
                        addHyperlinkParameter : JRHyperlinkParameter => Unit) = {
    o match {
      case None =>
        ret(setHyperlinkType(HyperlinkTypeEnum.NONE))
      case Reference(url) =>
        ret(setHyperlinkType(HyperlinkTypeEnum.REFERENCE)) >>
        drop(url.transform)(setHyperlinkReferenceExpression)
      case LocalAnchor(anchor) =>
        ret(setHyperlinkType(HyperlinkTypeEnum.LOCAL_ANCHOR)) >>
        drop(anchor.transform)(setHyperlinkAnchorExpression)
      case LocalPage(index) =>
        ret(setHyperlinkType(HyperlinkTypeEnum.LOCAL_PAGE)) >>
        drop(index.transform)(setHyperlinkPageExpression)
      case RemoteAnchor(reference, anchor) =>
        ret(setHyperlinkType(HyperlinkTypeEnum.REMOTE_ANCHOR)) >>
        drop(reference.transform)(setHyperlinkReferenceExpression) >>
        drop(anchor.transform)(setHyperlinkAnchorExpression)
      case RemotePage(reference, index) =>
        ret(setHyperlinkType(HyperlinkTypeEnum.REMOTE_PAGE)) >>
        drop(reference.transform)(setHyperlinkReferenceExpression) >>
        drop(index.transform)(setHyperlinkPageExpression)
      case Custom(typeName, args) =>
        def transarg(t : (String, Expression[Any])) : Transformer[JRDesignHyperlinkParameter] =
          t._2.transform >>= { e =>
            val p = new JRDesignHyperlinkParameter()
            p.setName(t._1)
            p.setValueExpression(e)
            ret(p)
          }
        ret(setHyperlinkType(HyperlinkTypeEnum.CUSTOM)) >>
        ret(setLinkType(typeName)) >>
        all(args map transarg toList) >>= { ps =>
          ps foreach addHyperlinkParameter
          ret()
        }
    }
  }
}

sealed abstract class HyperlinkTarget

object HyperlinkTarget {
  case object Self extends HyperlinkTarget
  case object Blank extends HyperlinkTarget
  case object Parent extends HyperlinkTarget
  case object Top extends HyperlinkTarget

  /** @see net.sf.jasperreports.engine.export.JRHyperlinkTargetProducer.
    * If no target producer is found, the engine looks for any hyperlink parameter having the same name as the
    * specified custom target. */
  sealed case class Custom(s: String) extends HyperlinkTarget

  val default = Self

  private[core] def put(o: HyperlinkTarget,
                        setHyperlinkTarget : HyperlinkTargetEnum => Unit,
                        setLinkTarget : String => Unit) = {
    o match {
      case Self => ret(setHyperlinkTarget(HyperlinkTargetEnum.SELF))
      case Blank => ret(setHyperlinkTarget(HyperlinkTargetEnum.BLANK))
      case Parent => ret(setHyperlinkTarget(HyperlinkTargetEnum.PARENT))
      case Top => ret(setHyperlinkTarget(HyperlinkTargetEnum.TOP))
      case Custom(s) =>
        ret(setHyperlinkTarget(HyperlinkTargetEnum.CUSTOM))
        ret(setLinkTarget(s))
    }
  }
}

// type HyperlinkTooltip = Expression

sealed case class Link(hyperlink: Hyperlink,
                       target: HyperlinkTarget,
                       tooltip: Option[Expression[String]])

object Link {
  val empty = new Link(Hyperlink.None, HyperlinkTarget.default, None)

  private[core] def put(o: Link,
                        setHyperlinkType : HyperlinkTypeEnum => Unit,
                        setHyperlinkReferenceExpression : JRExpression => Unit,
                        setHyperlinkWhenExpression : JRExpression => Unit,
                        setHyperlinkAnchorExpression : JRExpression => Unit,
                        setHyperlinkPageExpression : JRExpression => Unit,
                        setLinkType : String => Unit,
                        addHyperlinkParameter : JRHyperlinkParameter => Unit,
                        setHyperlinkTarget : HyperlinkTargetEnum => Unit,
                        setLinkTarget : String => Unit,
                        setHyperlinkTooltipExpression : JRExpression => Unit) = {
    Hyperlink.put(
      o.hyperlink,
      setHyperlinkType,
      setHyperlinkReferenceExpression,
      setHyperlinkWhenExpression,
      setHyperlinkAnchorExpression,
      setHyperlinkPageExpression,
      setLinkType,
      addHyperlinkParameter
    ) >>
    HyperlinkTarget.put(
      o.target,
      setHyperlinkTarget,
      setLinkTarget
    ) >>
    drop(orNull(o.tooltip map { _.transform }))(setHyperlinkTooltipExpression)
  }

}
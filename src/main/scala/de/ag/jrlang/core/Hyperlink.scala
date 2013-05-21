package de.ag.jrlang.core

import net.sf.jasperreports.engine.{JRHyperlinkParameter, JRExpression}
import net.sf.jasperreports.engine.design.JRDesignHyperlinkParameter
import net.sf.jasperreports.engine.`type`.{HyperlinkTypeEnum, HyperlinkTargetEnum}

/** When the user clicks a hyperlink, he or she is redirected to a local destination within the current document or
  * to an external resource. */
abstract class Hyperlink extends EnvCollector;
object Hyperlink {
  case object None extends Hyperlink{
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = e0
  }

  /** point to an external resource, specified by the given expression */
  sealed case class Reference(url: Expression) extends Hyperlink {
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = url.collectEnv(e0)
  }

  /** point to a local anchor, whose name (?) is specified by the given expression */
  sealed case class LocalAnchor(anchor: Expression) extends Hyperlink {
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = anchor.collectEnv(e0)
  }

  /** point to a local page, whose 1-based index is specified with the given expression */
  sealed case class LocalPage(index: Expression) extends Hyperlink {
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = index.collectEnv(e0)
  }

  /** point to an anchor within an external document (url?) */
  sealed case class RemoteAnchor(reference: Expression, anchor: Expression) extends Hyperlink {
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = reference.collectEnv(anchor.collectEnv(e0))
  }

  /** point to a page within an external document (url?) */
  sealed case class RemotePage(reference: Expression, index: Expression) extends Hyperlink {
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = reference.collectEnv(index.collectEnv(e0))
  }

  /** @see net.sf.jasperreports.engine.export.JRHyperlinkProducer */
  sealed case class Custom(typeName: String, arguments: Map[String, Expression]) extends Hyperlink {
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) =
      (arguments.values toSeq).collectEnv(e0)
  }

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
        setHyperlinkType(HyperlinkTypeEnum.NONE)
      case Reference(url) =>
        setHyperlinkType(HyperlinkTypeEnum.REFERENCE)
        setHyperlinkReferenceExpression(url)
      case LocalAnchor(anchor) =>
        setHyperlinkType(HyperlinkTypeEnum.LOCAL_ANCHOR)
        setHyperlinkAnchorExpression(anchor)
      case LocalPage(index) =>
        setHyperlinkType(HyperlinkTypeEnum.LOCAL_PAGE)
        setHyperlinkPageExpression(index)
      case RemoteAnchor(reference, anchor) =>
        setHyperlinkType(HyperlinkTypeEnum.REMOTE_ANCHOR)
        setHyperlinkReferenceExpression(reference)
        setHyperlinkAnchorExpression(anchor)
      case RemotePage(reference, index) =>
        setHyperlinkType(HyperlinkTypeEnum.REMOTE_PAGE)
        setHyperlinkReferenceExpression(reference)
        setHyperlinkPageExpression(index)
      case Custom(typeName, args) =>
        setHyperlinkType(HyperlinkTypeEnum.CUSTOM)
        setLinkType(typeName)
        for ((n, e) <- args) {
          val p = new JRDesignHyperlinkParameter();
          p.setName(n);
          p.setValueExpression(e);
          addHyperlinkParameter(p)
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
      case Self => setHyperlinkTarget(HyperlinkTargetEnum.SELF);
      case Blank => setHyperlinkTarget(HyperlinkTargetEnum.BLANK);
      case Parent => setHyperlinkTarget(HyperlinkTargetEnum.PARENT);
      case Top => setHyperlinkTarget(HyperlinkTargetEnum.TOP);
      case Custom(s) =>
        setHyperlinkTarget(HyperlinkTargetEnum.CUSTOM)
        setLinkTarget(s)
    }
  }
}

// type HyperlinkTooltip = Expression

sealed case class Link(hyperlink: Hyperlink,
                       target: HyperlinkTarget,
                       tooltip: Option[Expression]) extends EnvCollector {
  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) =
    hyperlink.collectEnv(tooltip.collectEnv(e0))
}

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
    )
    HyperlinkTarget.put(
      o.target,
      setHyperlinkTarget,
      setLinkTarget
    )
    setHyperlinkTooltipExpression(o.tooltip)
  }

}
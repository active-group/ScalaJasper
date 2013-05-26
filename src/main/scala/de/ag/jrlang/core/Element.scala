package de.ag.jrlang.core

import net.sf.jasperreports.engine.{JRExpression, JRChild, JRAnchor}
import net.sf.jasperreports.engine.design._

import Transformer._

sealed abstract class Element extends Transformable[JRChild] {
  def +(e: Element) = ElementGroup(content = Vector(this, e)) // TODO: Optimize if one is a group already
  // more... side-by-side, move etc....?

  /*
  def transform : Transformer[JRChild] =
    this match {
      case v : Ellipse => v.transform
      case v : StaticText => StaticText.drop(v)
      case v : Image => Image.drop(v)
      case v : TextField => TextField.drop(v)
      case v : Subreport => Subreport.drop(v)
      case v : Break => Break.drop(v)
      case v : Frame => Frame.drop(v)
      case v : Line => Line.drop(v)
      case v : Rectangle => Rectangle.drop(v)
      case v : ComponentElement => ComponentElement.drop(v)
      // GenericElement, Crosstab, Chart
    }
  */
}

object Element {
  // could probably do the 'implicit CanBuildFrom' technique here; but not worth it.
  // def foldStyles(st0: StylesMap) : Element;


  /*
  def foldAllStyles(cs: Seq[Element], st: StylesMap) : (Vector[Element], StylesMap) =
    cs.foldLeft((Vector[Element](), st)) {
      case ((c, st0), v) => {
        val (v_, st1) /*: (Element, StylesMap) */ = v match {
          case v: Ellipse => v.foldStyles(st0)
          case v: StaticText => v.foldStyles(st0)
          case v: Image => v.foldStyles(st0)
          case v: TextField => v.foldStyles(st0)
          case v: Subreport => v.foldStyles(st0)
          case v: Break => v.foldStyles(st0)
          case v: Frame => v.foldStyles(st0)
          case v: Line => v.foldStyles(st0)
          case v: Rectangle => v.foldStyles(st0)
          case v: ComponentElement => v.foldStyles(st0)
          case _: Element => (v, st0) // undefined, no styles
        }
        ((c :+ v_), st1)
      }}
  */
}

abstract class Anchor extends Transformable[(JRDesignExpression, Int)]

object Anchor {
  case object None extends Anchor {
    def transform = ret(null, JRAnchor.NO_BOOKMARK)
  }

  /** just a plain Anchor */
  sealed case class Plain(name: Expression[String]) extends Anchor {
    def transform = name.transform >>= { e => ret(e, JRAnchor.NO_BOOKMARK)}
  }

  /** an anchor, which shows up in a table of contents on the given level (> 0) */
  sealed case class Bookmark(level : Int, name: Expression[String]) extends Anchor {
    def transform = name.transform >>= { e => ret(e, level) }
  }

  private[core] def put(o: Anchor,
                        setAnchorNameExpression: JRExpression => Unit,
                        setBookmarkLevel: Int => Unit) = {
    o.transform >>= { case(e, l ) => {
      setAnchorNameExpression(e)
      setBookmarkLevel(l)
      ret()
    }}
  }
}


abstract sealed class EvaluationTime(val value: net.sf.jasperreports.engine.`type`.EvaluationTimeEnum)

object EvaluationTime {
  case object Auto extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.AUTO)
  case object Band extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.BAND)
  case object Column extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.COLUMN)
  sealed case class Group(group: de.ag.jrlang.core.Group) extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.GROUP)
  case object Now extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.NOW)
  case object Page extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.PAGE)
  case object Report extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.REPORT)

  private[core] def putEvaluationTime(
      o: EvaluationTime,
      setTime: net.sf.jasperreports.engine.`type`.EvaluationTimeEnum => Unit,
      setGroup: net.sf.jasperreports.engine.JRGroup => Unit) = {
    o match {
      case Group(g) => {
        setTime(o.value)
        drop(g.transform) { setGroup(_) } >> // TODO: Test, maybe groups have to registered globally?
        ret()
      }
      case _ => 
        setTime(o.value)
        ret()
    }
  }
}

/*
sealed abstract class PrintWhen;
object PrintWhen {
  sealed case class Expression(expression: Expression) extends PrintWhen;
  
}
*/

sealed case class Size(
    height: Int,
    width: Int,
    stretchType: net.sf.jasperreports.engine.`type`.StretchTypeEnum)

object Size {
  /**
   * The element preserves its original specified height.
   */
  def fixed(height: Int, width: Int) = Size(height, width, net.sf.jasperreports.engine.`type`.StretchTypeEnum.NO_STRETCH)

  /**
   * The element stretches to the tallest element in it's group (@see ElementGroup).
   */
  def relativeToTallest(height: Int, width: Int) = Size(height, width, net.sf.jasperreports.engine.`type`.StretchTypeEnum.RELATIVE_TO_TALLEST_OBJECT)

  /**
   * The element will adapt its height to match the new height of the report section it placed on, which has been
   * affected by stretch.
   */
  def relativeToBand(height: Int, width: Int) = Size(height, width, net.sf.jasperreports.engine.`type`.StretchTypeEnum.RELATIVE_TO_BAND_HEIGHT)

}

sealed case class Pos(
    x: Int,
    y: Int,
    positionType: net.sf.jasperreports.engine.`type`.PositionTypeEnum)

object Pos {
  /**
   * The element will float in its parent section if it is pushed downwards by other elements fount above it.
   * It will try to conserve the distance between it and the neighboring elements placed immediately above.
   */
  def float(x: Int, y: Int) = Pos(x, y, net.sf.jasperreports.engine.`type`.PositionTypeEnum.FLOAT)

  /**
   * The element will simply ignore what happens to the other section elements and tries to
   * conserve the y offset measured from the top of its parent report section.
   */
  def fixedTop(x: Int, y: Int) = Pos(x, y, net.sf.jasperreports.engine.`type`.PositionTypeEnum.FIX_RELATIVE_TO_TOP)

  /**
   * If the height of the parent report section is affected by elements that stretch, the current element will try to
   * conserve the original distance between its bottom margin and the bottom of the band.
   */
  def fixedBottom(x: Int, y: Int) = Pos(x, y, net.sf.jasperreports.engine.`type`.PositionTypeEnum.FIX_RELATIVE_TO_BOTTOM)
}

sealed case class Conditions(
    printInFirstWholeBand: Boolean = false,
    printWhenExpression: Option[Expression[Boolean]] = None,
    printRepeatedValues: Boolean = true, // important!!
    printWhenDetailOverflows: Boolean = false,
    // TODO printWhenGroupChanges? is a JRGroup - probably needs a reference to a group defined elsewhere
    removeLineWhenBlank: Boolean = false)

object Conditions {
  val default = Conditions()
}

private[core] object ElementUtils {
  // sets properties common to all report elements
  def putReportElement(
      key:String,
      style:AbstractStyle,
      pos:Pos,
      size:Size,
      conditions:Conditions,
      // custom properties?
      tgt:JRDesignElement) = {
    tgt.setKey(if (key == "") null else key) // don't know if it's important to be null
    tgt.setHeight(size.height)
    tgt.setWidth(size.width)
    tgt.setStretchType(size.stretchType)
    tgt.setX(pos.x)
    tgt.setY(pos.y)
    tgt.setPositionType(pos.positionType)
    tgt.setPrintRepeatedValues(conditions.printRepeatedValues)
    tgt.setPrintInFirstWholeBand(conditions.printInFirstWholeBand)
    tgt.setPrintWhenDetailOverflows(conditions.printWhenDetailOverflows)
    drop(orNull(conditions.printWhenExpression map { _.transform })) { tgt.setPrintWhenExpression(_) } >>
    // might take colors and mode out of the style - if it's worth it
    // forecolor: Option[java.awt.Color],
    // backcolor: Option[java.awt.Color],
    // mode: Option[net.sf.jasperreports.engine.`type`.ModeEnum],
    // tgt.setForecolor(src.forecolor.getOrElse(null));
    // tgt.setBackcolor(src.backcolor.getOrElse(null));
    // tgt.setMode(src.mode.getOrElse(null));
    drop(style.transform) { so => tgt.setStyleNameReference(so.getOrElse(null)) }
  }
  
  // Various classes need this, though they don't have a common type
  def contentTransformer(content: Seq[Element],
                         addElement: JRDesignElement => Unit,
                         addElementGroup: JRDesignElementGroup => Unit) = {
    // obj will 'own' the created child objects (like in DOM)
    (all(content map { _.transform })) >>= { lst =>
      for (co <- lst) {
        // although elements and groups end up in the same children list,
        // there is no add method for children, but only for the two
        // classes of children, elements and element groups -
        // that API crime should be healed here
        /*c match {
          case g: ElementGroup => addElementGroup(g)
          case e: Element => {
            val eo = Element.drop(e).asInstanceOf[net.sf.jasperreports.engine.design.JRDesignElement];
            addElement(eo);
          }
        }*/
        co match {
          case g : net.sf.jasperreports.engine.design.JRDesignElementGroup => addElementGroup(g)
          case e : net.sf.jasperreports.engine.design.JRDesignElement => addElement(e)
          case _ => throw new RuntimeException("Unexpected type of child: " + co.getClass)
        }
      }
      ret()
    }
  }
}

/* TODO
sealed case class Chart(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions,
    link: Link,
    anchor: Anchor,
    chartType : ChartType, // contains type byte, plot and dataset
    customizerClass : String,
    legend: ChartLegend,
    showLegend: Boolean,
    theme: String,
    title: ChartTitle,
    subtitle: ChartSubtitle,
    renderType: String,
    evaluation: EvaluationTime)
  extends Element with Transformable[JRDesignChart] {

}
*/

sealed case class Break(
    key: String,
    pos : Pos,
    conditions : Conditions,
    breakType : net.sf.jasperreports.engine.`type`.BreakTypeEnum)
  extends Element with Transformable[JRDesignBreak] {

  override def transform : Transformer[JRDesignBreak] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignBreak()
    ElementUtils.putReportElement(key = key, style=Style.empty, pos=pos,
      size=Size.fixed(0, 0), conditions=conditions, r) >>
    ret(r.setType(breakType)) >>
    ret(r)
  }
}
object Break {
  /** Creates a page break */
  def page(pos: Pos, conditions: Conditions = Conditions.default, key:String = "") = new Break(
      key = key,
      pos = pos,
      conditions = conditions,
      breakType = net.sf.jasperreports.engine.`type`.BreakTypeEnum.PAGE)
  
  /** Creates a column break */
  def column(pos: Pos, conditions: Conditions = Conditions.default, key:String = "") = new Break(
      key = key,
      pos = pos,
      conditions = conditions,
      breakType = net.sf.jasperreports.engine.`type`.BreakTypeEnum.COLUMN)
  
}

/** The only reason to group your report elements is to customize their stretch behavior. */
sealed case class ElementGroup( // different from a "group"!
    content: Seq[Element])
  extends Element with Transformable[JRDesignElementGroup] {

  def transform = {
    val r = new JRDesignElementGroup()
    ElementUtils.contentTransformer(content, r.addElement(_), r.addElementGroup(_)) >>
    ret(r)
  }
}

object ElementGroup {
  val empty = new ElementGroup(Vector.empty)
}

sealed case class Frame(
    size : Size,
    pos : Pos,
    content: Seq[Element],
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element with Transformable[JRDesignFrame] {
  def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignFrame()
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    ElementUtils.contentTransformer(content, r.addElement(_), r.addElementGroup(_)) >>
    ret(r)
  }
}

sealed case class Ellipse(
    size: Size,
    pos: Pos,
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element with Transformable[JRDesignEllipse] {

  def transform : Transformer[JRDesignEllipse] = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignEllipse(null)
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    ret(r)
  }
}

sealed case class Image(
    size: Size,
    pos: Pos,
    expression : Expression[Any],
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "",
    /** default depends on type of image expression */
    usingCache: Option[Boolean] = None,
    /** load at fill time or export time (use href in html for example); if True image expression must be of
      * type String */
    lazily: Boolean = false,
    onError: net.sf.jasperreports.engine.`type`.OnErrorTypeEnum = net.sf.jasperreports.engine.`type`.OnErrorTypeEnum.ERROR,
    evaluationTime: EvaluationTime = EvaluationTime.Now,
    link: Link = Link.empty,
    anchor: Anchor = Anchor.None
    ) extends Element with Transformable[JRDesignImage]
{
  def transform : Transformer[JRDesignImage] = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignImage(null)
    r.setUsingCache(if (usingCache.isDefined) (usingCache.get : java.lang.Boolean) else null)
    r.setLazy(lazily)
    r.setOnErrorType(onError)
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    EvaluationTime.putEvaluationTime(evaluationTime, r.setEvaluationTime(_), r.setEvaluationGroup(_)) >>
    Link.put(link,
      r.setHyperlinkType(_),
      r.setHyperlinkReferenceExpression(_),
      r.setHyperlinkWhenExpression(_),
      r.setHyperlinkAnchorExpression(_),
      r.setHyperlinkPageExpression(_),
      r.setLinkType(_),
      r.addHyperlinkParameter(_),
      r.setHyperlinkTarget(_),
      r.setLinkTarget(_),
      r.setHyperlinkTooltipExpression(_)
    ) >>
    Anchor.put(anchor,
      r.setAnchorNameExpression(_),
      r.setBookmarkLevel(_)) >>
    drop(expression.transform)(r.setExpression(_)) >>
    ret(r)
  }
}

sealed case class Line(
    size : Size,
    pos : Pos,
    style: AbstractStyle = Style.inherit,
    conditions : Conditions = Conditions.default,
    key: String = "",
    /** The direction attribute determines which one of the two diagonals of the rectangle
      *  should be drawn:
      *  - direction="TopDown" draws a diagonal line from the top-left corner of the
      *    rectangle to the bottom-right corner.
      *  - direction="BottomUp" draws a diagonal line from the bottom-left corner to
      *    the upper-right corner.
      *  The default direction for a line is top-down. */
    direction: net.sf.jasperreports.engine.`type`.LineDirectionEnum = net.sf.jasperreports.engine.`type`.LineDirectionEnum.TOP_DOWN)
  extends Element with Transformable[JRDesignLine] {

  def transform : Transformer[JRDesignLine] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignLine()
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    ret(r.setDirection(direction)) >>
    ret(r)
  }
}

sealed case class Rectangle(
    size: Size,
    pos: Pos,
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element with Transformable[JRDesignRectangle] {

  def transform : Transformer[JRDesignRectangle] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignRectangle()
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    ret(r)
  }
}

sealed case class StaticText(
    size : Size,
    pos : Pos,
    text: String,
    key: String = "",
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default)
  extends Element with Transformable[JRDesignStaticText] {

  def transform : Transformer[JRDesignStaticText] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignStaticText()
    r.setText(text)
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    ret(r)
  }
}

sealed case class TextField(
    size: Size,
    pos: Pos,
    expression: Expression[Any],
    key: String = "",
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default,
    link: Link = Link.empty,
    anchor: Anchor = Anchor.None,
    /** Ensure that if the specified height for the text field is not sufficient,
      *  it will automatically be increased (never decreased) in order to be able
      *  to display the entire text content. */
    stretchWithOverflow: Boolean = false,
    evaluationTime: EvaluationTime = EvaluationTime.Now,
    // Not sure: optionally overrides the static pattern in style.pattern
    patternExpression: Option[Expression[Any]] = None) // or Expression[String] ?
  extends Element with Transformable[JRDesignTextField] {

  def transform : Transformer[JRDesignTextField] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignTextField()
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    Link.put(link,
      r.setHyperlinkType(_),
      r.setHyperlinkReferenceExpression(_),
      r.setHyperlinkWhenExpression(_),
      r.setHyperlinkAnchorExpression(_),
      r.setHyperlinkPageExpression(_),
      r.setLinkType(_),
      r.addHyperlinkParameter(_),
      r.setHyperlinkTarget(_),
      r.setLinkTarget(_),
      r.setHyperlinkTooltipExpression(_)
    ) >>
    Anchor.put(anchor,
      r.setAnchorNameExpression(_),
      r.setBookmarkLevel(_)) >>
    ret(r.setStretchWithOverflow(stretchWithOverflow)) >>
    EvaluationTime.putEvaluationTime(evaluationTime, r.setEvaluationTime(_), r.setEvaluationGroup(_)) >>
    drop(expression.transform) { r.setExpression(_) } >>
    drop(orNull(patternExpression map {_.transform})) { r.setPatternExpression(_) } >>
    ret(r)
  }
}

sealed case class Subreport(
   size: Size,
   pos: Pos,
   /** The location (filename etc.) */
   subreportExpression: Expression[Any],
   style: AbstractStyle = Style.inherit,
   conditions: Conditions = Conditions.default,
   key: String = "",
   /** adds to the map created by argumentsMapExpression; overrides individual parameters */
   arguments: Map[String, Expression[Any]] = Map.empty,
   argumentsMapExpression: Option[Expression[Any]] = None,
   /** default depends on subreportExpression type */
   usingCache: Option[Boolean] = None
   // TODO custom properties?
   // TODO returnValue
   // TODO connection, datasource -- simialy/same as Data (-Run, -Source)?
   ) extends Element with Transformable[JRDesignSubreport]
{
  private def transArg(v: (String, Expression[Any])) : Transformer[JRDesignSubreportParameter] = {
    val (n, e) = v
    val po = new net.sf.jasperreports.engine.design.JRDesignSubreportParameter()
    po.setName(n)
    drop(e.transform) { po.setExpression(_) }
    ret(po)
  }

  def transform : Transformer[JRDesignSubreport] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignSubreport(null)
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    drop(subreportExpression.transform) { r.setExpression(_) } >>
    ret(r.setUsingCache(if (usingCache.isDefined) (usingCache.get : java.lang.Boolean) else null)) >>
    drop(orNull(argumentsMapExpression map {_.transform})) { r.setParametersMapExpression(_) }
    (all(arguments map transArg toSeq) >>= { ps =>
      ps foreach { r.addParameter(_) }
      ret()
    }) >>
    ret(r)
  }
}

object Subreport {
  /** Quite common expression that passes all parameters of the main report to the subreport.
   *  The map does not need to be copied anymore (since JasperReports 3.0.1) 
   */
  val inheritParametersExpression = Expression.R("REPORT_PARAMETERS_MAP")
}

sealed case class ComponentElement(size : Size,
                                   pos : Pos,
                                   component: components.Component,
                                   style: AbstractStyle = Style.inherit,
                                   conditions: Conditions = Conditions.default,
                                   key: String = "")
  extends Element with Transformable[JRDesignComponentElement] {

  def transform : Transformer[JRDesignComponentElement] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignComponentElement()
    ElementUtils.putReportElement(key, style, pos, size, conditions, r) >>
    (component.transform >>= { case (transcomp, transkey) => {
      r.setComponent(transcomp)
      r.setComponentKey(transkey)
      ret()
    }}) >>
    ret(r)
  }
}

/* TODO
sealed case class Crosstab(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    // etc ... dataset: JRCrosstabDataset,
    ) extends Element with Transformable[] {
  def transform = null
}

sealed case class GenericElement(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element{

}
*/

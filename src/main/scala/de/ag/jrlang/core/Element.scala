package de.ag.jrlang.core

import net.sf.jasperreports.engine.JRAnchor

sealed abstract class Element extends EnvCollector

object Element {
  // could probably do the 'implicit CanBuildFrom' technique here; but not worth it.
  // def foldStyles(st0: StylesMap) : Element;

  implicit def drop(c : Element) : net.sf.jasperreports.engine.JRChild =
    c match {
      case v : Ellipse => Ellipse.drop(v)
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
  
}

abstract class Anchor extends EnvCollector

object Anchor {
  case object None extends Anchor{
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = e0
  }

  /** just a plain Anchor */
  sealed case class Plain(name: Expression) extends Anchor{
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = name.collectEnv(e0)
  }

  /** an anchor, which shows up in a table of contents on the given level (> 0) */
  sealed case class Bookmark(level : Int, name: Expression) extends Anchor{
    private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]) = name.collectEnv(e0)
  }

  private[core] def put(o: Anchor,
                        setAnchorNameExpression: Expression => Unit,
                        setBookmarkLevel: Int => Unit) {
    o match {
      case Plain(name) =>
        setAnchorNameExpression(name)
        setBookmarkLevel(JRAnchor.NO_BOOKMARK) // = 0
      case Bookmark(level, name) =>
        setAnchorNameExpression(name)
        setBookmarkLevel(level)
      case None =>
        setAnchorNameExpression(null)
        setBookmarkLevel(JRAnchor.NO_BOOKMARK)
    }
  }
}


abstract sealed class EvaluationTime(val value: net.sf.jasperreports.engine.`type`.EvaluationTimeEnum)

object EvaluationTime {
  case object Auto extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.AUTO)
  case object Band extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.BAND)
  case object Column extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.COLUMN)
  sealed case class Group(group: JRDesignGroup) extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.GROUP)
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
        // TODO setGroup(g);
      }
      case _ => 
        setTime(o.value)
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
    printWhenExpression: Option[Expression] = None,
    printRepeatedValues: Boolean = true, // important!!
    printWhenDetailOverflows: Boolean = false,
    // TODO printWhenGroupChanges? is a JRGroup - probably needs a reference to a group defined elsewhere
    removeLineWhenBlank: Boolean = false)
  extends EnvCollector{

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    printWhenExpression.collectEnv(e0)
}

object Conditions {
  val default = Conditions()
}

private[core] object ElementUtils {
  // sets properties common to all report elements
  def putReportElement(
      key:String,
      style:Style,
      pos:Pos,
      size:Size,
      conditions:Conditions,
      // custom properties?
      tgt:net.sf.jasperreports.engine.design.JRDesignElement) {
    tgt.setKey(if (key == "") null else key) // don't know if it's important to be null
    tgt.setHeight(size.height)
    tgt.setWidth(size.width)
    tgt.setStretchType(size.stretchType)
    tgt.setX(pos.x)
    tgt.setY(pos.y)
    tgt.setPositionType(pos.positionType)
    tgt.setPrintWhenExpression(conditions.printWhenExpression) // TODO: put these
    tgt.setPrintRepeatedValues(conditions.printRepeatedValues) // two in one; as only one can be used (expression has precedence)
    tgt.setPrintInFirstWholeBand(conditions.printInFirstWholeBand)
    tgt.setPrintWhenDetailOverflows(conditions.printWhenDetailOverflows)

    // might take colors and mode out of the style - if it's worth it
    // forecolor: Option[java.awt.Color],
    // backcolor: Option[java.awt.Color],
    // mode: Option[net.sf.jasperreports.engine.`type`.ModeEnum],
    // tgt.setForecolor(src.forecolor.getOrElse(null));
    // tgt.setBackcolor(src.backcolor.getOrElse(null));
    // tgt.setMode(src.mode.getOrElse(null));
    style match {
      // after global style folding, it should always be External or empty
      case s : Style.Internal => {
        var so : net.sf.jasperreports.engine.design.JRDesignStyle = null
        if (!s.isEmpty) so = s
        tgt.setStyle(so)
        tgt.setStyleNameReference(null)
      }
      case s : Style.External => {
        tgt.setStyle(null)
        tgt.setStyleNameReference(s.reference)
      }
    }
  }
  
  // Various classes need this, though they don't have a common type
  def addChildren(
      children: Seq[Element],
      addElement: net.sf.jasperreports.engine.design.JRDesignElement => Unit,
      addElementGroup: net.sf.jasperreports.engine.design.JRDesignElementGroup => Unit) {
    // obj will 'own' the created child objects (like in DOM)
    for (c <- children) {
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
      val co : net.sf.jasperreports.engine.JRChild = Element.drop(c)
      co match {
        case g : net.sf.jasperreports.engine.design.JRDesignElementGroup => addElementGroup(g)
        case e : net.sf.jasperreports.engine.design.JRDesignElement => addElement(e)
        case _ => throw new RuntimeException("Unexpected type of child: " + co.getClass)
      }
    }
  }
}
    
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
  extends Element {

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    List(conditions, anchor, link, anchor).collectEnv(e0)
}

sealed case class Break(
    key: String,
    pos : Pos,
    conditions : Conditions,
    breakType : net.sf.jasperreports.engine.`type`.BreakTypeEnum)
  extends Element with StyleFoldable[Break] {

  private[core] override def foldStyles(st0 : StylesMap) =
    (this, st0) // no styles

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    conditions.collectEnv(e0)
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
  
  implicit def drop(o: Break): net.sf.jasperreports.engine.design.JRDesignBreak = {
    val r = new net.sf.jasperreports.engine.design.JRDesignBreak()
    ElementUtils.putReportElement(key = o.key, style=Style.Internal.empty, pos=o.pos, size=Size.fixed(0, 0), conditions=o.conditions, r)
    r.setType(o.breakType)
    r
  }
}

/** The only reason to group your report elements is to customize their stretch behavior. */
sealed case class ElementGroup( // different from a "group"!
    content: Seq[Element])
  extends Element with StyleFoldable[ElementGroup] {

  private[core] override def foldStyles(st0 : StylesMap) = {
    val (children_, st1) = Element.foldAllStyles(content, st0)
    (copy(content = children_),
        st1)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    content.collectEnv(e0)
}

object ElementGroup {
  val empty = new ElementGroup(Vector.empty)
  
  implicit def dropElementGroup(o:ElementGroup) : net.sf.jasperreports.engine.design.JRDesignElementGroup = {
    val r = new net.sf.jasperreports.engine.design.JRDesignElementGroup()
    ElementUtils.addChildren(o.content, r.addElement(_), r.addElementGroup(_))
    r
  }
}

sealed case class Frame(
    size : Size,
    pos : Pos,
    content: Seq[Element],
    style: Style = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element with StyleFoldable[Frame] {

  private[core] override def foldStyles(st0 : StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    val (children_, st2) = Element.foldAllStyles(content, st1)
    (copy(style = style_, content = children_),
        st2)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(content.collectEnv(e0)))

}

object Frame {

  implicit def drop(o:Frame) : net.sf.jasperreports.engine.design.JRDesignFrame = {
    val r = new net.sf.jasperreports.engine.design.JRDesignFrame()
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    ElementUtils.addChildren(o.content, r.addElement(_), r.addElementGroup(_))
    r
  }
}

sealed case class Ellipse(
    size: Size,
    pos: Pos,
    style: Style = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element with StyleFoldable[Ellipse] {

  private[core] def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    (copy(style = style_),
        st1)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    conditions.collectEnv(
      style.collectEnv(e0)
    )
}

object Ellipse {

  implicit def drop(o: Ellipse) : net.sf.jasperreports.engine.design.JRDesignEllipse = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignEllipse(null)
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    r
  }
}

sealed case class Image(
    size: Size,
    pos: Pos,
    expression : Expression,
    style: Style = Style.inherit,
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
    ) extends Element with StyleFoldable[Image]
{
  def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    (copy(style = style_),
        st1)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(link.collectEnv(anchor.collectEnv(expression.collectEnv(e0)))))
}

object Image {

  implicit def drop(o: Image) : net.sf.jasperreports.engine.design.JRDesignImage = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignImage(null)
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    r.setUsingCache(if (o.usingCache.isDefined) (o.usingCache.get : java.lang.Boolean) else null)
    r.setLazy(o.lazily)
    r.setOnErrorType(o.onError)
    EvaluationTime.putEvaluationTime(o.evaluationTime, r.setEvaluationTime(_), r.setEvaluationGroup(_))
    Link.put(o.link,
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
    )
    Anchor.put(o.anchor,
      r.setAnchorNameExpression(_),
      r.setBookmarkLevel(_))
    r.setExpression(o.expression)
    r
  }
}

sealed case class Line(
    size : Size,
    pos : Pos,
    style: Style = Style.inherit,
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
  extends Element with StyleFoldable[Line] with EnvCollector {

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(e0))

  private[core] def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    (copy(style = style_),
      st1)
  }
}

object Line {

  implicit def drop(o: Line) : net.sf.jasperreports.engine.design.JRDesignLine = {
    val r = new net.sf.jasperreports.engine.design.JRDesignLine()
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    r.setDirection(o.direction)
    r
  }
}

sealed case class Rectangle(
    size: Size,
    pos: Pos,
    style: Style = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element with StyleFoldable[Rectangle] with EnvCollector {

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(e0))

  private[core] def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    (copy(style = style_),
      st1)
  }
}

object Rectangle {
  implicit def drop(o: Rectangle) : net.sf.jasperreports.engine.design.JRDesignRectangle = {
    val r = new net.sf.jasperreports.engine.design.JRDesignRectangle()
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    r
  }
}

sealed case class StaticText(
    size : Size,
    pos : Pos,
    text: String,
    key: String = "",
    style: Style = Style.inherit,
    conditions: Conditions = Conditions.default)
  extends Element with StyleFoldable[StaticText] {

  private[core] def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    (copy(style = style_),
        st1)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(e0))
}

object StaticText {

  implicit def drop(o: StaticText) : net.sf.jasperreports.engine.design.JRDesignStaticText = {
    val r = new net.sf.jasperreports.engine.design.JRDesignStaticText()
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    r.setText(o.text)
    r
  }
}

sealed case class TextField(
    size: Size,
    pos: Pos,
    expression: Expression,
    key: String = "",
    style: Style = Style.inherit,
    conditions: Conditions = Conditions.default,
    link: Link = Link.empty,
    anchor: Anchor = Anchor.None,
    /** Ensure that if the specified height for the text field is not sufficient,
      *  it will automatically be increased (never decreased) in order to be able
      *  to display the entire text content. */
    stretchWithOverflow: Boolean = false,
    evaluationTime: EvaluationTime = EvaluationTime.Now,
    // Not sure: optionally overrides the static pattern in style.pattern
    patternExpression: Option[Expression] = None)
  extends Element with StyleFoldable[TextField] {

  private[core] def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    (copy(style = style_),
        st1)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(link.collectEnv(anchor.collectEnv(expression.collectEnv(e0)))))

}

object TextField {

  implicit def drop(o: TextField) : net.sf.jasperreports.engine.design.JRDesignTextField = {
    val r = new net.sf.jasperreports.engine.design.JRDesignTextField()
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    Link.put(o.link,
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
    )
    Anchor.put(o.anchor,
      r.setAnchorNameExpression(_),
      r.setBookmarkLevel(_))
    r.setStretchWithOverflow(o.stretchWithOverflow)
    EvaluationTime.putEvaluationTime(o.evaluationTime, r.setEvaluationTime(_), r.setEvaluationGroup(_))
    r.setExpression(o.expression)
    r.setPatternExpression(o.patternExpression.getOrElse(null))
    r
  }
}

sealed case class Subreport(
   size: Size,
   pos: Pos,
   /** The location (filename etc.) */
   subreportExpression: Expression,
   style: Style = Style.inherit,
   conditions: Conditions = Conditions.default,
   key: String = "",
   /** adds to the map created by argumentsMapExpression; overrides individual parameters */
   arguments: Map[String, Expression] = Map.empty,
   argumentsMapExpression: Option[Expression] = None,
   /** default depends on subreportExpression type */
   usingCache: Option[Boolean] = None
   // TODO custom properties?
   // TODO returnValue
   // TODO connection, datasource -- simialy/same as Data (-Run, -Source)?
   ) extends Element with StyleFoldable[Subreport]
{
  def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    (copy(style = style_),
        st1)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(argumentsMapExpression.collectEnv(
      subreportExpression.collectEnv((arguments.values toSeq).collectEnv(e0)))))

}

object Subreport {
  /** Quite common expression that passes all parameters of the main report to the subreport.
   *  The map does not need to be copied anymore (since JasperReports 3.0.1) 
   */
  val inheritParametersExpression = Expression.R("REPORT_PARAMETERS_MAP")

  def drop(o: Subreport) : net.sf.jasperreports.engine.design.JRDesignSubreport = {
    val r = new net.sf.jasperreports.engine.design.JRDesignSubreport(null)
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    r.setExpression(o.subreportExpression)
    r.setUsingCache(if (o.usingCache.isDefined) (o.usingCache.get : java.lang.Boolean) else null)
    r.setParametersMapExpression(o.argumentsMapExpression)
    for ((n,e) <- o.arguments) {
      val po = new net.sf.jasperreports.engine.design.JRDesignSubreportParameter()
      po.setName(n)
      po.setExpression(e)
    }
    r
  }
}

sealed case class ComponentElement(size : Size,
                                   pos : Pos,
                                   component: net.sf.jasperreports.engine.component.Component,
                                   // automatically set for internal components
                                   componentKey: net.sf.jasperreports.engine.component.ComponentKey = null,
                                   style: Style = Style.inherit,
                                   conditions: Conditions = Conditions.default,
                                   key: String = "")
  extends Element with StyleFoldable[ComponentElement] with EnvCollector {

  private[core] def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0)
    // TODO: type test on Component, then more in there
    (copy(style = style_),
      st1)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(e0))
}

object ComponentElement {

  implicit def drop(o: ComponentElement) : net.sf.jasperreports.engine.design.JRDesignComponentElement = {
    val r = new net.sf.jasperreports.engine.design.JRDesignComponentElement()
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r)
    val (transcomp, transkey) = o.component match {
      case no : components.Component => no.drop()
      case _ =>
        assert(o.componentKey != null) // exception? user's fault...
        (o.component, o.componentKey)
    }
    r.setComponent(transcomp)
    r.setComponentKey(transkey)
    r
  }
}

sealed case class Crosstab(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    // etc ... dataset: JRCrosstabDataset,
    ) extends Element{
  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(e0))

}

sealed case class GenericElement(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element{
  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    style.collectEnv(conditions.collectEnv(e0))

}


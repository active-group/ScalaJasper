package de.ag.jrlang.core

import net.sf.jasperreports.engine.{JRDataSource, JRExpression, JRChild, JRAnchor}
import net.sf.jasperreports.engine.design._

import Transformer._
import net.sf.jasperreports.engine.`type`.{PositionTypeEnum, StretchTypeEnum, CalculationEnum}
import Dimensions._

sealed abstract class Element {
  /** The vertical extent of this element. Usually this is the Y position plus the height. */
  def verticalExtent: Length

  /* The horizontal extent of this element. Usually this is the X position plus the width. */
  // not possible? I think we can't know the max horizontalExtent of groups until transformation time
  // - which is 'more' 20% or 40px ? Add collections to restricted lengths???
  // def horizontalExtent: RestrictedLength

  /** Returns a sequence of all primitive elements that make up this element. The resulting sequence is guaranteed
    * not to contain any [[de.ag.jrlang.core.ElementSeq]] object. */
  // TODO: Unit test this guarantee?
  def seq = Seq(this)

  /** Combines this element with another element to form a new element consisting of both of them. */
  def +(e: Element) = // also overridden in ElementSeq
    e match {
      case ElementSeq(tl) => ElementSeq(this +: tl)
      case _ => ElementSeq(Vector(this, e))
    }

  private[core] def transform : Transformer[JRChild]

  /** Adds the specified length (which can be negative) to the vertical position of this element */
  def moveVertically(len: Length) : Element

  /* Adds the specified length (which can be negative) to the horizontal position of this element */
  //def moveHorizontally(len: RestrictedLength) : Element

  /** "Move" this element below that element. Note that the Y position of this element is sort of preserved as
    * spacing between the elements, e.g. if (and only if) this has a Y position of 0 (the default) the
    * returned element abuts the other one.
    * @see See function `stack` in the companion object for a function that vertically distributes multiple elements.
    */
  def below(that: Element) = moveVertically(that.verticalExtent)

  // def rightOf(that: Element) = moveHorizontally(that.horizontalExtent)
}

object Element {
  /** An element representing 'nothing', which forms the neutral element for element addition. */
  val zero : Element = ElementSeq(Seq.empty)

  /** Stack all elements below each other, starting with the first element (which will remain where is it).
    * @see See also method `below` for the exact definition of "below".
    */
  def stack(elements: Seq[Element]) : Seq[Element] =
    elements.foldLeft(Vector[Element]()) { (s, e) => s :+ (if (s.isEmpty) e else e.below(s.last)) }

  // def juxtapose
}

/** Element sequences are totally transparent, e.g. an Element e behaves exactly the same if it is nested
  * in an ElementSeq or not. Various utilities in this library freely pack and unpack elements from these sequences. */
// This is a new 'virtual' Element type, because an ElementGroup has a small semantic meaning, to those elements
// contained in it with height=RelativeToTallest; so for a fully indifferent container type, we need this:
sealed case class ElementSeq(elements: Seq[Element]) extends Element {
  override def verticalExtent = ElementUtils.maxHeight(elements)

  // override def horizontalExtent = ElementUtils.maxWidth(elements)

  override private[core] def transform = null // treated specially

  override def +(e: Element) =
    e match {
      case ElementSeq(tl) => ElementSeq(elements ++ tl)
      case _ => ElementSeq(elements :+ e)
    }

  override def seq = elements

  override def moveVertically(len: Length) = elements map { _.moveVertically(len) }
}

abstract class Anchor {
  private[core] def transform : Transformer[(JRExpression, Int)]
}

object Anchor {
  case object None extends Anchor {
    private[core] def transform = ret(null, JRAnchor.NO_BOOKMARK)
  }

  /** just a plain Anchor */
  sealed case class Plain(name: Expression[String]) extends Anchor {
    private[core] def transform = name.transform >>= { e => ret(e, JRAnchor.NO_BOOKMARK)}
  }

  /** an anchor, which shows up in a table of contents on the given level (> 0) */
  sealed case class Bookmark(level : Int, name: Expression[String]) extends Anchor {
    private[core] def transform = name.transform >>= { e => ret(e, level) }
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
        drop(g.transform) { setGroup(_) } >>
        ret()
      }
      case _ => 
        setTime(o.value)
        ret()
    }
  }
}

/** Height of an element */
sealed case class Height(
    value: VerticalLength,
    stretchType: StretchTypeEnum)

object Height {
  /**
   * The element preserves its original specified height.
   */
  def fixed(height: VerticalLength) = Height(value=height, StretchTypeEnum.NO_STRETCH)

  /**
   * The element stretches to the tallest element in it's group (@see ElementGroup).
   */
  def relativeToTallest(height: VerticalLength) = Height(value=height, StretchTypeEnum.RELATIVE_TO_TALLEST_OBJECT)

  /**
   * The element will adapt its height to match the new height of the report section it placed on, which has been
   * affected by stretch.
   */
  def relativeToBand(height: VerticalLength) = Height(value=height, StretchTypeEnum.RELATIVE_TO_BAND_HEIGHT)

}

abstract sealed class Width {
  private[core] def within(x: Length, total: Length) : Length
}

object Width {
  sealed case class Specific(value: RestrictedLength) extends Width {
    private[core] override def within(x: Length, total: Length) = value.asPartOf(total)
  }

  case object Remaining extends Width {
    private[core] override def within(x: Length, total: Length) = total - x
  }

}

/** Vertical position of an element */
sealed case class YPos(
    value: VerticalLength,
    positionType: PositionTypeEnum)

object YPos {
  /**
   * The element will float in its parent section if it is pushed downwards by other elements fount above it.
   * It will try to conserve the distance between it and the neighboring elements placed immediately above.
   */
  def float(y: VerticalLength) = YPos(y, PositionTypeEnum.FLOAT)

  /**
   * The element will simply ignore what happens to the other section elements and tries to
   * conserve the y offset measured from the top of its parent report section.
   */
  def fixedTop(y: VerticalLength) = YPos(y, PositionTypeEnum.FIX_RELATIVE_TO_TOP)

  /**
   * If the height of the parent report section is affected by elements that stretch, the current element will try to
   * conserve the original distance between its bottom margin and the bottom of the band.
   */
  def fixedBottom(y: VerticalLength) = YPos(y, PositionTypeEnum.FIX_RELATIVE_TO_BOTTOM)
}

sealed case class Conditions(
    printInFirstWholeBand: Boolean = false,
    printWhenExpression: Option[Expression[Boolean]] = None,
    printRepeatedValues: Boolean = true, // important!!
    printWhenDetailOverflows: Boolean = false,
    printWhenGroupChanges: Option[Group] = None,
    removeLineWhenBlank: Boolean = false)

object Conditions {
  val default = Conditions()
}

private[core] object ElementUtils {
  // sets properties common to all report elements
  def putReportElement(
      key:String,
      style:AbstractStyle,
      x: RestrictedLength,
      y: YPos,
      width: Width,
      height: Height,
      conditions:Conditions,
      // custom properties?
      // uuid?
      tgt:JRDesignElement) = {
    tgt.setKey(if (key == "") null else key) // don't know if it's important to be null
    tgt.setHeight(height.value relativeTo(style) inAbsolutePixels)
    tgt.setStretchType(height.stretchType)
    tgt.setY(y.value relativeTo(style) inAbsolutePixels)
    tgt.setPositionType(y.positionType)
    tgt.setPrintRepeatedValues(conditions.printRepeatedValues)
    tgt.setPrintInFirstWholeBand(conditions.printInFirstWholeBand)
    tgt.setPrintWhenDetailOverflows(conditions.printWhenDetailOverflows)

    drop(nextUUID) { tgt.setUUID(_) } >>
    drop(orNull(conditions.printWhenGroupChanges map {_.transform})) { tgt.setPrintWhenGroupChanges(_) } >>
    (currentContainerWidth >>= { parentWidth => {
      val absX = x asPartOf parentWidth
      tgt.setWidth(width within (absX, parentWidth) inAbsolutePixels)
      tgt.setX(absX inAbsolutePixels)
      ret()
    }}) >>
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

  def maxHeight(elements: Seq[Element]): Length =
    (elements map {_.verticalExtent}).foldLeft(0 px) { (l1:Length, l2:Length) =>
      math.max(l1.inAbsolutePixels, l2.inAbsolutePixels).px }

  // Various classes need this, though they don't have a common type
  def contentTransformer(content: Seq[Element],
                         addElement: JRDesignElement => Unit,
                         addElementGroup: JRDesignElementGroup => Unit) = {
    def transformAll(e: Element) : Seq[Transformer[JRChild]] =
      e match {
        // we need to 'flat out' ElementSeq, that are not real Elements, but just a 'virtual' utility class
        case ElementSeq(es) => es flatMap transformAll // TODO: Make tail recursive
        case _ => Seq(e.transform)
      }
    // obj will 'own' the created child objects (like in DOM)
    (all(content flatMap transformAll)) >>= { lst =>
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

sealed case class Break(
    key: String,
    y: YPos,
    conditions : Conditions,
    breakType : net.sf.jasperreports.engine.`type`.BreakTypeEnum)
  extends Element
{

  override private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignBreak()
    ElementUtils.putReportElement(key = key, style=Style.empty, x=(0 px),
      y = y, width = Width.Specific(0 px), height = Height.fixed(0 px), conditions=conditions, r) >>
    ret(r.setType(breakType)) >>
    ret(r)
  }

  override def verticalExtent = y.value relativeTo(Font(fontSize=Some(0))) // breaks have no height and no style!! :-/

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(Font(fontSize=Some(0))) + len))
}

object Break {
  /** Creates a page break */
  def page(y: YPos, conditions: Conditions = Conditions.default, key:String = "") = new Break(
      key = key,
      y = y,
      conditions = conditions,
      breakType = net.sf.jasperreports.engine.`type`.BreakTypeEnum.PAGE)
  
  /** Creates a column break */
  def column(y: YPos, conditions: Conditions = Conditions.default, key:String = "") = new Break(
      key = key,
      y = y,
      conditions = conditions,
      breakType = net.sf.jasperreports.engine.`type`.BreakTypeEnum.COLUMN)
  
}

/** The only reason to group your report elements is to customize their stretch behavior. */
sealed case class ElementGroup( // different from a "group"!
    content: Seq[Element])
  extends Element {

  override private[core] def transform = {
    val r = new JRDesignElementGroup()
    ElementUtils.contentTransformer(content, r.addElement(_), r.addElementGroup(_)) >>
    ret(r)
  }

  override def verticalExtent = ElementUtils.maxHeight(content)

  override def moveVertically(len: Length) = copy(content = content map { _.moveVertically(len) })
}

object ElementGroup {
  val empty = new ElementGroup(Vector.empty)
}

sealed case class Frame(
    height: Height,
    content: Element,
    x: RestrictedLength = (0 px),
    y: YPos = YPos.float(0 px),
    width: Width = Width.Remaining,
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element {

  override private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignFrame()
    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
    ElementUtils.contentTransformer(content.seq, r.addElement(_), r.addElementGroup(_)) >>
    ret(r)
  }

  override def verticalExtent =
    y.value.relativeTo(style) + height.value.relativeTo(style) // correct? only frame height, and content height is irrelevant?

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
}

sealed case class Ellipse(
    width: Width,
    height: Height,
    x: RestrictedLength = (0 px),
    y: YPos = YPos.float(0 px),
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element {

  override private[core] def transform = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignEllipse(null)
    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
    ret(r)
  }

  override def verticalExtent = y.value.relativeTo(style) + height.value.relativeTo(style)

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
}

sealed case class Image(
    expression : Expression[Any],
    width: Width,
    height: Height,
    x: RestrictedLength = (0 px),
    y: YPos = YPos.float(0 px),
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
    ) extends Element {

  override private[core] def transform = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignImage(null)
    r.setUsingCache(if (usingCache.isDefined) (usingCache.get : java.lang.Boolean) else null)
    r.setLazy(lazily)
    r.setOnErrorType(onError)
    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
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

  override def verticalExtent = y.value.relativeTo(style) + height.value.relativeTo(style)

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
}

sealed case class Line(
    width: Width,
    height: Height,
    x: RestrictedLength = 0.px,
    y: YPos = YPos.float(0 px),
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
  extends Element {

  override private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignLine()
    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
    ret(r.setDirection(direction)) >>
    ret(r)
  }

  override def verticalExtent = y.value.relativeTo(style) + height.value.relativeTo(style)

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
}

sealed case class Rectangle(
    width: Width,
    height: Height,
    x: RestrictedLength = (0 px),
    y: YPos = YPos.float(0 px),
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default,
    key: String = "")
  extends Element {

  override private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignRectangle()
    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
    ret(r)
  }

  override def verticalExtent = y.value.relativeTo(style) + height.value.relativeTo(style)

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
}

sealed case class StaticText(
    text: String,
    height: Height = Height.fixed(1.0.em),
    width: Width = Width.Remaining,
    x: RestrictedLength = 0.px,
    y: YPos = YPos.float(0 px),
    key: String = "",
    style: AbstractStyle = Style.inherit,
    conditions: Conditions = Conditions.default)
  extends Element {

  override private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignStaticText()
    r.setText(text)
    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
    ret(r)
  }

  override def verticalExtent = y.value.relativeTo(style) + height.value.relativeTo(style)

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
}

sealed case class TextField(
    expression: Expression[Any],
    height: Height = Height.fixed(1.0.em),
    width: Width = Width.Remaining,
    x: RestrictedLength = (0 px),
    y: YPos = YPos.float(0 px),
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
  extends Element {

  override private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignTextField()
    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
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

  override def verticalExtent = y.value.relativeTo(style) + height.value.relativeTo(style)

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
}

sealed case class ReturnValue(subreportVariable: String,
                              /** A variable of the master report used when returning values from subreports should
                                * be declared with System calculation */
                              toVariable: String,
                              calculation: CalculationEnum = CalculationEnum.NOTHING,
                              incrementerFactoryClassName: Option[String])
{
  private[core] def transform = {
    val r = new JRDesignSubreportReturnValue()
    r.setSubreportVariable(subreportVariable)
    r.setToVariable(toVariable)
    r.setCalculation(calculation)
    r.setIncrementerFactoryClassName(incrementerFactoryClassName.getOrElse(null))
    ret(r)
  }
}

sealed case class Subreport(
   /** The location (filename etc.); can be of one of the types: java.lang.String
   java.io.File
   java.net.URL
   java.io.InputStream
   net.sf.jasperreports.engine.JasperReport */
   subreportExpression: Expression[Any],
   height: Height,
   width: Width = Width.Remaining,
   x: RestrictedLength = (0 px),
   y: YPos = YPos.float(0 px),
   style: AbstractStyle = Style.inherit,
   conditions: Conditions = Conditions.default,
   key: String = "",
   /** adds to the map created by argumentsMapExpression; overrides individual parameters */
   arguments: Map[String, Expression[Any]] = Map.empty,
   argumentsMapExpression: Option[Expression[java.util.Map[String, AnyRef]]] = None,
   /** default depends on subreportExpression type */
   usingCache: Option[Boolean] = None,
   // TODO?? propertyExpressions? customProperties: Map[String, String] = Map.empty,
   dataSourceExpression: Option[Expression[JRDataSource]] = None,
   connectionExpression: Option[Expression[java.sql.Connection]] = None,
   returnValues : Seq[ReturnValue] = Vector.empty
   ) extends Element
{
  // we could provide a different constructor, which takes a Report, calls prepare(), a take the JasperReport and
  // the map of arguments, to fill the corresponding attributes

  private def transArg(v: (String, Expression[Any])) : Transformer[JRDesignSubreportParameter] = {
    val (n, e) = v
    val po = new net.sf.jasperreports.engine.design.JRDesignSubreportParameter()
    po.setName(n)
    drop(e.transform) { po.setExpression(_) }
    ret(po)
  }

  override private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignSubreport(null)
    r.setUsingCache(if (usingCache.isDefined) (usingCache.get : java.lang.Boolean) else null)

    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
    drop(subreportExpression.transform) { r.setExpression(_) } >>
    drop(orNull(argumentsMapExpression map {_.transform})) { r.setParametersMapExpression(_) }
    (all(arguments map transArg toSeq) >>= { ps =>
      ps foreach { r.addParameter(_) }
      ret()
    }) >>
    drop(orNull(dataSourceExpression map {_.transform})){r.setDataSourceExpression(_)} >>
    drop(orNull(connectionExpression map {_.transform})){r.setConnectionExpression(_)} >>
    (all(returnValues map {_.transform}) >>= { l => l.foreach { r.addReturnValue(_) }; ret() }) >>
    ret(r)
  }

  override def verticalExtent = y.value.relativeTo(style) + height.value.relativeTo(style)

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
}

object Subreport {
  /** Quite common expression that passes all parameters of the main report to the subreport.
   *  The map does not need to be copied anymore (since JasperReports 3.0.1) 
   */
  val inheritParametersExpression = Expression.R("REPORT_PARAMETERS_MAP")
}

sealed case class ComponentElement(
     component: components.Component,
     height: Height, // derive like BandHeight?
     width: Width = Width.Remaining,
     x: RestrictedLength = (0 px),
     y: YPos = YPos.float(0 px),
     style: AbstractStyle = Style.inherit,
     conditions: Conditions = Conditions.default,
     key: String = "")
  extends Element {

  override private[core] def transform = {
    val r = new net.sf.jasperreports.engine.design.JRDesignComponentElement()
    ElementUtils.putReportElement(key, style, x, y, width, height, conditions, r) >>
    (component.transform >>= { case (transcomp, transkey) => {
      r.setComponent(transcomp)
      r.setComponentKey(transkey)
      ret()
    }}) >>
    ret(r)
  }

  override def verticalExtent = y.value.relativeTo(style) + height.value.relativeTo(style)

  override def moveVertically(len: Length) = copy(y = y.copy(value = y.value.relativeTo(style) + len))
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

/* TODO
sealed case class Crosstab(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    // etc ... dataset: JRCrosstabDataset,
    ) extends Element with Transformable[] {
  private[core] def transform = null
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

package de.ag.jrlang.core

import net.sf.jasperreports.engine.`type`.HorizontalAlignEnum

sealed abstract class Element
{
  // could probably do the 'implicit CanBuildFrom' technique here; but not worth it.
  // def foldStyles(st0: StylesMap) : Element;

};

object Element {
  implicit def drop(c : Element) : net.sf.jasperreports.engine.JRChild =
    c match {
      case v : Ellipse => Ellipse.drop(v)
      case v : StaticText => StaticText.drop(v)
      case v : Image => Image.drop(v)
    }
  
  def foldAllStyles(c: Seq[Element], st: StylesMap) =
    c.foldLeft((Vector.empty:Vector[Element], st)) {
      case ((c, st0), v) => {
        val (v_, st1) = v match {
          case v: Ellipse => v.foldStyles(st0)
          case v: StaticText => v.foldStyles(st0)
          case v: Image => v.foldStyles(st0)
          case _: Element => (v, st0) // undefined, no styles
        };
        ((c :+ v_), st1)
      }};
  
}

sealed case class JRHyperlinkParameter(
    name: String,
    valueExpression: Expression);

// TODO: Is that correct for Image? See XML docu, that has much less attributes.
// Looks like a seperate API for constructing URLs - there are probably better ones for that
sealed case class JRHyperlink( // move
    anchorExpression: Expression,
    pageExpression: Expression,
    parameters: Seq[JRHyperlinkParameter],
    referenceExpression: Expression,
    hyperlinkTarget: net.sf.jasperreports.engine.`type`.HyperlinkTargetEnum,
    hyperlinkType: net.sf.jasperreports.engine.`type`.HyperlinkTypeEnum,
    linkTarget: String,
    linkType: String
    );

object JRHyperlink {
  val empty = new JRHyperlink(
      anchorExpression = "",
      pageExpression = "",
      parameters = Vector.empty,
      referenceExpression = "",
      hyperlinkTarget = net.sf.jasperreports.engine.`type`.HyperlinkTargetEnum.NONE, // ??
      hyperlinkType = net.sf.jasperreports.engine.`type`.HyperlinkTypeEnum.NONE, // ??
      linkTarget = "",
      linkType = ""
      );
}

sealed case class JRAnchor( // move
    anchorNameExpression: Expression,
    bookmarkLevel: Int
    );

abstract sealed class EvaluationTime(val value: net.sf.jasperreports.engine.`type`.EvaluationTimeEnum);

object EvaluationTime {
  case object Auto extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.AUTO);
  case object Band extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.BAND);
  case object Column extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.COLUMN);
  sealed case class Group(group: JRDesignGroup) extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.GROUP);
  case object Now extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.NOW);
  case object Page extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.PAGE);
  case object Report extends EvaluationTime(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.REPORT);

  private[core] def putEvaluationTime(
      o: EvaluationTime,
      setTime: net.sf.jasperreports.engine.`type`.EvaluationTimeEnum => Unit,
      setGroup: net.sf.jasperreports.engine.JRGroup => Unit) = {
    o match {
      case Group(g) => {
        setTime(o.value);
        // TODO setGroup(g);
      }
      case _ => 
        setTime(o.value);
    }
  }
}

sealed case class JRParagraph(
    lineSpacing: Option[net.sf.jasperreports.engine.`type`.LineSpacingEnum]
    );
object JRParagraph {
  val empty = JRParagraph(
      lineSpacing = None);
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
    stretchType: net.sf.jasperreports.engine.`type`.StretchTypeEnum
    );
object Size {
  val empty = new Size(
      height = 0,
      width = 0,
      stretchType = net.sf.jasperreports.engine.`type`.StretchTypeEnum.NO_STRETCH //??
      );
}

sealed case class Pos(
    x: Int, // x and y are mandatory?!
    y: Int,
    positionType: net.sf.jasperreports.engine.`type`.PositionTypeEnum
    );
object Pos {
  val empty = new Pos(
      x = 0,
      y = 0,
      positionType = net.sf.jasperreports.engine.`type`.PositionTypeEnum.FLOAT // ??
     );
}

sealed case class Conditions(
    printInFirstWholeBand: Boolean,
    printWhenExpression: Expression,
    printRepeatedValues: Boolean,
    printWhenDetailOverflows: Boolean,
    // TODO printWhenGroupChanges? is a JRGroup - probably needs a reference to a group defined elsewhere
    removeLineWhenBlank: Boolean
    );
object Conditions {
  val empty = new Conditions(
      printInFirstWholeBand = false,
      printWhenExpression = "",
      printRepeatedValues = true, // important!!
      printWhenDetailOverflows = false,
      removeLineWhenBlank = false
      );
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
      tgt:net.sf.jasperreports.engine.design.JRDesignElement) = {  
    tgt.setKey(if (key == "") null else key); // don't know if it's important to be null
    tgt.setHeight(size.height);
    tgt.setWidth(size.width);
    tgt.setStretchType(size.stretchType);
    tgt.setX(pos.x);
    tgt.setY(pos.y);
    tgt.setPositionType(pos.positionType);
    tgt.setPrintWhenExpression(conditions.printWhenExpression); // TODO: put these
    tgt.setPrintRepeatedValues(conditions.printRepeatedValues); // two in one; as only one can be used (expression has precedence)
    tgt.setPrintInFirstWholeBand(conditions.printInFirstWholeBand);
    tgt.setPrintWhenDetailOverflows(conditions.printWhenDetailOverflows);

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
        var so : net.sf.jasperreports.engine.design.JRDesignStyle = null;
        if (!s.isEmpty) so = s;
        tgt.setStyle(so)
        tgt.setStyleNameReference(null)
      }
      case s : Style.External => {
        tgt.setStyle(null);
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
      val co : net.sf.jasperreports.engine.JRChild = Element.drop(c);
      co match {
        case g : net.sf.jasperreports.engine.design.JRDesignElementGroup => addElementGroup(g)
        case e : net.sf.jasperreports.engine.design.JRDesignElement => addElement(e)
        case _ => throw new RuntimeException("Unexpected type of child: " + co.getClass())
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
    anchor: JRAnchor,
    hyperlink: JRHyperlink,
    chartType : ChartType, // contains type byte, plot and dataset
    customizerClass : String,
    legend: ChartLegend,
    showLegend: Boolean,
    theme: String,
    title: ChartTitle,
    subtitle: ChartSubtitle,
    renderType: String,
    evaluation: EvaluationTime) extends Element;

sealed case class Break(
    key: String,
    // style and size (?) don't make any sense
    // style: Style,
    // size : Size,
    pos : Pos,
    conditions : Conditions,
    breakType : net.sf.jasperreports.engine.`type`.BreakTypeEnum
    )
    extends Element with StyleFoldable[Break] {
  override def foldStyles(st0 : StylesMap) = {
    (this, st0); // no styles
  }
}
object Break {
  /** A default page break */
  val page = new Break(
      key = "",
      pos = Pos.empty,
      conditions = Conditions.empty,
      breakType = net.sf.jasperreports.engine.`type`.BreakTypeEnum.PAGE);
  
  /** A default column break */
  val column =  new Break(
      key = "",
      pos = Pos.empty,
      conditions = Conditions.empty,
      breakType = net.sf.jasperreports.engine.`type`.BreakTypeEnum.COLUMN);
  
  val empty = page;
  
  implicit def dropBreak(o: Break): net.sf.jasperreports.engine.design.JRDesignBreak = {
    val r = new net.sf.jasperreports.engine.design.JRDesignBreak();
    ElementUtils.putReportElement(key = o.key, style=Style.Internal.empty, pos=o.pos, size=Size.empty, conditions=o.conditions, r);
    r.setType(o.breakType);
    r
  }
}

/** The only reason to group your report elements is to customize their stretch behavior. */
sealed case class ElementGroup( // different from a "group"!
    children: Seq[Element]
    ) extends Element with StyleFoldable[ElementGroup] {
  override def foldStyles(st0 : StylesMap) = {
    val (children_, st1) = Element.foldAllStyles(children, st0);
    (copy(children = children_),
        st1)
  }
};

object ElementGroup {
  val empty = new ElementGroup(Vector.empty)
  
  implicit def dropElementGroup(o:ElementGroup) : net.sf.jasperreports.engine.design.JRDesignElementGroup = {
    val r = new net.sf.jasperreports.engine.design.JRDesignElementGroup();
    ElementUtils.addChildren(o.children, r.addElement(_), r.addElementGroup(_));
    r
  }
}

sealed case class Frame(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions,
    children: Seq[Element]
    ) extends Element with StyleFoldable[Frame] {
  override def foldStyles(st0 : StylesMap) = {
    val (style_, st1) = style.foldStyles(st0);
    val (children_, st2) = Element.foldAllStyles(children, st0);
    (copy(style = style_, children = children_),
        st2)
  }
};

object Frame {
  val empty = new Frame(
      key = "",
      style = Style.Internal.empty,
      size = Size.empty,
      pos = Pos.empty,
      conditions = Conditions.empty,
      children = Vector.empty);
  
  implicit def dropFrame(o:Frame) : net.sf.jasperreports.engine.design.JRDesignFrame = {
    val r = new net.sf.jasperreports.engine.design.JRDesignFrame();
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r);
    ElementUtils.addChildren(o.children, r.addElement(_), r.addElementGroup(_));
    r
  }
}

sealed case class Ellipse(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions,
    pen : Option[net.sf.jasperreports.engine.`type`.PenEnum]
) extends Element with StyleFoldable[Ellipse]
{
  def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0);
    (copy(style = style_),
        st1)
  }
};

object Ellipse {
  val empty = new Ellipse(
      key = "",
      style = Style.Internal.empty,
      size = Size.empty,
      pos = Pos.empty,
      conditions = Conditions.empty,
      pen = None
      )

  implicit def drop(o: Ellipse) : net.sf.jasperreports.engine.design.JRDesignEllipse = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignEllipse(null);
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r);
    r
  }
}

sealed case class Pen(
    lineColor: Option[java.awt.Color],
    lineStyle: Option[net.sf.jasperreports.engine.`type`.LineStyleEnum],
    lineWidth: Option[Float]);

object Pen {
  val lineWidth0 = net.sf.jasperreports.engine.JRPen.LINE_WIDTH_0;
  val lineWidth1 = net.sf.jasperreports.engine.JRPen.LINE_WIDTH_1;
  
  val empty = new Pen(None, None, None);
  
  private[core] def putPen(o: Pen, tgt: net.sf.jasperreports.engine.base.JRBoxPen) = {
    tgt.setLineColor(o.lineColor.getOrElse(null));
    tgt.setLineStyle(o.lineStyle.getOrElse(null));
    var w : java.lang.Float = null;
    if (o.lineWidth.isDefined) w = o.lineWidth.get;
    tgt.setLineWidth(w);
  }
}

sealed case class BoxPen(
    top : Pen,
    left : Pen,
    bottom : Pen,
    right : Pen) {

  def isUniform = (top == left) && (left == bottom) && (bottom == right)

}
object BoxPen {
  val empty = new BoxPen(Pen.empty, Pen.empty, Pen.empty, Pen.empty);
  
  /** Creates a pen that is used on all sides of box */
  implicit def uniform(pen: Pen) = new BoxPen(pen, pen, pen, pen);

  private[core] def putBoxPen(o: BoxPen, tgt: net.sf.jasperreports.engine.JRLineBox) = {
    // we assume tgt is default-initialized
    if (o.isUniform) // all pens equal?
      Pen.putPen(o.top, tgt.getPen())
    else {
      Pen.putPen(o.top, tgt.getTopPen());
      Pen.putPen(o.left, tgt.getLeftPen());
      Pen.putPen(o.bottom, tgt.getBottomPen());
      Pen.putPen(o.right, tgt.getRightPen());
    }
  }
}

sealed case class BoxPadding(
    top: Option[Int],
    left: Option[Int],
    bottom: Option[Int],
    right: Option[Int]
    ) {
  def isUniform = (top == left) && (left == bottom) && (bottom == right)
}
object BoxPadding {
  val empty = new BoxPadding(None, None, None, None);

  val none : BoxPadding = 0
  
  implicit def uniform(padding: Int) : BoxPadding =
    new BoxPadding(Some(padding), Some(padding), Some(padding), Some(padding))
  
  private[core] def putBoxPadding(o: BoxPadding, tgt: net.sf.jasperreports.engine.JRLineBox) = {
    // we assume tgt is default-initialized
    def optInt(i: Option[Int]) : java.lang.Integer = if (i.isDefined) i.get else null
    if (o.isUniform)
      tgt.setPadding(optInt(o.top))
    else {
      tgt.setTopPadding(optInt(o.top))
      tgt.setLeftPadding(optInt(o.left))
      tgt.setBottomPadding(optInt(o.bottom))
      tgt.setRightPadding(optInt(o.right))
    }
  }
}

sealed case class LineBox(
    pen: BoxPen,
    padding : BoxPadding
    // style is a fake property, taken from parent "BoxContainer"
    );
object LineBox {
  val empty = new LineBox(
      pen = BoxPen.empty,
      padding = BoxPadding.empty);
  
  private[core] def putLineBox(o: LineBox, tgt: net.sf.jasperreports.engine.JRLineBox) {
    BoxPen.putBoxPen(o.pen, tgt);
    BoxPadding.putBoxPadding(o.padding, tgt);
  }
}

sealed abstract class Align(
    val horizontal : net.sf.jasperreports.engine.`type`.HorizontalAlignEnum,
    val vertical : net.sf.jasperreports.engine.`type`.VerticalAlignEnum);

object Align {
  case object TopLeft extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.LEFT,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.TOP);
  case object TopCenter extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.CENTER,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.TOP);
  case object TopRight extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.RIGHT,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.TOP);
  case object MiddleLeft extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.LEFT,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.MIDDLE);
  case object Center extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.CENTER,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.MIDDLE);
  case object MiddleRight extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.RIGHT,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.MIDDLE);
  case object BottomLeft extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.LEFT,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.BOTTOM);
  case object BottomCenter extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.CENTER,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.BOTTOM);
  case object BottomRight extends Align(
      net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.RIGHT,
      net.sf.jasperreports.engine.`type`.VerticalAlignEnum.BOTTOM);
  
  private[core] def putAlign(o: Align, tgt: net.sf.jasperreports.engine.JRAlignment) {
    // TODO: should be optional - resp. corresponds to style... so use this only in Styles?!
    tgt.setHorizontalAlignment(o.horizontal);
    tgt.setVerticalAlignment(o.vertical);
  }
};

sealed case class Image(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions,
    box : LineBox,
    scale : net.sf.jasperreports.engine.`type`.ScaleImageEnum,
    // already in Style: align : Align, // only applicable on scale=Clip or scale=RetainShape
    usingCache : Option[Boolean], // default depends on type of image expression
    lazily : Boolean, // load at fill time or export time (use href in html for example); if True image expression must be of type String
    onError : net.sf.jasperreports.engine.`type`.OnErrorTypeEnum,
    evaluationTime : EvaluationTime,
    hyperlink : JRHyperlink, // ?? Correct?
    // TODO? bookmarkLevel
    expression : Expression
    ) extends Element with StyleFoldable[Image]
{
  def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0);
    (copy(style = style_),
        st1)
  }
};

object Image {
  def apply(
      expression: Expression,
      scale: net.sf.jasperreports.engine.`type`.ScaleImageEnum) = {
    new Image(
      key = "",
      style = Style.Internal.empty,
      size = Size.empty,
      pos = Pos.empty,
      conditions = Conditions.empty,
      box = LineBox.empty,
      scale = scale,
      // align = Align.TopLeft,
      usingCache = None,
      lazily = false,
      onError = net.sf.jasperreports.engine.`type`.OnErrorTypeEnum.ERROR,
      evaluationTime = EvaluationTime.Now,
      hyperlink = JRHyperlink.empty,
      expression = expression
      )
  }

  implicit def drop(o: Image) : net.sf.jasperreports.engine.design.JRDesignImage = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignImage(null);
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r);
    LineBox.putLineBox(o.box, r.getLineBox());
    r.setScaleImage(o.scale);
    // use style Align.putAlign(o.align, r);
    r.setUsingCache(if (o.usingCache.isDefined) (o.usingCache.get : java.lang.Boolean) else null);
    r.setLazy(o.lazily);
    r.setOnErrorType(o.onError);
    EvaluationTime.putEvaluationTime(o.evaluationTime, r.setEvaluationTime(_), r.setEvaluationGroup(_));
    // TODO: Hyperlink
    r.setExpression(o.expression);
    r
  }
};

sealed case class Line(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions,
    pen : Option[net.sf.jasperreports.engine.`type`.PenEnum],
    /** The direction attribute determines which one of the two diagonals of the rectangle
     *  should be drawn:
     *  - direction="TopDown" draws a diagonal line from the top-left corner of the
     *    rectangle to the bottom-right corner.
     *  - direction="BottomUp" draws a diagonal line from the bottom-left corner to
     *    the upper-right corner.
     *  The default direction for a line is top-down. */
    direction: net.sf.jasperreports.engine.`type`.LineDirectionEnum
    ) extends Element;

sealed case class Rectangle(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions,
    pen : Option[net.sf.jasperreports.engine.`type`.PenEnum],
    /** The radius attribute specifies the radius for the arcs used to draw the corners
     *  of the rectangle. The default value is 0, meaning that the rectangle has normal,
     *  square corners.
     */
    radius: Int
    ) extends Element;

sealed case class StaticText(
    text: String,
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions,
    box : LineBox
    ) extends Element with StyleFoldable[StaticText]
{
  def foldStyles(st0: StylesMap) = {
    val (style_, st1) = style.foldStyles(st0);
    (copy(style = style_),
        st1)
  }
};;

object StaticText {
  def apply(text: String) = new StaticText(
      text = text,
      key = "",
      style = Style.Internal.empty,
      size = Size.empty,
      pos = Pos.empty,
      conditions = Conditions.empty,
      box = LineBox.empty);
  
  implicit def drop(o: StaticText) : net.sf.jasperreports.engine.design.JRDesignStaticText = {
    val r = new net.sf.jasperreports.engine.design.JRDesignStaticText();
    r.setText(o.text);
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r);
    LineBox.putLineBox(o.box, r.getLineBox());
    r
  }
}

sealed case class TextField(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;

sealed case class ComponentElement(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions,
    component: net.sf.jasperreports.engine.component.Component,
    componentKey: net.sf.jasperreports.engine.component.ComponentKey
    ) extends Element;

sealed case class Crosstab(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    // etc ... dataset: JRCrosstabDataset,
    ) extends Element;

sealed case class GenericElement(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;

sealed case class Subreport(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;


package de.ag.jrlang.core

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
    }
  
  def foldAllStyles(c: Seq[Element], st0: StylesMap) =
    c.foldLeft((Vector.empty:Vector[Element], st0)) {
      case ((c, st), v) => {
        val (v_, st_) = v match {
          case v: Ellipse => v.foldStyles(st)
          case v: StaticText => v.foldStyles(st)
          case _: Element => (v, st) // undefined, no styles
        };
        ((c :+ v_), st_)
      }};
  
}

sealed case class Break(
    breakType : net.sf.jasperreports.engine.`type`.BreakTypeEnum
    ) extends Element;

sealed case class JRHyperlinkParameter(
    name: String,
    valueExpression: Expression);

sealed case class JRHyperlink( // move
    anchorExpression: Expression,
    pageExpression: Expression,
    parameters: Seq[JRHyperlinkParameter],
    referenceExpression: Expression,
    hyperlinkTarget: net.sf.jasperreports.engine.`type`.HyperlinkTargetEnum,
    hyperlinkType: net.sf.jasperreports.engine.`type`.HyperlinkTypeEnum,
    linkTarget: String,
    linkType: String
    )

sealed case class JRAnchor( // move
    anchorNameExpression: Expression,
    bookmarkLevel: Int
    );

abstract sealed class JREvaluation(value: net.sf.jasperreports.engine.`type`.EvaluationTimeEnum);

object JREvaluation {
  case object Auto extends JREvaluation(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.AUTO);
  case object Band extends JREvaluation(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.BAND);
  case object Column extends JREvaluation(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.COLUMN);
  sealed case class Group(group: JRDesignGroup) extends JREvaluation(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.GROUP);
  case object Now extends JREvaluation(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.NOW);
  case object Page extends JREvaluation(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.PAGE);
  case object Report extends JREvaluation(net.sf.jasperreports.engine.`type`.EvaluationTimeEnum.REPORT);
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

private object ElementUtils {
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
    evaluation: JREvaluation) extends Element;

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

/** The only reason to group your report elements is to customize their stretch behavior. */
sealed case class ElementGroup( // different from a "group"!
    children: Seq[Element]
    ) extends Element;

sealed case class Ellipse(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
// TODO: Graphic?
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
      conditions = Conditions.empty
      )

  implicit def drop(o: Ellipse) : net.sf.jasperreports.engine.design.JRDesignEllipse = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignEllipse(null);
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r);
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
    ) extends Element;

sealed case class GenericElement(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;

sealed case class Image(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;

sealed case class Line(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;

sealed case class Rectangle(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;

sealed case class StaticText(
    text: String,
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
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
      conditions = Conditions.empty);
  
  implicit def drop(o: StaticText) : net.sf.jasperreports.engine.design.JRDesignStaticText = {
    val r = new net.sf.jasperreports.engine.design.JRDesignStaticText();
    r.setText(o.text);
    ElementUtils.putReportElement(o.key, o.style, o.pos, o.size, o.conditions, r);
    r
  }
}

sealed case class Subreport(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;

sealed case class TextField(
    key: String,
    style: Style,
    size : Size,
    pos : Pos,
    conditions : Conditions
    ) extends Element;

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

sealed case class JRFont(
    fontName: Option[String],
    fontSize: Option[Int],
    bold: Option[Boolean],
    italic: Option[Boolean],
    strikeThrough: Option[Boolean],
    underline: Option[Boolean],
    pdfEncoding: Option[String],
    pdfFontName: Option[String],
    pdfEmbedded: Option[Boolean]
    );
object JRFont {
  val empty = new JRFont(
      fontName = None,
      fontSize = None,
      bold = None,
      italic = None,
      strikeThrough = None,
      underline = None,
      pdfEncoding = None,
      pdfFontName = None,
      pdfEmbedded = None)
}

/*
sealed abstract class PrintWhen;
object PrintWhen {
  sealed case class Expression(expression: Expression) extends PrintWhen;
  
}
*/

sealed case class JRCommon(
    key: String,
    forecolor: Option[java.awt.Color],
    backcolor: Option[java.awt.Color],
    height: Int,
    width: Int,
    x: Int,
    y: Int,
    positionType: net.sf.jasperreports.engine.`type`.PositionTypeEnum,
    mode: Option[net.sf.jasperreports.engine.`type`.ModeEnum],
    printInFirstWholeBand: Boolean,
    printWhenExpression: Expression,
    printRepeatedValues: Boolean,
    printWhenDetailOverflows: Boolean,
    // printWhenGroupChanges?
    removeLineWhenBlank: Boolean,
    stretchType: net.sf.jasperreports.engine.`type`.StretchTypeEnum,
    style: Option[Style] // aka parentStyle
    ) extends StyleFoldable[JRCommon]
{
  def foldStyles(st0: StylesMap) = {
    val (style_, st1) = StyleFoldable.foldOption(style, st0)
    (copy(style = style_), st1)
  }
}

object JRCommon { // TODO: Split up along more logical groups
  val empty = new JRCommon(
      key = "",
      forecolor = None, // remove colors in favour of style (compiler could decide to use them)
      backcolor = None,
      height = 0,
      width = 0,
      x = 0,
      y = 0,
      positionType = net.sf.jasperreports.engine.`type`.PositionTypeEnum.FLOAT, // ??
      mode = None,
      printInFirstWholeBand = false,
      printWhenExpression = "",
      printRepeatedValues = true, // !!
      printWhenDetailOverflows = false,
      removeLineWhenBlank = false,
      stretchType = net.sf.jasperreports.engine.`type`.StretchTypeEnum.NO_STRETCH, //??
      style = None
      )
  
  def put(src: JRCommon, tgt:net.sf.jasperreports.engine.design.JRDesignElement) = {
    tgt.setKey(if (src.key == "") null else src.key); // don't know if it's important to be null
    tgt.setForecolor(src.forecolor.getOrElse(null));
    tgt.setBackcolor(src.backcolor.getOrElse(null));
    tgt.setHeight(src.height);
    tgt.setWidth(src.width);
    tgt.setX(src.x);
    tgt.setY(src.y);
    tgt.setPositionType(src.positionType);
    tgt.setMode(src.mode.getOrElse(null));
    tgt.setPrintWhenExpression(src.printWhenExpression); // TODO: put these
    tgt.setPrintRepeatedValues(src.printRepeatedValues); // two in one; as only one can be used (expression has precedence)
    tgt.setPrintInFirstWholeBand(src.printInFirstWholeBand);
    tgt.setPrintWhenDetailOverflows(src.printWhenDetailOverflows);
    tgt.setStretchType(src.stretchType);
    src.style match {
      case None =>
        (); // ?? tgt.setStyle(null);
      case Some(s : Style.Internal) =>
        tgt.setStyle(s)
      case Some(s : Style.External) =>
        tgt.setStyleNameReference(s.reference)
    }
  }
}
    
sealed case class Chart(
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
    evaluation: JREvaluation,
    common: JRCommon) extends Element;

sealed case class ComponentElement(
    component: net.sf.jasperreports.engine.component.Component,
    componentKey: net.sf.jasperreports.engine.component.ComponentKey,
    common: JRCommon
    ) extends Element;

sealed case class Crosstab(
    // etc ... dataset: JRCrosstabDataset,
    
    common: JRCommon
    ) extends Element;

sealed case class Group(
    children: Seq[Element]
    ) extends Element;

sealed case class Ellipse(
    common: JRCommon
    ) extends Element with StyleFoldable[Ellipse]
{
  def foldStyles(st0: StylesMap) = {
    val (common_, st1) = common.foldStyles(st0);
    (copy(common = common_),
        st1)
  }
};

object Ellipse {
  val empty = Ellipse(common = JRCommon.empty)

  implicit def drop(o: Ellipse) : net.sf.jasperreports.engine.design.JRDesignEllipse = {
    // Unlike other elements (e.g. TextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignEllipse(null);
    JRCommon.put(o.common, r);
    r
  }
}

sealed case class Frame(
    common: JRCommon,
    children: Seq[Element]
    ) extends Element;

sealed case class GenericElement(
    // TODO
    ) extends Element;

sealed case class Image(
    ) extends Element;

sealed case class Line(
    ) extends Element;

sealed case class Rectangle(
    ) extends Element;

sealed case class StaticText(
    text: String,
    common: JRCommon
    ) extends Element with StyleFoldable[StaticText]
{
  def foldStyles(st0: StylesMap) = {
    val (common_, st1) = common.foldStyles(st0);
    (copy(common = common_),
        st1)
  }
};;

object StaticText {
  def apply(text: String) = new StaticText(
      text = text,
      common = JRCommon.empty);
  
  implicit def drop(o: StaticText) : net.sf.jasperreports.engine.design.JRDesignStaticText = {
    val r = new net.sf.jasperreports.engine.design.JRDesignStaticText();
    r.setText(o.text);
    JRCommon.put(o.common, r);
    r
  }
}

sealed case class Subreport(
    ) extends Element;

sealed case class TextField(
    ) extends Element;

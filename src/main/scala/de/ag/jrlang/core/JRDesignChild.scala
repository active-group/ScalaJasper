package de.ag.jrlang.core

sealed abstract class JRDesignChild
  //extends BaseAdapter[net.sf.jasperreports.engine.JRChild]
  //extends LeafAdapter[OOClass];

object JRDesignChild {
  implicit def drop(c : JRDesignChild) : net.sf.jasperreports.engine.JRChild =
    c match {
      case v : JREllipse => JREllipse.drop(v)
      case v : JRStaticText => JRStaticText.drop(v)
    }
}

sealed case class JRBreak(
    breakType : net.sf.jasperreports.engine.`type`.BreakTypeEnum
    ) extends JRDesignChild;

sealed case class JRHyperlinkParameter(
    name: String,
    valueExpression: JRDesignExpression);

sealed case class JRHyperlink( // move
    anchorExpression: JRDesignExpression,
    pageExpression: JRDesignExpression,
    parameters: Seq[JRHyperlinkParameter],
    referenceExpression: JRDesignExpression,
    hyperlinkTarget: net.sf.jasperreports.engine.`type`.HyperlinkTargetEnum,
    hyperlinkType: net.sf.jasperreports.engine.`type`.HyperlinkTypeEnum,
    linkTarget: String,
    linkType: String
    )

sealed case class JRAnchor( // move
    anchorNameExpression: JRDesignExpression,
    bookmarkLevel: Int
    );

sealed case class JRChartLegend(
    color: Option[java.awt.Color],
    backgroundColor: Option[java.awt.Color],
    position: net.sf.jasperreports.charts.`type`.EdgeEnum);

sealed case class JRChartTitle(
    color: Option[java.awt.Color],
    expression: JRDesignExpression,
    font: Option[JRFont],
    position: net.sf.jasperreports.charts.`type`.EdgeEnum);

sealed case class JRChartSubtitle(
    color: Option[java.awt.Color],
    expression: JRDesignExpression,
    font: Option[JRFont]);

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

abstract sealed class JRStyle;
object JRStyle {
  sealed case class Internal(
      name: String,
      isDefault: Boolean,
      parentStyle: Option[JRStyle],
      conditionalStyles: Any, // TODO
      backcolor: Option[java.awt.Color],
      forecolor: Option[java.awt.Color],
      font: JRFont, // though bold,italic are not optional here; pdfEmbedded is ignored
      horizontalAlignment: Option[net.sf.jasperreports.engine.`type`.HorizontalAlignEnum],
      paragraph: JRParagraph,
      markup: Option[String],
      mode: Option[net.sf.jasperreports.engine.`type`.ModeEnum],
      pattern: Option[String],
      radius: Option[Int],
      rotation: Option[net.sf.jasperreports.engine.`type`.RotationEnum],
      scaleImage: Option[net.sf.jasperreports.engine.`type`.ScaleImageEnum],
      verticalAlignment: Option[net.sf.jasperreports.engine.`type`.VerticalAlignEnum],
      fill: Option[net.sf.jasperreports.engine.`type`.FillEnum]
      ) extends JRStyle {
    lazy val obj = Internal.dropNew(this) // TODO: explain
  }
  object Internal {
    def apply(name: String) =
      new Internal(
          name = name,
          isDefault = false,
          parentStyle = None,
          conditionalStyles = Vector(),
          backcolor = None,
          forecolor = None,
          font = JRFont.empty,
          horizontalAlignment = None,
          paragraph = JRParagraph.empty,
          markup = None,
          mode = None,
          pattern = None,
          radius = None,
          rotation = None,
          scaleImage = None,
          verticalAlignment = None,
          fill = None);
    
    implicit def drop(o:Internal) : net.sf.jasperreports.engine.design.JRDesignStyle =
      o.obj
    
    def dropNew(o:Internal) : net.sf.jasperreports.engine.design.JRDesignStyle = {
      val r = new net.sf.jasperreports.engine.design.JRDesignStyle();
      r.setName(o.name);
      r.setDefault(o.isDefault);
      if (o.parentStyle.isDefined)
        put(o.parentStyle.get, r);
      // TODO: conditional styles?
      r.setBackcolor(o.backcolor.getOrElse(null));
      r.setForecolor(o.forecolor.getOrElse(null));
      r.setFontName(o.font.fontName.getOrElse(null));
      if (o.font.fontSize.isDefined)
        r.setFontSize(o.font.fontSize.get); // else null:Integer
      r.setBold(o.font.bold.getOrElse(false));
      r.setItalic(o.font.italic.getOrElse(false));
      r.setStrikeThrough(o.font.strikeThrough.getOrElse(false));
      r.setUnderline(o.font.underline.getOrElse(false));
      r.setPdfEncoding(o.font.pdfEncoding.getOrElse(null));
      r.setPdfFontName(o.font.pdfFontName.getOrElse(null));
      // n/a pdfEmbedded ??
      r
    }
  }

  sealed case class External(reference: String);

  def put(src: JRStyle, tgt:net.sf.jasperreports.engine.design.JRDesignElement) = {
    src match {
      case v:Internal => tgt.setStyle(Internal.drop(v))
      case v:External => tgt.setStyleNameReference(v.reference)
    }
  }
  def put(src: JRStyle, tgt:net.sf.jasperreports.engine.design.JRDesignStyle) = {
    src match {
      case v:Internal => tgt.setParentStyle(Internal.drop(v))
      case v:External => tgt.setParentStyleNameReference(v.reference)
    }
  }
}

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
    printRepeatedValues: Boolean,
    printWhenDetailOverflows: Boolean,
    removeLineWhenBlank: Boolean,
    stretchType: net.sf.jasperreports.engine.`type`.StretchTypeEnum,
    style: Option[JRStyle] // aka parentStyle
    );
object JRCommon {
  val empty = new JRCommon(
      key = "",
      forecolor = None,
      backcolor = None,
      height = 0,
      width = 0,
      x = 0,
      y = 0,
      positionType = net.sf.jasperreports.engine.`type`.PositionTypeEnum.FLOAT, // ??
      mode = None,
      printInFirstWholeBand = false,
      printRepeatedValues = false,
      printWhenDetailOverflows = false,
      removeLineWhenBlank = false,
      stretchType = net.sf.jasperreports.engine.`type`.StretchTypeEnum.NO_STRETCH, //??
      style = None
      )
  
  def put(src: JRCommon, tgt:net.sf.jasperreports.engine.design.JRDesignElement) = {
    tgt.setKey(src.key);
    tgt.setForecolor(src.forecolor.getOrElse(null));
    tgt.setBackcolor(src.backcolor.getOrElse(null));
    tgt.setHeight(src.height);
    tgt.setWidth(src.width);
    tgt.setX(src.x);
    tgt.setY(src.y);
    tgt.setPositionType(src.positionType);
    tgt.setMode(src.mode.getOrElse(null));
    // ?? tgt.setPrintWhenExpression(expression)
    tgt.setPrintInFirstWholeBand(src.printInFirstWholeBand);
    tgt.setPrintRepeatedValues(src.printRepeatedValues);
    tgt.setPrintWhenDetailOverflows(src.printWhenDetailOverflows);
    tgt.setStretchType(src.stretchType);
    // TODO tgt.setStyle(src.style.getOrElse(null));
  }
}
    
sealed case class JRChart(
    anchor: JRAnchor,
    hyperlink: JRHyperlink,
    chartType : JRChartType, // contains type byte, plot and dataset
    customizerClass : String,
    legend: JRChartLegend,
    showLegend: Boolean,
    theme: String,
    title: JRChartTitle,
    subtitle: JRChartSubtitle,
    renderType: String,
    evaluation: JREvaluation,
    common: JRCommon) extends JRDesignChild;

sealed case class JRComponentElement(
    component: net.sf.jasperreports.engine.component.Component,
    componentKey: net.sf.jasperreports.engine.component.ComponentKey,
    common: JRCommon
    ) extends JRDesignChild;

sealed case class JRCrosstab(
    // etc ... dataset: JRCrosstabDataset,
    
    common: JRCommon
    ) extends JRDesignChild;

sealed case class JRElementGroup(
    children: Seq[JRDesignChild]
    ) extends JRDesignChild;

sealed case class JREllipse(
    common: JRCommon
    ) extends JRDesignChild;

object JREllipse {
  val empty = JREllipse(common = JRCommon.empty)

  def drop(o: JREllipse) : net.sf.jasperreports.engine.design.JRDesignEllipse = {
    // Unlike other elements (e.g. JRDesignTextField), the ellipse is missing a default constructor;
    // setting JRDefaultStyleProvider to null like the others do.
    val r = new net.sf.jasperreports.engine.design.JRDesignEllipse(null);
    JRCommon.put(o.common, r);
    r
  }
}

sealed case class JRDesignFrame(
    common: JRCommon,
    children: Seq[JRDesignChild]
    ) extends JRDesignChild;

sealed case class JRGenericElement(
    // TODO
    ) extends JRDesignChild;

sealed case class JRImage(
    ) extends JRDesignChild;

sealed case class JRDesignLine(
    ) extends JRDesignChild;

sealed case class JRRectangle(
    ) extends JRDesignChild;

sealed case class JRStaticText(
    text: String,
    common: JRCommon
    ) extends JRDesignChild;

object JRStaticText {
  def apply(text: String) = new JRStaticText(
      text = text,
      common = JRCommon.empty);
  
  def drop(o: JRStaticText) : net.sf.jasperreports.engine.design.JRDesignStaticText = {
    val r = new net.sf.jasperreports.engine.design.JRDesignStaticText();
    r.setText(o.text);
    JRCommon.put(o.common, r);
    r
  }
}

sealed case class JRSubreport(
    ) extends JRDesignChild;

sealed case class JRDesignTextField(
    ) extends JRDesignChild;

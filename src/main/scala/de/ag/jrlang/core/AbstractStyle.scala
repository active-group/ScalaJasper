package de.ag.jrlang.core

import net.sf.jasperreports.engine.base.{JRBaseStyle, JRBaseParagraph}
import net.sf.jasperreports.engine.`type`.TabStopAlignEnum

import Transformer._
import net.sf.jasperreports.engine.design.{JRDesignConditionalStyle, JRDesignStyle}

// TODO: Not used yet
sealed abstract class Align(val horizontal : net.sf.jasperreports.engine.`type`.HorizontalAlignEnum,
                            val vertical : net.sf.jasperreports.engine.`type`.VerticalAlignEnum)

object Align {
  case object TopLeft extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.LEFT,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.TOP)
  case object TopCenter extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.CENTER,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.TOP)
  case object TopRight extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.RIGHT,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.TOP)
  case object MiddleLeft extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.LEFT,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.MIDDLE)
  case object Center extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.CENTER,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.MIDDLE)
  case object MiddleRight extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.RIGHT,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.MIDDLE)
  case object BottomLeft extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.LEFT,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.BOTTOM)
  case object BottomCenter extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.CENTER,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.BOTTOM)
  case object BottomRight extends Align(
    net.sf.jasperreports.engine.`type`.HorizontalAlignEnum.RIGHT,
    net.sf.jasperreports.engine.`type`.VerticalAlignEnum.BOTTOM)

  private[core] def putAlign(o: Align, tgt: net.sf.jasperreports.engine.JRAlignment) {
    // TODO: should be optional - resp. corresponds to style... so use this only in Styles?!
    tgt.setHorizontalAlignment(o.horizontal)
    tgt.setVerticalAlignment(o.vertical)
  }
}


sealed case class Font(
    fontName: Option[String],
    fontSize: Option[Int],
    bold: Option[Boolean],
    italic: Option[Boolean],
    strikeThrough: Option[Boolean],
    underline: Option[Boolean],
    pdfEncoding: Option[String],
    pdfFontName: Option[String],
    pdfEmbedded: Option[Boolean]
    ) {
  def isEmpty = this == Font.empty
}
object Font {
  val empty = new Font(
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

sealed case class Pen(lineColor: Option[java.awt.Color],
                      lineStyle: Option[net.sf.jasperreports.engine.`type`.LineStyleEnum],
                      lineWidth: Option[Float])

object Pen {
  val lineWidth0 = net.sf.jasperreports.engine.JRPen.LINE_WIDTH_0
  val lineWidth1 = net.sf.jasperreports.engine.JRPen.LINE_WIDTH_1

  val empty = new Pen(None, None, None)

  private[core] def putPen(o: Pen, tgt: net.sf.jasperreports.engine.JRPen) {
    tgt.setLineColor(o.lineColor.getOrElse(null))
    tgt.setLineStyle(o.lineStyle.getOrElse(null))
    var w : java.lang.Float = null
    if (o.lineWidth.isDefined) w = o.lineWidth.get
    tgt.setLineWidth(w)
  }
}

sealed case class BoxPen(top : Pen,
                         left : Pen,
                         bottom : Pen,
                         right : Pen) {

  def isUniform = (top == left) && (left == bottom) && (bottom == right)
}

object BoxPen {
  val empty = new BoxPen(Pen.empty, Pen.empty, Pen.empty, Pen.empty)

  /** Creates a box pen that uses the same pen on all sides of the box */
  implicit def uniform(pen: Pen) = new BoxPen(pen, pen, pen, pen)

  private[core] def putBoxPen(o: BoxPen, tgt: net.sf.jasperreports.engine.JRLineBox) {
    // we assume tgt is default-initialized
    if (o.isUniform) // all pens equal?
      Pen.putPen(o.top, tgt.getPen)
    else {
      Pen.putPen(o.top, tgt.getTopPen)
      Pen.putPen(o.left, tgt.getLeftPen)
      Pen.putPen(o.bottom, tgt.getBottomPen)
      Pen.putPen(o.right, tgt.getRightPen)
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
  val empty = new BoxPadding(None, None, None, None)

  val none : BoxPadding = 0

  implicit def uniform(padding: Int) : BoxPadding =
    new BoxPadding(Some(padding), Some(padding), Some(padding), Some(padding))

  private[core] def putBoxPadding(o: BoxPadding, tgt: net.sf.jasperreports.engine.JRLineBox) {
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

sealed case class LineBox(pen: BoxPen,
                          padding : BoxPadding
                          // style is a fake property, taken from parent "BoxContainer"
                          )
object LineBox {
  val empty = new LineBox(
    pen = BoxPen.empty,
    padding = BoxPadding.empty)

  private[core] def putLineBox(o: LineBox, tgt: net.sf.jasperreports.engine.JRLineBox) {
    BoxPen.putBoxPen(o.pen, tgt)
    BoxPadding.putBoxPadding(o.padding, tgt)
  }
}

sealed case class TabStop(position: Int, align: TabStopAlignEnum)

object TabStop {
  def leftAlign(position: Int) = TabStop(position, TabStopAlignEnum.LEFT)
  def rightAlign(position: Int) = TabStop(position, TabStopAlignEnum.RIGHT)
  def centerAlign(position: Int) = TabStop(position, TabStopAlignEnum.CENTER)

  implicit def drop(o: TabStop): net.sf.jasperreports.engine.TabStop =
    new net.sf.jasperreports.engine.TabStop(o.position, o.align)

}

sealed case class Paragraph(lineSpacing: Option[net.sf.jasperreports.engine.`type`.LineSpacingEnum] = None,
                            lineSpacingSize: Option[Float] = None,
                            firstLineIndent: Option[Int] = None,
                            leftIndent: Option[Int] = None,
                            rightIndent: Option[Int] = None,
                            spacingBefore: Option[Int] = None,
                            spacingAfter: Option[Int] = None,
                            tabStopWidth: Option[Int] = None,
                            tabStops: Seq[TabStop] = Seq.empty)

object Paragraph {
  val empty = Paragraph()

  private[core] def put(o: Paragraph, r: JRBaseParagraph) {
    def optFloat(v: Option[Float]) : java.lang.Float = if (v.isDefined) v.get else null
    def optInt(v: Option[Int]) : java.lang.Integer = if (v.isDefined) v.get else null

    r.setLineSpacing(o.lineSpacing.getOrElse(null))
    r.setLineSpacingSize(optFloat(o.lineSpacingSize))
    r.setFirstLineIndent(optInt(o.firstLineIndent))
    r.setLeftIndent(optInt(o.leftIndent))
    r.setRightIndent(optInt(o.rightIndent))
    r.setSpacingBefore(optInt(o.spacingBefore))
    r.setSpacingAfter(optInt(o.spacingAfter))
    r.setTabStopWidth(optInt(o.tabStopWidth))
    for (t <- o.tabStops)
      r.addTabStop(t)
  }
}


// always transforms to a 'style reference'; the style itself will be added to the transformation state
abstract sealed class AbstractStyle extends Transformable[Option[String]]

sealed case class Style(
                         // name is isDefault intentionally left out (see top level JaperDesign)
                         parentStyle: Option[AbstractStyle] = None,
                         // The conditionalStyles must not have a parentStyle, and probably not conditionalStyles themselves
                         conditionalStyles: Seq[(Expression[Boolean], Style)] = Vector.empty,
                         backcolor: Option[java.awt.Color] = None,
                         forecolor: Option[java.awt.Color] = None,
                         font: Font = Font.empty,
                         horizontalAlignment: Option[net.sf.jasperreports.engine.`type`.HorizontalAlignEnum] = None,
                         paragraph: Paragraph = Paragraph.empty,
                         markup: Option[String] = None, // "none", "styled", "html", "rtf"
                         /** Report elements can either be transparent or opaque, depending on the value
                           *  you specify for the mode attribute. The default value for this attribute
                           *  depends on the type of the report element. Graphic elements like rectangles
                           *  and lines are opaque by default, while images are transparent. Both static
                           *  texts and text fields are transparent by default, and so are the subreport elements. */
                         mode: Option[net.sf.jasperreports.engine.`type`.ModeEnum] = None,

                         pattern: Option[String] = None,
                         radius: Option[Int] = None,
                         rotation: Option[net.sf.jasperreports.engine.`type`.RotationEnum] = None,
                         scaleImage: Option[net.sf.jasperreports.engine.`type`.ScaleImageEnum] = None,
                         verticalAlignment: Option[net.sf.jasperreports.engine.`type`.VerticalAlignEnum] = None,
                         line: Pen = Pen.empty,
                         box: LineBox = LineBox.empty,
                         fill: Option[net.sf.jasperreports.engine.`type`.FillEnum] = None,
                         blankWhenNull: Option[Boolean] = None
                         ) extends AbstractStyle {

  def isEmpty = (this == Style.empty)

  def transform = {
    if (this.isEmpty)
      ret(None)
    else {
      styleName(this, { () => mkDesignStyle }) >>= { s => ret(Some(s)) }
    }
  }

  private[core] def mkDesignStyle : Transformer[JRDesignStyle] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignStyle()
    // name is isDefault are set externally
    //r.setName(o.name);
    //r.setDefault(o.isDefault);
    drop(orNull(parentStyle map {_.transform})) { op => r.setParentStyleNameReference(if (op == null) null else op.getOrElse(null)) } >>
      (all(conditionalStyles map Style.transCond) >>= { cs =>
      // JRConditionalStyleFactory suggests, that the parentStyle should always refer to this 'containing' style
        cs foreach { _.setParentStyle(r) }
        cs foreach { r.addConditionalStyle(_) }
        ret()
      }) >>
      Style.putBase(this, r) >>
      ret(r)
  }

}
object Style {
  val empty = Style()

  /** inherit all style definitions from 'environment' or the default style, depending on the element */
  val inherit = Style.empty

  private def transCond(v: (Expression[Any], Style)) : Transformer[JRDesignConditionalStyle] = {
    val (e, s) = v
    val r = new net.sf.jasperreports.engine.design.JRDesignConditionalStyle()
    // these are not allowed for conditional styles (new type?)
    assert(s.parentStyle == None) // exception?
    assert(s.conditionalStyles.isEmpty) // exception?
    putBase(s, r) >>
      drop(e.transform)(r.setConditionExpression(_)) >>
      ret(r)
  }

  private def putBase(o: Style, r: net.sf.jasperreports.engine.base.JRBaseStyle) : Transformer[Unit] = {

    def optBool(v: Option[Boolean]) : java.lang.Boolean =
      if (v.isDefined) v.get else null
    def optInt(v: Option[Int]) : java.lang.Integer =
      if (v.isDefined) v.get else null
    // only simple things currently (need not be within transformer monad)
    r.setBackcolor(o.backcolor.getOrElse(null))
    r.setForecolor(o.forecolor.getOrElse(null))
    // Though the properties are the same, JRStyle does not use a JRFont :-/
    r.setFontName(o.font.fontName.getOrElse(null))
    // the java.lang.Boolean and Integer overloads are different!
    // - null means 'inherit' or 'undefined'
    r.setFontSize(optInt(o.font.fontSize)); // Integer
    r.setBold(optBool(o.font.bold))
    r.setItalic(optBool(o.font.italic))
    r.setStrikeThrough(optBool(o.font.strikeThrough))
    r.setUnderline(optBool(o.font.underline))
    r.setPdfEncoding(o.font.pdfEncoding.getOrElse(null))
    r.setPdfFontName(o.font.pdfFontName.getOrElse(null))
    r.setPdfEmbedded(optBool(o.font.pdfEmbedded))

    Pen.putPen(o.line, r.getLinePen)
    LineBox.putLineBox(o.box, r.getLineBox)
    r.setScaleImage(o.scaleImage.getOrElse(null))
    r.setRadius(optInt(o.radius))
    r.setBlankWhenNull(optBool(o.blankWhenNull))
    Paragraph.put(o.paragraph, r.getParagraph.asInstanceOf[JRBaseParagraph])
    ret(r)
  }
}

sealed case class StyleReference(reference: String) extends AbstractStyle {
  def transform = ret(Some(reference))
}

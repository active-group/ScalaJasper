package de.ag.jrlang.core

import net.sf.jasperreports.engine.base.{JRBaseStyle, JRBaseParagraph}
import net.sf.jasperreports.engine.`type`.{LineSpacingEnum, TabStopAlignEnum}

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
    fontName: Option[String] = None,
    fontSize: Option[Int] = None,
    bold: Option[Boolean] = None,
    italic: Option[Boolean] = None,
    strikeThrough: Option[Boolean] = None,
    underline: Option[Boolean] = None,
    pdfEncoding: Option[String] = None,
    pdfFontName: Option[String] = None,
    pdfEmbedded: Option[Boolean] = None
    ) {
  def isEmpty = this == Font.empty

  def ++(rhs: Font) =
    Font(
      fontName = rhs.fontName.orElse(this.fontName),
      fontSize = rhs.fontSize.orElse(this.fontSize),
      bold = rhs.bold.orElse(this.bold),
      italic = rhs.italic.orElse(this.italic),
      strikeThrough = rhs.strikeThrough.orElse(this.strikeThrough),
      underline = rhs.underline.orElse(this.underline),
      pdfEncoding = rhs.pdfEncoding.orElse(this.pdfEncoding),
      pdfFontName = rhs.pdfFontName.orElse(this.pdfFontName),
      pdfEmbedded = rhs.pdfEmbedded.orElse(this.pdfEmbedded)
    )
}
object Font {
  val empty = new Font()
}

sealed case class Pen(lineColor: Option[java.awt.Color] = None,
                      lineStyle: Option[net.sf.jasperreports.engine.`type`.LineStyleEnum] = None,
                      lineWidth: Option[Float] = None) {
  def ++(rhs: Pen) =
    Pen(
      lineColor = rhs.lineColor.orElse(this.lineColor),
      lineStyle = rhs.lineStyle.orElse(this.lineStyle),
      lineWidth = rhs.lineWidth.orElse(this.lineWidth)
    )
}

object Pen {
  val lineWidth0 = net.sf.jasperreports.engine.JRPen.LINE_WIDTH_0
  val lineWidth1 = net.sf.jasperreports.engine.JRPen.LINE_WIDTH_1

  val empty = new Pen()

  private[core] def putPen(o: Pen, tgt: net.sf.jasperreports.engine.JRPen) {
    tgt.setLineColor(o.lineColor.getOrElse(null))
    tgt.setLineStyle(o.lineStyle.getOrElse(null))
    var w : java.lang.Float = null
    if (o.lineWidth.isDefined) w = o.lineWidth.get
    tgt.setLineWidth(w)
  }
}

sealed case class BoxPen(top : Pen = Pen.empty,
                         left : Pen = Pen.empty,
                         bottom : Pen = Pen.empty,
                         right : Pen = Pen.empty) {

  def isUniform = (top == left) && (left == bottom) && (bottom == right)

  def ++(rhs: BoxPen) =
    BoxPen(
      top = this.top ++ rhs.top,
      left = this.left ++ rhs.left,
      bottom = this.bottom ++ rhs.bottom,
      right = this.right ++ rhs.right
    )
}

object BoxPen {
  val empty = new BoxPen()

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

sealed case class BoxPadding(top: Option[Int] = None,
                             left: Option[Int] = None,
                             bottom: Option[Int] = None,
                             right: Option[Int] = None) {
  def isUniform = (top == left) && (left == bottom) && (bottom == right)

  def ++(rhs: BoxPadding) =
    BoxPadding(
      top = rhs.top.orElse(this.top),
      left = rhs.left.orElse(this.left),
      bottom = rhs.bottom.orElse(this.bottom),
      right = rhs.right.orElse(this.right)
    )
}
object BoxPadding {
  val empty = new BoxPadding()

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

sealed case class LineBox(pen: BoxPen = BoxPen.empty,
                          padding : BoxPadding = BoxPadding.empty
                          // style is a fake property, taken from parent "BoxContainer"
                          ) {
  def ++(rhs: LineBox) =
    LineBox(
      pen = this.pen ++ rhs.pen,
      padding = this.padding ++ rhs.padding
    )
}

object LineBox {
  val empty = new LineBox()

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

abstract sealed class LineSpacing extends Transformable[(LineSpacingEnum, Float)]

object LineSpacing {
  /**
   * Normal spacing between lines.
   */
  case object Single extends LineSpacing {
    def transform = ret(LineSpacingEnum.SINGLE, 0)
  }

  /**
   * Spacing between lines of 50% more than normal.
   */
  case object OneAndHalf extends LineSpacing {
    def transform = ret(LineSpacingEnum.ONE_AND_HALF, 0)
  }

  /**
   * Spacing between lines of double size than normal.
   */
  case object Double extends LineSpacing {
    def transform = ret(LineSpacingEnum.DOUBLE, 0)
  }

  /**
   * Spacing between lines of at least the specified size.
   */
  sealed case class AtLeast(size: Float) extends LineSpacing {
    def transform = ret(LineSpacingEnum.AT_LEAST, size)
  }

  /**
   * Spacing between lines of the specified size.
   */
  sealed case class Fixed(size: Float) extends LineSpacing {
    def transform = ret(LineSpacingEnum.FIXED, size)
  }

  /**
   * Spacing between lines to the specified proportion of the normal line spacing.
   */
  sealed case class Proportional(factor: Float) extends LineSpacing {
    def transform = ret(LineSpacingEnum.PROPORTIONAL, factor)
  }
}

sealed case class Paragraph(lineSpacing: Option[LineSpacing] = None,
                            firstLineIndent: Option[Int] = None,
                            leftIndent: Option[Int] = None,
                            rightIndent: Option[Int] = None,
                            spacingBefore: Option[Int] = None,
                            spacingAfter: Option[Int] = None,
                            tabStopWidth: Option[Int] = None,
                            tabStops: Seq[TabStop] = Seq.empty) {
  def ++(rhs: Paragraph) =
    Paragraph(
      lineSpacing = rhs.lineSpacing.orElse(this.lineSpacing),
      firstLineIndent = rhs.firstLineIndent.orElse(this.firstLineIndent),
      leftIndent = rhs.leftIndent.orElse(this.leftIndent),
      rightIndent = rhs.rightIndent.orElse(this.rightIndent),
      spacingBefore = rhs.spacingBefore.orElse(this.spacingBefore),
      spacingAfter = rhs.spacingAfter.orElse(this.spacingAfter),
      tabStopWidth = rhs.tabStopWidth.orElse(this.tabStopWidth),
      /* tabStops - how should they be reasonably combined? */
      tabStops = this.tabStops ++ rhs.tabStops
    )
}

object Paragraph {
  val empty = Paragraph()

  private[core] def put(o: Paragraph, r: JRBaseParagraph) {
    def optFloat(v: Option[Float]) : java.lang.Float = if (v.isDefined) v.get else null
    def optInt(v: Option[Int]) : java.lang.Integer = if (v.isDefined) v.get else null

    r.setFirstLineIndent(optInt(o.firstLineIndent))
    r.setLeftIndent(optInt(o.leftIndent))
    r.setRightIndent(optInt(o.rightIndent))
    r.setSpacingBefore(optInt(o.spacingBefore))
    r.setSpacingAfter(optInt(o.spacingAfter))
    r.setTabStopWidth(optInt(o.tabStopWidth))
    for (t <- o.tabStops)
      r.addTabStop(t)
    if (o.lineSpacing.isDefined)
      drop(o.lineSpacing.get.transform) { case(en, sz) =>
        r.setLineSpacing(en)
        r.setLineSpacingSize(sz)
      }
    else ret()
  }
}


// always transforms to a 'style reference'; the style itself will be added to the transformation state
abstract sealed class AbstractStyle extends Transformable[Option[String]]

sealed case class Style(
                         // name is isDefault intentionally left out (see top level JaperDesign)
                         parentStyle: Option[StyleReference] = None, // for now, only allow manually defined parentStyles; just combine or copy for auto styles
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

  /** combine this style with given style. If a property is set in both styles, the right side replaces the left.
    * This is different from the copy method, in that it is a 'deep' combination. ParentStyle and conditionalStyles
    * can hardly be combined automatically; so don't expect anything of one of the styles uses it. */
  def ++(rhs: Style) =
    Style(
      parentStyle = rhs.parentStyle.orElse(this.parentStyle),
      conditionalStyles = this.conditionalStyles ++ rhs.conditionalStyles,
      backcolor = rhs.backcolor.orElse(this.backcolor),
      forecolor = rhs.forecolor.orElse(this.forecolor),
      font = this.font ++ rhs.font,
      horizontalAlignment = rhs.horizontalAlignment.orElse(this.horizontalAlignment),
      paragraph = this.paragraph ++ rhs.paragraph,
      markup = rhs.markup.orElse(this.markup),
      mode = rhs.mode.orElse(this.mode),
      pattern = rhs.pattern.orElse(this.pattern),
      radius = rhs.radius.orElse(this.radius),
      rotation = rhs.rotation.orElse(this.rotation),
      scaleImage = rhs.scaleImage.orElse(this.scaleImage),
      verticalAlignment = rhs.verticalAlignment.orElse(this.verticalAlignment),
      line = this.line ++ rhs.line,
      box = this.box ++ rhs.box,
      fill = rhs.fill.orElse(this.fill),
      blankWhenNull = rhs.blankWhenNull.orElse(this.blankWhenNull)
    )

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
    r.setHorizontalAlignment(o.horizontalAlignment.getOrElse(null))
    r.setVerticalAlignment(o.verticalAlignment.getOrElse(null))
    r.setMarkup(o.markup.getOrElse(null))
    r.setMode(o.mode.getOrElse(null))
    r.setPattern(o.pattern.getOrElse(null))
    r.setRotation(o.rotation.getOrElse(null))
    r.setFill(o.fill.getOrElse(null))

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

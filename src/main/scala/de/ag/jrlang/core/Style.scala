package de.ag.jrlang.core

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

abstract sealed class Style extends StyleFoldable[Style] {
}

object Style {
  sealed case class Internal(
      // name is isDefault intentionally left out (see top level JaperDesign)
      parentStyle: Option[Style],
      conditionalStyles: Any, // TODO
      backcolor: Option[java.awt.Color],
      forecolor: Option[java.awt.Color],
      font: Font,
      horizontalAlignment: Option[net.sf.jasperreports.engine.`type`.HorizontalAlignEnum],
      paragraph: JRParagraph,
      markup: Option[String], // "none", "styled", "html", "rtf" ??!! (why is that here, not at Text?)
      /** Report elements can either be transparent or opaque, depending on the value
       *  you specify for the mode attribute. The default value for this attribute 
       *  depends on the type of the report element. Graphic elements like rectangles
       *  and lines are opaque by default, while images are transparent. Both static
       *  texts and text fields are transparent by default, and so are the subreport elements. */
      mode: Option[net.sf.jasperreports.engine.`type`.ModeEnum],

      pattern: Option[String],
      radius: Option[Int],
      rotation: Option[net.sf.jasperreports.engine.`type`.RotationEnum],
      scaleImage: Option[net.sf.jasperreports.engine.`type`.ScaleImageEnum],
      verticalAlignment: Option[net.sf.jasperreports.engine.`type`.VerticalAlignEnum],
      fill: Option[net.sf.jasperreports.engine.`type`.FillEnum]
      ) extends Style {
    lazy val obj = Internal.dropNew(this) // TODO: explain -- not needed anymore with global style folding?
    
    def foldStyles(st0: StylesMap) = {
      val (parentStyle_, st1) = StyleFoldable.foldOption(parentStyle, st0);
      // TODO: Check: Problem if we replace parent style here - for comparing styles (as map keys?)
      val nthis = copy(parentStyle = parentStyle_)
      // Special case: don't register an empty style (must be stored as null at usage places!)
      if (nthis.isEmpty)
        (Internal.empty, st1)
      else
        st1.lookup(nthis)
    }
    
    def isEmpty = (this == Internal.empty)
  }
  object Internal {
    val empty =
      new Internal(
          parentStyle = None,
          conditionalStyles = Vector(),
          backcolor = None,
          forecolor = None,
          font = Font.empty,
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
      def optBool(v: Option[Boolean]) : java.lang.Boolean =
        if (v.isDefined) v.get else null
      def optInt(v: Option[Int]) : java.lang.Integer =
        if (v.isDefined) v.get else null
      
      val r = new net.sf.jasperreports.engine.design.JRDesignStyle();
      // name is isDefault are set externally
      //r.setName(o.name);
      //r.setDefault(o.isDefault);
      if (o.parentStyle.isDefined)
        put(o.parentStyle.get, r);
      // TODO: conditional styles?
      r.setBackcolor(o.backcolor.getOrElse(null));
      r.setForecolor(o.forecolor.getOrElse(null));
      // Though the properties are the same, JRStyle does not use a JRFont :-/
      r.setFontName(o.font.fontName.getOrElse(null));
      // the java.lang.Boolean and Integer overloads are different!
      // - null means 'inherit' or 'undefined'
      r.setFontSize(optInt(o.font.fontSize)); // Integer 
      r.setBold(optBool(o.font.bold));
      r.setItalic(optBool(o.font.italic));
      r.setStrikeThrough(optBool(o.font.strikeThrough));
      r.setUnderline(optBool(o.font.underline));
      r.setPdfEncoding(o.font.pdfEncoding.getOrElse(null));
      r.setPdfFontName(o.font.pdfFontName.getOrElse(null));
      r.setPdfEmbedded(optBool(o.font.pdfEmbedded));
      r
    }
  }

  sealed case class External(reference: String) extends Style {
    def foldStyles(st0: StylesMap) = (this, st0)
  }

  def put(src: Style, tgt:net.sf.jasperreports.engine.design.JRDesignElement) = {
    src match {
      case v:Internal => tgt.setStyle(Internal.drop(v))
      case v:External => tgt.setStyleNameReference(v.reference)
    }
  }
  def put(src: Style, tgt:net.sf.jasperreports.engine.design.JRDesignStyle) = {
    src match {
      case v:Internal => tgt.setParentStyle(Internal.drop(v))
      case v:External => tgt.setParentStyleNameReference(v.reference)
    }
  }
}

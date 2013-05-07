package de.ag.jrlang.core

abstract sealed class Style extends StyleFoldable[Style] {
}

object Style {
  sealed case class Internal(
      // name is isDefault intentionally left out (see top level JaperDesign)
      parentStyle: Option[Style],
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
      ) extends Style {
    lazy val obj = Internal.dropNew(this) // TODO: explain -- not needed with global style folding
    
    def foldStyles(st0: StylesMap) = {
      val (parentStyle_, st1) = StyleFoldable.foldOption(parentStyle, st0);
      // TODO: Check: Problem if we replace parent style here - for comparing styles (as map keys?)
      val nthis = copy(parentStyle = parentStyle_)
      st1.lookup(nthis)
    }
  }
  object Internal {
    val empty =
      new Internal(
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
      // name is isDefault are set externally
      //r.setName(o.name);
      //r.setDefault(o.isDefault);
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

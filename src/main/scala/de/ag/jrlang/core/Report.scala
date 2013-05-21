package de.ag.jrlang.core

import net.sf.jasperreports.{engine => jre}
import scala.collection.JavaConversions._

// we have around 40 fields; but Scala functions and case classes cannot have more than 22 parameters :-/
// have split it up somehow...

sealed case class FloatingBand(
    band : Option[Band],
    floating : Boolean)
  extends StyleFoldable[FloatingBand] with EnvCollector {
    def foldStyles(st: StylesMap) = {
      val (band_, st_) = StyleFoldable.foldOption(band, st);
      (copy(band = band_), st_)
    }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    band.collectEnv(e0)
}

object FloatingBand {
  val empty = new FloatingBand(
      band = None,
      floating = false);
}

// same order as in CSS
sealed case class Margins(
    top : Int,
    right : Int,
    bottom : Int,
    left : Int);

sealed case class SummaryBand(
    band : Option[Band],
    newPage : Boolean,
    withPageHeaderAndFooter : Boolean)
  extends StyleFoldable[SummaryBand] with EnvCollector {
    def foldStyles(st: StylesMap) = {
      val (band_, st_) = StyleFoldable.foldOption(band, st);
      (copy(band = band_), st_)
    }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    band.collectEnv(e0)
}


object SummaryBand {
  val empty = new SummaryBand(
      band = None,
      newPage = false,
      withPageHeaderAndFooter = false);
}

sealed case class TitleBand(
    band : Option[Band],
    newPage : Boolean)
  extends StyleFoldable[TitleBand] with EnvCollector {
    def foldStyles(st: StylesMap) = {
      val (band_, st_) = StyleFoldable.foldOption(band, st);
      (copy(band = band_), st_)
    }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    band.collectEnv(e0)
}

object TitleBand {
  val empty = new TitleBand(
      band = None,
      newPage = false);
}

sealed case class Page(
    height : Int,
    width : Int,
    margins : Margins,
    orientation : net.sf.jasperreports.engine.`type`.OrientationEnum,
    footer : Option[Band],
    header : Option[Band],
    background : Option[Band]
) extends StyleFoldable[Page] with EnvCollector {
    def foldStyles(st0: StylesMap) = {
      val (footer_, st1) = StyleFoldable.foldOption(footer, st0);
      val (header_, st2) = StyleFoldable.foldOption(header, st1);
      val (background_, st3) = StyleFoldable.foldOption(background, st2);
      (copy(
          footer = footer_,
          header = header_,
          background = background_),
          st3)
    }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    footer.collectEnv(header.collectEnv(background.collectEnv(e0)))
}

object Page {
  val empty = {
    // very explicit defaults...
    val o = new net.sf.jasperreports.engine.design.JasperDesign();
    new Page(
        height = o.getPageHeight(),
        width = o.getPageWidth(),
        margins = new Margins(
            left = o.getLeftMargin(),
            right = o.getRightMargin(),
            top = o.getTopMargin(),
            bottom = o.getBottomMargin()),
        orientation = o.getOrientationValue(),
        footer = None,
        header = None,
        background = None
    )
  }
}

sealed case class Columns(
    count : Int,
    direction : net.sf.jasperreports.engine.`type`.RunDirectionEnum,
    footer : FloatingBand,
    header : Option[Band],
    spacing : Int,
    width : Int,
    printOrder : net.sf.jasperreports.engine.`type`.PrintOrderEnum
) extends StyleFoldable[Columns] with EnvCollector {
  def foldStyles(st0: StylesMap) = {
     val (footer_, st1) = footer.foldStyles(st0);
     val (header_, st2) = StyleFoldable.foldOption(header, st1)
     (copy(footer = footer_, header = header_),
         st2)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    footer.collectEnv(header.collectEnv(e0))
};

object Columns {
  val empty = {
    // very explicit defaults...
    val o = new net.sf.jasperreports.engine.design.JasperDesign();
    new Columns(
        count = o.getColumnCount(),
        direction = o.getColumnDirection(),
        footer = FloatingBand.empty,
        header = None,
        spacing = o.getColumnSpacing(),
        width = o.getColumnWidth(),
        printOrder = o.getPrintOrderValue()
        )
  }
}

sealed case class Report(
  name : String,
  details: Seq[Band],
  defaultStyle: Style.Internal,
  templates : IndexedSeq[jre.JRReportTemplate],
  subDatasets: Map[String, Dataset],
  mainDataset: Dataset,
  imports : Set[String],
  columns : Columns,
  // formatFactoryClass
  ignorePagination : Boolean,
  language : String, // Java or Groovy
  lastPageFooter : Option[Band],
  noData : Option[Band],
  page : Page,
  summary : SummaryBand,
  title : TitleBand
  // UUID probably not
) extends StyleFoldable[Report] with EnvCollector {
  def foldStyles(st0:StylesMap) = {
    val (details_, st1) = StyleFoldable.foldAll(details, st0);
    val (columns_, st2) = columns.foldStyles(st1);
    val (lastPageFooter_, st3) = StyleFoldable.foldOption(lastPageFooter, st2)
    val (noData_, st4) = StyleFoldable.foldOption(noData, st3)
    val (page_, st5) = page.foldStyles(st4);
    val (summary_, st6) = summary.foldStyles(st5);
    val (title_, st7) = title.foldStyles(st6);
    (copy(
         details = details_,
         columns = columns_,
         lastPageFooter = lastPageFooter_,
         noData = noData_,
         page = page_,
         summary = summary_,
         title = title_
         ),
     st7)
  }

  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    details.collectEnv(
      defaultStyle.collectEnv(
        mainDataset.collectEnv(
          // subDatasets? or treat that differently?
          columns.collectEnv(
            lastPageFooter.collectEnv(
              noData.collectEnv(
                page.collectEnv(
                  summary.collectEnv(
                    title.collectEnv(e0)
                  )
                )
              )
            )
          )
        )
      )
    )
}

object Report {
  def apply(name: String) : Report =
    new Report(
      name = name, // TODO: Validate non-empty
      details = Vector.empty,
      defaultStyle = Style.Internal.empty,
      templates = Vector.empty,
      subDatasets = Map.empty, // TODO: Check if this is a candidate for collecting from subelements (like styles)
      mainDataset = Dataset.empty,
      imports = Set.empty, // "style templates"
      columns = Columns.empty,
      ignorePagination = false,
      language = net.sf.jasperreports.engine.JRReport.LANGUAGE_JAVA,
      lastPageFooter = None,
      noData = None,
      page = Page.empty,
      summary = SummaryBand.empty,
      title = TitleBand.empty
      );

  def translate(o: Report, r: jre.design.JasperDesign, defaultStyleName: String, styles: Seq[(String, Style.Internal)]) = {
    r.setName(o.name);

    r.addStyle({
      val s: net.sf.jasperreports.engine.design.JRDesignStyle = o.defaultStyle;
      s.setName(defaultStyleName);
      s.setDefault(true);
      s
    })
    for ((n,v) <- styles) {
      val s: net.sf.jasperreports.engine.design.JRDesignStyle = v;
      s.setName(n);
      r.addStyle(s);
    }
    for (v <- o.details) r.getDetailSection().asInstanceOf[net.sf.jasperreports.engine.design.JRDesignSection].addBand(v);
    r.getTemplatesList().addAll(o.templates); // why does it work here, but not for the others??
    // TODO for (v <- mainDataset.groups) r.addGroup(v);
    // TODO for (v <- mainDataset.scriptlets) r.addScriptlet(v);
    for (v <- o.mainDataset.parameters) r.addParameter(v);
    // TODO for (v <- mainDataset.variables) r.addVariable(v);
    for ((name, s) <- o.subDatasets) {
      val ds : net.sf.jasperreports.engine.design.JRDesignDataset = s
      ds.setName(name)
      r.addDataset(ds)
    }
    for (s <- o.imports) r.addImport(s); // Java imports for expressions - remove?
    r.setColumnCount(o.columns.count);
    r.setColumnDirection(o.columns.direction);
    r.setColumnFooter(o.columns.footer.band.getOrElse(null));
    r.setFloatColumnFooter(o.columns.footer.floating);
    r.setColumnHeader(o.columns.header.getOrElse(null));
    r.setColumnSpacing(o.columns.spacing);
    r.setColumnWidth(o.columns.width);
    r.setPrintOrder(o.columns.printOrder)
    r.setIgnorePagination(o.ignorePagination);
    r.setLanguage(o.language);
    r.setLastPageFooter(o.lastPageFooter.getOrElse(null));
    r.setNoData(o.noData.getOrElse(null));
    r.setPageHeight(o.page.height);
    r.setPageWidth(o.page.width);
    r.setTopMargin(o.page.margins.top);
    r.setRightMargin(o.page.margins.right);
    r.setBottomMargin(o.page.margins.bottom);
    r.setLeftMargin(o.page.margins.left);
    r.setOrientation(o.page.orientation);
    r.setPageFooter(o.page.footer.getOrElse(null));
    r.setPageHeader(o.page.header.getOrElse(null));
    r.setBackground(o.page.background.getOrElse(null));
    r.setSummary(o.summary.band.getOrElse(null));
    r.setSummaryNewPage(o.summary.newPage);
    r.setSummaryWithPageHeaderAndFooter(o.summary.withPageHeaderAndFooter);
    r.setTitle(o.title.band.getOrElse(null));
    r.setTitleNewPage(o.title.newPage);
    r;
  }
}

package de.ag.jrlang.core

import net.sf.jasperreports.{engine => jre}
import scala.collection.JavaConversions._
import net.sf.jasperreports.engine.design.{JRDesignParameter, JRDesignSection, JasperDesign}

import Transformer._
import Dimensions._

import net.sf.jasperreports.engine.`type`.{PrintOrderEnum, RunDirectionEnum, OrientationEnum}

// we have around 40 fields; but Scala functions and case classes cannot have more than 22 parameters :-/
// have split it up somehow...

sealed case class FloatingBand(
    band : Option[Band] = None,
    floating : Boolean = false)

object FloatingBand {
  val empty = new FloatingBand()
}

/** top and bottom margins can be relative to the page height, left and right relative to the page with */
// same order as in CSS
sealed case class Margins(
    top : RestrictedLength = 0 px,
    right : RestrictedLength = 0 px,
    bottom : RestrictedLength = 0 px,
    left : RestrictedLength = 0 px)

object Margins {
  val none = Margins()

  /** as defined in JRBaseReport.java */
  val default = Margins(left=20 px, right=20 px, top=30 px, bottom=30 px)
}

sealed case class SummaryBand(
    band : Option[Band] = None,
    newPage : Boolean = false,
    withPageHeaderAndFooter : Boolean = false)

object SummaryBand {
  val empty = new SummaryBand()
}

sealed case class TitleBand(
    band : Option[Band] = None,
    newPage : Boolean = false)

object TitleBand {
  val empty = TitleBand()
}

/** Orientation is used mostly to inform printers of page layouts. */
sealed case class PageFormat(width: Length, height: Length, orientation: OrientationEnum) {
  def quotient : Double = height / width
}
object PageFormat {
  /** DIN A4 portrait */
  val A4portrait = PageFormat(210 mm, 297 mm, OrientationEnum.PORTRAIT)
  val A4 = A4portrait
  /** DIN A4 landscape */
  val A4landscape = PageFormat(297 mm, 210 mm, OrientationEnum.LANDSCAPE)
}

sealed case class Page(
    format: PageFormat = PageFormat.A4,
    margins : Margins = Margins.default,
    footer : Option[Band] = None,
    header : Option[Band] = None,
    background : Option[Band] = None)

object Page {
  val empty = Page()
}

sealed case class Columns(
    count : Int = 1,
    direction : RunDirectionEnum = RunDirectionEnum.LTR,
    footer : FloatingBand = FloatingBand.empty,
    header : Option[Band] = None,
    spacing : Length = 0 px, // between columns

    /* 100% = page width - left margin - right margin - (spacing * (column count - 1)) */
    width : RestrictedLength = 100 percent, // 555 is jasper's default, which should be 100%...?!
    /** only relevant when count > 1 */
    printOrder : PrintOrderEnum = PrintOrderEnum.VERTICAL
)

object Columns {
  val empty = Columns()
}

sealed case class Report(
  name : String, // TODO: Validate non-empty
  details: Seq[Band] = Vector.empty,
  defaultStyle: Style = Style.empty,
  styles: Map[String, Style] = Map.empty, // additional user-defined styles, usually not needed
  templates : IndexedSeq[jre.JRReportTemplate] = Vector.empty,
  subDatasets: Map[String, Dataset] = Map.empty,
  mainDataset: Dataset = Dataset.empty,
  imports : Set[String] = Set.empty,
  columns : Columns = Columns.empty,
  // formatFactoryClass
  ignorePagination : Boolean = false,
  language : String = net.sf.jasperreports.engine.JRReport.LANGUAGE_JAVA, // Java or Groovy
  lastPageFooter : Option[Band] = None,
  noData : Option[Band] = None,
  page : Page = Page.empty,
  summary : SummaryBand = SummaryBand.empty,
  title : TitleBand = TitleBand.empty
  // UUID probably not
) extends Transformable[JasperDesign] {

  private def absoluteRightMargin = page.margins.right asPartOf page.format.width
  private def absoluteLeftMargin = page.margins.left asPartOf page.format.width
  private[core] def absoluteColumnWidth = columns.width.asPartOf(page.format.width - absoluteLeftMargin - absoluteRightMargin - (columns.spacing * (columns.count - 1)))


  def transform = {
    val r = new JasperDesign()
    //(o: Report, r: jre.design.JasperDesign, defaultStyleName: String, styles: Seq[(String, Style.Internal)]) = {
    // simple properties first that need not run in the monad:
    r.setName(name)
    r.setPageHeight(page.format.height.inAbsolutePixels)
    r.setPageWidth(page.format.width.inAbsolutePixels)
    r.setTopMargin(page.margins.top asPartOf page.format.height inAbsolutePixels)
    r.setRightMargin(absoluteRightMargin inAbsolutePixels)
    r.setBottomMargin(page.margins.bottom asPartOf page.format.height inAbsolutePixels)
    r.setLeftMargin(absoluteLeftMargin inAbsolutePixels)
    r.setOrientation(page.format.orientation)
    r.setColumnCount(columns.count)
    r.setColumnDirection(columns.direction)
    r.setFloatColumnFooter(columns.footer.floating)
    r.setColumnSpacing(columns.spacing inAbsolutePixels)
    r.setColumnWidth(absoluteColumnWidth inAbsolutePixels)
    r.setPrintOrder(columns.printOrder)
    r.setIgnorePagination(ignorePagination)
    r.setLanguage(language)
    r.setSummaryNewPage(summary.newPage)
    r.setSummaryWithPageHeaderAndFooter(summary.withPageHeaderAndFooter)
    r.getTemplatesList.addAll(templates)
    imports foreach { r.addImport(_) } // Java imports for expressions - remove?
    r.setTitleNewPage(title.newPage)

    // monadic transformation...

    // user defined styles (generated styles are added by caller)
    drop(defaultStyle.mkDesignStyle) { s => s.setName("default"); s.setDefault(true); r.setDefaultStyle(s) }
    (all(styles map { case(n,s) => s.mkDesignStyle >>= { js => js.setName(n); ret(js) } } toSeq) >>= {
      sts => sts foreach { r.addStyle(_) }; ret()
    }) >>
    (all(details map {_.transform}) >>= {
      bands => bands foreach { r.getDetailSection.asInstanceOf[JRDesignSection].addBand(_) }; ret()
    }) >>
    // user defined datasets, generated datasets are added by caller
    (all(subDatasets map {
      case(n,d) => d.transform >>= { o => o.setName(n); ret(o) }
    } toList) >>= {
      ds => ds foreach { r.addDataset(_) }; ret()
    }) >>
    drop(orNull(columns.footer.band map {_.transform})) { r.setColumnFooter(_) } >>
    drop(orNull(columns.header map {_.transform})) { r.setColumnHeader(_) } >>
    drop(orNull(lastPageFooter map {_.transform})) { r.setLastPageFooter(_) } >>
    drop(orNull(noData map {_.transform})) { r.setNoData(_) } >>
    drop(orNull(page.footer map {_.transform})) { r.setPageFooter(_) } >>
    drop(orNull(page.header map {_.transform})) { r.setPageHeader(_) } >>
    drop(orNull(page.background map {_.transform})) { r.setBackground(_) } >>
    drop(orNull(summary.band map {_.transform})) { r.setSummary(_) } >>
    drop(orNull(title.band map {_.transform})) { r.setTitle(_) } >>
    mainDataset.fill(r.getMainDesignDataset) >> // must be last!
    ret(r)
  }

}

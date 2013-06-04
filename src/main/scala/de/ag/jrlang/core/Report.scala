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
    band : Band,
    floating : Boolean = false)

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
    band : Band,
    newPage : Boolean = false,
    // probably only relevant when newPage=true... (TODO encode that?)
    withPageHeaderAndFooter : Boolean = false)

sealed case class TitleBand(
    band : Band,
    newPage : Boolean = false)

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

sealed case class Columns(count : Int = 1,
                          direction : RunDirectionEnum = RunDirectionEnum.LTR,
                          footer : Option[FloatingBand] = None,
                          header : Option[Band] = None,
                          spacing : Length = 0 px, // between columns

                          /* 100% = page width - left margin - right margin - (spacing * (column count - 1)) */
                          width : RestrictedLength = 100 percent, // 555 is jasper's default, which should be 100%...?!
                          printOrder : PrintOrderEnum = PrintOrderEnum.VERTICAL
                          )

object Columns {
  val singleColumn = Columns(count=1)
}

sealed case class Page(
    format: PageFormat = PageFormat.A4,
    margins : Margins = Margins.default,
    footer : Option[Band] = None,
    header : Option[Band] = None,
    background : Option[Band] = None,
    columns: Columns = Columns.singleColumn)

object Page {
  val default = Page()
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
  // formatFactoryClass
  ignorePagination : Boolean = false,
  language : String = net.sf.jasperreports.engine.JRReport.LANGUAGE_JAVA, // Java or Groovy
  lastPageFooter : Option[Band] = None,
  noData : Option[Band] = None,
  page : Page = Page.default,
  summary : Option[SummaryBand] = None,
  title : Option[TitleBand] = None
  // UUID probably not
) extends Transformable[JasperDesign] {

  private def absoluteRightMargin = page.margins.right asPartOf page.format.width
  private def absoluteLeftMargin = page.margins.left asPartOf page.format.width
  private def absolutePageContentWith = page.format.width - absoluteLeftMargin - absoluteRightMargin

  private[core] def absoluteColumnWidth = {
    val allColumnWidth = absolutePageContentWith - (page.columns.spacing * (page.columns.count - 1))
    page.columns.width.asPartOf(allColumnWidth / page.columns.count)
  }


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
    r.setColumnCount(page.columns.count)
    r.setColumnDirection(page.columns.direction)
    r.setFloatColumnFooter(page.columns.footer map {_.floating} getOrElse(false))
    r.setColumnSpacing(page.columns.spacing inAbsolutePixels)
    r.setColumnWidth(absoluteColumnWidth inAbsolutePixels)
    r.setPrintOrder(page.columns.printOrder)
    r.setIgnorePagination(ignorePagination)
    r.setLanguage(language)
    r.setSummaryNewPage(summary map { _.newPage } getOrElse(false))
    r.setSummaryWithPageHeaderAndFooter(summary map { _.withPageHeaderAndFooter } getOrElse(false))
    r.getTemplatesList.addAll(templates)
    imports foreach { r.addImport(_) } // Java imports for expressions - remove?
    r.setTitleNewPage(title map { _.newPage } getOrElse(false))

    // monadic transformation...

    // user defined styles (generated styles are added by caller)
    drop(defaultStyle.mkDesignStyle) { s => s.setName("default"); s.setDefault(true); r.setDefaultStyle(s) }
    (all(styles map { case(n,s) => s.mkDesignStyle >>= { js => js.setName(n); ret(js) } } toSeq) >>= {
      sts => sts foreach { r.addStyle(_) }; ret()
    }) >>
    // user defined datasets, generated datasets are added by caller
    (all(subDatasets map {
      case(n,d) => d.transform >>= { o => o.setName(n); ret(o) }
    } toList) >>= {
      ds => ds foreach { r.addDataset(_) }; ret()
    }) >>
    (withContainerWidth(absoluteColumnWidth) {
      // although jasper allows details to be wider than the column, you usually want 100% to mean that width
      (all(details map {_.transform}) >>= {
        bands => bands foreach { r.getDetailSection.asInstanceOf[JRDesignSection].addBand(_) }; ret()
      }) >>
      drop(orNull(page.columns.footer map {_.band.transform})) { r.setColumnFooter(_) } >>
      drop(orNull(page.columns.header map {_.transform})) { r.setColumnHeader(_) }
    }) >>
    (withContainerWidth(absolutePageContentWith) {
      drop(orNull(lastPageFooter map {_.transform})) { r.setLastPageFooter(_) } >>
      drop(orNull(noData map {_.transform})) { r.setNoData(_) } >>
      drop(orNull(page.footer map {_.transform})) { r.setPageFooter(_) } >>
      drop(orNull(summary map {_.band.transform})) { r.setSummary(_) } >>
      drop(orNull(title map {_.band.transform})) { r.setTitle(_) } >>
      drop(orNull(page.header map {_.transform})) { r.setPageHeader(_) } >>
      drop(orNull(page.background map {_.transform})) { r.setBackground(_) }
    }) >>
    mainDataset.fill(r.getMainDesignDataset) >> // must be last!
    ret(r)
  }

}

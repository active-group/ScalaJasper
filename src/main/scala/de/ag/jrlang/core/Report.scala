package de.ag.jrlang.core

import net.sf.jasperreports.{engine => jre}
import scala.collection.JavaConversions._
import net.sf.jasperreports.engine.design.{JRDesignParameter, JRDesignSection, JasperDesign}

import Transformer._

// we have around 40 fields; but Scala functions and case classes cannot have more than 22 parameters :-/
// have split it up somehow...

sealed case class FloatingBand(
    band : Option[Band] = None,
    floating : Boolean = false)

object FloatingBand {
  val empty = new FloatingBand()
}

// same order as in CSS
sealed case class Margins(
    top : Int,
    right : Int,
    bottom : Int,
    left : Int)

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

sealed case class Page(
    height : Int,
    width : Int,
    margins : Margins,
    orientation : net.sf.jasperreports.engine.`type`.OrientationEnum,
    footer : Option[Band],
    header : Option[Band],
    background : Option[Band])

object Page {
  val empty = {
    // very explicit defaults...
    val o = new net.sf.jasperreports.engine.design.JasperDesign();
    new Page(
        height = o.getPageHeight,
        width = o.getPageWidth,
        margins = new Margins(
            left = o.getLeftMargin,
            right = o.getRightMargin,
            top = o.getTopMargin,
            bottom = o.getBottomMargin),
        orientation = o.getOrientationValue,
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
    printOrder : net.sf.jasperreports.engine.`type`.PrintOrderEnum)

object Columns {
  val empty = {
    // very explicit defaults...
    val o = new net.sf.jasperreports.engine.design.JasperDesign();
    new Columns(
        count = o.getColumnCount,
        direction = o.getColumnDirection,
        footer = FloatingBand.empty,
        header = None,
        spacing = o.getColumnSpacing,
        width = o.getColumnWidth,
        printOrder = o.getPrintOrderValue
        )
  }
}

sealed case class Report(
  name : String, // TODO: Validate non-empty
  details: Seq[Band] = Vector.empty,
  defaultStyle: Style.Internal = Style.Internal.empty,
  styles: Map[String, Style.Internal] = Map.empty, // additional user-defined styles, usually not needed
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
  def transform = {
    val r = new JasperDesign()
    //(o: Report, r: jre.design.JasperDesign, defaultStyleName: String, styles: Seq[(String, Style.Internal)]) = {
    // simple properties first that need not run in the monad:
    r.setName(name)
    r.setPageHeight(page.height)
    r.setPageWidth(page.width)
    r.setTopMargin(page.margins.top)
    r.setRightMargin(page.margins.right)
    r.setBottomMargin(page.margins.bottom)
    r.setLeftMargin(page.margins.left)
    r.setOrientation(page.orientation)
    r.setColumnCount(columns.count)
    r.setColumnDirection(columns.direction)
    r.setFloatColumnFooter(columns.footer.floating)
    r.setColumnSpacing(columns.spacing)
    r.setColumnWidth(columns.width)
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
    mainDataset.fill(r.getMainDesignDataset) >>
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
    ret(r)
  }

}

package de.ag.jrlang.core

//import net.sf.jasperreports.engine.JRParameter
import net.sf.jasperreports.engine.design.{JasperDesign => JD}
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

// we have around 40 fields; but Scala functions and case classes cannot have more than 22 parameters :-/
// have split it up somehow...

case class FloatingBand(
    band : net.sf.jasperreports.engine.JRBand,
    floating : Boolean);

// same order as in CSS
case class Margins(
    top : Int,
    right : Int,
    bottom : Int,
    left : Int);

case class SummaryBand(
    band : net.sf.jasperreports.engine.JRBand,
    newPage : Boolean,
    withPageHeaderAndFooter : Boolean);

case class TitleBand(
    band : net.sf.jasperreports.engine.JRBand,
    newPage : Boolean);

case class Pages(
    height : Int,
    width : Int,
    margins : Margins,
    orientation : net.sf.jasperreports.engine.`type`.OrientationEnum,
    footer : net.sf.jasperreports.engine.JRBand,
    header : net.sf.jasperreports.engine.JRBand,
    background : net.sf.jasperreports.engine.JRBand
);

case class Columns(
    count : Int,
    direction : net.sf.jasperreports.engine.`type`.RunDirectionEnum,
    footer : FloatingBand,
    header : net.sf.jasperreports.engine.JRBand,
    spacing : Int,
    width : Int,
    printOrder : net.sf.jasperreports.engine.`type`.PrintOrderEnum
);

case class Data(
    datasets: IndexedSeq[net.sf.jasperreports.engine.design.JRDesignDataset],  // Map-Like
    fields : Seq[net.sf.jasperreports.engine.JRField], // Map-Like
    sortFields : Seq[net.sf.jasperreports.engine.JRSortField]
  // setQuery?
    )

case class JasperDesign(
  name : String,
  styles : IndexedSeq[net.sf.jasperreports.engine.JRStyle], // Map-Like
  templates : IndexedSeq[net.sf.jasperreports.engine.JRReportTemplate],
  groups : Seq[net.sf.jasperreports.engine.design.JRDesignGroup], // Map-Like
  scriptlets : IndexedSeq[net.sf.jasperreports.engine.JRScriptlet], // Map-Like
  parameters : Seq[net.sf.jasperreports.engine.JRParameter], // without system parameters!  // Map-Like
  variables : Seq[net.sf.jasperreports.engine.design.JRDesignVariable], // without system variables!  // Map-Like
  imports : Set[String],
  columns : Columns,
  // ?? def defaultStyle()
  // filterExpression
  // formatFactoryClass
  ignorePagination : Boolean,
  language : String, // Java or Groovy
  lastPageFooter : net.sf.jasperreports.engine.JRBand,
  // mainDataset??
  noData : net.sf.jasperreports.engine.JRBand,
  pages : Pages,
  // resourceBundle?
  // setScripletClass?
  summary : SummaryBand,
  title : TitleBand
  // UUID?
) {
  lazy val obj : JD = {
    def l[J, T](tgt : java.util.List[J], src: Seq[T]) : Unit =
      //tgt.addAll(src asJava)
      // TODO: Optimize
      for (s <- src) tgt.add(s.asInstanceOf[J])
    
    val r = new JD();
    r.setName(name);
    l(r.getStylesList(), styles);
    //l(r.getTemplatesList(), templates);
    r.getTemplatesList().addAll(templates); // why does it work here, but not for the others??
    l(r.getGroupsList(), groups);
    l(r.getScriptletsList(), scriptlets);
    l(r.getParametersList(), parameters);
    l(r.getVariablesList(), variables);
    for (s <- imports) r.addImport(s);
    r.setColumnCount(columns.count);
    r.setColumnDirection(columns.direction);
    r.setColumnFooter(columns.footer.band);
    r.setFloatColumnFooter(columns.footer.floating);
    r.setColumnHeader(columns.header);
    r.setColumnSpacing(columns.spacing);
    r.setColumnWidth(columns.width);
    r.setPrintOrder(columns.printOrder)
    r.setIgnorePagination(ignorePagination);
    r.setLanguage(language);
    r.setLastPageFooter(lastPageFooter);
    r.setNoData(noData);
    r.setPageHeight(pages.height);
    r.setPageWidth(pages.width);
    r.setTopMargin(pages.margins.top);
    r.setRightMargin(pages.margins.right);
    r.setBottomMargin(pages.margins.bottom);
    r.setLeftMargin(pages.margins.left);
    r.setOrientation(pages.orientation);
    r.setPageFooter(pages.footer);
    r.setPageHeader(pages.header);
    r.setBackground(pages.background);
    r.setSummary(summary.band);
    r.setSummaryNewPage(summary.newPage);
    r.setSummaryWithPageHeaderAndFooter(summary.withPageHeaderAndFooter);
    r.setTitle(title.band);
    r.setTitleNewPage(title.newPage);

    r;
  }
  
  def systemParameters : Seq[net.sf.jasperreports.engine.JRParameter] =
    // obj.getParametersList() filter { p : Any => p.asInstanceOf[net.sf.jasperreports.engine.JRParameter].isSystemDefined() };
    obj.getParametersList().asInstanceOf[List[net.sf.jasperreports.engine.JRParameter]] filter { p => p.isSystemDefined() };
  
  def systemVariables : Seq[net.sf.jasperreports.engine.JRVariable] =
    obj.getVariablesList().asInstanceOf[List[net.sf.jasperreports.engine.JRVariable]] filter { p => p.isSystemDefined() };
}

object JasperDesign {
  def apply() : JasperDesign =
    apply(new JD())
    
  /*private implicit def liftToVector[T](a : Array[T]) : Vector[T] =
    (new Traversable[T] {
      def foreach[U](f : T => U): Unit =
        for (i <- 0 until a.length) f(a(i))
    }).toList*/
  
  def vec[J, T](a : Traversable[J]) : Vector[T] =
    a.foldLeft(Vector[T]()) { (r, v) => v.asInstanceOf[T] +: r }
    
  def apply(o: JD) = new JasperDesign(
      name = o.getName(),
      styles = o.getStyles(),
      templates = o.getTemplates(),
      groups = vec(o.getGroups()),
      scriptlets = o.getScriptlets(),
      parameters = o.getParameters() filter { p => !p.isSystemDefined() }, // without system parameters!
      variables = vec(o.getVariables() filter { p => !p.isSystemDefined() }), // without system variables!
      columns = Columns(
          count = o.getColumnCount(),
          direction = o.getColumnDirection(),
          footer = FloatingBand(o.getColumnFooter(), o.isFloatColumnFooter()),
          header = o.getColumnHeader(),
          spacing = o.getColumnSpacing(),
          width = o.getColumnWidth(),
          printOrder = o.getPrintOrderValue()
          ),
      imports = (if (o.getImports() == null) Set.empty else o.getImports() toSet),
      // ?? def defaultStyle()
      // filterExpression
      // formatFactoryClass
      ignorePagination = o.isIgnorePagination(),
      language = o.getLanguage(),
      lastPageFooter = o.getLastPageFooter(),
      // mainDataset??
      noData = o.getNoData(),
      pages = Pages(
          height = o.getPageHeight(),
          width = o.getPageWidth(),
          margins = Margins(
              top = o.getTopMargin(),
              right = o.getRightMargin(),
              bottom = o.getBottomMargin(),
              left = o.getLeftMargin()),
          orientation = o.getOrientationValue(),
          footer = o.getPageFooter(),
          header = o.getPageHeader(),
          background = o.getBackground()
          ),
      // resourceBundle?
      // setScripletClass?
      summary = SummaryBand(
          band = o.getSummary(),
          newPage = o.isSummaryNewPage(),
          withPageHeaderAndFooter = o.isSummaryWithPageHeaderAndFooter()),
      title = TitleBand(
          band = o.getTitle(),
          newPage = o.isTitleNewPage())
  );

  /** o will never be modified in an observable way */
  implicit def lift(o : JD) : JasperDesign = JasperDesign(o)
  
  implicit def drop(o : JasperDesign) : JD = o.obj;
}

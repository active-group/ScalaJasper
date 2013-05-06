package de.ag.jrlang.core

//import net.sf.jasperreports.engine.JRParameter
import net.sf.jasperreports.engine.design.{JasperDesign => JD}
import de.ag.jrlang.core.Columns;
import de.ag.jrlang.core.FloatingBand;
import de.ag.jrlang.core.JRDesignDataset;
import de.ag.jrlang.core.Pages;
import de.ag.jrlang.core.SummaryBand;
import de.ag.jrlang.core.TitleBand;
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

// we have around 40 fields; but Scala functions and case classes cannot have more than 22 parameters :-/
// have split it up somehow...

sealed case class FloatingBand(
    band : Option[JRDesignBand],
    floating : Boolean);

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
    band : Option[JRDesignBand],
    newPage : Boolean,
    withPageHeaderAndFooter : Boolean);

object SummaryBand {
  val empty = new SummaryBand(
      band = None,
      newPage = false,
      withPageHeaderAndFooter = false);
}

sealed case class TitleBand(
    band : Option[JRDesignBand],
    newPage : Boolean);

object TitleBand {
  val empty = new TitleBand(
      band = None,
      newPage = false);
}

sealed case class Pages(
    height : Int,
    width : Int,
    margins : Margins,
    orientation : net.sf.jasperreports.engine.`type`.OrientationEnum,
    footer : Option[JRDesignBand],
    header : Option[JRDesignBand],
    background : Option[JRDesignBand]
);
object Pages {
  val empty = {
    // very explicit defaults...
    val o = new net.sf.jasperreports.engine.design.JasperDesign();
    new Pages(
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
    header : Option[JRDesignBand],
    spacing : Int,
    width : Int,
    printOrder : net.sf.jasperreports.engine.`type`.PrintOrderEnum
);

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

sealed case class JasperDesign(
  name : String,
  details: Seq[JRDesignBand],
  styles : IndexedSeq[JRStyle.Internal], // Map-Like
  templates : IndexedSeq[net.sf.jasperreports.engine.JRReportTemplate],
  subDatasets: Map[String, JRDesignDataset],
  mainDataset: JRDesignDataset,
  imports : Set[String],
  columns : Columns,
  // ?? def defaultStyle()
  // formatFactoryClass
  ignorePagination : Boolean,
  language : String, // Java or Groovy
  lastPageFooter : Option[JRDesignBand],
  noData : Option[JRDesignBand],
  pages : Pages,
  summary : SummaryBand,
  title : TitleBand
  // UUID?
) {
  def drop : net.sf.jasperreports.engine.design.JasperDesign = {
    val r = new JD();
    r.setName(name);
    for (v <- details) r.getDetailSection().asInstanceOf[net.sf.jasperreports.engine.design.JRDesignSection].addBand(v);
    for (v <- styles) r.addStyle(v);
    r.getTemplatesList().addAll(templates); // why does it work here, but not for the others??
    // TODO for (v <- mainDataset.groups) r.addGroup(v);
    // TODO for (v <- mainDataset.scriptlets) r.addScriptlet(v);
    for (v <- mainDataset.parameters) r.addParameter(v);
    // TODO for (v <- mainDataset.variables) r.addVariable(v);
    // TODO subDatasets
    for (s <- imports) r.addImport(s);
    r.setColumnCount(columns.count);
    r.setColumnDirection(columns.direction);
    r.setColumnFooter(columns.footer.band.getOrElse(null));
    r.setFloatColumnFooter(columns.footer.floating);
    r.setColumnHeader(columns.header.getOrElse(null));
    r.setColumnSpacing(columns.spacing);
    r.setColumnWidth(columns.width);
    r.setPrintOrder(columns.printOrder)
    r.setIgnorePagination(ignorePagination);
    r.setLanguage(language);
    r.setLastPageFooter(lastPageFooter.getOrElse(null));
    r.setNoData(noData.getOrElse(null));
    r.setPageHeight(pages.height);
    r.setPageWidth(pages.width);
    r.setTopMargin(pages.margins.top);
    r.setRightMargin(pages.margins.right);
    r.setBottomMargin(pages.margins.bottom);
    r.setLeftMargin(pages.margins.left);
    r.setOrientation(pages.orientation);
    r.setPageFooter(pages.footer.getOrElse(null));
    r.setPageHeader(pages.header.getOrElse(null));
    r.setBackground(pages.background.getOrElse(null));
    r.setSummary(summary.band.getOrElse(null));
    r.setSummaryNewPage(summary.newPage);
    r.setSummaryWithPageHeaderAndFooter(summary.withPageHeaderAndFooter);
    r.setTitle(title.band.getOrElse(null));
    r.setTitleNewPage(title.newPage);

    r;
  }
  
  
  /* TODO: These are static
  def systemParameters : Seq[net.sf.jasperreports.engine.JRParameter] =
    // obj.getParametersList() filter { p : Any => p.asInstanceOf[net.sf.jasperreports.engine.JRParameter].isSystemDefined() };
    obj.getParametersList().asInstanceOf[List[net.sf.jasperreports.engine.JRParameter]] filter { p => p.isSystemDefined() };
  
  def systemVariables : Seq[net.sf.jasperreports.engine.JRVariable] =
    obj.getVariablesList().asInstanceOf[List[net.sf.jasperreports.engine.JRVariable]] filter { p => p.isSystemDefined() };

  */ 
}

object JasperDesign {
  def apply(name: String) : JasperDesign =
    new JasperDesign(
      name = name, // TODO: Validate non-empty
      details = Vector.empty,
      styles = Vector.empty,
      templates = Vector.empty,
      subDatasets = Map.empty,
      mainDataset = JRDesignDataset.empty,
      imports = Set.empty,
      columns = Columns.empty,
      ignorePagination = false,
      language = net.sf.jasperreports.engine.JRReport.LANGUAGE_JAVA,
      lastPageFooter = None,
      noData = None,
      pages = Pages.empty,
      summary = SummaryBand.empty,
      title = TitleBand.empty
      );

  /*
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

  */
}

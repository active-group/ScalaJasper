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

case class StylesMap(
    predefined: Map[JRStyle.Internal, JRStyle.External],
    styles : Map[JRStyle.Internal, JRStyle.External]) {
  def lookup(style: JRStyle.Internal): (JRStyle.External, StylesMap) = {
    val pre = predefined.get(style);
    if (pre.isDefined)
      (pre.get, this)
    else {
      val e = styles.getOrElse(style, JRStyle.External(StylesMap.newName(styles)));
      (e, copy(styles = styles.updated(style, e)))
    }
  }
  
  def list: Seq[(String, JRStyle.Internal)] =
    styles.map({ case(i,e) => (e.reference, i) }) toSeq
}
object StylesMap {
  def apply(predefined: Map[String, JRStyle.Internal]) =
    new StylesMap(predefined map { case(n, i) => (i, JRStyle.External(n)) }, Map.empty)    
  
  private def newName(styles : Map[JRStyle.Internal, JRStyle.External]) : String =
    "autodef" + (styles.size) // could be a little more intelligent
}

trait StyleFoldable[T] {
  def foldStyles(st: StylesMap): (T, StylesMap)
}
object StyleFoldable {
  
  def foldAll[T <: StyleFoldable[T]](coll : Seq[T], st: StylesMap) : (Vector[T], StylesMap) =
    coll.foldLeft((Vector.empty:Vector[T], st)) { case((c, st), v) => {
      val (v_, st_) = v.foldStyles(st);
      (c :+ v_, st_)
    }}
  
  def foldOption[T <: StyleFoldable[T]](v: Option[T], st: StylesMap) =
    if (v.isDefined) {
      val (v_, st_) = v.get.foldStyles(st)
      (Some(v_), st_)
    }
    else (None, st)
      
}

// we have around 40 fields; but Scala functions and case classes cannot have more than 22 parameters :-/
// have split it up somehow...

sealed case class FloatingBand(
    band : Option[JRDesignBand],
    floating : Boolean)
  extends StyleFoldable[FloatingBand] {
    def foldStyles(st: StylesMap) = {
      val (band_, st_) = StyleFoldable.foldOption(band, st);
      (copy(band = band_), st_)
    }
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
    band : Option[JRDesignBand],
    newPage : Boolean,
    withPageHeaderAndFooter : Boolean)
  extends StyleFoldable[SummaryBand] {
    def foldStyles(st: StylesMap) = {
      val (band_, st_) = StyleFoldable.foldOption(band, st);
      (copy(band = band_), st_)
    }
  }


object SummaryBand {
  val empty = new SummaryBand(
      band = None,
      newPage = false,
      withPageHeaderAndFooter = false);
}

sealed case class TitleBand(
    band : Option[JRDesignBand],
    newPage : Boolean)
  extends StyleFoldable[TitleBand] {
    def foldStyles(st: StylesMap) = {
      val (band_, st_) = StyleFoldable.foldOption(band, st);
      (copy(band = band_), st_)
    }
  }

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
) extends StyleFoldable[Pages] {
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
  }

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
) extends StyleFoldable[Columns] {
  def foldStyles(st0: StylesMap) = {
     val (footer_, st1) = footer.foldStyles(st0);
     val (header_, st2) = StyleFoldable.foldOption(header, st1)
     (copy(footer = footer_, header = header_),
         st2)
  }
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

sealed case class JasperDesign(
  name : String,
  details: Seq[JRDesignBand],
  // styles : IndexedSeq[JRStyle.Internal], // Map-Like
  defaultStyle: JRStyle.Internal,
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
) // noone needs to know: extends StyleFoldable[JasperDesign]
{
  private def foldStyles(st0: StylesMap) = {
    val (details_, st1) = StyleFoldable.foldAll(details, st0);
    val (columns_, st2) = columns.foldStyles(st1);
    val (lastPageFooter_, st3) = StyleFoldable.foldOption(lastPageFooter, st2)
    val (noData_, st4) = StyleFoldable.foldOption(noData, st3)
    val (pages_, st5) = pages.foldStyles(st4);
    val (summary_, st6) = summary.foldStyles(st5);
    val (title_, st7) = title.foldStyles(st6);
    (copy(
         details = details_,
         columns = columns_,
         lastPageFooter = lastPageFooter_,
         noData = noData_,
         pages = pages_,
         summary = summary_,
         title = title_
         ),
     st7)
  }
  
  def drop : net.sf.jasperreports.engine.design.JasperDesign =
    JasperDesign.dropNew(this)


  
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
      defaultStyle = JRStyle.Internal.empty,
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

  private def dropNew(o: JasperDesign) = {
    // 1. compilation step: collect all styles used, replacing them with uniquely generated names.
    val predef = Map("default" -> o.defaultStyle);
    val (o_, styles) = o.foldStyles(StylesMap(predef));
    // o should now contain only style-references ("External"), no internal styles anymore
    dropNew2(o_, styles.list) // well, could include predef, if identifying default correctly
  }
  
  private def dropNew2(o: JasperDesign, styles: Seq[(String, JRStyle.Internal)]) = {
    val r = new JD();

    r.setName(o.name);

    r.addStyle({
      val s: net.sf.jasperreports.engine.design.JRDesignStyle = o.defaultStyle;
      s.setName("default"); // FIXME name defined above already
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
    // TODO subDatasets
    for (s <- o.imports) r.addImport(s);
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
    r.setPageHeight(o.pages.height);
    r.setPageWidth(o.pages.width);
    r.setTopMargin(o.pages.margins.top);
    r.setRightMargin(o.pages.margins.right);
    r.setBottomMargin(o.pages.margins.bottom);
    r.setLeftMargin(o.pages.margins.left);
    r.setOrientation(o.pages.orientation);
    r.setPageFooter(o.pages.footer.getOrElse(null));
    r.setPageHeader(o.pages.header.getOrElse(null));
    r.setBackground(o.pages.background.getOrElse(null));
    r.setSummary(o.summary.band.getOrElse(null));
    r.setSummaryNewPage(o.summary.newPage);
    r.setSummaryWithPageHeaderAndFooter(o.summary.withPageHeaderAndFooter);
    r.setTitle(o.title.band.getOrElse(null));
    r.setTitleNewPage(o.title.newPage);
    r;
  }
  
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

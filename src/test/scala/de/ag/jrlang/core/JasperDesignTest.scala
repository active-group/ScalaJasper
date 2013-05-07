package de.ag.jrlang.core

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import de.ag.jrlang.core._

@RunWith(classOf[JUnitRunner])
class JasperDesignTest extends FunSuite {


  def testPrint(d : JasperDesign) = {
    val r = JasperDesignTest.compile(d)
    val p = JasperDesignTest.print(r);
    //val bytes = net.sf.jasperreports.engine.JasperExportManager.exportReportToPdf(p);
    // it's not easily verifiable, because it contains current timestamps ("of course")
    // printf("%s", new String(bytes))
    //net.sf.jasperreports.engine.JasperPrintManager.printReport(p, true, )
    //net.sf.jasperreports.engine.JasperExportManager.exportReportToPdfFile(p, "/Users/frese/tmp/test.pdf");
    val s = net.sf.jasperreports.engine.JasperExportManager.exportReportToXml(p)
    printf("%s", s);
  }

  test("empty report") {
    // running from sbt crashes with some classloader/resources bug,
    // running from eclipse works ("of course"); so hard to debug.
    val r = JasperDesign("empty report")
    testPrint(r);
  }

  test("simple report") {
    // TODO: which style defs are mandatory?
    val mystyle = JRStyle.Internal.empty.copy(
        font = JRFont.empty.copy(
            //fontName = Some("DejaVu Sans"),
            fontSize = Some(12),
            pdfFontName = Some("Helvetica"),
            pdfEncoding = Some("Cp1252"),
            pdfEmbedded = Some(false))
        );
    val rect = JRCommon.empty.copy(
        x = 0, y = 0,
        width = 55, height = 15,
        //style = Some(JRStyle.External(reference="mystyle")), // only the name is references, as it seems
        
        style = Some(mystyle.copy(font = mystyle.font.copy(fontSize = Some(8)))),
        
        // Experiment: We could use 'parentStyle' for every copy that is made automatically...?!
        //style = Some(JRStyle.Internal.empty.copy(
        //    parentStyle = Some(mystyle),
        //    font = JRFont.empty.copy(fontSize = Some(8)))),
        
        // style = Some(mystyle),
        
        backcolor = Some(java.awt.Color.black),
        forecolor = Some(java.awt.Color.white)
        // mode = Some(net.sf.jasperreports.engine.`type`.ModeEnum.OPAQUE)
        );
    val myband = JRDesignBand.empty.copy(
            height = 20,
            children = Vector(
                JREllipse.empty.copy(common = rect),
                JRStaticText("Hello").copy(common = rect)
                ));
    val r = JasperDesign("hello-world-report").copy(
        defaultStyle = mystyle,
        // details = Vector(myband),
        // title = TitleBand.empty.copy(band = Some(myband), newPage = true),
        pages = Pages.empty.copy(
            header = Some(myband)
            // footer
            ),
        summary = SummaryBand.empty.copy(
            // band = Some(myband)
            )
        );
    testPrint(r);
  }
  

  
  /*
  test("persistency") {
     val o0 = JasperDesign();
     val o1 = o0.copy(columns = o0.columns.copy(width = 42));
     assert(o1.columns.width == 42);
     val o2 = o1.copy(columns = o1.columns.copy(width = 10));
     assert(o2.columns.width == 10);
     assert(o1.columns.width == 42);
     
     // ...not really useful test anymore...
     val p = o2.parameters;
     val sz = p.size;
     val o1_ = o1.copy(parameters = p);
     val o2_ = o2.copy(parameters = List());
     assert(o2_.parameters.size == 0);
     assert(o2.parameters.size == sz);
  }*/

  // verify how ugly the Java API really is
  test("ugliness of java") {
    def n() = new net.sf.jasperreports.engine.design.JasperDesign();
    
    
    // List properties actually mutate the property
    // and although you can actually remove default properties:
    val v1 = n();
    val xp = v1.getParameters().length;
    v1.getParametersList().remove(0);
    assert(v1.getParameters().length == (xp-1));
    // they are still magically there:
    val v2 = n();
    val p : net.sf.jasperreports.engine.JRParameter = v2.getParameters()(0);
    v2.getParametersList().clear();
    assert(v2.getParameters().length == 0);
    intercept[net.sf.jasperreports.engine.JRException] {
      v2.addParameter(p);
    };
    // although they can be added via the list:
    v2.getParametersList().asInstanceOf[java.util.List[net.sf.jasperreports.engine.JRParameter]].add(p);
    // WTF!?
    v2.getParametersList().clear();
    assert(v2.getParametersList().size() == 0);
  }
}


object JasperDesignTest {
  def compile(d : JasperDesign) = {
    val r : net.sf.jasperreports.engine.design.JasperDesign = d;
    show(r)
    net.sf.jasperreports.engine.JasperCompileManager.compileReport(r);
  }
  
  // -> de.ag.jrlang.util ?
  def print(rep : net.sf.jasperreports.engine.JasperReport) : net.sf.jasperreports.engine.JasperPrint = {
    val p = new java.util.HashMap[java.lang.String,java.lang.Object];
    p.put("ReportTitle", "Test");
    val ds = new net.sf.jasperreports.engine.JREmptyDataSource();
    net.sf.jasperreports.engine.JasperFillManager.fillReport(rep, p, ds);
    // net.sf.jasperreports.engine.JasperFillManager.fillReport(rep, p);
  }
  def show(jd: net.sf.jasperreports.engine.design.JasperDesign) = {
    System.err.println("Report: ");
    val props = jd.getPropertiesMap()
    for (p <- props.getPropertyNames()) {
      System.err.println("  %s -> %s", p, props.getProperty(p));
    }
  }

}
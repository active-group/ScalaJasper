package de.ag.jrlang.core

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import de.ag.jrlang.core.SummaryBand;
import de.ag.jrlang.core.TitleBand;
import de.ag.jrlang.core._

@RunWith(classOf[JUnitRunner])
class JasperDesignTest extends FunSuite {

  def compile(d : JasperDesign) = {
    val r : net.sf.jasperreports.engine.design.JasperDesign = d;
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

  def testPrint(d : JasperDesign) = {
    val r = compile(d)
    val p = print(r);
    val bytes = net.sf.jasperreports.engine.JasperExportManager.exportReportToPdf(p);
    // it's not easily verifiable, because it contains current timestamps ("of course")
    printf("%s", new String(bytes))
    //net.sf.jasperreports.engine.JasperPrintManager.printReport(p, true, )
    net.sf.jasperreports.engine.JasperExportManager.exportReportToPdfFile(p, "/Users/frese/tmp/test.pdf");
  }

  /*def testPrintJ(d : JasperDesign) = {
    val jd = d.drop;
    val r = de.ag.jrlang.test.Noxml.compile(jd)
    val p = de.ag.jrlang.test.Noxml.print(r)
    de.ag.jrlang.test.Noxml.write(p, "/Users/frese/tmp/test2.pdf")
  }*/
  
  test("empty report") {
    // running from sbt crashes with some classloader/resources bug,
    // running from eclipse works ("of course"); so hard to debug.
    val r = JasperDesign("empty report")
    //testPrint(r);
  }

  test("simple report") {
    // TODO: which style defs are mandatory?
    val mystyle = JRStyle.Internal("mystyle").copy(
        isDefault = true,
        font = JRFont.empty.copy(
            //fontName = Some("DejaVu Sans"),
            fontSize = Some(12),
            pdfFontName = Some("Helvetica"),
            pdfEncoding = Some("Cp1252"),
            pdfEmbedded = Some(false))
        );
    // setting the internal style here - that won't be one of the objects in the styles list.. problem?
    val rect = JRCommon.empty.copy(
        x = 0, y = 0,
        width = 200, height = 50,
        style = Some(mystyle), // only the name is references, as it seems
        forecolor = Some(java.awt.Color.BLACK),
        backcolor = Some(java.awt.Color.WHITE),
        mode = Some(net.sf.jasperreports.engine.`type`.ModeEnum.OPAQUE)
        );
    val myband = JRDesignBand.empty.copy(
            height = 200,
            children = Vector(
                // JREllipse.empty.copy(common = rect),
                JRStaticText("Hello world").copy(common = rect)
                ));
    // Damn, he sill not prints anything.. setting the band everywhere... still nothing :-(
    val r = JasperDesign("hello-world-report").copy(
        // styles = Vector(mystyle), // is needed, but maybe auto collect them?!
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

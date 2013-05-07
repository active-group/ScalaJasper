package de.ag.jrlang.core

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import de.ag.jrlang.core._

@RunWith(classOf[JUnitRunner])
class ReportTest extends FunSuite {
  
  /*
  def testPrint(d : Report) = {
    val r = ReportTest.compile(d)
    val p = ReportTest.print(r, Map.empty);
    //val bytes = net.sf.jasperreports.engine.JasperExportManager.exportReportToPdf(p);
    // it's not easily verifiable, because it contains current timestamps ("of course")
    // printf("%s", new String(bytes))
    //net.sf.jasperreports.engine.JasperPrintManager.printReport(p, true, )
    //net.sf.jasperreports.engine.JasperExportManager.exportReportToPdfFile(p, "/Users/frese/tmp/test.pdf");
    val s = net.sf.jasperreports.engine.JasperExportManager.exportReportToXml(p)
    printf("%s", s);
  }
  */

  test("empty report") {
    // running from sbt crashes with some classloader/resources bug,
    // running from eclipse works ("of course"); so hard to debug.
    val r = Report("empty report")
    
    val expected = 
<jasperPrint xmlns="http://jasperreports.sourceforge.net/jasperreports/print" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/print http://jasperreports.sourceforge.net/xsd/jasperprint.xsd" name="empty report" pageWidth="595" pageHeight="842" topMargin="30" leftMargin="20" bottomMargin="30" rightMargin="20" locale="en_US" timezone="Europe/Berlin">
  <property name="net.sf.jasperreports.export.xml.start.page.index" value="0"/>
  <property name="net.sf.jasperreports.export.xml.end.page.index" value="0"/>
  <property name="net.sf.jasperreports.export.xml.page.count" value="1"/>
  <origin band="detail"/>
  <style name="default" isDefault="true" />
  <page/>
</jasperPrint>

    val actual = ReportTest.printToXML(r, Map.empty);
    ReportTest.compareJasperPrintXML(expected, actual);
  }

  test("simple report") {
    // TODO: which style defs are mandatory?
    val mystyle = Style.Internal.empty.copy(
        font = Font.empty.copy(
            //fontName = Some("DejaVu Sans"),
            fontSize = Some(12),
            pdfFontName = Some("Helvetica"),
            pdfEncoding = Some("Cp1252"),
            pdfEmbedded = Some(false))
        );
    val style2 = mystyle.copy(
        font = mystyle.font.copy(fontSize = Some(8)),
        backcolor = Some(java.awt.Color.black),
        forecolor = Some(java.awt.Color.white)
        // mode = Some(net.sf.jasperreports.engine.`type`.ModeEnum.OPAQUE)
        );
    // Experiment: We could use 'parentStyle' for every copy that is made automatically...?!
    //style = Some(Style.Internal.empty.copy(
    //    parentStyle = Some(mystyle),
    //    font = JRFont.empty.copy(fontSize = Some(8)))),

    val myband = Band.empty.copy(
            height = 20,
            children = Vector(
                Ellipse.empty.copy(
                    style = style2,
                    pos = Pos.empty.copy(x = 0, y = 0),
                    size = Size.empty.copy(width=55, height = 15)
                    ),
                StaticText("Hello").copy(
                    style = style2,
                    pos = Pos.empty.copy(x = 0, y = 0),
                    size = Size.empty.copy(width=55, height = 15)
                    )
                ));
    val r = Report("hello-world-report").copy(
        defaultStyle = mystyle,
        // details = Vector(myband),
        // title = TitleBand.empty.copy(band = Some(myband), newPage = true),
        page = Page.empty.copy(
            header = Some(myband)
            // footer
            ),
        summary = SummaryBand.empty.copy(
            // band = Some(myband)
            )
        );
    
    val expected =
<jasperPrint xmlns="http://jasperreports.sourceforge.net/jasperreports/print" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/print http://jasperreports.sourceforge.net/xsd/jasperprint.xsd" name="hello-world-report" pageWidth="595" pageHeight="842" topMargin="30" leftMargin="20" bottomMargin="30" rightMargin="20" locale="en_US" timezone="Europe/Berlin">
  <property name="net.sf.jasperreports.export.xml.start.page.index" value="0"/>
  <property name="net.sf.jasperreports.export.xml.end.page.index" value="0"/>
  <property name="net.sf.jasperreports.export.xml.page.count" value="1"/>
  <origin band="pageHeader"/>
  <origin band="detail"/>
  <style name="default" isDefault="true" fontSize="12" isPdfEmbedded="false" pdfFontName="Helvetica" pdfEncoding="Cp1252"/>
  <style name="autodef0" backcolor="#000000" fontSize="8" forecolor="#FFFFFF" isPdfEmbedded="false" pdfEncoding="Cp1252" pdfFontName="Helvetica"/>
  <page>
    <ellipse>
      <reportElement uuid="e09461da-2eb3-41f9-9d0c-629f1532d1e8" style="autodef0" x="20" y="30" width="55" height="15" origin="0" srcId="1"/>
    </ellipse>
    <text textHeight="9.421875" lineSpacingFactor="1.1777344" leadingOffset="-1.6875">
      <reportElement uuid="2155e5cc-96c2-4125-a527-c2124b38f01f" style="autodef0" x="20" y="30" width="55" height="15" origin="0" srcId="2"/>
      <textContent><![CDATA[Hello]]></textContent>
    </text>
  </page>
</jasperPrint>
   
    val actual = ReportTest.printToXML(r, Map.empty);
    ReportTest.compareJasperPrintXML(expected, actual);
  }
  

  
  /*
  test("persistency") {
     val o0 = Report();
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


object ReportTest {
  def compile(d : net.sf.jasperreports.engine.design.JasperDesign) = {
    net.sf.jasperreports.engine.JasperCompileManager.compileReport(d);
  }
  
  // -> de.ag.jrlang.util ?
  def print(rep : net.sf.jasperreports.engine.JasperReport, params: Map[String, AnyRef]) : net.sf.jasperreports.engine.JasperPrint = {
    val p = new java.util.HashMap[java.lang.String,java.lang.Object];
    //p.put("ReportTitle", "Test");
    for ((k,v) <- params)
      p.put(k, v)
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

  def printToXML(d: Report, params: Map[String, AnyRef]) = {
    val r = Compiler.compile(d);
    val rr = compile(r);
    val p = print(rr, params);
    val s = net.sf.jasperreports.engine.JasperExportManager.exportReportToXml(p)
    val xml:scala.xml.Elem = scala.xml.XML.loadString(s);
    xml
  }
  
  def compareJasperPrintXML(expected_ : scala.xml.Elem, actual_ : scala.xml.Elem) = {
    val expected = prepareForCompare(expected_);
    val actual = prepareForCompare(actual_);
    if (!(expected == actual)) {
      val pp = new scala.xml.PrettyPrinter(80, 2)
      System.err.printf("Expected: %s\nActual: %s\n", pp.format(expected), pp.format(actual));
      // these methods are not that clever:
      //System.err.printf("Missing: %s\n", expected diff actual);
      //System.err.printf("Unexpected: %s\n", actual diff expected);
      val e:String = expected.toString;
      val a:String = actual.toString;
      val (suff1, suff2) = (e, a).zipped.dropWhile(Function.tupled(_ == _)).unzip;
      //val (inter1, inter2) = (suff1.reverse, suff2.reverse).dropWhile...
      System.err.printf("First differences around:\n");
      System.err.printf("...%s\n", suff1.mkString);
      System.err.printf("...%s\n", suff2.mkString);
    }
    assert(expected == actual)
  }
  
  def prepareForCompare(xml: scala.xml.Elem) =
    removeAttr("uuid", // remove all attributes named uuid, in all nested elements
      scala.xml.Utility.sort( // sort
          scala.xml.Utility.trim(xml))); // remove whitespace

  def removeAttr(n: String, xml:scala.xml.Node) =
    xml match {
      case e:scala.xml.Elem => removeElemAttr(n, e)
      case x => x
    }
  def removeElemAttr(n: String, xml:scala.xml.Elem) : scala.xml.Elem =
    xml.copy(child = xml.child.foldLeft(Vector[scala.xml.Node]()) { case(r, e) =>
      e match {
        case v:scala.xml.Elem => r :+ removeElemAttr(n, v)
        case x => r :+ x
      }
    }, attributes = xml.attributes.remove(n))
}
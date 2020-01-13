package de.activegroup.scalajasper.core

import net.sf.jasperreports.engine.`type`._
import net.sf.jasperreports.view.JasperViewer
import org.scalatest.funsuite.AnyFunSuite

class ReportTest extends AnyFunSuite {
  
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
<jasperPrint xmlns="http://jasperreports.sourceforge.net/jasperreports/print" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/print http://jasperreports.sourceforge.net/xsd/jasperprint.xsd" name="empty report" pageWidth="595" pageHeight="841" topMargin="30" leftMargin="20" bottomMargin="30" rightMargin="20"
             locale="en_US" timezone="GMT">
  <property name="net.sf.jasperreports.export.xml.start.page.index" value="0"/>
  <property name="net.sf.jasperreports.export.xml.end.page.index" value="0"/>
  <property name="net.sf.jasperreports.export.xml.page.count" value="1"/>
  <origin band="detail"/>
  <page/>
</jasperPrint>

    val actual = ReportTest.printToXML(r, Map.empty)
    ReportTest.compareJasperPrintXML(expected, actual)
  }

  test("simple report") {
    // TODO: which style defs are mandatory?
    val mystyle = Style(
        font = Font.empty.copy(
            //fontName = Some("DejaVu Sans"),
            fontSize = Some(12),
            pdfFontName = Some("Helvetica"),
            pdfEncoding = Some("Cp1252"),
            pdfEmbedded = Some(false))
        )
    val style2 = mystyle.copy(
        font = mystyle.font.copy(fontSize = Some(8)),
        forecolor = Some(java.awt.Color.black),
        backcolor = Some(java.awt.Color.white),
        line = Pen(lineWidth = Some(1.0F), lineStyle = Some(LineStyleEnum.SOLID), lineColor = Some(new java.awt.Color(0xc0, 0, 0))) // some red
        // mode = Some(net.sf.jasperreports.engine.`type`.ModeEnum.OPAQUE)
        )
    // Experiment: We could use 'parentStyle' for every copy that is made automatically...?!
    //style = Some(Style.Internal.empty.copy(
    //    parentStyle = Some(mystyle),
    //    font = JRFont.empty.copy(fontSize = Some(8)))),

    val myband = Band(
            splitType = SplitTypeEnum.STRETCH,
            height = 200.px,
            content = Vector(
                Line(
                    style = style2,
                    width=55.px, height = Height.fixed(15.px)
                    ),
                StaticText(
                    text = "Hello",
                    style = style2,
                    width=55.px, height = Height.fixed(15.px)
                    )
                ))
    val r = Report("hello-world-report").copy(
        // details = Vector(myband),
        // title = TitleBand.empty.copy(band = Some(myband), newPage = true),
        page = Page(
            header = Some(myband)
            // footer
            )
        )
    
    val expected =
      <jasperPrint
      bottomMargin="30" leftMargin="20" locale="en_US" name="hello-world-report" pageHeight="841" pageWidth="595" rightMargin="20" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/print http://jasperreports.sourceforge.net/xsd/jasperprint.xsd" timezone="GMT" topMargin="30" xmlns="http://jasperreports.sourceforge.net/jasperreports/print" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <property name="net.sf.jasperreports.export.xml.start.page.index" value="0"/>
        <property name="net.sf.jasperreports.export.xml.end.page.index" value="0"/>
        <property name="net.sf.jasperreports.export.xml.page.count" value="1"/>
        <origin band="pageHeader"/>
        <origin band="detail"/>
        <style
        backcolor="#FFFFFF" fontSize="8" forecolor="#000000" isPdfEmbedded="false" name="auto0" pdfEncoding="Cp1252" pdfFontName="Helvetica">
          <pen lineColor="#C00000" lineStyle="Solid" lineWidth="1.0"/>
        </style>
        <page>
          <line>
            <reportElement
            height="15" origin="0" printId="1" srcId="1" style="auto0" uuid="f1450306-5176-34a5-beaf-bbf8ed995985" width="55" x="20" y="30">
            </reportElement>
          </line>
          <text
          leadingOffset="-1.6875" lineSpacingFactor="1.1777344" textHeight="9.421875">
            <reportElement
            height="15" origin="0" printId="1" srcId="2" style="auto0" uuid="f1d3ff84-4329-3732-862d-f21dc4e57262" width="55" x="20" y="30">
            </reportElement>
            <textContent>Hello</textContent>
          </text>
        </page>
      </jasperPrint>

    val args : Map[String, AnyRef] = Map.empty
    // ReportTest.printToPDF(r, args, "/Users/frese/tmp/test.pdf")

    val actual = ReportTest.printToXML(r, args)
    ReportTest.compareJasperPrintXML(expected, actual)
  }
  
  test("image") {
    val mystyle = Style.empty
    val myband = Band(
            height = 200.px,
            splitType = SplitTypeEnum.STRETCH,
            content = Vector(
                Image(expression = Expression.const("src/test/resources/butterfly.jpg"),
                      style = mystyle.copy(scaleImage = Some(net.sf.jasperreports.engine.`type`.ScaleImageEnum.RETAIN_SHAPE)),
                      lazily = true,
                      width = 150.px, height = Height.fixed(150.px))
                ))
    val r = Report("myfirstimage").copy(
        page = Page(
            header = Some(myband)
            )
        )
    
    val expected =
      <jasperPrint
      bottomMargin="30" leftMargin="20" locale="en_US" name="myfirstimage" pageHeight="841" pageWidth="595" rightMargin="20" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/print http://jasperreports.sourceforge.net/xsd/jasperprint.xsd" timezone="GMT" topMargin="30" xmlns="http://jasperreports.sourceforge.net/jasperreports/print" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <property name="net.sf.jasperreports.export.xml.start.page.index" value="0"/>
        <property name="net.sf.jasperreports.export.xml.end.page.index" value="0"/>
        <property name="net.sf.jasperreports.export.xml.page.count" value="1"/>
        <origin band="pageHeader"/>
        <origin band="detail"/>
        <style name="auto0" scaleImage="RetainShape"/>
        <page>
          <image hAlign="Left" isLazy="true" scaleImage="RetainShape" vAlign="Top">
            <reportElement
            height="150" origin="0" printId="1" srcId="1" style="auto0" uuid="f1d3ff84-4329-3732-862d-f21dc4e57262" width="150" x="20" y="30">
            </reportElement>
            <imageSource>src/test/resources/butterfly.jpg</imageSource>
          </image>
        </page>
      </jasperPrint>
   
    val actual = ReportTest.printToXML(r, Map.empty)
    ReportTest.compareJasperPrintXML(expected, actual)
  }

  test("textfield with parameter") {
    val myband = Band(
            height = 200.px,
            splitType = SplitTypeEnum.STRETCH,
            content = Vector(
                TextField(expression = Expression.raw("$P{myarg1}"),
                  width=100.percent, height = Height.fixed(50.px),
                  x = 40.px, y = YPos.float(80.px))
                ))
    val r = Report("text-parameter").copy(
        mainDataset = Dataset.empty.copy(
            parameters = Vector(
                Parameter("myarg1", Some(Expression.raw("\"mydefault\"")))
                )),
        page = Page(
            header = Some(myband)
            )
        )
    
    val expected =
      <jasperPrint
      bottomMargin="30" leftMargin="20" locale="en_US" name="text-parameter" pageHeight="841" pageWidth="595" rightMargin="20" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/print http://jasperreports.sourceforge.net/xsd/jasperprint.xsd" timezone="GMT" topMargin="30" xmlns="http://jasperreports.sourceforge.net/jasperreports/print" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <property name="net.sf.jasperreports.export.xml.start.page.index" value="0"/>
        <property name="net.sf.jasperreports.export.xml.end.page.index" value="0"/>
        <property name="net.sf.jasperreports.export.xml.page.count" value="1"/>
        <origin band="pageHeader"/>
        <origin band="detail"/>
        <page>
          <text
          leadingOffset="-2.109375" lineSpacingFactor="1.1777344" textHeight="11.777344">
            <reportElement
            height="50" origin="0" printId="1" srcId="1" uuid="f1d3ff84-4329-3732-862d-f21dc4e57262" width="555" x="60" y="110">
            </reportElement>
            <textContent>mydefault</textContent>
          </text>
        </page>
      </jasperPrint>
   
    val actual = ReportTest.printToXML(r, Map.empty)
    ReportTest.compareJasperPrintXML(expected, actual)
  }

  def keepShowing(f: () => Report): Unit = {
    def show(r: Report) = {
      val jp = print(r, Map.empty)
      val viewer = new JasperViewer(jp)
      viewer.setVisible(true)
      viewer
    }
    def hide(v: JasperViewer): Unit = {
      v.setVisible(false)
    }
    def dorun() = {
      while (true) {
        val r = f()
        val v = show(r)
        var r2 = r
        do {
          Thread.sleep(500)
          r2 = f()
        } while (r2 == r)
        hide(v)
      }
    }
    new Thread(new Runnable {
      override def run(): Unit = { dorun() }
    }).run()
  }
  /*
  test("make regions cheat sheet") {
    val rep = makeRegionsCheatSheet()

    //ReportTest.printToPDF(r, Map.empty, "/Users/frese/tmp/JasperCheatSheet.pdf")
  }*/

  def makeRegionsCheatSheet() = {
    def demoBand(name: String, h:Height = Height.fixed(2.cm)) = {
      val st = Style(box = LineBox(pen = BoxPen.uniform(Pen(lineWidth = Some(0.5F)))),
        verticalTextAlignment = Some(VerticalTextAlignEnum.MIDDLE),
        horizontalTextAlignment = Some(HorizontalTextAlignEnum.CENTER))
      Band(
        height = BandHeight.Auto,
        splitType = SplitTypeEnum.STRETCH,
        content = Vector(
          TextField(
            expression = Expression.const(name),
            style = st,
            width=100.percent, height = h)
        ))
    }
    val r = Report(name = "cheat sheet",
      page = Page(
        header = Some(demoBand("page header")),
        footer = Some(demoBand("page footer")),
        columns = Columns(count = 2,
          spacing = 5.px,
          header = Some(demoBand("column header")),
          footer = Some(FloatingBand(demoBand("column footer (floating)"), floating=true))
          ),
        background = Some(Band(splitType = SplitTypeEnum.IMMEDIATE, // split type of background?
          content = Vector(TextField(
            expression = Expression.const("background"),
            width = 100.percent, height = Height.relativeToBand(100.px)))))
      ),
      details = (1 to 12) map {i => demoBand("details "+i) },
      summary = Some(SummaryBand(demoBand("summary (new page, with header and footer)"), newPage=true, withPageHeaderAndFooter=true)),
      title = Some(TitleBand(demoBand("title (new page)"), newPage=true))
      // would override page footer lastPageFooter = Some(demoBand("last page footer"))
    )
    r
  }

}


object ReportTest {

  def show(jd: net.sf.jasperreports.engine.design.JasperDesign) = {
    System.err.println("Report: ")
    val props = jd.getPropertiesMap
    for (p <- props.getPropertyNames) {
      System.err.println("  %s -> %s", p, props.getProperty(p))
    }
  }

  def printToXML(d: Report, args: Map[String, AnyRef]) = {
    val p = print(d, args)
    val s = net.sf.jasperreports.engine.JasperExportManager.exportReportToXml(p)
    val xml:scala.xml.Elem = scala.xml.XML.loadString(s)
    xml
  }

  def printToPDF(d: Report, args: Map[String, AnyRef], pdfFilename: String): Unit = {
    val p = print(d, args)
    net.sf.jasperreports.engine.JasperExportManager.exportReportToPdfFile(p, pdfFilename)
  }

  def compareJasperPrintXML(expected_ : scala.xml.Elem, actual_ : scala.xml.Elem) = {
    val expected = prepareForCompare(expected_)
    val actual = prepareForCompare(actual_)
    if (!(expected == actual)) {
      val pp = new scala.xml.PrettyPrinter(80, 2)
      System.err.printf("Expected: %s\nActual: %s\n", pp.format(expected), pp.format(actual))
      // these methods are not that clever:
      //System.err.printf("Missing: %s\n", expected diff actual);
      //System.err.printf("Unexpected: %s\n", actual diff expected);
      val e:String = expected.toString()
      val a:String = actual.toString()
      val (suff1, suff2) = e.lazyZip(a).dropWhile(Function.tupled(_ == _)).unzip
      //val (inter1, inter2) = (suff1.reverse, suff2.reverse).dropWhile...
      System.err.printf("First differences around:\n")
      System.err.printf("...%s\n", suff1.mkString)
      System.err.printf("...%s\n", suff2.mkString)
    }
    assert(expected == actual)
  }
  
  def prepareForCompare(xml: scala.xml.Elem) =
    // more diffs than that... removeAttr("orientation", // TODO only on jasperPrint element; JR 4 set's this to Portrait, while JR 5 doesn't set it?!
      //removeAttr("uuid", // remove all attributes named uuid, in all nested elements
        scala.xml.Utility.sort( // sort
            scala.xml.Utility.trim(xml)) // remove whitespace

  def removeAttr(n: String, xml:scala.xml.Node) =
    xml match {
      case e:scala.xml.Elem => removeElemAttr({_ => true}, n, e)
      case x => x
    }
  def removeElemAttr(node: String => Boolean, attr: String, xml:scala.xml.Elem) : scala.xml.Elem =
    xml.copy(child = xml.child.foldLeft(Vector[scala.xml.Node]()) { case(r, e) =>
      e match {
        case v:scala.xml.Elem => r :+ removeElemAttr(node, attr, v)
        case x => r :+ x
      }
    }, attributes = if (node(xml.label)) xml.attributes.remove(attr) else xml.attributes)

  def removeElemAttr(attrName: String, xml:scala.xml.Elem) : scala.xml.Elem =
    removeElemAttr({_ => true}, attrName, xml)

  def removeElemAttr(nodeName: String, attrName: String, xml:scala.xml.Elem) : scala.xml.Elem =
    removeElemAttr({n => n == nodeName}, attrName, xml)
}
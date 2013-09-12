package de.ag.scalajasper.core

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import net.sf.jasperreports.engine.design.{JRDesignStyle, JRDesignStaticText, JRDesignBand, JasperDesign}
import net.sf.jasperreports.view.JasperViewer
import net.sf.jasperreports.engine._
import scala.xml
import net.sf.jasperreports.engine.`type`.SplitTypeEnum

@RunWith(classOf[JUnitRunner])
class JavaTests extends FunSuite {
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


  def minimalJasperDesign() = {
    val r = new JasperDesign()
    r.setName("test")
    r
  }
  def print(jd: JasperDesign) = {
    val jr = net.sf.jasperreports.engine.JasperCompileManager.compileReport(jd)
    printJasperReport(jr)
  }
  def toXML(p: JasperPrint) : scala.xml.Elem = {
    val s = net.sf.jasperreports.engine.JasperExportManager.exportReportToXml(p)
    val xml:scala.xml.Elem = scala.xml.XML.loadString(s)
    xml
  }
  def toXML(jd: JasperDesign) : scala.xml.Elem = {
    val p = print(jd)
    toXML(p)
  }
  def viewAndExit(jd: () => JasperDesign) {
    var jasperViewer : JasperViewer = null
    var prev = jd()
    while (true) {
      val jasperPrint = print(prev)
      jasperViewer = new JasperViewer(
        DefaultJasperReportsContext.getInstance(),
        jasperPrint,
        true,
        null,
        null
        )
      jasperViewer.setVisible(true)
      var unchanged = true
      while (unchanged) {
        val r = jd()
        if (r == prev)
          Thread.sleep(2000)
        else {
          jasperViewer.setVisible(false)
          prev = r
        }
      }
    }
    //JasperViewer.viewReport(print(jd), true)
    //Thread.sleep(60000)
  }

  test("minimal report") {
    val jd1 = minimalJasperDesign()
    System.err.println(toXML(jd1))
  }

  test("no element reuse") {
    val jd = minimalJasperDesign()

    val company = new JRDesignStaticText()
    company.setFontName("Helvetica")
    company.setFontSize(12)
    company.setHeight(14)
    company.setBold(true)
    company.setWidth(200)
    company.setX(0)
    company.setY(0)
    company.setText("My Company")

    val t : JRElement = company;

    val b = new JRDesignBand()
    b.setHeight(20)
    b.addElement(t)
    b.addElement(t)

    val b2 = new JRDesignBand()
    b2.setHeight(20)
    b2.addElement(t)
    b2.addElement(t)

    jd.setPageFooter(b)
    jd.setPageHeader(b)
    jd.setPageHeader(null)
    // expected to throw an exception
    System.err.println(toXML(jd))
    //intercept[JRException] { System.err.println(toXML(jd)) }
    //viewAndExit({ () => jd })
  }

  test("article-1") {
    def myCompanyBanner() = {
      val band = new JRDesignBand()
      band.setHeight(30)

      val t = new JRDesignStaticText()
      t.setFontName("Helvetica")
      t.setFontSize(12)
      t.setHeight(12)
      t.setWidth(200)
      t.setX(0)
      t.setY(0)
      t.setText("My Company")
      band.addElement(t)

      band
    }

    def myReport() = {
      val d = new JasperDesign()
      d.setName("myreport")

      val banner = myCompanyBanner()
      d.setPageHeader(banner)

      d
    }

    toXML(myReport())
  }

  test("article-2") {
    def boldSmallText() = {
      val st = new JRDesignStyle()
      st.setName("bold")
      st.setFontName("Helvetica")
      st.setFontSize(12)
      st.setBold(true)
      st
    }

    def myCompanyBanner() = {
      val band = new JRDesignBand()
      band.setHeight(30)

      val t = new JRDesignStaticText()
      val st = boldSmallText()
      t.setStyle(st)
      t.setHeight(12)
      t.setWidth(200)
      t.setX(0)
      t.setY(0)
      t.setText("My Company")
      band.addElement(t)

      band
    }

    def myReport() = {
      val d = new JasperDesign()
      d.setName("myreport")

      val banner = myCompanyBanner()
      d.setPageHeader(banner)

      d
    }

    // toXML(myReport())
    intercept[JRRuntimeException] { toXML(myReport()) }
  }

  test("article-3") {
    def boldSmallText(real: Boolean) = {
      val st = new JRDesignStyle()
      st.setName("bold")
      if (real) {
        st.setFontName("Helvetica")
        st.setFontSize(12)
        st.setBold(true)
      }
      st
    }

    def myCompanyBanner() = {
      val band = new JRDesignBand()
      band.setHeight(30)

      val t = new JRDesignStaticText()
      val st = boldSmallText(false)
      t.setStyle(st)
      t.setHeight(12)
      t.setWidth(200)
      t.setX(0)
      t.setY(0)
      t.setText("My Company")
      band.addElement(t)

      band
    }

    def myReport() = {
      val d = new JasperDesign()
      d.setName("myreport")

      val banner = myCompanyBanner()
      d.setPageHeader(banner)

      d.addStyle(boldSmallText(true))

      d
    }

    System.out.println(toXML(myReport()))
    //intercept[JRRuntimeException] { toXML(myReport()) }
  }

  test("article-3-new") {
    val boldSmallText = Style(
      font = Font(
        fontName = Some("Helvetica"),
        fontSize = Some(12),
        bold = Some(true)
      )
    )

    val myCompanyBanner = Band(
      height = 30 px,
      splitType = SplitTypeEnum.STRETCH,
      content = StaticText(
        x = 0 px,
        y = YPos.float(0 px),
        width = 200 px,
        height = Height.fixed(12 px),
        style = boldSmallText,
        text = "My Company"
      )
    )

    def myReport() = Report(
      name = "myreport",
      page = Page(header = Some(myCompanyBanner))
    )

    System.out.println(toXML(de.ag.scalajasper.core.print(myReport())))
    //intercept[JRRuntimeException] { toXML(myReport()) }
  }
}

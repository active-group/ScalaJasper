package de.ag.jrlang.core

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import net.sf.jasperreports.engine.`type`.SplitTypeEnum

@RunWith(classOf[JUnitRunner])
class ElementTests extends FunSuite {

  test("placing elements above each other") {
    val e1 = Break.page()
    val e2 = TextField(Expression.const("Hello"), style=Style(font = Font(fontSize = Some(12))))
    val e3 = ElementGroup(Seq(
      Line(Width.Remaining, Height.fixed(1 px)),
      TextField(Expression.const("World"))
    ))

    val combo = e1 below (e2 above e3)

    val actual =
      ReportTest.printToXML(Report("test", details = Seq(Band(splitType=SplitTypeEnum.IMMEDIATE, content = combo))),
        args=Map.empty)

    val expected = <jasperPrint
    bottomMargin="30" leftMargin="20" locale="en_US" name="test" pageHeight="841" pageWidth="595" rightMargin="20" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/print http://jasperreports.sourceforge.net/xsd/jasperprint.xsd" timezone="GMT" topMargin="30" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://jasperreports.sourceforge.net/jasperreports/print">
      <property name="net.sf.jasperreports.export.xml.start.page.index" value="0">
      </property>
      <property name="net.sf.jasperreports.export.xml.end.page.index" value="0">
      </property>
      <property name="net.sf.jasperreports.export.xml.page.count" value="1">
      </property>
      <origin band="detail"></origin>
      <style fontSize="12" name="auto0"></style>
      <page>
        <text textHeight="0.0">
          <reportElement
          height="12" origin="0" srcId="1" style="auto0" uuid="584a15a9-0f2f-359d-8703-594ad447ae93" width="555" x="20" y="30">
          </reportElement>
          <textContent></textContent>
        </text>
        <line>
          <reportElement
          height="1" origin="0" srcId="2" uuid="f11177d2-ec63-3995-bb4a-c628e0d782df" width="555" x="20" y="42">
          </reportElement>
        </line>
        <text textHeight="0.0">
          <reportElement
          height="0" origin="0" srcId="3" uuid="f1450306-5176-34a5-beaf-bbf8ed995985" width="555" x="20" y="42">
          </reportElement>
          <textContent></textContent>
        </text>
      </page>
    </jasperPrint>

    ReportTest.compareJasperPrintXML(expected, actual)
  }

}

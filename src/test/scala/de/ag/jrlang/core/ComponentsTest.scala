package de.ag.jrlang.core

import scala.collection.JavaConversions._

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import de.ag.jrlang.core._
import de.ag.jrlang.core.components.{TableCell, TableColumn, Table}
import net.sf.jasperreports.components.table.WhenNoDataTypeTableEnum
import net.sf.jasperreports.engine.data.JRMapArrayDataSource

@RunWith(classOf[JUnitRunner])
class ComponentsTest extends FunSuite {
  test("table component") {
    val r = Report("basic table").copy(details = List(
      Band.empty.copy(
        height = 200,
        content = Vector(
          ComponentElement(
            pos = Pos.float(x = 0, y = 0),
            size = Size.fixed(height = 100, width = 400),
            component = Table(whenNoData = WhenNoDataTypeTableEnum.ALL_SECTIONS_NO_DETAIL,
              data = DatasetRun(datasetName = "dummy", arguments=Map.empty, dataSourceExpression = Expression.P("table_datasource")),
              columns = List(TableColumn(width = 100,
                detail = TableCell(height = 50, content = List(
                  TextField(Size.fixed(15, 100), Pos.float(0, 0), Expression.F("f1")))
                ))))
        ))
      )),
      mainDataset = Dataset.empty.copy(parameters = Vector(
        Parameter("table_datasource").copy(valueClassName = "net.sf.jasperreports.engine.JRDataSource")
      )),
      subDatasets = Map("dummy" -> Dataset.empty.copy(
        // parameters = Vector(Parameter("p1").copy(valueClassName = "java.util.List"))
        fields = Map("f1" -> "java.lang.String")
      ))
    )
    val data : Array[java.util.Map[java.lang.String,AnyRef]] =
      Array(mapAsJavaMap(Map("f1" -> new java.lang.String("Hello").asInstanceOf[AnyRef])),
            mapAsJavaMap(Map("f1" -> new java.lang.String("World").asInstanceOf[AnyRef]))
            );
    val args = Map("table_datasource" -> new JRMapArrayDataSource(data.asInstanceOf[Array[AnyRef]]))

    val expected =
      <jasperPrint xmlns="http://jasperreports.sourceforge.net/jasperreports/print" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/print http://jasperreports.sourceforge.net/xsd/jasperprint.xsd"
                   name="basic table" pageWidth="595" pageHeight="842" topMargin="30" leftMargin="20" bottomMargin="30" rightMargin="20" locale="en_US" timezone="Europe/Berlin">
        <property name="net.sf.jasperreports.export.xml.start.page.index"/>
        <property name="net.sf.jasperreports.export.xml.end.page.index"/>
        <property name="net.sf.jasperreports.export.xml.page.count"/>
        <origin band="detail"/>
        <origin band="detail">
        </origin>
        <page>
          <frame>
            <reportElement height="100" origin="0" srcId="1" width="100" x="20" y="30">
              <property
              name="net.sf.jasperreports.export.headertoolbar.tableUUID">
              </property>
            </reportElement>
            <frame>
              <reportElement height="50" origin="1" srcId="4" width="100" x="0" y="0">
              </reportElement>
              <text
              leadingOffset="-2.109375" lineSpacingFactor="1.1777344" textHeight="11.777344">
                <reportElement height="15" origin="1" srcId="5" width="100" x="0" y="0">
                </reportElement>
                <textContent>Hello</textContent>
              </text>
            </frame>
            <frame>
              <reportElement height="50" origin="1" srcId="4" width="100" x="0" y="50">
              </reportElement>
              <text
              leadingOffset="-2.109375" lineSpacingFactor="1.1777344" textHeight="11.777344">
                <reportElement height="15" origin="1" srcId="5" width="100" x="0" y="0">
                </reportElement>
                <textContent>World</textContent>
              </text>
            </frame>
          </frame>
        </page>
      </jasperPrint>

    val actual = ReportTest.printToXML(r, args);
    ReportTest.compareJasperPrintXML(expected,
      ReportTest.removeElemAttr("property", "value", // would only be needed for property name=tableUUID ...
        ReportTest.removeElemAttr("origin", "report", // on origin band detail ...
          actual)))
  }
}

package de.ag.jrlang.core

import scala.collection.JavaConversions._

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import de.ag.jrlang.core._
import de.ag.jrlang.core.Dimensions._
import de.ag.jrlang.core.components.{TableCell, TableColumn, Table}
import net.sf.jasperreports.components.table.WhenNoDataTypeTableEnum
import net.sf.jasperreports.engine.data.JRMapArrayDataSource
import net.sf.jasperreports.engine.JRDataSource
import net.sf.jasperreports.engine.`type`.SplitTypeEnum

@RunWith(classOf[JUnitRunner])
class ComponentsTest extends FunSuite {
  test("table component") {
    val r = Report("basic table").copy(details = List(
      Band(
        height = 200 px,
        splitType = SplitTypeEnum.STRETCH,
        content = Vector(
          ComponentElement(
            pos = Pos.float(x = 0 px, y = 0 px),
            size = Size.fixed(height = 100 px, width = 400 px),
            component = Table(whenNoData = WhenNoDataTypeTableEnum.ALL_SECTIONS_NO_DETAIL,
              data = DatasetRun(datasetName = "dummy", arguments=Map.empty, dataSourceExpression = Some(Expression.P("table_datasource"))),
              columns = List(TableColumn(width = 100 px,
                detail = TableCell(height = 50 px, content = List(
                  TextField(Size.fixed(height=15 px, width=100 percent), Pos.float(0 px, 0 px), Expression.F("f1")))
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

  test("table component automatic dataset") {
    val data : Array[java.util.Map[java.lang.String,AnyRef]] =
      Array(mapAsJavaMap(Map("f1" -> new java.lang.String("Hello").asInstanceOf[AnyRef])),
        mapAsJavaMap(Map("f1" -> new java.lang.String("World").asInstanceOf[AnyRef]))
      )
    val datasource = new JRMapArrayDataSource(data.asInstanceOf[Array[AnyRef]])
    val dataset = Dataset.empty.copy(
      // parameters = Vector(Parameter("p1").copy(valueClassName = "java.util.List"))
      fields = Map("f1" -> "java.lang.String")
    )

    val r = Report("basic table").copy(details = List(
      Band(
        height = 200 px,
        splitType = SplitTypeEnum.STRETCH,
        content = Vector(
          ComponentElement(
            pos = Pos.float(x = 0 cm, y = 0 mm),
            size = Size.fixed(height = 100 px, width = 400 px),
            component = Table(whenNoData = WhenNoDataTypeTableEnum.ALL_SECTIONS_NO_DETAIL,
              data = DataDef(dataset = dataset, source=Expression.const(datasource)),
              columns = List(TableColumn(width = 80 percent,
                detail = TableCell(height = 50 px, content = List(
                  TextField(Size.fixed(height=15 px, width=90 percent), Pos.float(0 px, 0 px),
                    Expression.call({t:String => t+t}, Expression.F("f1"))))
                ))))
          ))
      ))
    )

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
            <reportElement height="100" origin="0" srcId="1" width="444" x="20" y="30">
              <property
              name="net.sf.jasperreports.export.headertoolbar.tableUUID">
              </property>
            </reportElement>
            <frame>
              <reportElement height="50" origin="1" srcId="4" width="444" x="0" y="0">
              </reportElement>
              <text
              leadingOffset="-2.109375" lineSpacingFactor="1.1777344" textHeight="11.777344">
                <reportElement height="15" origin="1" srcId="5" width="400" x="0" y="0">
                </reportElement>
                <textContent>HelloHello</textContent>
              </text>
            </frame>
            <frame>
              <reportElement height="50" origin="1" srcId="4" width="444" x="0" y="50">
              </reportElement>
              <text
              leadingOffset="-2.109375" lineSpacingFactor="1.1777344" textHeight="11.777344">
                <reportElement height="15" origin="1" srcId="5" width="400" x="0" y="0">
                </reportElement>
                <textContent>WorldWorld</textContent>
              </text>
            </frame>
          </frame>
        </page>
      </jasperPrint>

    val actual = ReportTest.printToXML(r, Map.empty);
    ReportTest.compareJasperPrintXML(expected,
      ReportTest.removeElemAttr("property", "value", // would only be needed for property name=tableUUID ...
        ReportTest.removeElemAttr("origin", "report", // on origin band detail ...
          actual)))
  }

  test("two tables in one report (with same dataset)") {
    val data : Array[java.util.Map[java.lang.String,AnyRef]] =
      Array(mapAsJavaMap(Map("f1" -> new java.lang.String("Hello").asInstanceOf[AnyRef])),
        mapAsJavaMap(Map("f1" -> new java.lang.String("World").asInstanceOf[AnyRef]))
      )
    val datasource = new JRMapArrayDataSource(data.asInstanceOf[Array[AnyRef]])
    val dataset = Dataset.empty.copy(
      // parameters = Vector(Parameter("p1").copy(valueClassName = "java.util.List"))
      fields = Map("f1" -> "java.lang.String")
    )

    def tab(textExpr: Expression[String]) =
      Table(whenNoData = WhenNoDataTypeTableEnum.ALL_SECTIONS_NO_DETAIL,
        data = DataDef(dataset = dataset, source=Expression.const(datasource)),
        columns = List(TableColumn(width = 100 px,
          detail = TableCell(height = 50 px, content = List(
            TextField(Size.fixed(height=15 px, width=100 px), Pos.float(0 px, 0 px),
              textExpr))
          ))))

    val r = Report("two tables").copy(details = List(
      Band(
        height = 200 px,
        splitType = SplitTypeEnum.STRETCH,
        content = Vector(
          // same dataset etc, but different (auto-) parameters (in the form of expressions)
          ComponentElement(
            pos = Pos.float(x = 0 px, y = 0 px),
            size = Size.fixed(height = 100 px, width = 400 px),
            component = tab(Expression.call({t:String => t+t}, Expression.F("f1")))
          ),
          ComponentElement(
            pos = Pos.float(x = 0 px, y = 100 px),
            size = Size.fixed(height = 100 px, width = 400 px),
            component = tab(Expression.call({t:String => t+" ...and... "+t}, Expression.F("f1")))
          ))
      ))
    )

    // just test successfull compilation for now...
    val actual = ReportTest.printToXML(r, Map.empty);
    assert(actual != null)
    /*
    ReportTest.compareJasperPrintXML(expected,
      ReportTest.removeElemAttr("property", "value", // would only be needed for property name=tableUUID ...
        ReportTest.removeElemAttr("origin", "report", // on origin band detail ...
          actual)))
    */
  }
}

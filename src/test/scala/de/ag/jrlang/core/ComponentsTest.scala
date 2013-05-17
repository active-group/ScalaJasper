package de.ag.jrlang.core

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import de.ag.jrlang.core._
import de.ag.jrlang.core.components.{TableColumn, Table}
import net.sf.jasperreports.components.table.WhenNoDataTypeTableEnum

@RunWith(classOf[JUnitRunner])
class ComponentsTest extends FunSuite {
  test("table component") {
    val r = Report("basic table").copy(details = List(
      Band.empty.copy(
        height = 200,
        children = Vector(
          ComponentElement.empty.copy(
            pos = Pos.empty.copy(x = 0, y = 0),
            size = Size.empty.copy(height = 100),
            component = Table(columns = List(), whenNoData = WhenNoDataTypeTableEnum.ALL_SECTIONS_NO_DETAIL)
        ))
    )))

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
}

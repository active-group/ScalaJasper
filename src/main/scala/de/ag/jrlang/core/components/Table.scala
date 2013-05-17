package de.ag.jrlang.core.components

import net.sf.jasperreports.components.table.WhenNoDataTypeTableEnum

import de.ag.jrlang.core._

import scala.collection.JavaConversions._
import net.sf.jasperreports.engine.component.ComponentKey
;

/** From the "Ultimate guide":
  * "In order to obtain truly dynamic table structures, users had to create report templates at runtime using the
  * JasperReports API. Basically, the whole report template had to be created programmatically, or at least partially
  * modified programatically, in order to remove or reorder columns in these table-like structures. Using the
  * JasperReports API at runtime, while it is a powerful and flexible approach to report template design, remains an
  * unnecessary overhead and complication for the majority of users. Thanks to the introduction of component support
  * in JasperReports, it is now easier to extend the reporting engine and introduce the true tables inside report
  * templates."
  *
  * So basically, Table and other components solve a problem that we don't have...
  */

// Like a ElementGroup, plus a bit more
sealed case class TableCell(
  style : Style,
  box : LineBox,
  height : Int,
  rowSpan : Option[Int],
  children: Seq[Element]);

object TableCell {
  private[core] implicit def drop(o: TableCell) : net.sf.jasperreports.components.table.DesignCell = {
    val r = new net.sf.jasperreports.components.table.DesignCell();
    r.setHeight(o.height);
    // TODO rest
    r
  }
}
// -> drop DesignCell

sealed case class TableGroupCell(
  groupName: String,
  cell: TableCell);

object TableGroupCell {
  private[core] implicit def drop(o: TableGroupCell) : net.sf.jasperreports.components.table.StandardGroupCell = {
    val r = new net.sf.jasperreports.components.table.StandardGroupCell();
    r.setGroupName(o.groupName);
    r.setCell(o.cell);
    r
  }
  private[core] implicit def dropAll(l : Seq[TableGroupCell]) : java.util.List[net.sf.jasperreports.components.table.GroupCell] =
    l map drop

}

abstract sealed class AbstractColumn(
                                      header: TableCell,
                                      footer: TableCell,
                                      groupHeaders : Seq[TableGroupCell],
                                      groupFooters : Seq[TableGroupCell],
                                      // no idea what 'tableHeader' and 'tableFooter' (of a column!) are
                                      tableHeader: TableCell,
                                      tableFooter: TableCell,
                                      width: Int,
                                      printWhenExpression: Expression) {
  implicit def drop() : net.sf.jasperreports.components.table.BaseColumn

  protected def fill(tgt: net.sf.jasperreports.components.table.StandardBaseColumn) = {
    tgt.setColumnHeader(header);
    tgt.setColumnFooter(footer);
    tgt.setGroupHeaders(groupHeaders);
    tgt.setGroupFooters(groupFooters);
    tgt.setTableHeader(tableHeader);
    tgt.setTableFooter(tableFooter);
    tgt.setWidth(width);
    tgt.setPrintWhenExpression(printWhenExpression);
  }
}

sealed case class TableColumn(
  header: TableCell,
  footer: TableCell,
  groupHeaders : Seq[TableGroupCell],
  groupFooters : Seq[TableGroupCell],
  tableHeader: TableCell,
  tableFooter: TableCell,
  width: Int,
  printWhenExpression: Expression,
  detail: TableCell)
  extends AbstractColumn(header, footer, groupHeaders, groupFooters, tableHeader, tableFooter, width, printWhenExpression) {

  implicit override def drop() : net.sf.jasperreports.components.table.StandardColumn = {
    val r = new net.sf.jasperreports.components.table.StandardColumn();
    super.fill(r);
    r.setDetailCell(detail)
    r
  }

}

sealed case class TableColumnGroup(
  header: TableCell,
  footer: TableCell,
  groupHeaders : Seq[TableGroupCell],
  groupFooters : Seq[TableGroupCell],
  tableHeader: TableCell,
  tableFooter: TableCell,
  width: Int,
  printWhenExpression: Expression,
  columns: Seq[AbstractColumn])
  extends AbstractColumn(header, footer, groupHeaders, groupFooters, tableHeader, tableFooter, width, printWhenExpression) {

  implicit override def drop() : net.sf.jasperreports.components.table.StandardColumnGroup = {
    val r = new net.sf.jasperreports.components.table.StandardColumnGroup();
    super.fill(r);
    r.setColumns(columns map { c => c.drop() });
    r
  }
}

sealed case class Table(columns : Seq[AbstractColumn],
                        // datasetRun: JRDatasetRun,
                        whenNoData: WhenNoDataTypeTableEnum) extends Component {
  override implicit def drop() : (net.sf.jasperreports.components.table.StandardTable, net.sf.jasperreports.engine.component.ComponentKey) = {
    val r = new net.sf.jasperreports.components.table.StandardTable()
    r.setColumns(columns map { c => c.drop() })
    r.setWhenNoDataType(whenNoData)
    // TODO datasetRun?
    val dr = new net.sf.jasperreports.engine.design.JRDesignDatasetRun();
    dr.setDatasetName("dummy");

    r.setDatasetRun(dr)
    (r, new ComponentKey("http://jasperreports.sourceforge.net/jasperreports/components", "noprefix", "table"))
  }
}
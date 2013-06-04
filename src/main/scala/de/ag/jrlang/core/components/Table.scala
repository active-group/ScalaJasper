package de.ag.jrlang.core.components

import net.sf.jasperreports.components.table._

import de.ag.jrlang.core._

import scala.collection.JavaConversions._
import net.sf.jasperreports.engine.component.ComponentKey
import net.sf.jasperreports.engine.JRDatasetParameter
import net.sf.jasperreports.engine.design.{JRDesignDatasetParameter, JRDesignSubreportParameter}

import Transformer._
import de.ag.jrlang.core.Dimensions.RestrictedLength

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
    height : BandHeight = BandHeight.Auto,
    content: Seq[Element] = Seq.empty,
    style : AbstractStyle = Style.inherit,
    rowSpan : Option[Int] = None) extends Transformable[DesignCell] {

  def transform : Transformer[DesignCell] = {
    val r = new net.sf.jasperreports.components.table.DesignCell()
    r.setRowSpan(if (rowSpan.isDefined) rowSpan.get : java.lang.Integer else null)

    drop(style.transform) { so => r.setStyleNameReference(so.getOrElse(null)) } >>
    (ElementUtils.contentTransformer(content, r.addElement(_), r.addElementGroup(_)) >>=
      BandHeight.calc(height)(r.setHeight(_))) >>
    ret(r)
  }
}

sealed case class TableGroupCell(
  groupName: String,
  cell: TableCell) extends Transformable[StandardGroupCell] {

  def transform = {
    val r = new net.sf.jasperreports.components.table.StandardGroupCell()
    r.setGroupName(groupName)
    drop(cell.transform) { r.setCell(_) }
    ret(r)
  }
}

abstract sealed class AbstractColumn(
                                      header: Option[TableCell],
                                      footer: Option[TableCell],
                                      groupHeaders : Seq[TableGroupCell],
                                      groupFooters : Seq[TableGroupCell],
                                      // no idea what 'tableHeader' and 'tableFooter' (of a column!) are
                                      tableHeader: Option[TableCell],
                                      tableFooter: Option[TableCell],
                                      width: RestrictedLength,
                                      printWhenExpression: Option[Expression[Boolean]])
  extends Transformable[BaseColumn] {

  protected def fill(tgt: net.sf.jasperreports.components.table.StandardBaseColumn) = {
    currentContainerWidth >>= { containerWidth =>
      val absoluteWidth = width asPartOf containerWidth
      (withContainerWidth(absoluteWidth) {
        drop(orNull(header map { _.transform })) { tgt.setColumnHeader(_) } >>
        drop(orNull(footer map { _.transform })) { tgt.setColumnFooter(_) } >>
        drop(all(groupHeaders map { _.transform })) { tgt.setGroupHeaders(_) } >>
        drop(all(groupFooters map { _.transform })) { tgt.setGroupFooters(_) } >>
        drop(orNull(tableHeader map { _.transform })) { tgt.setTableHeader(_) } >>
        drop(orNull(tableFooter map { _.transform })) { tgt.setTableFooter(_) }
      }) >>
      ret(tgt.setWidth(absoluteWidth inAbsolutePixels)) >>
      drop(orNull(printWhenExpression map { _.transform })) { tgt.setPrintWhenExpression(_) } >>
      setCurrentContainerWidth(containerWidth) >> // reset
      ret(absoluteWidth) }
  }
}

sealed case class TableColumn(
  width: RestrictedLength, // restricted by containing element (page)
  detail: TableCell,
  header: Option[TableCell] = None,
  footer: Option[TableCell] = None,
  groupHeaders : Seq[TableGroupCell] = Vector.empty,
  groupFooters : Seq[TableGroupCell] = Vector.empty,
  tableHeader: Option[TableCell] = None,
  tableFooter: Option[TableCell] = None,
  printWhenExpression: Option[Expression[Boolean]] = None)
  extends AbstractColumn(header, footer, groupHeaders, groupFooters, tableHeader, tableFooter, width, printWhenExpression)
  with Transformable[StandardColumn]
{

  override def transform = {
    val r = new net.sf.jasperreports.components.table.StandardColumn();
    super.fill(r) >>= { absoluteWidth =>
      withContainerWidth(absoluteWidth) {
        drop(detail.transform) { r.setDetailCell(_) } >>
        ret(r)
      }
    }
  }
}

sealed case class TableColumnGroup(
  header: Option[TableCell],
  footer: Option[TableCell],
  groupHeaders : Seq[TableGroupCell],
  groupFooters : Seq[TableGroupCell],
  tableHeader: Option[TableCell],
  tableFooter: Option[TableCell],
  width: RestrictedLength,
  printWhenExpression: Option[Expression[Boolean]],
  columns: Seq[AbstractColumn])
  extends AbstractColumn(header, footer, groupHeaders, groupFooters, tableHeader, tableFooter, width, printWhenExpression)
  with Transformable[StandardColumnGroup] {

  override def transform : Transformer[StandardColumnGroup] = {
    val r = new net.sf.jasperreports.components.table.StandardColumnGroup()
    super.fill(r) >>= { absoluteWidth =>
      withContainerWidth(absoluteWidth) {
        drop(all(columns map {_.transform})) { r.setColumns(_) } >>
        ret(r)
      }
    }
  }
}

sealed case class Table(columns : Seq[AbstractColumn],
                        data: Data,
                        whenNoData: WhenNoDataTypeTableEnum)
  extends Component {

  override def transform : Transformer[(net.sf.jasperreports.components.table.StandardTable, net.sf.jasperreports.engine.component.ComponentKey)] = {
    val r = new net.sf.jasperreports.components.table.StandardTable()
    r.setWhenNoDataType(whenNoData)
    // The columns (the content) have to be transformed in a fresh expression environment, the auto args
    // then added to the data(-set) implicitly...
    //withNewEnvironment {
      drop(all(columns map {_.transform})) { r.setColumns(_) } >>
      // "If no dataset run is specified for a chart or crosstab, the main dataset of the report is used."
      // Apparently, that is not true for tables - a NPE is raised when you try it - so you cannot have a table of the main dataset
      drop(data.transform) { r.setDatasetRun(_) } >> // must be last!
      ret(r, new ComponentKey("http://jasperreports.sourceforge.net/jasperreports/components", "noprefix", "table"))
    //}
  }
}
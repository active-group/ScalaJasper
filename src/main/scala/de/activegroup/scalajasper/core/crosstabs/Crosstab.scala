package de.activegroup.scalajasper.core.crosstabs

import de.activegroup.scalajasper.core.Dimensions.Length
import de.activegroup.scalajasper.core.{Data, Dimensions, Element, ElementUtils, Expression, TextField, Transformer}
import de.activegroup.scalajasper.core.Transformer.{all, drop, orNull, ret}
import net.sf.jasperreports.crosstabs.`type`.CrosstabColumnPositionEnum
import Dimensions._
import net.sf.jasperreports.engine.`type`.CalculationEnum


// FIXME: when the fields are using Expression.const, it does not work due to
// some weird parameter passing stuff. Work around by placing
// textfields directly on the appropiate position
sealed case class CrosstabHeaderCell(
  fields: Seq[TextField]) {

  private[core] def transform = {
    val r = new net.sf.jasperreports.crosstabs.design.JRDesignCellContents()

    ElementUtils.contentTransformer(fields, r.addElement, r.addElementGroup) >>
    ret(r)
  }
}

sealed case class CrosstabBucket(
  expression: Expression[Any],
  orderByExpression: Option[Expression[Any]]) {

  private[core] def transform = {
    val r = new net.sf.jasperreports.crosstabs.design.JRDesignCrosstabBucket()
    r.setValueClassName("java.lang.String")

    drop(expression.transform) {r.setExpression(_)} >>
      drop(orNull(orderByExpression.map(_.transform))) {r.setOrderByExpression(_)} >>
      ret(r)

  }
}

sealed case class CrosstabRowGroup(
                                    name: String,
                                    width: Int,
                                    bucket: CrosstabBucket,
                                    headers: CrosstabHeaderCell
                                  ) {

  private[core] def transform = {
    val r = new net.sf.jasperreports.crosstabs.design.JRDesignCrosstabRowGroup()
    r.setName(name)
    r.setWidth(width)

    drop(bucket.transform) {r.setBucket} >>
      drop(headers.transform) {r.setHeader} >>
      ret(r)
  }
}

sealed case class CrosstabColGroup(
                                    name: String,
                                    height: Int,
                                    headerPos: CrosstabColumnPositionEnum,
                                    bucket: CrosstabBucket,
                                    headers: CrosstabHeaderCell) {
  private[core] def transform = {
    val r = new net.sf.jasperreports.crosstabs.design.JRDesignCrosstabColumnGroup()
    r.setName(name)
    r.setHeight(height)
    r.setPosition(headerPos)

    drop(bucket.transform) {r.setBucket} >>
      drop(headers.transform) {r.setHeader} >>
      ret(r)

  }


}

sealed case class CrosstabMeasure(
  name: String,
  expression: Expression[Any],
  className: String,
  calculation: CalculationEnum) {

  private[core] def transform = {
    val r = new net.sf.jasperreports.crosstabs.design.JRDesignCrosstabMeasure()
    r.setName(name)
    r.setCalculation(calculation)
    r.setValueClassName(className)
    drop(expression.transform) {r.setValueExpression} >>
      ret(r)
  }
}


sealed case class CrosstabCell(
                                width: Int,
                                height: Int,
                                cell: CrosstabHeaderCell) { // todo. header cell wrong name? or different types?‚
  private[core] def transform = {
    val r = new net.sf.jasperreports.crosstabs.design.JRDesignCrosstabCell()
    r.setWidth(width)
    r.setHeight(height)

    drop(cell.transform){r.setContents} >>
      ret(r)
  }

}

sealed case class CrosstabDataset(data: Data) {
  private[core] def transform = {
    val r = new net.sf.jasperreports.crosstabs.design.JRDesignCrosstabDataset()

    drop(data.transform){r.setDatasetRun} >>
      ret(r)
  }
}

sealed case class Crosstab(x: Int,
                           y: Int,
                           width: Int,
                           height: Int,
                           data: CrosstabDataset,
                           header: CrosstabHeaderCell,
                           rowGroups: Seq[CrosstabRowGroup],
                           colGroups: Seq[CrosstabColGroup],
                           measures: Seq[CrosstabMeasure],
                           cell: CrosstabCell) extends Element {

  private[core] def transform = {
    val r = new net.sf.jasperreports.crosstabs.design.JRDesignCrosstab()

    r.setX(x)
    r.setY(y)
    r.setWidth(width)
    r.setHeight(height)

    import net.sf.jasperreports.engine.design.JRDesignExpression

    // a crosstab should allow access to the auto-parameters, so
    // use the REPORT_PARAMETERS_MAP to pass them.
    val mapExp = new JRDesignExpression
    mapExp.setText("$P{REPORT_PARAMETERS_MAP}")
    r.setParametersMapExpression(mapExp)

    drop(header.transform) {r.setHeaderCell} >>
      drop(all(rowGroups map {_.transform})) { _.foreach(rg =>
        r.addRowGroup(rg))} >>
      drop(all(colGroups map {_.transform})) { _.foreach(cg =>
        r.addColumnGroup(cg))} >>
      drop(all(measures map {_.transform})) { _.foreach(m =>
        r.addMeasure(m))} >>
      drop(cell.transform) {r.addCell} >> // todo: more cells‚
      drop(data.transform) {r.setDataset} >>
      ret(r)
  }

  override def verticalExtent = Dimensions.Length(y + height, LengthUnit.px) // todo: w‚hy not .px directly?

  override def moveVertically(len: Length) = copy(y = y + len.inAbsolutePixels)

}

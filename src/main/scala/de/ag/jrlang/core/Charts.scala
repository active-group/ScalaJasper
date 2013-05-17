package de.ag.jrlang.core

sealed case class ChartLegend(
    color: Option[java.awt.Color],
    backgroundColor: Option[java.awt.Color],
    position: net.sf.jasperreports.charts.`type`.EdgeEnum);

// TODO: EnvCollection for all expressions
sealed case class ChartTitle(
    color: Option[java.awt.Color],
    expression: Expression,
    font: Font,
    position: net.sf.jasperreports.charts.`type`.EdgeEnum);

sealed case class ChartSubtitle(
    color: Option[java.awt.Color],
    expression: Expression,
    font: Font);


abstract sealed class ChartType(value: Byte);

abstract sealed class JRChartDataset(value : Byte);

sealed case class JRPieSeries(
    keyExpression: Expression,
    labelExpression: Expression,
    sectionHyperlink: JRHyperlink,
    valueExpression: Expression
    );

sealed case class JRPieDataset(
    series: IndexedSeq[JRPieSeries],
    maxCount: Int,
    minPercentage: Float,
    otherKeyExpression: Expression,
    otherLabelExpression: Expression,
    otherSectionHyperlink: JRHyperlink
    ) extends JRChartDataset(net.sf.jasperreports.engine.JRChartDataset.PIE_DATASET);

sealed case class JRItemLabel(
    color: java.awt.Color,
    backgroundColor: java.awt.Color,
    font: Font
    )

sealed case class JRPiePlot(
    circular: Boolean,
    itemLabel: JRItemLabel,
    labelFormat: String,
    legenLabelFormat: String,
    showLabels: Boolean
    );

sealed case class PieChart(
    dataset: JRPieDataset,
    plot: JRPiePlot
    ) extends ChartType(net.sf.jasperreports.engine.JRChart.CHART_TYPE_PIE);

// TODO: about 10 more

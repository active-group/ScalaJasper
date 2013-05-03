package de.ag.jrlang.core


abstract sealed class JRChartType(value: Byte);

abstract sealed class JRChartDataset(value : Byte);

sealed case class JRPieSeries(
    keyExpression: JRDesignExpression,
    labelExpression: JRDesignExpression,
    sectionHyperlink: JRHyperlink,
    valueExpression: JRDesignExpression
    );

sealed case class JRPieDataset(
    series: IndexedSeq[JRPieSeries],
    maxCount: Int,
    minPercentage: Float,
    otherKeyExpression: JRDesignExpression,
    otherLabelExpression: JRDesignExpression,
    otherSectionHyperlink: JRHyperlink
    ) extends JRChartDataset(net.sf.jasperreports.engine.JRChartDataset.PIE_DATASET);

sealed case class JRItemLabel(
    color: java.awt.Color,
    backgroundColor: java.awt.Color,
    font: JRFont
    )

sealed case class JRPiePlot(
    circular: Boolean,
    itemLabel: JRItemLabel,
    labelFormat: String,
    legenLabelFormat: String,
    showLabels: Boolean
    );

sealed case class JRPieChart(
    dataset: JRPieDataset,
    plot: JRPiePlot
    ) extends JRChartType(net.sf.jasperreports.engine.JRChart.CHART_TYPE_PIE);

// TODO: about 10 more

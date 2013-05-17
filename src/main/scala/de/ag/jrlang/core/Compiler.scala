package de.ag.jrlang.core

import net.sf.jasperreports.{engine => jre}

object Compiler {
  implicit def compile(o : Report) : (jre.JasperReport, Map[String, AnyRef]) = {
    // 1. compilation step: collect all styles used, replacing them with uniquely generated names.
    val defaultStyleName = "default";
    val predef = Map(defaultStyleName -> o.defaultStyle);
    val (o2, st_) = o.foldStyles(StylesMap(predef));

    // TODO: Something similar with subdatasets and dataset-runs
    // The collected global environment for expressions is transformed into a list of automatic parameters and arguments
    val env = o2.collectEnv(Map.empty)
    val envParams = env map { case(p, _) => p } toList
    val envArgs = env map { case(p, v) => (p.name, v) }
    val allParams = o2.mainDataset.parameters ++ envParams
    val o3 = o2.copy(mainDataset = o2.mainDataset.copy(parameters = allParams))
    // o2 should now contain only style-references ("External"), no internal styles anymore

    // create new object, setting default style and the rest
    val res = new jre.design.JasperDesign();
    val styles = st_.list;
    val finaldesign = Report.translate(o3, res, defaultStyleName, styles);
    val finalreport = jre.JasperCompileManager.compileReport(finaldesign)
    (finalreport, envArgs)
  }



}
package de.ag.jrlang.core

import net.sf.jasperreports.{engine => jre}

object Compiler {
  implicit def compile(o : Report) : jre.design.JasperDesign = {
    // 1. compilation step: collect all styles used, replacing them with uniquely generated names.
    val defaultStyleName = "default";
    val predef = Map(defaultStyleName -> o.defaultStyle);
    val (o2, st_) = o.foldStyles(StylesMap(predef)); 
    // o2 should now contain only style-references ("External"), no internal styles anymore

    // create new object, setting default style and the rest
    val res = new jre.design.JasperDesign();
    val styles = st_.list;
    Report.translate(o2, res, defaultStyleName, styles)
  }



}
package de.ag.jrlang.core

sealed case class JRDesignVariable(
    name: String,
    calculation: net.sf.jasperreports.engine.`type`.CalculationEnum,
    expression: String,
    incrementerGroup: JRDesignGroup,
    incrementerType: net.sf.jasperreports.engine.`type`.IncrementTypeEnum,
    incrementerFactoryClassName: String,
    resetType: net.sf.jasperreports.engine.`type`.ResetTypeEnum,
    resetGroup: JRDesignGroup,
    // maybe always false? systemDefined: Boolean,
    valueClassName: String
    ); // extends LeafAdapter[net.sf.jasperreports.engine.design.JRDesignVariable]
package de.ag.jrlang.core

sealed case class JRDesignParameter(
    name: String,
    defaultValueExpression: String,
    isForPrompting: Boolean,
    nestedTypeName: String,
    // maybe always false? systemDefined: Boolean,
    valueClassName: String,
    // JRBaseParamter:
    description: String
);

object JRDesignParameter {
  implicit def dropParameter(o: JRDesignParameter) : net.sf.jasperreports.engine.design.JRDesignParameter = {
    val r = new net.sf.jasperreports.engine.design.JRDesignParameter();
    r.setName(o.name);
    r.setDefaultValueExpression(o.defaultValueExpression);
    r.setForPrompting(o.isForPrompting);
    r.setNestedTypeName(o.nestedTypeName);
    r.setValueClassName(o.valueClassName);
    r.setDescription(o.description);
    r
  }

}
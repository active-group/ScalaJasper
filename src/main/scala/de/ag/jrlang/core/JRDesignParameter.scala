package de.ag.jrlang.core

sealed case class JRDesignParameter(
    name: String,
    defaultValueExpression: Option[Expression],
    isForPrompting: Boolean, // True - show GUI to ask user for value; WTF?? remove?! description too; used for that
    nestedTypeName: String,
    // maybe always false? systemDefined: Boolean,
    valueClassName: String,
    // JRBaseParamter:
    description: String
);

object JRDesignParameter {
  def apply(name: String, defaultValueExpression : Option[Expression] = None) =
    new JRDesignParameter(
        name = name,
        defaultValueExpression = defaultValueExpression,
        isForPrompting = false,
        nestedTypeName = null, // TODO use "" here, and convert to null belows
        valueClassName = "java.lang.String", // maybe make subclasses "StringParameter", etc.?
        description = "");
  
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
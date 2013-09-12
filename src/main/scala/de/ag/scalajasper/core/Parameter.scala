package de.ag.scalajasper.core

import net.sf.jasperreports.engine.design.JRDesignParameter

import Transformer._

sealed case class Parameter( // add type parameter?
    name: String,
    defaultValueExpression: Option[Expression[Any]] = None,
    isForPrompting: Boolean = false, // True - show GUI to ask user for value; WTF?? remove?! description too; used for that
    nestedTypeName: String = null,
    // maybe always false? systemDefined: Boolean,
    valueClassName: String = "java.lang.String",
    // JRBaseParamter:
    description: String = ""
) {
  private[core] def transform = {
    val r = new JRDesignParameter()
    ret(r.setName(name)) >>
    drop(orNull(defaultValueExpression map { _.transform })) { r.setDefaultValueExpression(_) } >>
    ret(r.setForPrompting(isForPrompting)) >>
    ret(r.setNestedTypeName(nestedTypeName)) >>
    ret(r.setValueClassName(valueClassName)) >>
    ret(r.setDescription(description)) >>
    ret(r)
  }
}

package de.ag.jrlang.core

sealed case class JRDesignSortField(
    name: String,
    order: net.sf.jasperreports.engine.`type`.SortOrderEnum,
    fieldType: net.sf.jasperreports.engine.`type`.SortFieldTypeEnum);

sealed case class JRDesignScriptlet(
    name: String,
    description: String,
    valueClassName: String);

sealed case class JRDesignGroup {
   // ... quite a lot
  
}

sealed case class JRDesignDataset(
    query: JRDesignQuery,
    parameters : Seq[JRDesignParameter], // without system parameters!  // Map-Like
    variables : Seq[JRDesignVariable], // without system variables!  // Map-Like
    fields : Map[String,String], // maps Name -> ClassName
    sortFields : Seq[JRDesignSortField],
  // setQuery?
    scriptlets : IndexedSeq[JRDesignScriptlet], // Map-Like
    scriptletClassName: String,
    groups : Seq[JRDesignGroup], // Map-Like
    resourceBundle: String,
    filterExpression: Option[Expression],
    whenResourceMissingType: net.sf.jasperreports.engine.`type`.WhenResourceMissingTypeEnum,
    customProperties: Map[String, String]
);
  
  /* TODO: These are more or less static (different for main and sub datasets)
  def systemParameters : Seq[net.sf.jasperreports.engine.JRParameter] =
    // obj.getParametersList() filter { p : Any => p.asInstanceOf[net.sf.jasperreports.engine.JRParameter].isSystemDefined() };
    obj.getParametersList().asInstanceOf[List[net.sf.jasperreports.engine.JRParameter]] filter { p => p.isSystemDefined() };
  
  def systemVariables : Seq[net.sf.jasperreports.engine.JRVariable] =
    obj.getVariablesList().asInstanceOf[List[net.sf.jasperreports.engine.JRVariable]] filter { p => p.isSystemDefined() };

  */ 

object JRDesignDataset {
  val empty = new JRDesignDataset(
      query = JRDesignQuery.empty,
      parameters = Vector.empty,
      variables = Vector.empty,
      fields = Map.empty,
      sortFields = Vector.empty,
      scriptlets = Vector.empty,
      scriptletClassName = "",
      groups = Vector.empty,
      resourceBundle = "",
      filterExpression = None,
      whenResourceMissingType = net.sf.jasperreports.engine.`type`.WhenResourceMissingTypeEnum.NULL, //??
      customProperties = Map.empty
      )
}
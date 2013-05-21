package de.ag.jrlang.core

import net.sf.jasperreports.engine.JRField
import net.sf.jasperreports.engine.design.JRDesignField

// TODO: global type
sealed case class DatasetRun(datasetName: String,
                             // TODO: more, parameters...?
                             dataSourceExpression: Expression)
// TODO: EnvCollector

object DatasetRun {
  implicit def drop(o: DatasetRun) = {
    // experimental
    val r = new net.sf.jasperreports.engine.design.JRDesignDatasetRun()
    r.setDatasetName(o.datasetName)
    r.setDataSourceExpression(o.dataSourceExpression)
    /*dr.addParameter({ val p = new JRDesignDatasetParameter(); p.setName("p1");
      //p.setExpression(Expression.raw("new java.util.ArrayList()"));
      //p.setExpression(Expression.call({ _:AnyRef => asJavaList(Vector("a", "b")) }, Expression.const(null)));
      p.setExpression(Expression.raw("Arrays.asList(new String[]{\"a\", \"b\"})"))
      p})*/
    r
  }
}


sealed case class JRDesignSortField(
    name: String,
    order: net.sf.jasperreports.engine.`type`.SortOrderEnum,
    fieldType: net.sf.jasperreports.engine.`type`.SortFieldTypeEnum);

sealed case class JRDesignScriptlet(
    name: String,
    description: String,
    valueClassName: String);

sealed case class JRDesignGroup() {
   // ... quite a lot
  
}

sealed case class Dataset(
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
) extends EnvCollector{
  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    filterExpression.collectEnv(e0)
};
  
  /* TODO: These are more or less static (different for main and sub datasets)
  def systemParameters : Seq[net.sf.jasperreports.engine.JRParameter] =
    // obj.getParametersList() filter { p : Any => p.asInstanceOf[net.sf.jasperreports.engine.JRParameter].isSystemDefined() };
    obj.getParametersList().asInstanceOf[List[net.sf.jasperreports.engine.JRParameter]] filter { p => p.isSystemDefined() };
  
  def systemVariables : Seq[net.sf.jasperreports.engine.JRVariable] =
    obj.getVariablesList().asInstanceOf[List[net.sf.jasperreports.engine.JRVariable]] filter { p => p.isSystemDefined() };

  */ 

object Dataset {
  val empty = new Dataset(
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

  implicit def drop(o: Dataset): net.sf.jasperreports.engine.design.JRDesignDataset = {
    val r = new net.sf.jasperreports.engine.design.JRDesignDataset(false); // isMain = false
    // TODO: Common code with Report?!
    for (p <- o.parameters)
      r.addParameter(p);
    for ((n,c) <- o.fields)
      r.addField({ val f = new JRDesignField(); f.setName(n); f.setValueClassName(c); f })
    // TODO: rest
    r;
  }
}
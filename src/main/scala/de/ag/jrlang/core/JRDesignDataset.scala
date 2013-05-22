package de.ag.jrlang.core

import net.sf.jasperreports.engine.JRField
import net.sf.jasperreports.engine.design.{JRDesignDatasetParameter, JRDesignField}

abstract sealed class Data;

/** A dataset run declaration supplies the values for the dataset parameters as well as the data source through which
  * the dataset will iterate. Optionally, a java.sql.Connection can be passed to the dataset instead of a JRDataSource
  * instance, when there is a SQL query associated with the dataset. */
// removing the connection+query option altogether could simplify things a lot; we have the better option of specifying
// a function call as datasourceExpression, which returns a JRResultSetDataSource at runtime.
 sealed case class DatasetRun(datasetName: String,
                              arguments: Map[String, Expression],
                              dataSourceExpression: Expression) extends Data
// TODO: EnvCollector

object DatasetRun {
  implicit def drop(o: DatasetRun) = {
    val r = new net.sf.jasperreports.engine.design.JRDesignDatasetRun()
    r.setDatasetName(o.datasetName)
    for ((n, e) <- o.arguments) {
      val p = new JRDesignDatasetParameter()
      p.setName(n)
      p.setExpression(e)
      r.addParameter(p)
    }
    r.setDataSourceExpression(o.dataSourceExpression)
    r
  }
}

sealed case class DataQuery(fields: Map[String,String],
                            sortFields: Seq[SortField],
                            filter: Expression,
                            groups: Seq[JRDesignGroup],
                            resourceBundle: String, // ??
                            scriptlets : IndexedSeq[Scriptlet], // Map-Like
                            scriptletClassName: String
                            )

sealed case class DataDef(query : DataQuery, source : Expression) extends Data
// translate this to a DatasetRun and a new Dataset

sealed case class SortField(
    name: String,
    order: net.sf.jasperreports.engine.`type`.SortOrderEnum,
    fieldType: net.sf.jasperreports.engine.`type`.SortFieldTypeEnum);

sealed case class Scriptlet(
    name: String,
    description: String,
    valueClassName: String);

sealed case class JRDesignGroup() {
   // ... quite a lot
  
}

// A dataset is a sort of parametrized schema definition of data, or alternatively of an sql query expression
// We could/should try to derive all subdatasets of a report by moving the definition to all places that reference
// subdatasets by name (and generate the names) - the places are all local; in datasetRuns within components,
// charts, crosstabs; and maybe more? Like Style.Internal and External.. make DatasetRun.Implicit/Explicit or .Reference/.Schema
sealed case class Dataset(
    query: JRDesignQuery,
    parameters : Seq[JRDesignParameter], // without system parameters!  // Map-Like
    variables : Seq[JRDesignVariable], // without system variables!  // Map-Like
    fields : Map[String,String], // maps Name -> ClassName
    sortFields : Seq[SortField],
  // setQuery?
    scriptlets : IndexedSeq[Scriptlet], // Map-Like
    scriptletClassName: String,
    groups : Seq[JRDesignGroup], // Map-Like
    resourceBundle: String,
    filterExpression: Option[Expression],
    whenResourceMissingType: net.sf.jasperreports.engine.`type`.WhenResourceMissingTypeEnum,
    customProperties: Map[String, String]
) extends EnvCollector{
  private[core] def collectEnv(e0: Map[JRDesignParameter, AnyRef]): Map[JRDesignParameter, AnyRef] =
    // correct? take care about which expressions are evaluated in the report environment, and those that are
    // evaluated in the sub-data environment.
    // not correct: filterExpression.collectEnv(e0)
    e0
}
  
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
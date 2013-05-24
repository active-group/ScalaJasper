package de.ag.jrlang.core

import net.sf.jasperreports.engine.JRField
import net.sf.jasperreports.engine.design.{JRDesignDataset, JRDesignDatasetRun, JRDesignDatasetParameter, JRDesignField}

import Transformer._
import net.sf.jasperreports.engine.JRDataSource


abstract sealed class Data extends Transformable[JRDesignDatasetRun]

/** A dataset run declaration supplies the values for the dataset parameters as well as the data source through which
  * the dataset will iterate. Optionally, a java.sql.Connection can be passed to the dataset instead of a JRDataSource
  * instance, when there is a SQL query associated with the dataset. */
/** use DataDef instead */
// removing the connection+query option altogether could simplify things a lot; we have the better option of specifying
// a function call as datasourceExpression, which returns a JRResultSetDataSource at runtime.
sealed case class DatasetRun(datasetName: String,
                             arguments: Map[String, Expression[Any]],
// TODO parametersMapExpression? connectionExpression?
                             dataSourceExpression: Expression[Any]) extends Data{
  def transform = {
    val r = new JRDesignDatasetRun()
    r.setDatasetName(datasetName)
    (all(arguments map { case(n, e) => {
      val p = new JRDesignDatasetParameter()
      p.setName(n)
      drop(e.transform) { p.setExpression(_) } >>
      ret(p)
    }} toSeq) >>= {
      ps => ps foreach { r.addParameter(_) }; ret()
    }) >>
    drop(dataSourceExpression.transform) { r.setDataSourceExpression(_)} >>
    ret(r)
  }
}

sealed case class DataQuery(fields: Map[String,String],
                            sortFields: Seq[SortField],
                            filter: Expression[Boolean],
                            groups: Seq[JRDesignGroup],
                            resourceBundle: String, // ??
                            scriptlets : IndexedSeq[Scriptlet], // Map-Like
                            scriptletClassName: String
                            )

sealed case class DataDef(query : DataQuery, source : Expression[JRDataSource]) extends Data {
  // translate this to a DatasetRun and a new Dataset
  def transform = {
    // TODO
    ret(null)
  }
}

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
    parameters : Seq[Parameter], // without system parameters!  // Map-Like
    variables : Seq[JRDesignVariable], // without system variables!  // Map-Like
    fields : Map[String,String], // maps Name -> ClassName
    sortFields : Seq[SortField],
  // setQuery?
    scriptlets : IndexedSeq[Scriptlet], // Map-Like
    scriptletClassName: String,
    groups : Seq[JRDesignGroup], // Map-Like
    resourceBundle: String,
    filterExpression: Option[Expression[Boolean]],
    whenResourceMissingType: net.sf.jasperreports.engine.`type`.WhenResourceMissingTypeEnum,
    customProperties: Map[String, String])
  extends Transformable[JRDesignDataset] {

  def transform = {
    val r = new JRDesignDataset(false) // isMain = false
    fill(r) >>
    ret(r)
  }

  private[core] def fill(r : JRDesignDataset) = {
    for ((n,c) <- fields)
      r.addField({
        val f = new JRDesignField();
        f.setName(n); f.setValueClassName(c); f })

    // TODO for (v <- mainDataset.groups) r.addGroup(v);
    // TODO for (v <- mainDataset.scriptlets) r.addScriptlet(v);
    // user defined parameters (generated parameters are added by caller)
    (all(parameters map {_.transform}) >>= {
      ps => ps foreach { r.addParameter(_) }; ret()
    }) >>
    // TODO for (v <- mainDataset.variables) r.addVariable(v);
    // TODO: rest
    ret()
  }
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

  /*
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
  */
}
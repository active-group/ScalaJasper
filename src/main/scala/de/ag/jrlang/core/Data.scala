package de.ag.jrlang.core

import net.sf.jasperreports.engine.{JRScriptlet, JRDatasetRun, JRField, JRDataSource}
import net.sf.jasperreports.engine.design._

import Transformer._
import net.sf.jasperreports.engine.`type`.WhenResourceMissingTypeEnum


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

sealed case class DataDef(query : Dataset,
                          source : Expression[JRDataSource],
                          arguments : Map[String, Expression[Any]] = Map.empty) extends Data {
  // translate this to a DatasetRun and a new Dataset
  def transform = {
    // transform into global env? or new one... then put env into args here...?
    Transformer.datasetName(query, { () => query.transform }) >>= {
      name => {
        DatasetRun(name, arguments, source).transform
      }
    }
  }
}

sealed case class SortField(
    name: String,
    order: net.sf.jasperreports.engine.`type`.SortOrderEnum,
    fieldType: net.sf.jasperreports.engine.`type`.SortFieldTypeEnum) extends Transformable[JRDesignSortField] {

  def transform = {
    val r = new JRDesignSortField()
    r.setName(name)
    r.setOrder(order)
    r.setType(fieldType)
    ret(r)
  }
}

sealed case class Group(

                         ) extends Transformable[JRDesignGroup] {
   // ... quite a lot
  def transform : Transformer[JRDesignGroup] = null
}

sealed case class Variable(name: String,
                           calculation: net.sf.jasperreports.engine.`type`.CalculationEnum,
                           expression: Expression[Any], // ??
                           valueClassName: String,
                           incrementerGroup: Group,
                           incrementType: net.sf.jasperreports.engine.`type`.IncrementTypeEnum,
                           incrementerFactoryClassName: String,
                           // TODO: resetType and resetGroup belong together - maybe the same for incrementer...
                           resetType: net.sf.jasperreports.engine.`type`.ResetTypeEnum,
                           resetGroup: Group
                           )
  extends Transformable[JRDesignVariable] {

  def transform = {
    val r = new JRDesignVariable()
    r.setName(name)
    r.setCalculation(calculation)
    r.setIncrementerFactoryClassName(incrementerFactoryClassName)
    r.setIncrementType(incrementType)
    r.setResetType(resetType)
    r.setValueClassName(valueClassName)

    drop(expression.transform) { r.setExpression(_) } >>
    // maybe groups have to be registered globally too???
    drop(incrementerGroup.transform) { r.setIncrementGroup(_) } >>
    drop(resetGroup.transform) { r.setResetGroup(_) } >>
    ret(r)
  }
}


// A dataset is a sort of parametrized schema definition of data, or alternatively of an sql query expression
// We could/should try to derive all subdatasets of a report by moving the definition to all places that reference
// subdatasets by name (and generate the names) - the places are all local; in datasetRuns within components,
// charts, crosstabs; and maybe more? Like Style.Internal and External.. make DatasetRun.Implicit/Explicit or .Reference/.Schema
sealed case class Dataset(
    parameters : Seq[Parameter] = Vector.empty, // without system parameters!  // Map-Like
    variables : Seq[Variable] = Vector.empty, // without system variables!  // Map-Like
    fields : Map[String,String] = Map.empty, // maps Name -> ClassName
    sortFields : Seq[SortField] = Vector.empty,
    query: JRDesignQuery = null, // don't use this, use a JRResultSetDataSource instead (remove?)
    /** Simple variable expressions cannot always implement complex functionality. This is where scriptlets
      * come in. Scriptlets are sequences of Java code that are executed every time a report event occurs. Through
      * scriptlets, users can affect the values stored by the report variables. */
    // so, maybe we don't need that
    scriptlets : IndexedSeq[JRScriptlet] = Vector.empty, // Map-Like
    scriptletClassName: String = "",
    groups : Seq[Group] =  Vector.empty, // Map-Like
    resourceBundle: String = "",
    filterExpression: Option[Expression[Boolean]] = None,
    whenResourceMissingType: WhenResourceMissingTypeEnum = WhenResourceMissingTypeEnum.NULL,
    customProperties: Map[String, String] = Map.empty) // remove?
  extends Transformable[JRDesignDataset] {

  def transform = {
    val r = new JRDesignDataset(false) // isMain = false
    // name must be set externally
    fill(r) >>
    ret(r)
  }

  private[core] def fill(r : JRDesignDataset) = {
    for ((n,c) <- fields)
      r.addField({
        val f = new JRDesignField()
        f.setName(n)
        f.setValueClassName(c)
        f })
    r.setQuery(query)
    scriptlets foreach { r.addScriptlet(_) }
    r.setScriptletClass(scriptletClassName)
    r.setResourceBundle(resourceBundle)
    r.setWhenResourceMissingType(whenResourceMissingType)
    customProperties foreach { case(n,e) => r.setProperty(n, e) }

    // user defined parameters (generated parameters are added by caller)
    (all(parameters map {_.transform}) >>= {
      ps => ps foreach { r.addParameter(_) }; ret()
    }) >>
    (all(variables map {_.transform}) >>= {
      vs => vs foreach { r.addVariable(_) }; ret()
    }) >>
    (all(sortFields map { _.transform }) >>= {
      sfs => sfs foreach { r.addSortField(_) }; ret()
    }) >>
    (all(groups map { _.transform }) >>= {
      g => g foreach { r.addGroup(_) }; ret()
    }) >>
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
  val empty = new Dataset()
}
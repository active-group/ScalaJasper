package de.ag.jrlang.core

import net.sf.jasperreports.engine.{JRScriptlet, JRDatasetRun, JRField, JRDataSource}
import net.sf.jasperreports.engine.design._

import Transformer._
import net.sf.jasperreports.engine.`type`._


abstract sealed class Data {
  private[core] def transform : Transformer[JRDesignDatasetRun]
}

/** A dataset run declaration supplies the values for the dataset parameters as well as the data source through which
  * the dataset will iterate. Optionally, a java.sql.Connection can be passed to the dataset instead of a JRDataSource
  * instance, when there is a SQL query associated with the dataset. */
/** use DataDef instead */
// removing the connection+query option altogether could simplify things a lot; we have the better option of specifying
// a function call as datasourceExpression, which returns a JRResultSetDataSource at runtime.
sealed case class DatasetRun(datasetName: String,
                             arguments: Map[String, Expression[Any]] = Map.empty,
                             argumentsMapExpression: Option[Expression[java.util.Map[String, AnyRef]]] = None,
                             dataSourceExpression: Option[Expression[JRDataSource]] = None,
                             connectionExpression: Option[Expression[java.sql.Connection]] = None
                             ) extends Data{
  private[core] def transform = {
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
    drop(orNull(argumentsMapExpression map {_.transform})) { r.setParametersMapExpression(_)} >>
    drop(orNull(dataSourceExpression map {_.transform})) { r.setDataSourceExpression(_)} >>
    drop(orNull(connectionExpression map {_.transform})) { r.setConnectionExpression(_)} >>
    ret(r)
  }
}

// TODO: From some (special) types of data sources, we might be able to infer the dataset...
sealed case class DataDef(dataset : Dataset,
                          source : Expression[JRDataSource],
                          arguments : Map[String, Expression[Any]] = Map.empty) extends Data {
  // translate this to a DatasetRun and a new Dataset
  private[core] def transform = {
    // getCurrentEnvironment >>= { env =>
      Transformer.datasetName(dataset, { () => dataset.transform }) >>= {
        name =>
          DatasetRun(datasetName = name, arguments = arguments, dataSourceExpression = Some(source),
            // pass all values from global report args, parameter declarations are added in compile() later
            argumentsMapExpression = Some(Expression.P("REPORT_PARAMETERS_MAP"))
          ).transform
        }
    // }
  }
}

sealed case class SortField(
    name: String,
    order: net.sf.jasperreports.engine.`type`.SortOrderEnum,
    fieldType: net.sf.jasperreports.engine.`type`.SortFieldTypeEnum) {

  private[core] def transform = {
    val r = new JRDesignSortField()
    r.setName(name)
    r.setOrder(order)
    r.setType(fieldType)
    ret(r)
  }
}

/*// although it only refers to a globally defined group, the API forces us to pass the object at compile time
abstract sealed class GroupRef extends Transformable[JRDesignGroup]

sealed case class Group
*/

sealed case class Group(/** consecutive records with the same value form the group */
                        expression: Expression[Any],
                        header: Seq[Band] = Vector.empty,
                        footer: Seq[Band] = Vector.empty,
                        footerPosition: FooterPositionEnum = FooterPositionEnum.NORMAL,
                        startNewColumn: Boolean = false,
                        resetPageNumber: Boolean = false,
                        reprintHeaderOnEachPage: Boolean = false,
                        minHeightToStartNewPage: Int = 0,
                        keepTogether: Boolean = false
                         )
{
   // ... quite a lot
  private[core] def transform : Transformer[JRDesignGroup] = {
     val r = new JRDesignGroup()
     r.setFooterPosition(footerPosition)
     r.setStartNewColumn(startNewColumn)
     r.setResetPageNumber(resetPageNumber)
     r.setReprintHeaderOnEachPage(reprintHeaderOnEachPage)
     r.setMinHeightToStartNewPage(minHeightToStartNewPage)
     r.setKeepTogether(keepTogether)

     drop(expression.transform) { r.setExpression(_) } >>
     (all(header map {_.transform}) >>= {
       bs => bs.foreach { r.getGroupHeaderSection.asInstanceOf[JRDesignSection].addBand(_) }; ret()
     }) >>
     (all(footer map {_.transform}) >>= {
       bs => bs.foreach { r.getGroupFooterSection.asInstanceOf[JRDesignSection].addBand(_) }; ret()
     }) >>
     ret(r)
   }
}

abstract sealed class Reset {
  private[core] def transform : Transformer[(ResetTypeEnum, Option[JRDesignGroup])]
}

object Reset {
  /**
   * The variable is initialized only once, at the beginning of the report filling process, with the value returned by
   * the variable's initial value expression.
   */
  case object Report extends Reset {
    private[core] def transform = ret(ResetTypeEnum.REPORT, Option.empty)
  }
  /**
   * The variable is reinitialized at the beginning of each new page.
   */
  case object Page extends Reset {
    private[core] def transform = ret(ResetTypeEnum.PAGE, Option.empty)
  }
  /**
   * The variable is reinitialized at the beginning of each new column.
   */
  case object Column extends Reset {
    private[core] def transform = ret(ResetTypeEnum.COLUMN, Option.empty)
  }
  /**
   * The variable is reinitialized every time the specified group breaks.
   */
  sealed case class Group(g : de.ag.jrlang.core.Group) extends Reset {
    private[core] def transform =
      g.transform >>= { jg =>
        ret(ResetTypeEnum.GROUP, Some(jg))
      }
  }
  /**
   * The variable will never be initialized using its initial value expression and will only contain values obtained by
   * evaluating the variable's expression.
   */
  case object None extends Reset {
    private[core] def transform = ret(ResetTypeEnum.NONE, Option.empty)
  }
}

abstract sealed class Increment {
  private[core] def transform : Transformer[(IncrementTypeEnum, Option[JRDesignGroup])]
}

object Increment {
  case object Report extends Increment {
    private[core] def transform = ret(IncrementTypeEnum.REPORT, Option.empty)
  }
  case object Page extends Increment {
    private[core] def transform = ret(IncrementTypeEnum.PAGE, Option.empty)
  }
  case object Column extends Increment {
    private[core] def transform = ret(IncrementTypeEnum.COLUMN, Option.empty)
  }
  sealed case class Group(g : de.ag.jrlang.core.Group) extends Increment {
    private[core] def transform =
      g.transform >>= { jg =>
        ret(IncrementTypeEnum.GROUP, Some(jg))
      }
  }
  case object None extends Increment {
    private[core] def transform = ret(IncrementTypeEnum.NONE, Option.empty)
  }

}


sealed case class Variable(name: String,
                           calculation: CalculationEnum,
                           expression: Expression[Any],
                           valueClassName: String = "java.lang.String",
                           increment: Increment = Increment.None,
                           reset: Reset = Reset.Report,
                           incrementerFactoryClassName: Option[String] = None
                           )
{

  private[core] def transform = {
    val r = new JRDesignVariable()
    r.setName(name)
    r.setCalculation(calculation)
    r.setIncrementerFactoryClassName(incrementerFactoryClassName.getOrElse(null))
    r.setValueClassName(valueClassName)

    drop(increment.transform) { case(t, g) =>
      r.setIncrementType(t)
      r.setIncrementGroup(g.getOrElse(null))
    } >>
    drop(expression.transform) { r.setExpression(_) } >>
    drop(reset.transform) { case(t, g) =>
      r.setResetType(t)
      r.setResetGroup(g.getOrElse(null))
      } >>
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
    fields : Map[String,String] = Map.empty, // maps Name -> ClassName TODO: Need to add description probably (see XMLDatasources, XPath)
    sortFields : Seq[SortField] = Vector.empty,
    query: JRDesignQuery = null, // don't use this, use a JRResultSetDataSource instead (remove?)
    /** Simple variable expressions cannot always implement complex functionality. This is where scriptlets
      * come in. Scriptlets are sequences of Java code that are executed every time a report event occurs. Through
      * scriptlets, users can affect the values stored by the report variables. */
    // so, maybe we don't need that
    scriptlets : IndexedSeq[JRScriptlet] = Vector.empty, // Map-Like
    scriptletClassName: Option[String] = None,
    /** A report group is represented by sequence of consecutive records in the data source that have something
      * in common, like the value of a certain report field. */
    groups : Map[String, Group] =  Map.empty,
    resourceBundle: Option[String] = None,
    filterExpression: Option[Expression[Boolean]] = None,
    whenResourceMissingType: WhenResourceMissingTypeEnum = WhenResourceMissingTypeEnum.NULL,
    customProperties: Map[String, String] = Map.empty) // remove?
{

  private[core] def transform = {
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
    r.setScriptletClass(scriptletClassName.getOrElse(null))
    r.setResourceBundle(resourceBundle.getOrElse(null))
    r.setWhenResourceMissingType(whenResourceMissingType)
    customProperties foreach { case(n,e) => r.setProperty(n, e) }

    // user defined parameters (generated parameters are added later)
    (all(parameters map {_.transform}) >>= {
      ps => ps foreach { r.addParameter(_) }; ret()
    }) >>
    (all(variables map {_.transform}) >>= {
      vs => vs foreach { r.addVariable(_) }; ret()
    }) >>
    (all(sortFields map { _.transform }) >>= {
      sfs => sfs foreach { r.addSortField(_) }; ret()
    }) >>
    (all(groups map { case(n, g) => g.transform >>= { jg => ret(n, jg) } } toSeq) >>= {
      l => l foreach { case(n, g) => g.setName(n); r.addGroup(g) }; ret()
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
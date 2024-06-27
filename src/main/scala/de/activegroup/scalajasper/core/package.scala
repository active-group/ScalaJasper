package de.activegroup.scalajasper

import scala.jdk.CollectionConverters._
import de.activegroup.scalajasper.core.Dimensions._
import net.sf.jasperreports.engine.{JRDataSource, JREmptyDataSource}
;

package object core {
  implicit def lengthValue(value: Double): LengthValue = new LengthValue(value)
  implicit def lengthValue(value: Int): LengthValue = new LengthValue(value)

  implicit def fractionValue(value: Double): FractionValue = FractionValue(value) // might get too confusing val p:Percent = 0.45 ?
  implicit def fractionValue(value: Int): FractionValue = FractionValue(value)

  implicit def partialLength(p: FractionValue): RestrictedLength = RestrictedLength(p, 0.px)
  implicit def absoluteLength(l: Length): RestrictedLength = RestrictedLength(0.0, l)

  implicit def specificWidth(value: RestrictedLength): Width.Specific = Width.Specific(value)
  implicit def specificWidth(value: FractionValue): Width.Specific = Width.Specific(value)
  implicit def specificWidth(value: Length): Width.Specific = Width.Specific(value)

  implicit def absoluteVertical(value: Length): AbsoluteVerticalLength = AbsoluteVerticalLength(value)
  implicit def fontRelatedVertical(value: FontSizedLength): FontRelatedVerticalLength = FontRelatedVerticalLength(value)

  // backward-compatiblity; should be removed maybe
  implicit def liftElements(l: Seq[Element]) : Element = if (l.size == 1) l(0) else ElementSeq(l)

  /** Creates a JasperReport object (a report template) and a map of fixed arguments to generated parameters,
    * which can be combined with additional arguments and passed to print later. This way the overhead can be
    * reduced if many similar reports have to be created for different data. */
  def prepare(r : Report) : (net.sf.jasperreports.engine.JasperReport, Map[String, AnyRef]) =
    Compiler.compile(r)

  /** Creates a JasperReport object (a report template) that can be stored for later use.
    * May throw an exception if the report cannot be stored; use prepare or print if unsure. */
  def compile(r : Report) : net.sf.jasperreports.engine.JasperReport = {
    val (jr, args) = prepare(r)
    if (args.isEmpty)
      jr
    else
      throw new Exception("This report cannot be compiled to a template, because it uses non-raw expressions. Use prepare or print.")
  }

  /** Creates a JasperPrint object of the JasperPrint, optionally specifying a map of named arguments and a datasource */
  def printJasperReport(jreport : net.sf.jasperreports.engine.JasperReport,
                        args : Map[String, AnyRef] = Map.empty,
                        ds : net.sf.jasperreports.engine.JRDataSource = null) : net.sf.jasperreports.engine.JasperPrint = {
    val ds_ = if (ds == null) new net.sf.jasperreports.engine.JREmptyDataSource() else ds
    // args map must be mutable, because fillReport adds some things! (oh yeah)
    val args_ = scala.collection.mutable.Map[String, AnyRef]() ++ args
    net.sf.jasperreports.engine.JasperFillManager.fillReport(jreport, args_.asJava, ds_)
  }

  /** Creates a JasperPrint object of the report, optionally specifying a map of named arguments and a datasource */
  def print(r : Report,
            args : Map[String, AnyRef] = Map.empty,
            ds : net.sf.jasperreports.engine.JRDataSource = null) : net.sf.jasperreports.engine.JasperPrint = {
    val (jreport, autoArgs) = prepare(r)
    val allArgs = args ++ autoArgs
    val p = printJasperReport(jreport, allArgs, ds)
    //p.setLocaleCode(localeCode)
    //p.setTimeZoneId(timeZoneId)
    p
  }
}
package de.activegroup.scalajasper.core

import net.sf.jasperreports.engine.design.JRDesignExpression

import Transformer._

/*
// a thing that can/must contribute to the global environment of a report (there is only one)
private[core] trait EnvCollector {
  private[core] def collectEnv(e0 : Map[Parameter, AnyRef]) : Map[Parameter, AnyRef]
}

object EnvCollector {
  implicit def opt(o : Option[EnvCollector]) : EnvCollector = {
    new EnvCollector {
      def collectEnv(e0 : Map[Parameter, AnyRef]) =
        o.map(_.collectEnv(e0)).getOrElse(e0)
    }
  }

  implicit def seq(l : Seq[EnvCollector]) : EnvCollector = {
    new EnvCollector {
      def collectEnv(e0 : Map[Parameter, AnyRef]) =
        l.foldLeft(e0) { (e1, v) => v.collectEnv(e1) }
    }
  }
}
*/

/** @see See companion object for various functions that return Expression objects. */
abstract class Expression[+A] {
  private[core] def transformRaw : Transformer[String]

  private[core] def transform : Transformer[JRDesignExpression] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignExpression()
    drop(transformRaw) { r.setText(_) } >>
    ret(r)
  }
}

private sealed case class RawExpression[+A](raw: String) extends Expression[A] {
  private[core] def transformRaw = ret(raw)
}

private sealed case class CallExpression[A, +R](f : Expression[A => R], a : Expression[A]) extends Expression[R] {
  private[core] def transformRaw = f.transformRaw >>= { ft => a.transformRaw >>= { at =>
    ret("((scala.Function1)" + ft + ").apply(" + at + ")")
  }}
}

private sealed case class LiftExpression[A <: AnyRef](v : A) extends Expression[A] {
  private[core] def transformRaw = binding(v) >>= { n => ret(Expression.stdraw("P", n)) }
}

/** This object provides various ways to create Expression objects. */
object Expression {
  /** lift value v into a (java) source expression which evaluates to it at report runtime */
  private def lift[A <: AnyRef](v: A) : LiftExpression[A] = LiftExpression(v)

  /** create an expression, which evaluates f and arg at runtime, and calls the resulting function on the argument value */
  private def calle[A, R](f: Expression[A => R], arg : Expression[A]) : Expression[R] = CallExpression(f, arg)

  /** Returns an expression that will call the given function with the value of the given expression. */
  // for now, we do heavy currying, but only because it seemed easier to define.
  def call[A, R](fn : A => R, arg : Expression[A]) : Expression[R] =
    calle(lift(fn), arg)

  /** Returns an expression that will call the given function with the values of the given expressions. */
  def call[A1, A2, R](fn : (A1, A2) => R, arg1 : Expression[A1], arg2 : Expression[A2]) : Expression[R] =
    calle(call({a1:A1 => a2: A2 => fn(a1, a2)}, arg1), arg2)

  /** Returns an expression that will call the given function with the values of the given expressions. */
  def call[A1, A2, A3, R](fn : (A1, A2, A3) => R, arg1 : Expression[A1], arg2 : Expression[A2], arg3 : Expression[A3]) : Expression[R] =
    calle(call({(a1:A1, a2:A2) => { a3:A3 => fn(a1, a2, a3)}}, arg1, arg2), arg3)

  /** Returns an expression that will call the given function with the values of the given expressions. */
  def call[A1, A2, A3, A4, R](fn : (A1, A2, A3, A4) => R, arg1 : Expression[A1], arg2 : Expression[A2], arg3 : Expression[A3], arg4 : Expression[A4]) : Expression[R] =
    calle(call({(a1:A1, a2:A2, a3:A3) => { a4:A4 => fn(a1, a2, a3, a4)}}, arg1, arg2, arg3), arg4)

  private[core] def escape(name: String) = name.replaceAllLiterally("$", "$$")
  private[core] def stdraw(id: String, name: String) = "$" + id + "{" + escape(name) + "}"
  private[core] def std[T <: AnyRef](id: String, name: String) : Expression[T] = raw(stdraw(id, name))

  /** Returns an expression that results in the value of the specified parameter values.
    * @see See parameters in [[de.activegroup.scalajasper.core.Dataset]]. */
  def P[T <: AnyRef](name: String) = std[T]("P", name)
  /** Returns an expression that results in the value of the specified resource variable.
    * @see See resourceBundle in [[de.activegroup.scalajasper.core.Dataset]]. */
  def R[T <: AnyRef](name: String) = std[T]("R", name)
  /** Returns an expression that results in the value of the specified field.
    * @see See fields in [[de.activegroup.scalajasper.core.Dataset]]. */
  def F[T <: AnyRef](name: String) = std[T]("F", name)
  /** Returns an expression that results in the value of the specified variable.
    * @see See variables in [[de.activegroup.scalajasper.core.Dataset]]. */
  def V[T <: AnyRef](name: String) = std[T]("V", name)

  /** Returns an expression that results in the specified value. */
  def const[T <: AnyRef](s: T) : Expression[T] =
     lift(s)

  /** Returns an expression representing the given raw jasper expression, a Java or Groovy expression, depending on
    * the value of language in [[de.activegroup.scalajasper.core.Report]].
    */
  def raw[T <: AnyRef](r: String) : Expression[T] = RawExpression(r)
};


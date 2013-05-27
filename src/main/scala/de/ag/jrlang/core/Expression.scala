package de.ag.jrlang.core

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

abstract class Expression[+A] extends Transformable[JRDesignExpression] {
  def transformRaw : Transformer[String]

  def transform : Transformer[JRDesignExpression] = {
    val r = new net.sf.jasperreports.engine.design.JRDesignExpression()
    drop(transformRaw) { r.setText(_) } >>
    ret(r)
  }
}

sealed case class RawExpression[+A](raw: String) extends Expression[A] {
  def transformRaw = ret(raw)
}

sealed case class CallExpression[A, +R](f : Expression[A => R], a : Expression[A]) extends Expression[R] {
  def transformRaw = f.transformRaw >>= { ft => a.transformRaw >>= { at =>
    ret("((scala.Function1)" + ft + ").apply(" + at + ")")
  }}
}

sealed case class LiftExpression[A <: AnyRef](v : A) extends Expression[A] {
  def transformRaw = binding(v) >>= { n => ret(Expression.stdraw("P", n)) }
}

object Expression {
  /** lift value v into a (java) source expression which evaluates to it at report runtime */
  private def lift[A <: AnyRef](v: A) : LiftExpression[A] = LiftExpression(v)

  /** create an expression, which evaluates f and arg at runtime, and calls the resulting function on the argument value */
  def calle[A, R](f: Expression[A => R], arg : Expression[A]) : Expression[R] = CallExpression(f, arg)

  // for now, we do heavy currying, but only because it seemed easier to define.
  def call[A, R](fn : A => R, arg : Expression[A]) : Expression[R] =
    calle(lift(fn), arg)

  def call[A1, A2, R](fn : (A1, A2) => R, arg1 : Expression[A1], arg2 : Expression[A2]) : Expression[R] =
    calle(call({a1:A1 => a2: A2 => fn(a1, a2)}, arg1), arg2)

  def call[T1, T2, T3, R](fn : (T1, T2, T3) => R, arg1 : Expression[T1], arg2 : Expression[T2], arg3 : Expression[T3]) : Expression[R] =
    calle(call({(a1:T1, a2:T2) => { a3:T3 => fn(a1, a2, a3)}}, arg1, arg2), arg3)

  def call[T1, T2, T3, T4, R](fn : (T1, T2, T3, T4) => R, arg1 : Expression[T1], arg2 : Expression[T2], arg3 : Expression[T3], arg4 : Expression[T4]) : Expression[R] =
    calle(call({(a1:T1, a2:T2, a3:T3) => { a4:T4 => fn(a1, a2, a3, a4)}}, arg1, arg2, arg3), arg4)

  private[core] def escape(name: String) = name.replaceAllLiterally("$", "$$")
  private[core] def stdraw(id: String, name: String) = "$" + id + "{" + escape(name) + "}"
  private[core] def std[T <: AnyRef](id: String, name: String) : Expression[T] = raw(stdraw(id, name))

  /** Parameter values, some built-in, some user defined */
  def P[T <: AnyRef](name: String) = std[T]("P", name)
  /** Resource values */
  def R[T <: AnyRef](name: String) = std[T]("R", name)
  /** Field values */
  def F[T <: AnyRef](name: String) = std[T]("F", name)

  def const[T <: AnyRef](s: T) : Expression[T] =
     lift(s)

  def raw[T <: AnyRef](r: String) : Expression[T] = RawExpression(r)
};


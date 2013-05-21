package de.ag.jrlang.core

// a thing that can/must contribute to the global environment of a report (there is only one)
private[core] trait EnvCollector {
  private[core] def collectEnv(e0 : Map[JRDesignParameter, AnyRef]) : Map[JRDesignParameter, AnyRef]
}

object EnvCollector {
  implicit def opt(o : Option[EnvCollector]) : EnvCollector = {
    new EnvCollector {
      def collectEnv(e0 : Map[JRDesignParameter, AnyRef]) =
        o.map(_.collectEnv(e0)).getOrElse(e0)
    }
  }

  implicit def seq(l : Seq[EnvCollector]) : EnvCollector = {
    new EnvCollector {
      def collectEnv(e0 : Map[JRDesignParameter, AnyRef]) =
        l.foldLeft(e0) { (e1, v) => v.collectEnv(e1) }
    }
  }
}

// we could add a type parameter, to be able to tell at some places
// that it must be an expression of a certain type; but most if not all of
// the time JasperReports is dynamically typed anyway, and allows a
// variety of types; so it would usually be Expression[Any]
sealed case class Expression private(raw: String, env: Map[JRDesignParameter, AnyRef]=Map.empty) extends EnvCollector {
  override private[core] def collectEnv(e0 : Map[JRDesignParameter, AnyRef]) = e0 ++ env
}

object Expression {
  /** lift value v into a (java) source expression which evaluates to a report runtime */
  private def lift(v: AnyRef) = {
    val uniqueParamName = "v__" + (if (v == null) "null" else v.hashCode()); // Anything unique, though this is not so bad
    val e = "$P{" + uniqueParamName + "}";
    val p = new JRDesignParameter(
      name=uniqueParamName,
      isForPrompting=false,
      defaultValueExpression=None,
      nestedTypeName="",
      valueClassName="Object",
      description="");
    val a = v;
    Expression(e, Map(p -> a))
  }

  /** create an expression, which evaluates f and arg at runtime, and calls the resulting function on the argument value */
  private def call1(f: Expression, arg : Expression) =
    Expression(
      "((scala.Function1)" + f.raw + ").apply(" + arg.raw + ")",
      f.env ++ arg.env
    )


  // for now, we do heavy currying, but only because it seemed easier to define.

  def call[T](fn : T => Any, arg : Expression) : Expression =
    call1(lift(fn), arg)

  def call[T1, T2](fn : (T1, T2) => Any, arg1 : Expression, arg2 : Expression) : Expression =
    call1(call({ a1:T1 => a2:T2 => fn(a1, a2) }, arg1), arg2)

  def call[T1, T2, T3](fn : (T1, T2, T3) => Any, arg1 : Expression, arg2 : Expression, arg3 : Expression) : Expression =
    call1(call({(a1:T1, a2:T2) => { a3:T3 => fn(a1, a2, a3)}}, arg1, arg2), arg3)

  def call[T1, T2, T3, T4](fn : (T1, T2, T3, T4) => Any, arg1 : Expression, arg2 : Expression, arg3 : Expression, arg4 : Expression) : Expression =
    call1(call({(a1:T1, a2:T2, a3:T3) => { a4:T4 => fn(a1, a2, a3, a4)}}, arg1, arg2, arg3), arg4)

  private def escape(name: String) = name.replaceAllLiterally("$", "$$");
  private def std(id: String, name: String) = raw("$" + id + "{" + escape(name) + "}")

  /** Parameter values, some built-in, some user defined */
  def P(name: String) = std("P", name)
  /** Resource values */
  def R(name: String) = std("R", name);
  /** Field values */
  def F(name: String) = std("F", name);

  def const(s: AnyRef) : Expression =
  // TODO: proper escaping; Java syntax - resp. if it depends on language, don't offer this.
  // raw("\"" + s + "\"");
  // or this:
  //  call({ _ => s }, raw(null))
     lift(s)

  def raw(r: String) : Expression = Expression(r)

  implicit def dropExpression(o : Expression) = {
    if (o == null)
      null
    else {
      val r = new net.sf.jasperreports.engine.design.JRDesignExpression()
      r.setText(o.raw)
      r
    }
  }

  implicit def dropOptExpression(o: Option[Expression]) = {
    if (o.isDefined)
      dropExpression(o.get)
    else
      null;
  }
};


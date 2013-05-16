package de.ag.jrlang.core

// we could add a type parameter, to be able to tell at some places
// that it must be an expression of a certain type; but most if not all of
// the time JasperReports is dynamically typed anyway, and allows a
// variety of types; so it would usually be Expression[Any]
abstract class Expression;

object Expression {
  /** don't use directly, use function Expression.raw instead */
  sealed case class Raw(expr: String) extends Expression;

  /** don't use directly, use function Expression.call instead */
  sealed case class Call(fn : () => Any, add : String=>String, inner: Seq[(JRDesignParameter, AnyRef)]) extends Expression {
    /* compiles this function call into a raw (java) expression, a parameter definition and a parameter value */
    def compile : (Raw, Seq[(JRDesignParameter, AnyRef)]) = {
      val uniqueParamName = "fn__" + this.hashCode(); // Anything unique, though this is not so bad

      val rawe = raw(add(P(uniqueParamName).expr + ".apply()"));
      val vclass = "scala.Function0";

      val parameter = new JRDesignParameter(
        name=uniqueParamName,
        isForPrompting=false,
        defaultValueExpression = None,
        nestedTypeName="",
        valueClassName=vclass,
        description="");

      (rawe, (parameter, fn) +: inner)
    }

    private[core] def append(arg : Expression) = {
      val (rawarg, innerParams) = Call.compile(arg)
      val prev = this.add;
      val other = this.inner;
      this.copy(
        add = { prev => "((scala.Function1)" + prev + ")" + Call.fapply(rawarg)},
        inner = other ++ innerParams)
    };
  }
  object Call {
    private def fapply(arg : Raw) = ".apply(" + arg.expr + ")"

    private def compile(e : Expression) : (Raw, Seq[(JRDesignParameter, AnyRef)]) = e match {
      case r : Raw => (r, List.empty)
      case c : Call => c.compile
    }
  }

  // for now, we do heavy currying, but only because it seemed easier to define.

  def call(fn : () => Any) : Call = new Call(fn, identity, List.empty)

  def call[T <: Any](fn : T => Any, arg : Expression) : Call =
    call({ () => fn }).append(arg)

  def call[T1 <: Any, T2 <: Any](fn : (T1, T2) => Any, arg1 : Expression, arg2 : Expression) : Call =
    call[T1]({ a1 => a2:T2 => fn(a1, a2) }, arg1).append(arg2);

  def call[T1 <: Any, T2 <: Any, T3 <: Any](fn : (T1, T2, T3) => Any, arg1 : Expression, arg2 : Expression, arg3 : Expression) : Call =
    call[T1, T2]({(a1, a2) => { a3:T3 => fn(a1, a2, a3)}}, arg1, arg2).append(arg3);

  def call[T1 <: Any, T2 <: Any, T3 <: Any, T4 <: Any](fn : (T1, T2, T3, T4) => Any, arg1 : Expression, arg2 : Expression, arg3 : Expression, arg4 : Expression) : Call =
    call[T1, T2, T3]({(a1, a2, a3) => { a4:T4 => fn(a1, a2, a3, a4)}}, arg1, arg2, arg3).append(arg4);

  private def escape(name: String) = name.replaceAllLiterally("$", "$$");
  private def std(id: String, name: String) = raw("$" + id + "{" + escape(name) + "}")

  /** Parameter values, some built-in, some user defined */
  def P(name: String) = std("P", name)
  /** Resource values */
  def R(name: String) = std("R", name);
  /** Field values */
  def F(name: String) = std("F", name);

  def const(s: Any) : Expression =
  // TODO: proper escaping; Java syntax - resp. if it depends on language, don't offer this.
  // raw("\"" + s + "\"");
  // or this:
    call({ () => s })

  def raw(r: String) : Expression.Raw = Raw(r)

  implicit def dropExpression(o : Expression) = {
    if (o == null)
      null
    else {
      o match {
        case raw: Expression.Raw => {
          val r = new net.sf.jasperreports.engine.design.JRDesignExpression()
          r.setText(raw.expr)
          r
        };
        case _ => throw new RuntimeException("Expression must be compiled to a raw expression")
      }
    }
  }

  implicit def dropOptExpression(o: Option[Expression]) = {
    if (o.isDefined)
      dropExpression(o.get)
    else
      null;
  }
};


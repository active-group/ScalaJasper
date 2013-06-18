package de.ag.jrlang.core

object Dimensions {
  case class LengthUnit(inJasperPixels: Double) {
    def *(factor: Double) = LengthUnit(inJasperPixels * factor)
  }
  object LengthUnit {
    // Jasper's page resolution is fixed to 72 dpi (defining it's coodinate system),
    // therefore we can use 'px' as unit without adding an arbitrary resolution
    val px = LengthUnit(1)
    val inch = px * 72.0
    val mm = inch * (1.0/25.4) // ~ 2.835016 dots per millimeter
    val cm = mm * 10.0
  }

  case class Length(value: Double, unit: LengthUnit) {
    /*def inMM = value * unit.inMM
    def inInch = inMM / 25.4
    */
    def inPixels = value * unit.inJasperPixels
    def inAbsolutePixels : Int = scala.math.floor(inPixels) toInt // floor to be sure, hope that's ok for everybody

    def -(rhs: Length) : Length = Length(this.inPixels - rhs.inPixels, LengthUnit.px) // or keep one of the units?
    def +(rhs: Length) : Length = Length(this.inPixels + rhs.inPixels, LengthUnit.px)
    def /(rhs: Length) : Double = (this.inPixels / rhs.inPixels)
    def /(rhs: Double) : Length = Length(this.inPixels / rhs, LengthUnit.px)
    def *(f: Double) : Length = Length(this.inPixels * f, LengthUnit.px)
  }

  /** a value which can be given a unit - only used implicitly and intermediately for DSL */
  sealed class LengthValue(value : Double) {
    def in(u: LengthUnit) = Length(value, u) // does not work implicitly

    def px = in(LengthUnit.px)
    def inch = in(LengthUnit.inch)
    def mm = in(LengthUnit.mm)
    def cm = in(LengthUnit.cm)

  }
  val zero = 0 px // in case you want to make explicit that 0 actually has no unit

  sealed case class FractionValue(value: Double) {
    // "percent" is like a modifier or predefined division by 100; also maked the DSL work nicely
    // The percent sign unfortunately does not work as a "unary postfix operator"
    def percent = FractionValue(value/100.0)

    def +(rhs: FractionValue) = FractionValue(this.value + rhs.value)
    def -(rhs: FractionValue) = FractionValue(this.value - rhs.value)
  }

  private object Demo {
    def t(v: Length) = v
    val strip = t(5 mm)
    def p(v: FractionValue) = v
    val x = p(15 percent)
  }

  /** A restricted length is composed of a fractional and an absolute part, so you can specify values
    * like `100 percent - 10 px` */
  sealed case class RestrictedLength(p: FractionValue, l: Length) {
    def asPartOf(total: Length) : Length =
      (total * p.value) + l

    def +(rhs: RestrictedLength) = RestrictedLength(p = this.p + rhs.p, l = this.l + rhs.l)
    def -(rhs: RestrictedLength) = RestrictedLength(p = this.p - rhs.p, l = this.l - rhs.l)

    // implicits don't work over two steps (or something...)

    def +(rhs: FractionValue) = this + RestrictedLength(p = rhs, 0 px)
    def -(rhs: FractionValue) = this - RestrictedLength(p = rhs, 0 px)

    def +(rhs: Length) = this + RestrictedLength(p = 0.0, l = rhs)
    def -(rhs: Length) = this - RestrictedLength(p = 0.0, l = rhs)
  }
}

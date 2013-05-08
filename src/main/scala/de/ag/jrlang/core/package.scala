package de.ag.jrlang

package object core {

  // TODO Utils for:
  
  // JRDesignSection == list of bands
  
  type Expression = String
  implicit def dropExpression(o: String) = {
    if ((o == "") || (o == null))
      null
      else {
        val r = new net.sf.jasperreports.engine.design.JRDesignExpression()
        r.setText(o);
        r
      }
  }
  
  object Expression {
    def const(s: String) : Expression =
      // TODO: proper escaping; Java syntax - resp. if it depends on language, don't offer this.
      "\"" + s + "\""
  }

}
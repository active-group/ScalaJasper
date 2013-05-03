package de.ag.jrlang

package object core {
  trait BaseAdapter[OOClass] {
    def fillObj(obj : OOClass);
  }
  
  trait LeafAdapter[OOClass] extends BaseAdapter[OOClass] {
    lazy val obj : OOClass = fillNewObj()
    
    def newObj() : OOClass
    
    def fillNewObj() : OOClass = {
      val r = newObj();
      fillObj(r);
      r
    } 
  }

  trait CompanionAdapter[OOClass, Class <: LeafAdapter[OOClass]] {
    def apply() : Class
    def apply(o: OOClass) : Class
    
    implicit def lift(o: OOClass) : Class = apply(o)
    implicit def drop(o: Class) : OOClass = o.fillNewObj()
  }
  

  // TODO Utils for:
  
  // JRDesignSection == list of bands
  type JRDesignExpression = String
  implicit def liftExpression(o : net.sf.jasperreports.engine.design.JRDesignExpression) =
    o.getText()
  implicit def dropExpression(o: String) = {
    val r = new net.sf.jasperreports.engine.design.JRDesignExpression()
    r.setText(o);
    r
  }

  
  implicit def dropReport(o: JasperDesign) : net.sf.jasperreports.engine.design.JasperDesign =
    o.drop
  

}
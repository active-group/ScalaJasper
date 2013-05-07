package de.ag.jrlang.core

sealed case class JRDesignQuery(
    // chunks?? don't understand that.
    language: String,
    text: String);

object JRDesignQuery {
  val empty = new JRDesignQuery("", "");
  
  implicit def dropQuery(o: JRDesignQuery): net.sf.jasperreports.engine.design.JRDesignQuery = {
    val r = new net.sf.jasperreports.engine.design.JRDesignQuery();
    r.setLanguage(o.language);
    r.setText(o.text);
    r
  }
}
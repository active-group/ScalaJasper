package de.ag.jrlang.core

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import de.ag.jrlang.core.JasperDesign._

@RunWith(classOf[JUnitRunner])
class JasperDesignTest extends FunSuite {

  
  test("persistency") {
     val o0 = JasperDesign();
     val o1 = o0.copy(columns = o0.columns.copy(width = 42));
     assert(o1.columns.width == 42);
     val o2 = o1.copy(columns = o1.columns.copy(width = 10));
     assert(o2.columns.width == 10);
     assert(o1.columns.width == 42);
     
     // ...not really useful test anymore...
     val p = o2.parameters;
     val sz = p.size;
     val o1_ = o1.copy(parameters = p);
     val o2_ = o2.copy(parameters = List());
     assert(o2_.parameters.size == 0);
     assert(o2.parameters.size == sz);
  }

  // verify how ugly the Java API really is
  test("ugliness of java") {
    def n() = new net.sf.jasperreports.engine.design.JasperDesign();
    
    
    // List properties actually mutate the property
    // and although you can actually remove default properties:
    val v1 = n();
    val xp = v1.getParameters().length;
    v1.getParametersList().remove(0);
    assert(v1.getParameters().length == (xp-1));
    // they are still magically there:
    val v2 = n();
    val p : net.sf.jasperreports.engine.JRParameter = v2.getParameters()(0);
    v2.getParametersList().clear();
    assert(v2.getParameters().length == 0);
    intercept[net.sf.jasperreports.engine.JRException] {
      v2.addParameter(p);
    };
    // although they can be added via the list:
    v2.getParametersList().asInstanceOf[java.util.List[net.sf.jasperreports.engine.JRParameter]].add(p);
    // WTF!?
    v2.getParametersList().clear();
    assert(v2.getParametersList().size() == 0);
  }
}

package de.ag.jrlang.core

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import de.ag.jrlang.core.Dimensions._
import net.sf.jasperreports.engine.`type`.SplitTypeEnum

@RunWith(classOf[JUnitRunner])
class ExpressionTests extends FunSuite {
  test("scala functions as expressions") {
    var fn0_was_called = false;
    val fn0 = { pmap : java.util.Map[String, Object] =>
      // parameters_map is:
      // {REPORT_LOCALE=en_US,
      // JASPER_REPORT=net.sf.jasperreports.engine.JasperReport@16daee9,
      // REPORT_FORMAT_FACTORY=net.sf.jasperreports.engine.util.DefaultFormatFactory@47f643ba,
      // REPORT_TIME_ZONE=sun.util.calendar.ZoneInfo[id="Europe/Berlin",offset=3600000,dstSavings=3600000,useDaylight=true,transitions=143,lastRule=java.util.SimpleTimeZone[id=Europe/Berlin,offset=3600000,dstSavings=3600000,useDaylight=true,startYear=0,startMode=2,startMonth=2,startDay=-1,startDayOfWeek=1,startTime=3600000,startTimeMode=2,endMode=2,endMonth=9,endDay=-1,endDayOfWeek=1,endTime=3600000,endTimeMode=2]],
      // REPORT_PARAMETERS_MAP=(this Map),
      // REPORT_DATA_SOURCE=net.sf.jasperreports.engine.JREmptyDataSource@3be6c541,
      // IS_IGNORE_PAGINATION=false, fn0=<function1>}
      println(pmap);
      fn0_was_called = true;
      true;
    }
    //val fnClassName:String = fn0.getClass().getName(); // like de.ag.jrlang.core.ExpressionTests$$anonfun$1$$anonfun$2 - could be more general
    // de.ag.jrlang.core.ExpressionTests$$anonfun$1$$anonfun$2 cannot be referenced using its binary name ??
    val fnClassName = "scala.Function1"
    val args = Map("fn0" -> fn0);
    val r = Report("test").copy(
      mainDataset = Dataset.empty.copy(parameters = List(Parameter("fn0").copy(valueClassName = fnClassName))),
      page = Page.empty.copy(
        header = Some(Band(
          splitType = SplitTypeEnum.STRETCH,
          height = 20 px,
          content = Vector(
            StaticText(
              text = "Hello",
              pos = Pos.float(x = 0 px, y = 0 px),
              size = Size.fixed(width=55 px, height = 15 px),
              conditions = Conditions(printWhenExpression=Some(Expression.raw("$P{fn0}.apply($P{REPORT_PARAMETERS_MAP})")))
            )
          )))
        // footer
      )
    );

    val actual = ReportTest.printToXML(r, args);
    assert(fn0_was_called);
  }

  /* works very different now...
  test("automatic scala expressions") {
    var fn0_was_called = false;
    val fn0 = { pmap : java.util.Map[String, Object] =>
      fn0_was_called = true;
      true
    }
    val expr1 = Expression.call(fn0, Expression.P("REPORT_PARAMETERS_MAP"));
    val (compiledExpr1, pargs) = (expr1.raw, expr1.env)
    val params = pargs map { case(p, _) => p } toList;
    val args = pargs map { case(p, v) => (p.name, v) } toMap;
    val r = Report("test").copy(
      mainDataset = Dataset.empty.copy(parameters = params),
      page = Page.empty.copy(
        header = Some(Band.empty.copy(
          height = 20,
          content = Vector(
            StaticText(
              text = "Hello",
              pos = Pos.float(x = 0, y = 0),
              size = Size.fixed(width=55, height = 15),
              conditions = Conditions(
                printWhenExpression=Some(expr1.copy(env=Map.empty)))
            )
          )))
        // footer
      )
    );
    // println(r.page.header.get.children);
    println(expr1);
    println(r.mainDataset.parameters);
    println(args);

    val actual = ReportTest.printToXML(r, args);
    assert(fn0_was_called);
  }
  */

  test("fully automatic scala expressions") {
    var fn0_was_called = false;
    val fn0 = { pmap : java.util.Map[String, Object] =>
      fn0_was_called = true;
      true
    }
    val r = Report("test").copy(
      page = Page.empty.copy(
        header = Some(Band(
          splitType = SplitTypeEnum.STRETCH,
          height = 20 px,
          content = Vector(
            StaticText(
              text = "Hello",
              pos = Pos.float(x = 0 px, y = 0 px),
              size = Size.fixed(width=55 px, height = 15 px),
              conditions = Conditions(
                printWhenExpression=Some(Expression.call(fn0, Expression.P("REPORT_PARAMETERS_MAP"))))
            )
          )))
        // footer
      )
    );

    val actual = ReportTest.printToXML(r, Map.empty);
    assert(fn0_was_called);
  }

  // TODO TEST: Use the same function/expression multiple times
}

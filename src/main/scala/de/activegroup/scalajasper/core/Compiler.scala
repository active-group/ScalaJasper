package de.activegroup.scalajasper.core

import java.util.{Locale, TimeZone, UUID}
import de.activegroup.scalajasper.core.Dimensions.Length
import net.sf.jasperreports.crosstabs.{JRCrosstab, JRCrosstabParameter}
import net.sf.jasperreports.crosstabs.design.{JRDesignCrosstab, JRDesignCrosstabParameter}
import net.sf.jasperreports.engine.design.{JRDesignDataset, JRDesignParameter, JRDesignStyle, JasperDesign}
import net.sf.jasperreports.{engine => jre}

private case class NamedObjects[T, +JT](lookup: Map[T, JT], nextId: Int) {
  def update[JT1 >: JT](t: T, jt: JT1, nid: Int): NamedObjects[T, JT1] = this.copy(lookup = lookup.updated(t, jt), nextId = nid)

  lazy val results = lookup.values
}

private[core] case class TransformationState(containerWidth: Length,
                                             env: NamedObjects[AnyRef, JRDesignParameter],
                                             styles: NamedObjects[AbstractStyle, JRDesignStyle],
                                             datasets: NamedObjects[Dataset, JRDesignDataset],
                                             next_uuid: Int) {

  private def autoName[T, JT](get: TransformationState => NamedObjects[T, JT], set: (TransformationState, NamedObjects[T, JT]) => TransformationState,
                              getName: JT => String, setName: (JT, String) => Unit)
                             (v: T, f : TransformationState => (JT, TransformationState)): ((JT, String), TransformationState) = {
    val o = get(this).lookup.get(v)
    if (o.isDefined)
      ((o.get, getName(o.get)), this)
    else {
      val (s, st1) = f(this)
      val id = get(st1).nextId
      val name = "auto" + id.toString
      setName(s, name)
      ((s, name), set(st1, get(st1).update(v, s, id+1))) // todo: return s always?
    }
  }

  def binding(v : AnyRef) = autoName[AnyRef, JRDesignParameter](st => st.env, (st, v) => st.copy(env = v), p => p.getName, (p, n) => p.setName(n))(v, (st) => {
    val p = new JRDesignParameter()
    // p.setValueClass is wrong, in that is results in the result of getName of the class to be inserted into Java
    // sourccode (a "bug" in Jasper). getCanonicalName would imho be closer to correctness, but unfortunately, most
    // classes generated by Scala simply do not have a name that can be used as the name of a class in Java
    // sourcecode (as Jasper generated type-cast expressions with this name). So, I don't know better than to use
    // Object for everything - there will be more type casts before usage of the value anyhow (to Function1 e.g.):
    // p.setValueClassName(if (v == null) "Object" else v.getClass.getName) // ok??
    // p.setValueClassName(v.getClass.getCanonicalName)
    p.setValueClassName("Object")
    (p, st)
    })

  val styleName = autoName[AbstractStyle, JRDesignStyle](st => st.styles, (st, v) => st.copy(styles = v), p => p.getName, (p, n) => p.setName(n)) _

  val datasetName = autoName[Dataset, JRDesignDataset](st => st.datasets, (st, v) => st.copy(datasets = v), p => p.getName, (p, n) => p.setName(n)) _

  def nextUUID : (UUID, TransformationState) = {
    // nameUUIDFromBytes uses MD5, so sequential numbers are 'mangled' anyway, so it should be ok
    val id = UUID.nameUUIDFromBytes(Array(
      ((next_uuid >>> 24) & 0xFF).toByte,
      ((next_uuid >>> 16) & 0xFF).toByte,
      ((next_uuid >>> 8) & 0xFF).toByte,
      (next_uuid & 0xFF).toByte
    ))
    (id, this.copy(next_uuid = next_uuid+1))
  }
}

private[core] object TransformationState {
  def initial(containerWidth: Length) = TransformationState(containerWidth, NamedObjects(Map.empty, 0), NamedObjects(Map.empty, 0), NamedObjects(Map.empty, 0), 0)
}

private[activegroup] trait Transformer[+A] { self =>
  def exec(st: TransformationState) : (A, TransformationState)

  def >>=[B](f : (A => Transformer[B])) : Transformer[B] =
    new Transformer[B] {
      def exec(st0 : TransformationState) = {
        val (a, st1) = self.exec(st0)
        f(a).exec(st1)
      }
    }

  // would be nice to define this only for A = Unit, but how? This one might throw at runtime?
  //def >>[B](n : Transformer[B]) = this >>= { Unit => n }

}

/* trait FilterMonadic[+A, +Repr] {
abstract def flatMap[B, That](f: (A) ⇒ GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That
abstract def foreach[U](f: (A) ⇒ U): Unit
abstract def map[B, That](f: (A) ⇒ B)(implicit bf: CanBuildFrom[Repr, B, That]): That
abstract def withFilter(p: (A) ⇒ Boolean): FilterMonadic[A, Repr]
 */

private[activegroup] trait ImperativeTransformer extends Transformer[Unit] {
  def >>[B](n : => Transformer[B]) = this >>= { _ => n }
}

private[activegroup] object Transformer {
  /** a transformer that already has the desired result */
  def ret[B](v : B) : Transformer[B] = new Transformer[B] {
    def exec(st: TransformationState) = (v, st)
  }

  val retUnit = ret(())

  // this ensures >> is only called on Unit Transformers
  // well, maybe the runtime overhead is to much just to prevent code errors...?
  implicit def isImperative(t: Transformer[Unit]) : ImperativeTransformer = new ImperativeTransformer {
    def exec(st: TransformationState) = t.exec(st)
  }

  /** drops the result of a transformation into a matching setter function */
  def drop[B](t : Transformer[B])(set : B => Unit) : Transformer[Unit] =
    t >>= { b => set(b); retUnit }

  def orNull[B <: AnyRef](o : Option[Transformer[B]]) : Transformer[B] =
    o.getOrElse(ret(null.asInstanceOf[B]))

  def all[B](l : Seq[Transformer[B]]) : Transformer[Seq[B]] =
    l.foldLeft(ret(Vector[B]())) {
      case (res, v) => v >>= { it => res >>= { lst => ret(lst :+ it) } }
    }

  /** creates a transformer that can also modify the transformation state */
  private def withState[A](f : TransformationState => (A, TransformationState)) : Transformer[A] =
    new Transformer[A] {
      def exec(st: TransformationState) = f(st)
    }

  private def getState : Transformer[TransformationState] = withState({ st => (st, st)})

  /** returns a parameter name for the given value */
  def binding(v : AnyRef) : Transformer[(JRDesignParameter, String)] = withState({st => st.binding(v)})

  /** returns a name for the given style. If it's a new style, f is called, a new name assigned to the result and
      stored for later retrieval (when the report if transformed. */
  def styleName(v : AbstractStyle, f : () => Transformer[JRDesignStyle]) : Transformer[(JRDesignStyle, String)] =
    withState({ st =>
      st.styleName(v, { st2 => f().exec(st2) }) // do we have to call exec?
    })

  /** returns a name for the given dataset. Like styleName */
  def datasetName(v : Dataset, f : () => Transformer[JRDesignDataset]) : Transformer[(JRDesignDataset,String)] =
    withState({ st =>
      st.datasetName(v, { st2 => f().exec(st2) }) // do we have to call exec?
    })

  /** returns all automatic parameter (from environment) collected to far */
  def getCurrentEnvironment : Transformer[Map[AnyRef, JRDesignParameter]] =
    getState >>= { st => ret(st.env.lookup) }

  def withNewEnvironment[T](f : => Transformer[T]) : Transformer[T] =
    withState({previousState => {
      val newState = previousState.copy(env = NamedObjects(Map.empty, 0))
      val (res, _) = f.exec(newState) // modified state ignored, must be grabbed before end of f
      (res, previousState)
    }})

  def setCurrentContainerWidth(width: Length) : Transformer[Unit] =
    withState({ st => ((), st.copy(containerWidth = width)) })

  def currentContainerWidth : Transformer[Length] =
    getState >>= { st => ret(st.containerWidth) }

  def withContainerWidth[A](width: Length)(f : => Transformer[A]) : Transformer[A] =
    currentContainerWidth >>= {
      prev =>
        setCurrentContainerWidth(width) >> f >>= { res =>
          setCurrentContainerWidth(prev) >>
          ret(res)
        }
    }

  def nextUUID : Transformer[UUID] =
    withState({ st => st.nextUUID })
}

object Compiler {
  // The defaults for locale and timeZone are not the JVM's defaultLocale() etc values, because it's
  // hardly imaginable that your reports suddenly translate automatically into another language, or that you want them
  // to look different depending on the OS settings of a user; so it's better (also for testing) to have a stable
  // default that makes you aware of the need to adjust it. As for the timezone - it probably should not be used at
  // all, but I haven't found out yet (except it appears in the xml print)
  def compile(o : Report,
              locale : Locale = Locale.US,
              timeZone : TimeZone = TimeZone.getTimeZone("GMT") ) : (jre.JasperReport, Map[String, AnyRef]) = {
    // basic object generation...
    // TODO: Create sequential or pseudo-random UUIDs for all elements (and other?) - to simplify testing
    val (r, tstate) = o.transform.exec(TransformationState.initial(0.px))

    // now insert collected auto-generated properties into basic object

    // environment = parameters + arguments
    tstate.env.results foreach { p => r.addParameter(p) }
    val envArgs = tstate.env.lookup map { case(v, p) => (p.getName, v) }

    // styles
    tstate.styles.results foreach { s => r.addStyle(s) }

    // datasets, also add all auto-parameters to the subdatasets - they might have been used there too
    tstate.datasets.results foreach { ds =>
      tstate.env.results foreach { p => ds.addParameter(p) }
      r.addDataset(ds)
    }
    // override some things where Jasper will set unpleasant defaults
    val defaultArgs = Map(
      "REPORT_LOCALE" -> Locale.US,
      "REPORT_TIME_ZONE" -> TimeZone.getTimeZone("GMT")
      // TODO? add this and remove 'resourceBundle' from Report? "REPORT_RESOURCE_BUNDLE"
      // TODO? add IS_IGNORE_PAGINATION and remove from report? or maybe add to print() - it's a printing and not a report property anyway, isn't it?
      // some more...?
    )


    /** the Crosstabs needs access to the auto-parameters, so pass the available parameters.   */
    r.getAllBands.toList.map(b =>
      b.getElements.toList.map {
        case crosstab: JRDesignCrosstab =>
          import net.sf.jasperreports.engine.design.JRDesignExpression
          r.getParametersMap.forEach { case (k, v) =>
            val newp = new JRDesignCrosstabParameter()
            newp.setName(k)
            val expr = new JRDesignExpression("$P{" + k + "}")
            newp.setExpression(expr)
            // fore some reason, Object does not work when reusing parameters. so use java.lang.Object
            if(v.getValueClassName == "Object") newp.setValueClassName("java.lang.Object") else newp.setValueClassName(v.getValueClassName)
            newp.setSystemDefined(v.isSystemDefined)
            newp.setDescription(v.getDescription)
            crosstab.removeParameter(k)
            crosstab.addParameter(newp)
          }
        case _ =>
      }
    )


    val finalreport = jre.JasperCompileManager.compileReport(r)
    (finalreport, defaultArgs ++ envArgs)
  }



}
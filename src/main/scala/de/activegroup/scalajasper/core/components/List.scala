package de.activegroup.scalajasper.core.components

import de.activegroup.scalajasper.core.{Data, Element, ElementUtils}
import de.activegroup.scalajasper.core.Transformer.{drop, ret}
import net.sf.jasperreports.engine.component.ComponentKey


sealed case class ListContent(fields: Seq[Element],
                              height: Int,
                              width: Int) {
  private[core] def transform = {
    val r = new net.sf.jasperreports.components.list.DesignListContents()

    r.setHeight(height)
    r.setWidth(width)

    ElementUtils.contentTransformer(fields, r.addElement, r.addElementGroup) >>
    ret(r)
  }
}


sealed case class List(contents: ListContent,
                       data: Data) extends Component {
  private[core] def transform = {
    val r = new net.sf.jasperreports.components.list.StandardListComponent()

    drop(contents.transform) { r.setContents(_)} >>
    drop(data.transform) { r.setDatasetRun(_)} >>
    ret(r, new ComponentKey("http://jasperreports.sourceforge.net/jasperreports/components", "noprefix", "list"))
  }
}
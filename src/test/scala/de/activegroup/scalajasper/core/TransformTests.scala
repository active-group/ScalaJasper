package de.activegroup.scalajasper.core

import de.activegroup.scalajasper.core.components.{TableCell, TableGroupCell}
import org.scalatest.FunSuite

class TransformTests extends FunSuite {

  test("table group cell is set appropiate") {
    val initTransform = TransformationState.initial(0 px)

    // use rowSpan just a an idication that the cells are equal.
    // Why are not equals-methods implemented in jasper reports?
    val tc = TableCell(content = Vector(), rowSpan = Some(723))
    val groupName = "group1"

    val tgc = TableGroupCell(groupName, tc)

    val tgcTransformed = tgc.transform.exec(initTransform)._1
    val tcTransformed = tc.transform.exec(initTransform)._1

    assert(tgcTransformed.getGroupName == groupName)
    assert(tgcTransformed.getCell.getRowSpan == tcTransformed.getRowSpan)

  }
}

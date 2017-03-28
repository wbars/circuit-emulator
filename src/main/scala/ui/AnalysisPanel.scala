package ui

import java.awt.Color
import javax.swing.BorderFactory

import core._

import scala.swing.Swing.{HGlue, VGlue}
import scala.swing._


class AnalysisPanel extends BoxPanel(Orientation.Vertical) {
  val cellSize = 30

  def formulaTable(formulas: Seq[(UiBulb, String)]): Panel = new BoxPanel(Orientation.Vertical) {
    peer.setBackground(Color.white)
    formulas.foreach(t => {
      contents += VGlue
      contents += new Label(t._1.getName + ": " + t._2)
    })
    contents += VGlue
  }

  def buildTable(analysisResult: Seq[(Seq[(Switch, Signal)], Seq[(Bulb, Signal)])],
                 input: Seq[Switch],
                 outputs: Seq[Bulb],
                 formulas: Seq[(UiBulb, String)]) {
    setContent(truthTable(analysisResult), formulaTable(formulas))
  }

  private def truthTable(analysisResult: Seq[(Seq[(Switch, Signal)], Seq[(Bulb, Signal)])]) = {
    val columnNames: Seq[String] = Main.panel.switches.map(_.getName) ++ Main.panel.bulbs.map(_.getName)
    val data: Array[Array[Any]] = analysisResult.map(t => (t._1.map(_._2.display()) ++ t._2.map(_._2.display())).toArray[Any]).toArray
    new Table(data, columnNames) {
      peer.setSize(columnNames.size * cellSize, data.length * cellSize)
    }
  }

  def analyse() {
    val input = Main.panel.switches.map(_.switch)
    val outputs = Main.panel.bulbs.map(_.bulb)
    buildTable(DeviceAnalyser.analyse(input, outputs), input, outputs, Main.panel.bulbs.map(uiBulb => (uiBulb, DeviceAnalyser.formula(uiBulb.bulb))))
  }

  def setContent(truthTable: Table, formulas: Component) {
    tablePanel.contents.clear()
    tablePanel.contents += HGlue
    tablePanel.contents += new ScrollPane(truthTable)
    tablePanel.contents += HGlue

    formulaTable.contents.clear()
    formulaTable.contents += VGlue
    formulaTable.contents += formulas
    formulaTable.contents += VGlue
  }

  peer.setBackground(Color.white)
  border = BorderFactory.createLineBorder(Color.black)
  contents += new BoxPanel(Orientation.Horizontal) {
    peer.setBackground(Color.white)
    border = BorderFactory.createLineBorder(Color.black)
    contents += HGlue
    contents += new Label("Analysis")
    contents += HGlue
  }
  val tablePanel = new BoxPanel(Orientation.Horizontal)
  val formulaTable = new BoxPanel(Orientation.Vertical) {
    peer.setBackground(Color.white)
  }
  contents += tablePanel
  contents += VGlue
  contents += formulaTable
  peer.setMinimumSize(new Dimension(300, 300))
}

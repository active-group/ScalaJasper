package de.ag.scalajasper.core

package object components {
  trait Component {
    private[core] def transform : Transformer[(net.sf.jasperreports.engine.component.Component, net.sf.jasperreports.engine.component.ComponentKey)]
  }

  def externalComponent(component: net.sf.jasperreports.engine.component.Component,
                        componentKey: net.sf.jasperreports.engine.component.ComponentKey) = {
    new Component {
      private[core] def transform = Transformer.ret(component, componentKey)
    }
  }
}

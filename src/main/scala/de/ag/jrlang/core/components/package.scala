package de.ag.jrlang.core

package object components {
  trait Component
    extends Transformable[(net.sf.jasperreports.engine.component.Component, net.sf.jasperreports.engine.component.ComponentKey)] {
  }

  def externalComponent(component: net.sf.jasperreports.engine.component.Component,
                        componentKey: net.sf.jasperreports.engine.component.ComponentKey) = {
    new Component {
      def transform = Transformer.ret(component, componentKey)
    }
  }
}

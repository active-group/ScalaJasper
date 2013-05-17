package de.ag.jrlang.core

package object components {
  trait Component extends net.sf.jasperreports.engine.component.Component {
    def drop(): (net.sf.jasperreports.engine.component.Component, net.sf.jasperreports.engine.component.ComponentKey)
  }
}

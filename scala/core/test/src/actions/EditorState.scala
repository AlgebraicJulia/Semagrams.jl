package semagrams.actions

import utest._
import upickle.default._
import semagrams._
import semagrams.actions._
import semagrams.controllers._
import semagrams.util._
import com.raquo.laminar.api.L._
import org.scalajs.dom

object EditorStateSpec extends TestSuite {
  def makeEditorState(controllers: Seq[Controller]) = {
    val div = dom.document.createElement("div")
    dom.document.body.appendChild(div)
    val svgElt = svg.svg()
    val es = new EditorState(svgElt, controllers)
    render(div, svgElt)
    es
  }

  def tests = Tests {
    test("construction") {
      val es = makeEditorState(Seq())
      assert(true)
    }

    test("viewport registering") {
      val es = makeEditorState(Seq())
      val v = new Viewport(Seq())
      assert(es.elt.ref.children.length == 0)
      es.register(v)
      assert(es.elt.ref.children.length == 1)
    }

    test("accessing controllers") {
      val es = makeEditorState(Seq(MouseController()))
      val c = es.controller(MouseController)
    }
  }
}

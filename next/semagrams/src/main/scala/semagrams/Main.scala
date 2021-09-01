package semagrams

import com.raquo.laminar.api.L._
import org.scalajs.dom

object Main {
  def Hello(
      helloNameStream: EventStream[String],
      helloColorStream: EventStream[String]
  ): Div = {
    div(
      fontSize := "20px", // static CSS property
      color <-- helloColorStream, // dynamic CSS property
      strong("Hello, "), // static child element with a grandchild text node
      child.text <-- helloNameStream // dynamic child (text node in this case)
    )
  }

  def main(args: Array[String]): Unit = {
    val nameBus = new EventBus[String]
    val colorStream: EventStream[String] = nameBus.events.map { name =>
      if (name == "Sebastien") "red" else "unset" // make Sebastien feel special
    }

    val appDiv: Div = div(
      h1("User Welcomer 9000"),
      div(
        "Please enter your name:",
        input(
          typ := "text",
          inContext(thisNode =>
            onInput.mapTo(thisNode.ref.value) --> nameBus
          ) // extract text entered into this input node whenever the user types in it
        )
      ),
      div(
        "Please accept our greeting: ",
        Hello(nameBus.events, colorStream)
      )
    )

    render(dom.document.querySelector("#appContainer"), appDiv)
  }
}

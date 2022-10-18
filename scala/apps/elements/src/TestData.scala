package semagrams.elements

import semagrams._
import semagrams.acsets._
import semagrams.util._

object TestData {
  val schLabeledDDS = """
    {
    "version": {
        "ACSetSchema": "0.0.1",
        "Catlab": "0.14.8"
    },
    "Ob": [
        {
        "name": "X"
        }
    ],
    "Hom": [
        {
        "name": "next",
        "codom": "X",
        "dom": "X"
        }
    ],
    "AttrType": [
        {
        "name": "Label"
        }
    ],
    "Attr": [
        {
        "name": "label",
        "codom": "Label",
        "dom": "X"
        }
    ]
    }
  """

  val exLabeledDDS = """
    {
    "X": [
        {
        "next": 2,
        "label": "a"
        },
        {
        "next": 3,
        "label": "b"
        },
        {
        "next": 4,
        "label": "c"
        },
        {
        "next": 1,
        "label": "d"
        }
    ]
    }
  """

  val props = Seq(
    (0, Complex(50, 50)),
    (1, Complex(50, 150)),
    (2, Complex(150, 50)),
    (3, Complex(150, 150))
  ).map((i, p) => (Part(i, SymOb("X")), PropMap() + (Center, p) + (Content, i.toString())))
}

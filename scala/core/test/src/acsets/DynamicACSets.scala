package semagrams.acsets

import utest._
import upickle.default._
import semagrams.acsets._
import semagrams._
import semagrams.util._

object DynamicACSetSpec extends TestSuite {
  object Data {
    val schDDS = """
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
        "AttrType": [],
        "Attr": []
        }
    """

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
  }

  def tests = Tests {
    test("read dynamic schemas") {
      val schLabeledDDS = read[DynSchema](Data.schLabeledDDS)

      assert(schLabeledDDS.obs contains DynOb("X"))
      assert(schLabeledDDS.homs(DynOb("X")) contains DynHom("next", DynOb("X"), DynOb("X")))
      assert(schLabeledDDS.attrs(DynOb("X")) contains DynAttr("label", DynOb("X"), DynAttrType("Label")))
    }

    test("read acset") {
      val schLabeledDDS = read[DynSchema](Data.schLabeledDDS)

      val dds = schLabeledDDS.readACSet(Data.exLabeledDDS)
    }
  }
}

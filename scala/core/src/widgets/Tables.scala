package semagrams.widgets

import com.raquo.laminar.api.L._
import org.scalajs.dom
import upickle.default._

import semagrams._
import semagrams.partprops._
import semagrams.state.KeyModifier
import scala.util._

/** PropTable - A generic, editable table class in laminar * */

/*  A `PropTable` is a sequence of `PropRow`s indexed by a `key:K`.
 *  Each `PropRow` is a sequence of `PropCell`s, indexed by a `col:Property`.def
 *  Each `PropCell` can toggle between a `tableCell` (static) and a `tableInput`
 *  (editable).
 *
 *  Each component (`PropTable`,`PropRow`,`PropCell`) is organized as a case class
 *  containing static data (e.g., columns). Each component has a method `laminarElt`
 *  that accepts appropriate signals and observers in order to generate the associated
 *  laminar element.
 *
 *  `PropTable` also has a related method `laminarEltWithCallback` which also returns
 *  a callback for triggering edits from outside the table. */

/** Messages * */

/** The editing state ( :Option[(K,Property)] ) is stored in the table; cells
  * and rows pass messages to modify it. There are three kinds of messages:
  *   - CellMsg contains low-level information about what values were returned
  *     and how (e.g., Enter, Esc, etc.).
  *   - TableMsg contains high-level information about modifications to values
  *     and the editing state.
  *
  * `CellMsg`s are converted to `TableMsg`s within `PropRow`, because this
  * requires knowledge of neighboring rows, which is hard to access at the
  * top-level table.
  */

enum CellMsg:
  case EscMsg
  case ToggleMsg(f: Property)
  case EnterMsg(v: PropChange[_])
  case TabMsg(v: PropChange[_], shift: Boolean)
import CellMsg._

sealed trait TableMsg[K <: Matchable]
sealed trait ChangeMsg[K]

case class EditMsg[K <: Matchable](
    change: Option[(K, PropChange[_])],
    newEdit: Option[(K, Property)]
) extends TableMsg[K]

case class SetValue[K](key: K, change: PropChange[_]) extends ChangeMsg[K]
case class HoverMsg[K <: Matchable](key: K, highlighted: Boolean = true)
    extends ChangeMsg[K]
    with TableMsg[K]:
  def change = if highlighted
  then PropChange(Hovered, None, ())
  else PropChange(Hovered, (), None)

/** A class to generate editable laminar tables * */
class PropTable[K <: Matchable](
    name: String,
    cols: Seq[Property],
    keys: Seq[Property]
):

  type EditState = Option[(K, Property)]
  val editingVar: Var[EditState] = Var(None)

  /* Update the edit state. For unknown reasons, this must be called from
   * within `laminarElt`. */
  def editUpdate(msg: TableMsg[K]) = msg match
    case msg: EditMsg[_]  => editingVar.set(msg.newEdit)
    case msg: HoverMsg[_] => ()

  def headerRow = tr(
    th(name) +: cols.map(prop => th(prop.toString())),
    backgroundColor := "lightgray"
  )

  /** Create an editable laminar element. */
  def laminarElt[Model](
      propSig: Signal[Seq[(K, PropMap)]],
      editStream: EventStream[EditMsg[K]],
      messenger: Observer[ChangeMsg[K]]
  ) =

    val msgObs: Observer[TableMsg[K]] = Observer(_ match
      case msg: HoverMsg[K] => messenger.onNext(SetValue(msg.key, msg.change))
      case msg: EditMsg[K] =>
        editingVar.set(msg.newEdit)
        msg.change match
          case Some(k -> change) =>
            messenger.onNext(SetValue(k, change))
          case None => ()
    )

    /* Create signal of `PropRow`s */
    val rowSig = propSig.map(rows =>
      val (ks, _) = rows.unzip
      rows.map((k, props) =>
        val (prv, nxt) = neighbors(k, ks)
        PropRow(k, cols, prv, nxt) -> props
      )
    )

    /* Create signal of row elements */
    val eltSig = rowSig.split(_._1) { (row, rowProps, rpSig) =>
      row.laminarElt(
        rpSig.map(_._2),
        editingVar.signal.map(
          _.flatMap((k, f) => if k == row.key then Some(f) else None)
        ),
        msgObs
      )
    }

    /* Return table element. `height` attribute for full-size divs in table */
    table(
      headerRow,
      editStream --> msgObs,
      children <-- eltSig,
      height := "max-content",
      margin := "2px"
    )

  /** Create a laminar table along with a callback function to trigger table
    * updates. *
    */
  def laminarEltWithCallback(
      rowSig: Signal[Seq[(K, PropMap)]],
      editStream: EventStream[EditMsg[K]],
      messenger: Observer[ChangeMsg[K]]
  ) =
    val elt = laminarElt(rowSig, editStream, messenger)
    def edit(k: K, col: Property) =
      editingVar.set(Some(k -> col))

    /* Return the pair */
    (elt, edit)

object PropTable:

  /** Convenience method to deduplicate and move keys to the front */
  def apply[K <: Matchable](
      name: String,
      cols: Seq[Property],
      keys: Seq[Property] = Seq()
  ) =
    keys match
      case Seq() =>
        new PropTable[K](name, cols.distinct, cols.distinct)
      case _ =>
        val ks = keys.distinct
        val cs = ks ++ cols.distinct.filterNot(ks.contains)
        new PropTable[K](name, cs, ks)

/** A case class to generate table row elements from `PropMap`s * */
case class PropRow[K <: Matchable](
    key: K,
    cols: Seq[Property],
    prv: Option[K],
    nxt: Option[K]
):

  /* Neighboring cells */
  val nbrs: Seq[(K, Property)] =
    if cols.isEmpty
    then Seq()
    else
      prv.map((_, cols.last)).toSeq
        ++ cols.map((key, _))
        ++ nxt.map((_, cols.head))

  /* Transform CellMsg into TableMsg */
  def transform(msg: CellMsg) = msg match
    case ToggleMsg(col) => EditMsg[K](None, Some(key -> col))
    case EscMsg         => EditMsg[K](None, None)
    case EnterMsg(pval) => EditMsg(Some(key -> pval), None)
    case TabMsg(pval, shift) =>
      val nextEdit =
        if shift
        then prev((key, pval.prop), nbrs)
        else next((key, pval.prop), nbrs)
      EditMsg(Some(key -> pval), nextEdit)

  /** Create an editable laminar row element. */
  def laminarElt(
      rowSig: Signal[PropMap],
      editSig: Signal[Option[Property]],
      tableObs: Observer[TableMsg[K]]
  ) =

    /* Create a modifier (not an element) for each column. */
    val cellMods = cols.map { col =>
      /* Split signal of a single column */
      val cellSig: Signal[Element] = rowSig.splitOne {
        _.get(col)
      } { (pval, pair, pairSig) =>
        val cell = PropCell[K, col.Value](key, col)(pval)

        cell.laminarElt(
          pairSig.map(props => props.get(col)),
          editSig.map(propOpt => propOpt.contains(col)),
          tableObs.contramap(transform)
        )
      }

      /* Return a modifier accepting the signal */
      child <-- cellSig
    }

    /* Return a row element */
    tr(
      td(key.toString),
      cellMods,
      backgroundColor <-- rowSig.map(_._1.get(Highlight) match
        case Some(_) => "lightblue"
        case None    => "white"
      ),
      onMouseEnter.mapTo(HoverMsg(key)) --> tableObs,
      onMouseLeave.mapTo(HoverMsg(key, false)) --> tableObs
    )

/** A case class to generate table cells from `Property`s * */
case class PropCell[K <: Matchable, T](
    key: K,
    col: Property { type Value = T }
)(tOpt: Option[T]):

  def laminarElt[Model](
      valSig: Signal[Option[col.Value]],
      editSig: Signal[Boolean],
      editObs: Observer[CellMsg]
  ) =
    td(
      cls := "dcell",
      child <-- editSig.map(_ match
        case true => tableInput(col, editObs, tOpt)
        case false =>
          tableCell(col, valSig, editObs.contramap((_: Unit) => ToggleMsg(col)))
      )
    )

/** Create a laminar table cell with a value signal and a input toggle on double
  * click.
  */
def tableCell(
    col: Property,
    vSig: Signal[Option[col.Value]],
    toggle: Observer[Unit]
) = div(
  cls("dcell-text"),
  child.text <-- vSig.map(_ match
    case Some(v) => noquotes(write(v)(col.rw))
    case None    => ""
  ),
  onDblClick.mapTo(()) --> toggle,
  height := "100%"
)

/** Create a laminar table cell input with an initial value. The input observes
  * pairs of `InputMsg` (key + mods) and an optional return value.
  */
def tableInput(
    col: Property,
    cellObs: Observer[CellMsg],
    init: Option[col.Value] = None
) =
  td(
    cls("dcell"),
    input(
      cls("dcell-edit"),
      value := (init match
        case Some(t) => noquotes(write(t)(col.rw))
        case None    => ""
      ),
      onKeyDown.stopPropagation --> Observer(_ => ()),
      onMountFocus,
      inContext(thisNode => onMountCallback(_.thisNode.ref.select())),
      modFilters.flatMap(mods =>
        def catchEmpty(s: String) = (init, readStr(s)(col.rw)) match
          case (None, Some("")) => None
          case (_, opt)         => opt

        Seq(
          onEsc
            .filter(evtFilter(mods))
            .mapToValue
            .map(_ => EscMsg) --> cellObs,
          onEnter
            .filter(evtFilter(mods))
            .mapToValue
            .map(v =>
              EnterMsg(PropChange(col, init, catchEmpty(v)))
            ) --> cellObs,
          onTab
            .filter(evtFilter(mods))
            .mapToValue
            .map(v =>
              TabMsg(
                PropChange(col, init, catchEmpty(v)),
                mods.contains(KeyModifier.Shift)
              )
            ) --> cellObs
        )
      )
    )
  )

/** Utilities * */

val onEnter = onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
val onEsc = onKeyDown.filter(_.keyCode == dom.KeyCode.Escape)
val onTab =
  onKeyDown.filter(evt => evt.keyCode == dom.KeyCode.Tab).preventDefault

/** Triggers for input modification */

/** All combinations of key modifiers */
val modFilters = KeyModifier.values.toSet.subsets.toSeq

/** Test a keyboard event against a set of key modifiers */
def evtFilter(modOptions: Set[KeyModifier])(evt: dom.KeyboardEvent) =
  KeyModifier.values.forall(mod =>
    (modOptions.contains(mod) & mod.isSet(evt)) |
      (!modOptions.contains(mod) & !mod.isSet(evt))
  )

/** Return `Some(next value)` in `ts` after `t` (or `None`) */
def next[T](t: T, ts: Seq[T]): Option[T] =
  if ts.indexOf(t) + 1 < ts.length
  then Some(ts(ts.indexOf(t) + 1))
  else None

/** Return `Some(prev value)` in `ts` before `t` (or `None`) */
def prev[T](t: T, ts: Seq[T]) =
  if ts.indexOf(t) > 0
  then Some(ts(ts.indexOf(t) - 1))
  else None

/** Return (optional) neighbors of `t` in `ts` */
def neighbors[T](t: T, ts: Seq[T]) = (prev(t, ts), next(t, ts))

/** Helper method with special handling for Value = String */
def readStr[T: ReadWriter](s: String): Option[T] =
  Try(read[T](s)) match
    case Success(t) => Some(t)
    case Failure(_) =>
      Try(read[T](write(s))) match
        case Success(t) => Some(t)
        case Failure(_) => None

def noquotes(s: String) = if s.length > 1 & s.head == s.last & s.head == '"'
then s.slice(1, s.length - 1)
else s

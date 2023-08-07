package semagrams

/** The external API for Semagram exposed to apps */
package object api {
  export semagrams.{
    EditorState,
    Event,
    EntityCollector,
    GlobalState,
    Semagram,
    Sprite,
    MouseButton,
    KeyModifier,
    PropMap,
    EntitySource,
    EntityMap,
    BoundingBox,
    EntityType,
    Entity,
    PValue
  }
  export semagrams.GenericProperty._
  export layout.{assignBends, FixedRangeExceptEnds}
  export sprites.{
    Arrow,
    Disc,
    Rect,
    WireStub,
    ACSetEntitySource,
    ACSetEdgeSource,
    WireProp,
    findCenter,
    wireProps,
    DPBox
  }
  export util.{
    Complex,
    UndoableVar,
    fromMaybe,
    toOption,
    onCancelOrError,
    realToComplex,
    msgError
  }
  export widgets.{Select, Position, PositionWrapper, Menu}
}

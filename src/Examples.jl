"""
Pre-fab acset schemas+semagrams schemas
"""
module Examples

export TheoryReactionNet, ReactionNet, ReactionNetSema,
  TheoryDirectedPortGraph, DirectedPortGraph, DirectedPortGraphSema,
  TheoryColoredDPG, ColoredDPG, ColoredDPGSema,
  TheoryCircuitGraph, CircuitGraph, CircuitGraphSema,
  TheoryDDS, DDS, DDSSema,
  TheoryUWD, UWD, UWDSema

using Catlab.Present, Catlab.CSetDataStructures
using JSExpr
using ..Schema
using ..Boxes

@present TheoryReactionNet(FreeSchema) begin
  (T,S,I,O)::Ob
  it::Hom(I,T)
  is::Hom(I,S)
  ot::Hom(O,T)
  os::Hom(O,S)
  N::Data
  L::Data
  rate::Attr(T,N)
  concentration::Attr(S,N)
  species_label::Attr(S,L)
  transitions_label::Attr(T,L)
end

const ReactionNet = ACSetType(TheoryReactionNet)

@semagramschema ReactionNetSema(TheoryReactionNet) begin
  @box S Circle label=:species_label
  @box T Square label=:transitions_label
  @wire I(is,it)
  @wire O(ot,os)
  @data N Numeric
end

@present TheoryDirectedPortGraph(FreeSchema) begin
  Box::Ob
  IPort::Ob
  OPort::Ob
  Wire::Ob
  ibox::Hom(IPort,Box)
  obox::Hom(OPort,Box)
  src::Hom(Wire,OPort)
  tgt::Hom(Wire,IPort)

  String::Data
  label::Attr(Box,String)
end

const DirectedPortGraph = ACSetType(TheoryDirectedPortGraph)

@semagramschema DirectedPortGraphSema(TheoryDirectedPortGraph) begin
  @box Box Square label=:label
  @port IPort(ibox) style="Input"
  @port OPort(obox) style="Output"
  @wire Wire(src,tgt)
  @data String Stringlike
end

@present TheoryColoredDPG <: TheoryDirectedPortGraph begin
  Color::Data
  iportcolor::Attr(IPort,Color)
  oportcolor::Attr(OPort,Color)
  wirecolor::Attr(Wire,Color)
end

const ColoredDPG = ACSetType(TheoryColoredDPG)

color(attr) = js"weights => { return { stroke: weights[$attr] }; }"

@semagramschema ColoredDPGSema(TheoryColoredDPG) begin
  @box Box Square label=:label
  @port IPort(ibox) style="Input" style_fn=color(:iportcolor)
  @port OPort(obox) style="Output" style_fn=color(:oportcolor)
  @wire Wire(src,tgt) style_fn=color(:wirecolor)
  @data String Stringlike
  @data Color Stringlike
end

@present TheoryCircuitGraph(FreeSchema) begin
  Box::Ob
  Port::Ob
  Wire::Ob
  box::Hom(Port,Box)
  src::Hom(Wire,Port)
  tgt::Hom(Wire,Port)

  Resistance::Data
  R::Attr(Wire, Resistance)
  Voltage::Data
  V::Attr(Port, Voltage)
end

const CircuitGraph = ACSetType(TheoryCircuitGraph)

@semagramschema CircuitGraphSema(TheoryCircuitGraph) begin
  @box Box Circle
  @port Port(box) style="Circular"
  @wire Wire(src, tgt)
  @data Resistance Stringlike
  @data Voltage Stringlike
end

@present TheoryDDS(FreeSchema) begin
  X::Ob
  next::Hom(X,X)
end

const DDS = ACSetType(TheoryDDS)

@semagramschema DDSSema(TheoryDDS) begin
  @box X Square
end

@present TheoryUWD(FreeSchema) begin
  Box::Ob
  Port::Ob
  Junction::Ob
  OuterPort::Ob
  box::Hom(Port,Box)
  junction::Hom(Port,Junction)
  outer_junction::Hom(OuterPort, Junction)
end

const UWD = ACSetType(TheoryUWD)

@semagramschema UWDSema(TheoryUWD) begin
  @box Box Circle
  @port Port(box) style="Circular"
  @box Junction TinyCircle
  @box OuterPort SmallCircle
end

end

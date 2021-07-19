"""
Pre-fab acset schemas+semagrams schemas
"""
module Examples

export TheoryPetri, ReactionNet, ReactionNetSema,
  TheoryDirectedPortGraph, DirectedPortGraph, DirectedPortGraphSema,
  TheoryColoredDPG, ColoredDPG, ColoredDPGSema

using Catlab.Present, Catlab.CSetDataStructures
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
  @box S Circle :species_label
  @box T Square :transitions_label
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
  @box Box Square :label
  @port IPort(ibox) "Input"
  @port OPort(obox) "Output"
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

@semagramschema ColoredDPGSema(TheoryColoredDPG) begin
  @box Box Square :label
  @port IPort(ibox) "Input"
  @port OPort(obox) "Output"
  @wire Wire(src,tgt)
  @data String Stringlike
  @data Color Stringlike
end

end

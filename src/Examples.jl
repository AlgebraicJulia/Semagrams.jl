"""
Pre-fab acset schemas+semagrams schemas
"""
module Examples

export TheoryPetri, Petri, PetriSema,
  TheoryDirectedPortGraph, DirectedPortGraph, DirectedPortGraphSema

using Catlab.Present, Catlab.CSetDataStructures
using ..Schema
using ..Boxes

@present TheoryPetri(FreeSchema) begin
    (T,S,I,O)::Ob
    it::Hom(I,T)
    is::Hom(I,S)
    ot::Hom(O,T)
    os::Hom(O,S)
end

const Petri = CSetType(TheoryPetri)

@semagramschema PetriSema(TheoryPetri) begin
    @box S Circle
    @box T Square
    @wire I(is,it)
    @wire O(ot,os)
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
end

const DirectedPortGraph = CSetType(TheoryDirectedPortGraph)

@semagramschema DirectedPortGraphSema(TheoryDirectedPortGraph) begin
    @box Box Square
    @port IPort(ibox) "Input"
    @port OPort(obox) "Output"
    @wire Wire(src,tgt)
end

end

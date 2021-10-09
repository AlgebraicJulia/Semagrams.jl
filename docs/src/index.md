# Semagrams.jl

## Installation

```julia
> using Pkg; Pkg.add("Semagrams"); Pkg.add("Catlab")
> using Semagrams, Catlab
```

Due to an [upstream issue](https://github.com/JuliaGizmos/WebIO.jl/issues/442), Semagrams.jl is currently only compatible with JupyterLab v2 (the latest version is v3). Therefore, you must downgrade your JupyterLab installation. For example, if you use Anaconda, you can do this by running `conda install jupyterlab=2.3.1`.

As an alternative to JupyterLab, you can use the standalone version of Semagrams.jl, which serves the application from a standalone web server; see the rest of this guide for how to do this.

## Quickstart

In a JupyterLab notebook:

```julia
> using Semagrams, Semagrams.Examples

> p = Semagram{ReactionNet{Float64,String}}(ReactionNetSema)

# Edit semagram (see below)

> get_acset(p)
```

## Extended Introduction

There are three steps to using Semagrams.

### 1. Prepare schemas.

This is the hardest part, and only has to be done when you are adding a *new* type of semagram; if you are using one of the semagrams supplied by this library (by `using Semagrams.Examples`) or another library, you can skip step 1.

First of all, you have to make an acset. Documentation for this can be found in [Catlab](https://algebraicjulia.github.io/Catlab.jl/dev/apis/categorical_algebra/#Acsets).

We will use the running examples of *Labeled Directed Port Graphs* and *Reaction Nets*. These have the schemas

```julia
using Catlab.Present.@present
using Catlab.CategoricalAlgebra.FreeSchema

@present TheoryReactionNet(FreeSchema) begin
  (T,S,I,O)::Ob
  it::Hom(I,T)
  is::Hom(I,S)
  ot::Hom(O,T)
  os::Hom(O,S)
  N::AttrType
  rate::Attr(T,N)
  concentration::Attr(S,N)
end

@acset_type ReactionNet(TheoryReactionNet)
```

and

```julia
@present TheoryDirectedPortGraph(FreeSchema) begin
  (Box,IPort,OPort,Wire)::Ob
  ibox::Hom(IPort,Box)
  obox::Hom(OPort,Box)
  src::Hom(Wire,OPort)
  tgt::Hom(Wire,IPort)
end

@acset_type DPG(TheoryDirectedPortGraph)
```

After making an acset, you must make a semagrams schema for the acset you made in step 1. The basic idea behind this is that you have to tell Semagrams.jl what role the objects in your acset play. Currently the three roles are
1. Box
2. Port
3. Wire
4. Data
Boxes have no outgoing morphisms (though, they can have attributes). Ports have exactly one outgoing morphism, to a box. Wires have two outgoing morphisms, one which gives the "source" of the wire, and another which gives the "target". These outgoing morphisms can go to *either* ports or boxes. Data objects in the acset schema must be given input types. The two choices are `Stringlike` or `Numeric`, which correspond to text boxes and sliders respectively (the slider goes from 0 to 100, we will eventually have something to configure this range).

You can designate a SVG object for each box. For instance, in a Petri net, you can make the species circles and the transitions squares. For the ports, you can designate a "style", which designates the placement of the ports. This can either be "Input", "Output", or "Circular", which places the ports vertically stacked on the left, vertically stacked on the right, and equally spaced around a circle.

For our two running examples, this looks like
```julia
@semagramschema ReactionNetSema(TheoryReactionNet) begin
  @box S Circle
  @box T Square
  @wire I(is,it)
  @wire O(ot,os)
  @data N Numeric
end
```

```julia
@semagramschema DirectedPortGraphSema(TheoryDirectedPortGraph) begin
  @box Box Square
  @port IPort(ibox) "Input"
  @port OPort(obox) "Output"
  @wire Wire(src,tgt)
  @data String Stringlike
end
```

### 2. Create and edit the semagram.

First you have to create the semagram.

```julia
my_awesome_petri_net = Semagram{ReactionNet{Float64}}(ReactionNetSema)
```

or

```julia
my_awesome_dpg = Semagram{DPG}(DirectedPortGraphSema)
```

When you run this line in a Jupyter notebook, the editor should pop up as the result IF you have WebIO properly installed (which actually somewhat tricky, and WebIO is not supported for the latest version of Jupyter yet).

If you are running in the Julia REPL, you have to do the following to interact with the Semagram:

```julia
serve_semagram(my_awesome_semagram)
```

By default, this will open a server on port 8000, you can change this port by passing in a `port` keyword argument to `serve_semagram`. It will also try to open a browser pointing to `http://localhost:$port` (`http://localhost:8000` by default).

Currently the editor is very barebones; you have to refer here for the keybindings.

- `b` adds a box under the cursor. If there are multiple types of box that one can add, it will pop up a modal window which allows you to select the type by typing a number.
- `p` adds a port to the box that the cursor is hovering over. If there are multiple types of port, then again a window will pop up which allows you to select the type of port that you want to add.
- `s` sets a port or box to be the current "source". When you select the source, it should turn *light blue*.
- `t` sets a port or box to be the current "target". When you select the target it should turn *light red*.
- `w` adds a new wire, using the ports/boxes selected as source and target. If there is no wire type compatible with the source/target pair, then this will remove the selection of source/target and not add a wire. If there is more than one wire type that could be added, it will pop up a modal asking you which type of wire you wish to add.
- `d` deletes the wire, port or box under the cursor.
- `D` prints the current value of the semagram to the javascript console. Useful for debugging.
- `Escape` clears the current selection of the source/target
- `?` brings up the help menu

When you click on an entity, below the main window should pop up widgets that allow you to edit the attributes of that entity.

### 3. Profit

Once you have edited your semagram, you can get an acset out of it using

```julia
get_acset(my_awesome_petri_net)
```

or

```julia
get_acset(my_awesome_dpg)
```

This will return the current value of the semagram as an acset. Then you can do all the acset-y good things that one does with acsets.

If you want to save the semagram, you have to save more than the acset, because we don't have automatic layout algorithms. To save the current state of the acset to a file, run

```julia
save(my_awesome_dpg, "my_awesome_dpg.sema")
```

Then, later on you can run

```julia
load(my_awesome_dpg, "my_awesome_dpg.sema")
```

and it will load up whatever you saved earlier.

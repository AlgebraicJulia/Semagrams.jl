<TeXmacs|1.99.18>

<style|generic>

<\body>
  <doc-data|<doc-title|A Generic Editor for Graph-like
  Structures>|<doc-author|<\author-data|<author-name|Owen Lynch>>
    \;
  </author-data>>>

  <section|Motivation>

  There are many graph-like structures that can be expressed as acsets. It
  would be neat if these could all be edited graphically with a single
  editor.

  There are several commonalities between the various graph-like structures,
  i.e.

  <\itemize>
    <item>Graphs

    <item>Petri Nets

    <item>Port Graphs

    <item>Wiring Diagrams
  </itemize>

  Namely, they can be broken down into the following components

  <\itemize>
    <item>Nodes: standalone nodes, they have no morphisms going out

    <item>Ports: attached to nodes/other ports, they have one morphism going
    out

    <item>Edges: source/target attached to nodes/ports, two morphisms going
    out

    <item>Colors (must match between ports/edges)

    <item>Weights
  </itemize>

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|prog-scripts|scheme>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
  </collection>
</references>
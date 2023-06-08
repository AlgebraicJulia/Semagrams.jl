<TeXmacs|2.1>

<style|generic>

<\body>
  <doc-data|<doc-title|A Structural Analysis of
  Semagrams>|<doc-author|<author-data|<author-name|Owen Lynch>>>>

  <section|Introduction>

  The purpose of this document is to develop doctrine and vocabulary for
  continued development of Semagrams. As Semagrams becomes more integrated
  with other components of a holistic system, it is necessary to formalize
  how the other parts of the system will interact with Semagrams.
  Additionally, development of doctrinal discipline around different parts of
  the system is necessarily in order to develop test suites for those parts
  in isolation.

  To this end, we present an analysis in three parts: state, communication,
  and purity.

  <section|State>

  In this section, we analyse where state lives in Semagrams. All of the rest
  of Semagrams is controlled, in one way or another, by the state, so it is
  important to first analyze where the state lives so that we can manage it.

  State in Semagrams can be organized into three categories.

  <\enumerate>
    <item>Model state. Semagrams operates as an editor of some model,
    typically an <verbatim|ACSet>, and this state must live somewhere.
    Currently, this lives in a Laminar <verbatim|Var>.

    <item>Control flow state. The control flow of complex editing operations
    acts as an implicit \Pstate machine\Q that can be suspended and resumed
    via the <verbatim|IO> monad from <verbatim|cats-effect>.

    <item>Application state. There is a certain amount of book-keeping that
    must be done in order to make sense of the actions of the user. For
    instance, we keep track of:

    <\itemize>
      <item>The current hovered entity

      <item>The state of a drag action

      <item>The current mouse position
    </itemize>

    The main location for application state is inside <verbatim|Controller>s,
    but it also shows up in other places. The idea behind
    <verbatim|EditorState> is to collect all of the application state into
    one structure.
  </enumerate>

  All three of these categories of state might plausibly be exposed to a
  larger context that Semagrams is embedded in. The model state is most
  straightforwardly exposed; we can simply share access to the relevant
  <verbatim|Var>. The control flow state is less straightforward. We might
  interact with it externally by passing events to it, and also it might
  interact with external things by simply running code. Finally, the
  application state might be queried externally.\ 

  <section|Communication>

  In this section, we analyze the communcation between different parts of
  Semagrams, and between Semagrams and the browser, and ultimately the user.

  <\big-figure|<image|figures/communication.pdf|256pt|148pt||>>
    Communication structure of Semagrams
  </big-figure>

  <section|Purity>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-2|<tuple|2|1>>
    <associate|auto-3|<tuple|3|1>>
    <associate|auto-4|<tuple|1|1>>
    <associate|auto-5|<tuple|4|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|figure>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        \;
      </surround>|<pageref|auto-4>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Introduction>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>State>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Communication>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>Purity>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
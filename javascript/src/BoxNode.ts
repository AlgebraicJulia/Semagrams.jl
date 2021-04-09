import { colorAttachment, EditorState } from "./EditorState";
import { map } from "@thi.ng/transducers";
import { BOXRADIUS } from "./LocatedWireViz";
import { box_attach } from "./WireViz";
import { PortNode } from "./PortNode";
import m from 'mithril';
import katex from 'katex';

interface BoxAttrs {
    state: EditorState,
    box_idx: number
}

export const BoxNode: m.Component<BoxAttrs> = {
    view: function({ attrs: { state, box_idx } }) {
        const box = state.lw.wireviz.boxes.get(box_idx)!;
        const boxty = state.lw.wireviz.schema.box_types[box.ty];
        const a = box_attach(box_idx);
        const loc = state.lw.getLoc(a)!;
        const attrs = {
            fill: colorAttachment(state, a),
            stroke: box.color ?? "black",
            transform: `translate(${loc[0]} ${loc[1]})`,
        };
        const portnodes = map(port_idx => m(PortNode, { state, box_idx, port_idx }),
            box.ports.keys());
        const handle = m("circle", {
            r: BOXRADIUS,
            "fill-opacity": "0",
            "stroke-opacity": "0",
            "data-a": JSON.stringify(a),
            onmouseover: state.handlemouseenterattachment,
            onmouseout: state.handlemouseoutattachment,
            onmousedown: state.handlemousedownbox,
            onmouseup: state.handlemouseupbox,
        })
        const highlight = m("g", {
            transform: "scale(1.1)",
            fill: state.selected == box_idx ? "yellow" : "none",
            stroke: "none",
        }, m.trust(boxty.shape));
        const text = m("foreignObject", {
            x: "-40px",
            y: "-40px",
            width: "80px",
            height: "80px",
        }, m("div", {
            xmlns: "http://www.w3.org/1999/xhtml",
            style: `display:flex;
                    justify-content: center;
                    align-items: center;
                    height:100%;
                    width:100%`
        }, m.trust(katex.renderToString("\\int_a^b f", {
            output: "mathml"
        }))));
        const b = m("g", attrs, highlight, m.trust(boxty.shape), text, handle);
        return m("g", b, ...portnodes);
    }
}

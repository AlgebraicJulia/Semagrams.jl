import { colorAttachment, EditorContext, EditorState } from "./EditorState";
import { map } from "@thi.ng/transducers";
import { BOXRADIUS } from "./LocatedWires";
import { box_attach } from "./Wires";
import { portnode } from "./PortNode";
import { BoxStyle } from "./WiresSchema";
import { add2 } from "@thi.ng/vectors";
import m from 'mithril';


const square = [
    [-BOXRADIUS, -BOXRADIUS],
    [BOXRADIUS, -BOXRADIUS],
    [BOXRADIUS, BOXRADIUS],
    [-BOXRADIUS, BOXRADIUS],
]

interface BoxAttrs {
    state: EditorState,
    box_idx: number
}

export const BoxNode: m.Component<BoxAttrs> = {
    view: function({ attrs: { state, box_idx } }) {
        const box = state.lw.wires.boxes.get(box_idx)!;
        const boxty = state.lw.wires.schema.box_types[box.ty];
        const a = box_attach(box_idx);
        const loc = state.lw.getLoc(a)!;
        const attrs = {
            fill: colorAttachment(state, a),
            stroke: box.color ?? "black",
            "data-a": JSON.stringify(a),
            onmousenter: state.handlemouseenterattachment,
            onmouseout: state.handlemouseoutattachment,
            onmousedown: state.handlemousedownbox,
            onmouseup: state.handlemouseupbox,
        };
        const portnodes = map(port_idx => [portnode, box_idx, port_idx], box.ports.keys());
        var b: m.Vnode;
        switch (boxty.style) {
            case BoxStyle.Circular: {
                b = m("circle", {
                    r: BOXRADIUS,
                    cx: loc[0],
                    cy: loc[1],
                    ...attrs
                });
                break;
            }
            case BoxStyle.Square: {
                b = m("polygon", {
                    points: square.map(p => [...add2([], loc, p)].join(",")).join(" "),
                    ...attrs
                });
                break;
            }
        }
        return m("g", b, ...portnodes);
    }
}

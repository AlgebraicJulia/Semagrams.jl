import * as svg from "@thi.ng/hiccup-svg";
import { colorAttachment, EditorContext } from "./EditorState";
import { port_attach } from "./Wires";

const PORTRADIUS = 7;

export function portnode({ state }: EditorContext, box_idx: number, port_idx: number) {
    const a = port_attach(box_idx, port_idx);
    const loc = state.lw.getLoc(a)!;
    const port = state.lw.wires.boxes.get(box_idx)!.ports.get(port_idx)!;
    const attrs = {
        fill: colorAttachment(state, a),
        stroke: port.color ?? "black",
        "data-a": JSON.stringify(a),
        onmouseenter: state.handlemouseenterattachment,
        onmouseout: state.handlemouseoutattachment,
    };
    return svg.circle(loc, PORTRADIUS, attrs);
}

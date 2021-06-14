import { map, flatten } from "@thi.ng/transducers";
import { EditorState } from "./EditorState";
import { WireNode } from "./WireNode";
import { BoxNode } from "./BoxNode";
import { PortNode } from "./PortNode";
import m from 'mithril';

export const EditorPane: m.Component<{ state: EditorState }> = {
    view({ attrs: { state } }) {
        /*
         * Two components (in order)
         * - Wires
         * - Boxes (boxes includes ports)
         */
        const boxnodes = map(box_idx => m(BoxNode, { state, box_idx }),
            state.boxes());
        const portnodes = map(({ box_idx, port_idx }) => m(PortNode, { state, box_idx, port_idx }),
            state.ports())
        const wirenodes = map(wire_idx => m(WireNode, { state, wire_idx }),
            state.wires());
        return m("g", ...wirenodes, ...boxnodes, ...portnodes);
    }
}

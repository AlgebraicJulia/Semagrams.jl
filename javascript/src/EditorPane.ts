import { map, flatten } from "@thi.ng/transducers";
import { EditorState } from "./EditorState";
import { WireNode } from "./WireNode";
import { BoxNode } from "./BoxNode";
import { PortNode } from "./PortNode";
import m from 'mithril';
import { HomNode } from "./Homs";

export const EditorPane: m.Component<{ state: EditorState, isExport: boolean }> = {
    view({ attrs: { state, isExport } }) {
        /*
         * Two components (in order)
         * - Wires
         * - Boxes (boxes includes ports)
         */
        const boxnodes = map(box_idx =>
            m(BoxNode, { state, box_idx, isExport }),
            state.boxes());
        const portnodes = map(({ box_idx, port_idx }) =>
            m(PortNode, { state, box_idx, port_idx, isExport }),
            state.ports())
        const wirenodes = map(wire_idx =>
            m(WireNode, { state, wire_idx, isExport }),
            state.wires());
        const homnodes = map(([src, tgt]) =>
            m(HomNode, { state, src, tgt, isExport }),
            state.homs());
        return m("g", ...homnodes, ...wirenodes, ...boxnodes, ...portnodes);
    }
}

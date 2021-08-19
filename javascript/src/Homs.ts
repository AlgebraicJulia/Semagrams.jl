import { EditorState } from "./EditorState";
import { Attachment, Entity } from "./Semagram";
import m from 'mithril';
import { curvePoints, svgPath } from "./ArrowUtils";

interface HomAttrs {
    state: EditorState,
    src: Entity,
    tgt: Entity
}

export const HomNode: m.Component<HomAttrs> = {
    view({ attrs: { state, src, tgt } }) {
        const sloc = state.ls.getLoc(src as Attachment)!;
        const tloc = state.ls.getLoc(tgt as Attachment)!;
        return m("path", {
            d: svgPath(curvePoints(sloc, tloc, 0)),
            stroke: "black",
            "stroke-dasharray": "5,5",
            "marker-mid": "url(#arrow-solid)",
            fill: "none"
        });
    }
}

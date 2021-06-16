import { colorAttachment, EditorState } from "./EditorState";
import { equiv } from "@thi.ng/equiv";
import { BOXRADIUS } from "./LocatedSemagram";
import { box_entity } from "./Semagram";
import * as CS from "./ColorScheme";
import m from 'mithril';

interface BoxAttrs {
    state: EditorState,
    box_idx: number
}

/**
 * Component giving control over a box.
 */
export const BoxHandle: m.Component<BoxAttrs> = {
    view: function({ attrs: { state, box_idx } }) {
        const a = box_entity(box_idx);
        const loc = state.ls.getLoc(a)!;
        return m("circle", {
            transform: `translate(${loc[0]} ${loc[1]})`,
            r: BOXRADIUS,
            "fill-opacity": "0",
            "stroke-opacity": "0",
            "data-a": JSON.stringify(a),
            onmouseover: state.cursor.handlemouseenterattachment,
            onmouseout: state.cursor.handlemouseoutattachment,
            onmousedown: state.cursor.handlemousedownbox,
            onmouseup: state.cursor.handlemouseupbox,
        });
    }
}

/**
 * Component displaying a box.
 * Note: Our philosophy is that rather than passing in just the
 * information relevant to the box, we pass in the entire EditorState
 * and the index of the box that we want to draw.
 * This seems like it could be overkill/violation of "separation of concerns".
 *
 * Maybe at some point we will decide that this is a bad philosophy, and we should
 * factor out all of the stuff actually relevant to drawing a single box, and then
 * have only that be the attributes passed into the BoxNode.
 *
 * TODO: get KaTeX working. This is blocked on first having a way of editting box attributes.
 *
 * Note: this is the parent of all of its ports.
 */
export const BoxNode: m.Component<BoxAttrs> = {
    view: function({ attrs: { state, box_idx } }) {
        const box = state.ls.sg.boxes.get(box_idx)!;
        const boxty = state.ls.sg.schema.box_types[box.ty];
        const a = box_entity(box_idx);
        const loc = state.ls.getLoc(a)!;
        const attrs = {
            fill: colorAttachment(state, a),
            stroke: box.color ?? CS.accent,
            transform: `translate(${loc[0]} ${loc[1]})`,
        };
        const highlight = m("g", {
            transform: "scale(1.1)",
            fill: equiv(state.dialogue.selected, box_entity(box_idx)) ? "yellow" : "none",
            stroke: "none",
        }, m.trust(boxty.shape));
        return m("g", attrs, highlight, m.trust(boxty.shape));
    }
}

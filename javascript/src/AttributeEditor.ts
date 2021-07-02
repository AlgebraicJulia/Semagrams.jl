import { EditorState } from './EditorState';
import { AttributeType } from './Schema';
import m from 'mithril';
import { map } from '@thi.ng/transducers';
import { Entity } from './Semagram';

interface WidgetAttrs {
    state: EditorState,
    entity: Entity,
    attribute_type: AttributeType,
    attribute: string
}

export const AttributeWidget: m.Component<WidgetAttrs> = {
    view({ attrs: { state, entity, attribute_type, attribute } }) {
        const obj = state.ls.sg.getEntity(entity)!;
        const curval = obj.weights[attribute];
        const oninput = (s: any) => {
            obj.weights[attribute] = s.target.value;
            state.save();
        };
        const input_elt = m("input", {
            "type": "text",
            value: curval,
            oninput: oninput
        });
        return m("label",
            `${attribute}: `,
            input_elt
        )
    }
}

export const AttributeEditor: m.Component<{ state: EditorState }> = {
    view({ attrs: { state } }) {
        if (state.dialogue.selected != null) {
            return m("div",
                ...map(([attrtype, a]) =>
                    m(AttributeWidget, {
                        state: state,
                        entity: state.dialogue.selected!,
                        attribute_type: attrtype,
                        attribute: a,
                    }),
                    state.ls.sg.weightTypes(state.dialogue.selected)!))
        } else {
            return m("div");
        }
    }
}

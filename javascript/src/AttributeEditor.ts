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

export const SliderWidget: m.Component<{ curval: string, oninput: Function }> = {
    view({ attrs: { curval, oninput } }) {

    }
}

export const AttributeWidget: m.Component<WidgetAttrs> = {
    view({ attrs: { state, entity, attribute_type, attribute } }) {
        var input_elt: m.Vnode;
        const curval = state.ls.sg.getEntity(entity)!.weights[attribute];
        const oninput = (input: any) => {

        };
        switch (attribute_type) {
            case AttributeType.Stringlike: {
                input_elt = m("input", {
                    "type": "text",
                    value: curval,
                    oninput: oninput
                });
                break;
            }
            case AttributeType.Numeric: {
                input_elt = m("input", {
                    "type": "range",
                    value: curval,
                    oninput: oninput
                });
                break;
            }
        }
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

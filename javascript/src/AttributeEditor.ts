import { EditorState } from './EditorState';
import { AttributeType } from './Schema';
import m from 'mithril';
import { map } from '@thi.ng/transducers';

interface WidgetAttrs {
    attribute_type: AttributeType,
    oninput: Function,
    curval: string,
    label: string
}

export const AttributeWidget: m.Component<WidgetAttrs> = {
    view({ attrs: { attribute_type, oninput, curval, label } }) {
        var input_type: string;
        switch (attribute_type) {
            case AttributeType.Stringlike: {
                input_type = "text";
            }
            case AttributeType.Numeric: {
                input_type = "range";
            }
        }
        return m("label",
            `${label}: `,
            m("input", {
                "type": input_type,
                value: curval,
                oninput
            }));
    }
}

export const AttributeEditor: m.Component<{ state: EditorState }> = {
    view({ attrs: { state } }) {
        if (state.dialogue.selected != null) {
            const obj = state.ls.sg.getEntity(state.dialogue.selected)!;
            return m("div",
                ...map(([attrtype, a]) =>
                    m(AttributeWidget, {
                        attribute_type: attrtype,
                        oninput: (s: any) => {
                            obj.weights[a] = s.target.value;
                            state.save();
                        },
                        curval: obj.weights[a] || "",
                        label: a
                    }),
                    state.ls.sg.weightTypes(state.dialogue.selected)!))
        } else {
            return m("div");
        }
    }
}

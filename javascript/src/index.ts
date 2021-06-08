import { Editor } from './Editor';
import { EditorState } from './EditorState';
import { LocatedSemagram } from './LocatedSemagram';
import m from "mithril";
import { Schema } from './Schema';


export function main(schema: Schema, divid: string, sendToJl: Function) {
    const dom = document.getElementById(divid)!;
    const state = new EditorState(new LocatedSemagram(schema), sendToJl);

    const App = {
        view() {
            return m(Editor, { state });
        }
    };

    m.mount(dom, App)
}

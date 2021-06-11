import { Editor } from './Editor';
import { EditorState } from './EditorState';
import { LocatedSemagram } from './LocatedSemagram';
import m from "mithril";
import { Schema } from './Schema';


/**
 * The main entrypoint into Semagrams. This makes a new Semagram based on
 * `schema`, a new Editor based on that Semagram that will send its state on save
 * using `sendToJl`, then mounts it to the div with id `divid`
 */
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

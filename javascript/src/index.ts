/**
 * @module
 */
import { Editor } from './Editor';
import { EditorState } from './EditorState';
import { ExportedLocatedSemagram, LocatedSemagram } from './LocatedSemagram';
import m from "mithril";

/**
 * The main entrypoint into Semagrams. This makes a new Semagram based on
 * `schema`, a new Editor based on that Semagram that will send its state on save
 * using `sendToJl`, then mounts it to the div with id `divid`
 */
export function main(init: ExportedLocatedSemagram, context: any,
                     sendToJl: Function, exportToJl: Function) {
    const state = new EditorState(LocatedSemagram.fromExported(init), sendToJl, exportToJl);
    context.state = state;

    const App = {
        view() {
            return m(Editor, { state });
        }
    };

    m.mount(context.element, App)
}

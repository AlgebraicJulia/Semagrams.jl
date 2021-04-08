import { Editor } from './Editor';
import { EditorState } from './EditorState';
import { LocatedWireViz } from './LocatedWireViz';
import m from "mithril";
import { WireVizSchema } from './WireVizSchema';


export function main(ws: WireVizSchema, divid: string, sendToJl: Function) {
    const dom = document.getElementById(divid)!;
    const state = new EditorState(new LocatedWireViz(ws), sendToJl);

    const App = {
        view() {
            return m(Editor, { state });
        }
    };

    m.mount(dom, App)
}

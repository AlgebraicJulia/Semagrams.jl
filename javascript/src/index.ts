import { DirectedPortGraphSchema, PetriSchema } from './WiresSchema'
import { Editor } from './Editor';
import { EditorState } from './EditorState';
import { LocatedWires } from './LocatedWires';
import m from "mithril";

const state = new EditorState(new LocatedWires(PetriSchema));

const App = {
    view() {
        return m(Editor, { state });
    }
}

m.mount(document.body, App)

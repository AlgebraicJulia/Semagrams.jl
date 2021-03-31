import { DirectedPortGraphSchema, PetriSchema } from './WiresSchema'
import { Editor } from './Editor';
import { start } from "@thi.ng/hdom";
import { EditorState } from './EditorState';
import { LocatedWires } from './LocatedWires';

const state = new EditorState(new LocatedWires(DirectedPortGraphSchema));

start([new Editor()], { root: document.body, ctx: { state } })

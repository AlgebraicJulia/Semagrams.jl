import { equiv } from "@thi.ng/equiv";
import { add2, dist2, round2, sub2, Vec2Like } from "@thi.ng/vectors";
import { LocatedSemagram } from "./LocatedSemagram";
import { Attachment } from "./Semagram";
import { AttachType, Schema } from "./Schema";
import * as CS from "./ColorScheme";

const GRIDSIZE = 60;
const SNAPSIZE = 15;

function snapToGrid(p: Vec2Like): Vec2Like {
    const nearest = round2([], p, [GRIDSIZE, GRIDSIZE]);
    if (dist2(p, nearest) < SNAPSIZE) {
        return nearest as Vec2Like;
    } else {
        return p;
    }
}

function compatiblePortTypes(schema: Schema, boxty: string): string[] {
    return Object.entries(schema.port_types)
        .filter(([_, portprops]) => portprops.box == boxty)
        .map(([portty, _]) => portty);
}

function compatibleWireTypes(schema: Schema,
    srcty: [AttachType, string],
    tgtty: [AttachType, string]): string[] {
    return Object.entries(schema.wire_types)
        .filter(([_, wireprops]) => equiv(wireprops.src, srcty) && equiv(wireprops.tgt, tgtty))
        .map(([wirety, _]) => wirety);
}

class InputConfig {
    color: string | undefined

    constructor() {
        this.color = undefined;
    }
}

export enum ModalState {
    Normal,
    SelectBox,
    SelectPort,
    SelectWire
}

interface ModalNormal {
    ty: ModalState.Normal
}

interface ModalSelectBox {
    ty: ModalState.SelectBox
    choices: string[]
}

interface ModalSelectPort {
    ty: ModalState.SelectPort
    choices: string[]
    box_idx: number
}

interface ModalSelectWire {
    ty: ModalState.SelectWire
    choices: string[]
}

type Modal = ModalNormal | ModalSelectBox | ModalSelectPort | ModalSelectWire;

/*
 * Here are the main functions of EditorState
 * - What is currently *clicked* (and where it was originally clicked)
 * - Where the cursor is
 * - Current settings
 *   - What color should new things be?
 * - State of modal, for selecting the type of new ports/boxes/wires
 * - What the cursor is hovering over
 * - What has been selected as src/tgt
 * - What the instantiated UI element is
 * - What the state of the semagram is
 *
 * We can break this down a bit
 * - State of the Semagram
 * - State of the cursor
 * - State of commands that require several pieces of input
 */

/**
 * The state of the cursor, and how it relates to the Semagram
 * All coordinates are relative to the SVG, computed using eventCoordsSVG
 */
export class CursorState {
    /** State */
    cursor: Vec2Like
    dragState: { box_idx: number, offset: Vec2Like } | null
    hoveredAttachment: Attachment | null
    svgelt: SVGSVGElement | null

    constructor(
        private setBoxLoc: (box_idx: number, loc: Vec2Like) => void,
        private getBoxLoc: (box_idx: number) => Vec2Like,
        private mousedown: (a: Attachment) => void,
        private mouseup: (a: Attachment) => void
    ) {
        this.cursor = [0, 0];
        this.dragState = null;
        this.hoveredAttachment = null;
        this.svgelt = null;
    }

    eventCoordsSVG(e: MouseEvent): Vec2Like {
        if (this.svgelt != null) {
            const pt = this.svgelt.createSVGPoint();
            pt.x = e.clientX;
            pt.y = e.clientY;

            const svgP = pt.matrixTransform((this.svgelt as any).getScreenCTM().inverse());

            return [svgP.x, svgP.y];
        } else {
            return [0, 0];
        }
    }

    handlemousemove = (e: MouseEvent) => {
        const p = this.eventCoordsSVG(e);
        this.cursor = p;
        if (this.dragState != null) {
            const { box_idx, offset } = this.dragState;
            this.setBoxLoc(box_idx, snapToGrid(add2([], p, offset) as Vec2Like));
        } else {
            (e as any).redraw = false;
        }
    }

    handlemouseenterattachment = (e: MouseEvent) => {
        this.hoveredAttachment = eventDataProperty(e, "data-a");
    }

    handlemouseoutattachment = () => {
        this.hoveredAttachment = null;
    }

    handlemousedownbox = (e: MouseEvent) => {
        const a = eventDataProperty(e, "data-a") as Attachment;
        const box_idx = a.box_idx;
        const loc = this.getBoxLoc(box_idx);
        const offset = sub2([], loc, this.eventCoordsSVG(e)) as Vec2Like;
        this.dragState = { box_idx, offset };
        this.mousedown(a);
    }

    handlemouseupbox = (e: MouseEvent) => {
        const a = eventDataProperty(e, "data-a") as Attachment;
        this.mouseup(a);
    }
}

export class DialogueState {
    src: Attachment | null
    tgt: Attachment | null
    modal: Modal
    selected: Attachment | null
    inputconfig: InputConfig

    constructor() {
        this.src = null;
        this.tgt = null;
        this.modal = { ty: ModalState.Normal };
        this.selected = null;
        this.inputconfig = new InputConfig();
    }
}

export class EditorState {
    cursor: CursorState
    dialogue: DialogueState
    ls: LocatedSemagram
    sendToJl: Function
    svgelt: SVGSVGElement | null

    constructor(sema: LocatedSemagram, sendToJl: Function) {
        this.ls = sema;
        this.dialogue = new DialogueState();
        this.cursor = new CursorState(
            this.ls.setBoxLoc,
            this.ls.getBoxLoc,
            (_a) => { },
            (_a) => { }
        );
        this.sendToJl = sendToJl;
        this.svgelt = null;
    }

    boxes() {
        return this.ls.sg.boxes.keys();
    }

    wires() {
        return this.ls.sg.wires.keys();
    }

    setSrc() {
        this.dialogue.src = this.cursor.hoveredAttachment;
    }

    setTgt() {
        this.dialogue.tgt = this.cursor.hoveredAttachment;
    }

    addBox() {
        const box_types = [...Object.keys(this.ls.sg.schema.box_types)];
        this.dialogue.modal = {
            ty: ModalState.SelectBox,
            choices: box_types
        };
    }

    addPort() {
        const a = this.cursor.hoveredAttachment;
        if (a != null) {
            const box_idx = a.box_idx;
            const box = this.ls.sg.boxes.get(box_idx)!;
            const port_types = compatiblePortTypes(this.ls.sg.schema, box.ty);
            if (port_types.length == 1) {
                this.ls.addPort(
                    port_types[0],
                    a.box_idx,
                    this.dialogue.inputconfig.color
                );
            } else if (port_types.length == 0) {
                // do nothing
            } else {
                this.dialogue.modal = {
                    ty: ModalState.SelectPort,
                    choices: port_types,
                    box_idx: box_idx
                };
            }
        }
    }

    remAttachment() {
        if (this.cursor.hoveredAttachment != null) {
            this.ls.remAttachment(this.cursor.hoveredAttachment);
        }
    }

    addWire() {
        const s = this.dialogue.src;
        const t = this.dialogue.tgt;
        if ((s != undefined) && (t != undefined)) {
            const wiretypeoptions = compatibleWireTypes(
                this.ls.sg.schema,
                this.ls.sg.attachmentType(s),
                this.ls.sg.attachmentType(t)
            );
            if (wiretypeoptions.length == 1) {
                this.ls.addWire(wiretypeoptions[0], s, t);
                this.dialogue.src = null;
                this.dialogue.tgt = null;
            } else if (wiretypeoptions.length == 0) {
                this.dialogue.src = null;
                this.dialogue.tgt = null;
            } else {
                this.dialogue.modal = {
                    ty: ModalState.SelectWire,
                    choices: wiretypeoptions
                };
            }
        }
    }

    debug() {
        console.log(this.ls);
    }

    save() {
        this.sendToJl(this.ls.sg.export());
    }

    modalInput(c: string) {
        const choice = parseInt(c);
        if (this.dialogue.modal.ty == ModalState.Normal) {
            return;
        }
        if (choice != NaN && 1 <= choice && choice <= this.dialogue.modal.choices.length) {
            switch (this.dialogue.modal.ty) {
                case ModalState.SelectPort: {
                    this.ls.addPort(this.dialogue.modal.choices[choice - 1],
                        this.dialogue.modal.box_idx,
                        this.dialogue.inputconfig.color);
                    break;
                }
                case ModalState.SelectWire: {
                    this.ls.addWire(this.dialogue.modal.choices[choice - 1],
                        this.dialogue.src!,
                        this.dialogue.tgt!)
                    this.dialogue.src = null;
                    this.dialogue.tgt = null;
                    break;
                }
                case ModalState.SelectBox: {
                    this.ls.addBox(this.dialogue.modal.choices[choice - 1],
                        this.dialogue.inputconfig.color,
                        this.cursor.cursor)
                    break;
                }
            }
        } else if (c == "Escape") {
            // Do nothing, just exit
        } else {
            return; // skip the reset
        }
        this.dialogue.modal = { ty: ModalState.Normal };
    }

    handlekeydown = (e: KeyboardEvent) => {
        if (this.dialogue.modal.ty == ModalState.Normal) {
            switch (e.key) {
                case "s": {
                    this.setSrc();
                    break;
                }
                case "t": {
                    this.setTgt();
                    break;
                }
                case "b": {
                    this.addBox();
                    break;
                }
                case "p": {
                    this.addPort();
                    break;
                }
                case "d": {
                    this.remAttachment();
                    break;
                }
                case "w": {
                    this.addWire();
                    break;
                }
                case "D": {
                    this.debug();
                    break;
                }
                case "S": {
                    this.save();
                    break;
                }
                case "Escape": {
                    this.dialogue.src = null;
                    this.dialogue.tgt = null;
                    this.dialogue.selected = null;
                    break;
                }
            }
        } else {
            this.modalInput(e.key);
        }
    }
}

export interface EditorContext {
    state: EditorState
}

function eventDataProperty(e: Event, prop: string): any {
    return JSON.parse((e.target as HTMLElement).getAttribute(prop)!);
}


export function colorAttachment(state: EditorState, a: Attachment): string {
    if (equiv(state.dialogue.src, a)) {
        return CS.source;
    } else if (equiv(state.dialogue.tgt, a)) {
        return CS.target;
    } else if (equiv(state.cursor.hoveredAttachment, a)) {
        return CS.over;
    } else {
        return CS.base;
    }
}

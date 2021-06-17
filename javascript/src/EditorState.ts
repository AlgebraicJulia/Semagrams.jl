import { equiv } from "@thi.ng/equiv";
import { add2, dist2, round2, sub2, Vec2Like } from "@thi.ng/vectors";
import { LocatedSemagram } from "./LocatedSemagram";
import { Attachment, Entity, wire_entity } from "./Semagram";
import { AttachType, EntityType, Schema } from "./Schema";
import { map, concat } from "@thi.ng/transducers";
import { Command, DEFAULT_KEYBINDINGS } from "./Commands";
import * as CS from "./ColorScheme";

/**
 * TODO: make these runtime-configurable.
 */
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

/** What types of port can go on this box? */
function compatiblePortTypes(schema: Schema, boxty: string): string[] {
    return Object.entries(schema.port_types)
        .filter(([_, portprops]) => portprops.box == boxty)
        .map(([portty, _]) => portty);
}

/** What types of wire can go between these two attachments? */
function compatibleWireTypes(schema: Schema,
    srcty: [AttachType, string],
    tgtty: [AttachType, string]): string[] {
    return Object.entries(schema.wire_types)
        .filter(([_, wireprops]) => equiv(wireprops.src, srcty) && equiv(wireprops.tgt, tgtty))
        .map(([wirety, _]) => wirety);
}

/**
 * This configures which default color is used when adding new ports/wires.
 * Note, I believe that this is currently unused, and leftover from an earlier version.
 * TODO: get colors working again, using colorAttribute.
 */
class InputConfig {
    color: string | undefined

    constructor() {
        this.color = undefined;
    }
}

/**
 * Sigh... I really wish there were a better way of doing sum types in Typescript.
 * Currently, this represents the state of the modal that selects which type of
 * box/port/wire to add.
 * TODO: Modal should be refactored to just be a generic way of selecting from
 * a list of options.
 */
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


/**
 * The state of the cursor, and how it relates to the Semagram.
 * Things we want to keep track of
 * - The current position of the cursor (`cursor`)
 * - Whether the cursor is dragging something currently, and if so what it is dragging (`dragState`)
 * - What the cursor is hovering over (`hoveredAttachment`)
 * - The SVG element that the semagrams UI is rendered in. This is used to convert
 * between the coordinates that we get from events, and coordinates that we need to
 * put into things in the SVG. All stored coordinates are relative to the SVG, computed using eventCoordsSVG which uses that SVG element. (`svgelt`)
 *
 * CursorState has several methods which are placed as callbacks on various UI nodes to allow us
 * to keep track of all of this information.
 *
 * CursorState also gets passed in several callbacks, which it calls when appropriate
 * to interact with the rest of the system (`setBoxLoc`, `getBoxLoc`, `mousedown`, `mouseup`)
 *
 * Note: if anyone has ideas for refactoring this to be less stateful, I'd love to hear them.
 */
export class CursorState {
    /** State */
    cursor: Vec2Like
    dragState: { box_idx: number, offset: Vec2Like } | null
    hoveredEntity: Entity | null
    svgelt: SVGSVGElement | null

    constructor(
        private setBoxLoc: (box_idx: number, loc: Vec2Like) => void,
        private getBoxLoc: (box_idx: number) => Vec2Like,
        private mousedown: (a: Attachment) => void,
        private mouseup: (a: Attachment) => void
    ) {
        this.cursor = [0, 0];
        this.dragState = null;
        this.hoveredEntity = null;
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
        this.hoveredEntity = eventDataProperty(e, "data-a");
    }

    handlemouseoutattachment = () => {
        this.hoveredEntity = null;
    }

    handlemouseenterwire = (e: MouseEvent) => {
        this.hoveredEntity = wire_entity(eventDataProperty(e, "data-w"));
    }

    handlemouseoutwire = () => {
        this.hoveredEntity = null;
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
        this.dragState = null;
        this.mouseup(a);
    }
}

/**
 * This contains all of the "logical" state of the editor.
 */
export class DialogueState {
    /** What attachment will be the src of a newly created wire */
    src: Attachment | null

    /** What attachment will be the tgt of a newly created wire */
    tgt: Attachment | null

    /* The current modal state */
    modal: Modal

    /**
     * What attachment is currently "selected", unused currently, but will
     * be used for stuff like setting weights.
     */
    selected: Entity | null

    /**
     * Any other settings having to do with defaults for new boxes/ports/wires
     */
    inputconfig: InputConfig

    /**
     * Whether or not the help window is showing
     * TODO: This should drill down into the detailed descriptions
     */
    helpwindow: boolean

    constructor() {
        this.src = null;
        this.tgt = null;
        this.modal = { ty: ModalState.Normal };
        this.selected = null;
        this.inputconfig = new InputConfig();
        this.helpwindow = false;
    }
}

/**
 * This contains all of the state of the editor.
 * It also has the main entrypoint to interacting with the editor,
 * which is through keypresses, which are handled in "handlekeydown".
 * These keypresses do different things depending on the current values of
 * `cursor` and `dialogue`. For instance, if you press 's', then the
 * current `hoveredAttachment` in `cursor` becomes the `src` in `dialogue`.
 *
 * When you press 'w', the current `src` and `tgt` in `dialogue` are used
 * as the source and target of the newly created wire, which is added to `ls`,
 * the LocatedSemagram that is currently being edited.
 *
 * TODO: Maybe `DialogueState` should be renamed to `LogicalState`?
 *
 * Finally, the editor has a function `sendToJl`, which it calls when
 * you save the diagram. This function should send the diagram down a
 * websocket to Julia, and the function should be passed in when creating
 * an EditorState.
 */
export class EditorState {
    cursor: CursorState
    dialogue: DialogueState
    ls: LocatedSemagram
    sendToJl: Function

    constructor(sema: LocatedSemagram, sendToJl: Function) {
        this.ls = sema;
        this.dialogue = new DialogueState();
        this.cursor = new CursorState(
            (box_idx, loc) => this.ls.setBoxLoc(box_idx, loc),
            (box_idx) => this.ls.getBoxLoc(box_idx),
            (a) => { this.dialogue.selected = a; }, /* This should set selected... */
            (_a) => { }
        );
        this.sendToJl = sendToJl;
    }

    /** Convenience function: an array of the indexes of all the boxes that currently exist. */
    boxes() {
        return this.ls.sg.boxes.keys();
    }

    /** Convenience function: an array of the indexes of all the wires that currently exist. */
    wires() {
        return this.ls.sg.wires.keys();
    }

    /** Convenience function: an array of the indices of all the ports for a box */
    ports_by_box(box_idx: number): IterableIterator<number> {
        const box = this.ls.sg.boxes.get(box_idx)!;
        return box.ports.keys();
    }

    ports(): IterableIterator<{ box_idx: number, port_idx: number }> {
        return concat(
            ...map(box_idx =>
                map(port_idx => { return { box_idx, port_idx }; }, this.ports_by_box(box_idx)),
                this.boxes()))
    }

    /**
     * The next functions are all of the "actions" that one can do,
     * and are bound to key presses.
     */

    /** Set `dialogue.src` to `cursor.hoveredAttachment` */
    setSrc() {
        if (this.cursor.hoveredEntity) {
            if (this.cursor.hoveredEntity.ty == EntityType.Box
                || this.cursor.hoveredEntity.ty == EntityType.Port) {
                this.dialogue.src = this.cursor.hoveredEntity;
            }
        }
    }

    /** Set `dialogue.tgt` to `cursor.hoveredAttachment` */
    setTgt() {
        if (this.cursor.hoveredEntity) {
            if (this.cursor.hoveredEntity.ty == EntityType.Box
                || this.cursor.hoveredEntity.ty == EntityType.Port) {
                this.dialogue.tgt = this.cursor.hoveredEntity;
            }
        }
    }

    /**
     * Add a new box at the `cursor.cursor` location.
     * If there's more than one type of box that could be added,
     * go into a modal state.
     */
    addBox() {
        const box_types = [...Object.keys(this.ls.sg.schema.box_types)];
        this.dialogue.modal = {
            ty: ModalState.SelectBox,
            choices: box_types
        };
    }

    /** Add a new port to the `cursor.hoveredAttachment` box, assuming it's a box */
    addPort() {
        const a = this.cursor.hoveredEntity;
        if (a != null && a.ty == EntityType.Box) {
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

    /** Remove `cursor.hoveredAttachment` or `cursor.hoveredWire` */
    remHovered() {
        if (this.cursor.hoveredEntity != null) {
            this.ls.remEntity(this.cursor.hoveredEntity);
        }
    }

    /** Add a new wire between `dialogue.src` and `dialogue.tgt` */
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

    /** Print out the current `ls` to the console */
    debug() {
        console.log(this.ls);
    }

    /** Export the current `ls`, and send it down the wire! */
    save() {
        this.sendToJl(this.ls.sg.export());
    }

    /**
     * Handle a keypress when we are in a modal state, possibly completing the modal action.
     */
    modalInput(c: string) {
        const choice = parseInt(c);
        if (this.dialogue.modal.ty == ModalState.Normal) {
            return;
        }
        if (!isNaN(choice) && 1 <= choice && choice <= this.dialogue.modal.choices.length) {
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

    /**
     * This is the keypress handler.
     */
    handlekeydown = (e: KeyboardEvent) => {
        if (this.dialogue.modal.ty == ModalState.Normal) {
            const cmd = DEFAULT_KEYBINDINGS.get(e.key);
            if (cmd != undefined) {
                switch (cmd) {
                    case Command.SetSrc: {
                        this.setSrc();
                        break;
                    }
                    case Command.SetTgt: {
                        this.setTgt();
                        break;
                    }
                    case Command.AddBox: {
                        this.addBox();
                        break;
                    }
                    case Command.AddPort: {
                        this.addPort();
                        break;
                    }
                    case Command.AddWire: {
                        this.addWire();
                        break;
                    }
                    case Command.RemHovered: {
                        this.remHovered();
                        break;
                    }
                    case Command.Debug: {
                        this.debug();
                        break;
                    }
                    case Command.Deselect: {
                        this.dialogue.src = null;
                        this.dialogue.tgt = null;
                        this.dialogue.selected = null;
                        break;
                    }
                    case Command.Help: {
                        this.dialogue.helpwindow = !this.dialogue.helpwindow;
                        break;
                    }
                }
            }
        } else {
            this.modalInput(e.key);
        }
        this.save();
    }
}

export interface EditorContext {
    state: EditorState
}

/**
 * We put data on DOM nodes, so that it can be accessed from events.
 * For instance, we put the current Attachment on the dom nodes for ports/boxes,
 * so that when a mouse event is triggered on them, we can know which port/box it
 * was triggered on.
 */
function eventDataProperty(e: Event, prop: string): any {
    return JSON.parse((e.target as HTMLElement).getAttribute(prop)!);
}

/**
 * What color should we color this attachment, to designate its current state.
 */
export function colorAttachment(state: EditorState, a: Attachment): string {
    if (equiv(state.dialogue.src, a)) {
        return CS.source;
    } else if (equiv(state.dialogue.tgt, a)) {
        return CS.target;
    } else if (equiv(state.cursor.hoveredEntity, a)) {
        return CS.over;
    } else {
        return CS.base;
    }
}

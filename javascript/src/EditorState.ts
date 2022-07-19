import { equiv } from "@thi.ng/equiv";
import { add2, dist2, round2, sub2, Vec2Like } from "@thi.ng/vectors";
import { ExportedLocatedSemagram, LocatedSemagram } from "./LocatedSemagram";
import { Attachment, box_entity, Entity, port_entity, wire_entity } from "./Semagram";
import { AttachType, EntityType, Schema } from "./Schema";
import { map, concat } from "@thi.ng/transducers";
import { Command, DEFAULT_KEYBINDINGS } from "./Commands";
import * as CS from "./ColorScheme";
import m from "mithril";
import { AffineTrans } from "./AffineTrans";
import { EditorPane } from "./EditorPane";
import { globalStyle, SvgDefs } from "./Editor";
import { SVG_HEIGHT } from "./Constants";

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

function compatibleHomTypes(schema: Schema,
    srcty: [AttachType, string],
    tgtty: [AttachType, string]): string[] {
    switch (srcty[0]) {
        case EntityType.Box: {
            return schema.box_types[srcty[1]].homs
                .filter((hom) => hom.codom == tgtty[1])
                .map((hom) => hom.name);
        }
        case EntityType.Port: {
            return schema.port_types[srcty[1]].homs
                .filter((hom) => hom.codom == tgtty[1])
                .map((hom) => hom.name);
        }
    }
}

/**
 * This configures which default color is used when adding new ports/wires.
 * Note, I believe that this is currently unused, and leftover from an earlier version.
 * TODO: get colors working again, using colorAttribute.
 * TODO: get sliders working again
 */
class InputConfig {
    color: string | undefined

    /* These set the min and the max value on sliders */
    minval: number
    maxval: number

    constructor() {
        this.color = undefined;
        this.minval = 0;
        this.maxval = 100;
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
    Select
}

interface ModalNormal {
    ty: ModalState.Normal
}

interface ModalSelect {
    ty: ModalState.Select
    choices: string[]
    continuation: (choice: string) => void
}

type Modal = ModalNormal | ModalSelect

enum DragStates {
    Box,
    Background,
    Empty
}

interface DragBox {
    ty: DragStates.Box
    box_idx: number
    offset: Vec2Like
}

interface DragBackground {
    ty: DragStates.Background
    offset: Vec2Like
}

interface NoDrag {
    ty: DragStates.Empty
}

type DragState = DragBox | DragBackground | NoDrag;


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
    dragState: DragState
    hoveredEntity: Entity | null
    svgelt: SVGSVGElement | null
    affineTrans: AffineTrans

    constructor(
        private setBoxLoc: (box_idx: number, loc: Vec2Like) => void,
        private getBoxLoc: (box_idx: number) => Vec2Like,
        private mousedown: (e: Entity) => void,
        private mouseup: (e: Entity) => void
    ) {
        this.cursor = [0, 0];
        this.dragState = { ty: DragStates.Empty };
        this.hoveredEntity = null;
        this.svgelt = null;
        this.affineTrans = new AffineTrans(1.5, [0, 0]);
    }

    eventCoordsSVG(e: MouseEvent): Vec2Like {
        if (this.svgelt != null) {
            const pt = this.svgelt.createSVGPoint();
            pt.x = e.clientX;
            pt.y = e.clientY;

            const svgP = pt.matrixTransform((this.svgelt as any).getScreenCTM().inverse());

            return this.affineTrans.apply_inv([svgP.x, svgP.y]);
        } else {
            return [0, 0];
        }
    }

    handlemousemove = (e: MouseEvent) => {
        const p = this.eventCoordsSVG(e);
        this.cursor = p;
        switch (this.dragState.ty) {
            case (DragStates.Box): {
                const { box_idx, offset } = this.dragState;
                this.setBoxLoc(box_idx, snapToGrid(add2([], p, offset) as Vec2Like));
                break;
            }
            case (DragStates.Background): {
                const { offset } = this.dragState;
                this.affineTrans.translate = this.affineTrans.apply(sub2([], p, offset) as Vec2Like);
                break;
            }
            case (DragStates.Empty): {
                (e as any).redraw = false;
                break;
            }
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
        this.dragState = { ty: DragStates.Box, box_idx, offset };
        this.mousedown(a);
    }

    handlemouseupbox = (e: MouseEvent) => {
        const a = eventDataProperty(e, "data-a") as Attachment;
        this.dragState = { ty: DragStates.Empty };
        this.mouseup(a);
    }

    handlemousedownport = (e: MouseEvent) => {
        this.mousedown(eventDataProperty(e, "data-a") as Attachment);
    }

    handlemouseupport = (e: MouseEvent) => {
        this.mouseup(eventDataProperty(e, "data-a") as Attachment);
    }

    handlemousedownwire = (e: MouseEvent) => {
        this.mousedown(wire_entity(eventDataProperty(e, "data-w")));
    }

    handlemouseupwire = (e: MouseEvent) => {
        this.mouseup(wire_entity(eventDataProperty(e, "data-w")));
    }

    handlemousedownpan = (e: MouseEvent) => {
        this.dragState = { ty: DragStates.Background, offset: this.eventCoordsSVG(e) };
    }

    handlemouseuppan = (_: MouseEvent) => {
        this.dragState = { ty: DragStates.Empty };
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
     * What entity is currently "selected", used for stuff like setting weights.
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
    cursor!: CursorState
    dialogue!: DialogueState
    ls: LocatedSemagram
    sendToJl: Function
    exportToJl: Function
    exported: boolean
    gridid: string

    constructor(sema: LocatedSemagram, sendToJl: Function, exportToJl: Function) {
        this.ls = sema;
        this.sendToJl = sendToJl;
        this.exportToJl = exportToJl;
        this.exported = false;
        this.gridid = Math.random().toString(16).substr(2, 8);
        this.reset();
    }

    exportSVG() {
        const svg = document.createElement("div");
        const pane = m(
            "svg", { width: "95%", height: `${SVG_HEIGHT}px` },
            m("style", m.trust(globalStyle)),
            m(SvgDefs, { state: this }),
            m("g",
                {
                    transform: this.cursor.affineTrans.svgExport()
                },
                m(EditorPane, { state: this, isExport: true }),
            ));
        m.render(svg, pane);
        const serializer = new XMLSerializer();
        const source = serializer.serializeToString(svg.children[0]);
        return source;
    }

    runExport() {
        this.exportToJl(this.exportSVG());
        this.exported = true;
    }

    reset() {
        var svgelt: SVGSVGElement | null = null;
        if (this.cursor != undefined) {
            svgelt = this.cursor.svgelt;
        }
        this.dialogue = new DialogueState();
        this.cursor = new CursorState(
            (box_idx, loc) => {
                this.exported = false;
                return this.ls.setBoxLoc(box_idx, loc);
            },
            (box_idx) => {
                this.exported = false;
                return this.ls.getBoxLoc(box_idx);
            },
            (a) => { this.dialogue.selected = a; }, /* This should set selected... */
            (_a) => { }
        );
        this.cursor.svgelt = svgelt;
    }

    setZoom(z: number) {
        this.cursor.affineTrans.zoom = z;
        m.redraw();
    }

    resetWith(e: ExportedLocatedSemagram) {
        this.ls = LocatedSemagram.fromExported(e);
        this.reset();
        this.save();
        m.redraw();
    }

    /** Convenience function: an array of the indexes of all the boxes that currently exist. */
    boxes() {
        return this.ls.sg.boxes.keys();
    }

    /** Convenience function: an array of the indexes of all the wires that currently exist. */
    wires() {
        return this.ls.sg.wires.keys();
    }

    homs() {
        const homs: Array<[Entity, Entity]> = [];
        for (const [box_idx, box] of this.ls.sg.boxes.entries()) {
            const src = box_entity(box_idx);
            for (const tgt of Object.values(box.homs)) {
                homs.push([src, tgt]);
            }
            for (const [port_idx, port] of box.ports.entries()) {
                const src = port_entity(box_idx, port_idx);
                for (const tgt of Object.values(port.homs)) {
                    homs.push([src, tgt]);
                }
            }
        }
        return homs;
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
        if (box_types.length == 1) {
            this.ls.addBox(box_types[0], this.cursor.cursor);
        } else if (box_types.length == 0) {
            // do nothing
        } else {
            this.dialogue.modal = {
                ty: ModalState.Select,
                choices: box_types,
                continuation: (choice: string) => {
                    this.ls.addBox(choice, this.cursor.cursor);
                }
            };
        }
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
                );
            } else if (port_types.length == 0) {
                // do nothing
            } else {
                this.dialogue.modal = {
                    ty: ModalState.Select,
                    choices: port_types,
                    continuation: (choice: string) => {
                        this.ls.addPort(choice, box_idx);
                    }
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
                    ty: ModalState.Select,
                    choices: wiretypeoptions,
                    continuation: (choice: string) => {
                        this.ls.addWire(choice, s, t);
                        this.dialogue.src = null;
                        this.dialogue.tgt = null;
                    }
                };
            }
        }
    }

    /** Set the homomorphism on src to point to tgt */
    setHom() {
        const s = this.dialogue.src;
        const t = this.dialogue.tgt;
        if ((s != undefined) && (t != undefined)) {
            const homtypeoptions = compatibleHomTypes(
                this.ls.sg.schema,
                this.ls.sg.attachmentType(s),
                this.ls.sg.attachmentType(t)
            );
            if (homtypeoptions.length == 1) {
                this.ls.setHom(homtypeoptions[0], s, t);
                this.dialogue.src = null;
                this.dialogue.tgt = null;
            } else if (homtypeoptions.length == 0) {
                this.dialogue.src = null;
                this.dialogue.tgt = null;
            } else {
                this.dialogue.modal = {
                    ty: ModalState.Select,
                    choices: homtypeoptions,
                    continuation: (choice: string) => {
                        this.ls.setHom(choice, s, t);
                        this.dialogue.src = null;
                        this.dialogue.tgt = null;
                    }
                };
            }
        }
    }
    debug() {
        console.log(this.ls);
    }

    /** Export the current `ls`, and send it down the wire! */
    save() {
        console.log(JSON.stringify(this.ls.export()))
        this.sendToJl(this.ls.export());
    }

    roundtrip() {
        this.ls = LocatedSemagram.fromExported(
            JSON.parse(
                JSON.stringify(this.ls.export())));
        this.reset();
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
            this.dialogue.modal.continuation(this.dialogue.modal.choices[choice - 1]);
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
        this.exported = false;
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
                    case Command.SetHom: {
                        this.setHom();
                        break;
                    }
                    case Command.Export: {
                        this.runExport();
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
                    case Command.Roundtrip: {
                        this.roundtrip();
                        break;
                    }
                    case Command.ZoomIn: {
                        this.cursor.affineTrans.zoomFrom(this.cursor.cursor, 1.1);
                        break;
                    }
                    case Command.ZoomOut: {
                        this.cursor.affineTrans.zoomFrom(this.cursor.cursor, 1 / 1.1);
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

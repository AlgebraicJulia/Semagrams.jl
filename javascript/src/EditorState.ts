import { equiv } from "@thi.ng/equiv";
import { add2, dist2, round2, sub2, Vec2Like } from "@thi.ng/vectors";
import { LocatedWireViz } from "./LocatedWireViz";
import { Attachment, box_attach } from "./WireViz";
import { AttachType, WireVizSchema } from "./WireVizSchema";

const GRIDSIZE = 100;
const SNAPSIZE = 20;

function snapToGrid(p: Vec2Like): Vec2Like {
    const nearest = round2([], p, [GRIDSIZE, GRIDSIZE]);
    if (dist2(p, nearest) < SNAPSIZE) {
        return nearest as Vec2Like;
    } else {
        return p;
    }
}

function compatiblePortTypes(schema: WireVizSchema, boxty: string): string[] {
    return Object.entries(schema.port_types)
        .filter(([_, portprops]) => portprops.box == boxty)
        .map(([portty, _]) => portty);
}

function compatibleWireTypes(schema: WireVizSchema,
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

export class EditorState {
    clicked: number | null
    clickedOffset: Vec2Like
    cursor: Vec2Like
    inputconfig: InputConfig
    lw: LocatedWireViz
    modal: Modal
    overAttachment: Attachment | null
    selected: number | null
    sendToJl: Function
    src: Attachment | null
    svgelt: SVGSVGElement | null
    tgt: Attachment | null

    constructor(initWires: LocatedWireViz, sendToJl: Function) {
        this.clicked = null;
        this.clickedOffset = [0, 0];
        this.cursor = [0, 0];
        this.inputconfig = new InputConfig();
        this.lw = initWires;
        this.modal = { ty: ModalState.Normal };
        this.overAttachment = null;
        this.selected = null;
        this.sendToJl = sendToJl;
        this.src = null;
        this.svgelt = null;
        this.tgt = null;
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
        if (this.clicked != null) {
            this.lw.setLoc(box_attach(this.clicked),
                snapToGrid(add2([], p, this.clickedOffset) as Vec2Like));
        } else {
            (e as any).redraw = false;
        }
    }

    handlekeydown = (e: KeyboardEvent) => {
        if (this.modal.ty == ModalState.Normal) {
            switch (e.key) {
                case "s": {
                    this.src = this.overAttachment;
                    break;
                }
                case "t": {
                    this.tgt = this.overAttachment;
                    break;
                }
                case "b": {
                    const box_types = [...Object.keys(this.lw.wireviz.schema.box_types)];
                    this.modal = {
                        ty: ModalState.SelectBox,
                        choices: box_types
                    };
                    break;
                }
                case "p": {
                    if (this.overAttachment != null) {
                        const box_idx = this.overAttachment.box_idx;
                        const box = this.lw.wireviz.boxes.get(box_idx)!;
                        const porttypeoptions = compatiblePortTypes(this.lw.wireviz.schema, box.ty);
                        if (porttypeoptions.length == 1) {
                            this.lw.addPort(
                                porttypeoptions[0],
                                this.overAttachment.box_idx,
                                this.inputconfig.color
                            );
                        } else if (porttypeoptions.length == 0) {
                            // do nothing
                        } else {
                            this.modal = {
                                ty: ModalState.SelectPort,
                                choices: porttypeoptions,
                                box_idx: box_idx
                            };
                        }
                    }
                    break;
                }
                case "d": {
                    if (this.overAttachment != null) {
                        this.lw.remAttachment(this.overAttachment);
                    }
                    break;
                }
                case "w": {
                    const s = this.src;
                    const t = this.tgt;
                    if ((s != undefined) && (t != undefined)) {
                        const wiretypeoptions = compatibleWireTypes(
                            this.lw.wireviz.schema,
                            this.lw.wireviz.attachmentType(s),
                            this.lw.wireviz.attachmentType(t)
                        );
                        if (wiretypeoptions.length == 1) {
                            this.lw.addWire(wiretypeoptions[0], s, t);
                            this.src = null;
                            this.tgt = null;
                        } else if (wiretypeoptions.length == 0) {
                            this.src = null;
                            this.tgt = null;
                        } else {
                            this.modal = {
                                ty: ModalState.SelectWire,
                                choices: wiretypeoptions
                            };
                        }
                    }
                    break;
                }
                case "D": {
                    console.log(this.lw);
                    break;
                }
                case "P": {
                    console.log(this.lw.wireviz);
                    break;
                }
                case "S": {
                    this.sendToJl(this.lw.wireviz.export());
                    break;
                }
                case "Escape": {
                    this.src = null;
                    this.tgt = null;
                    this.selected = null;
                    break;
                }
            }
        } else {
            const choice = parseInt(e.key);
            if (choice != NaN && 1 <= choice && choice <= this.modal.choices.length) {
                switch (this.modal.ty) {
                    case ModalState.SelectPort: {
                        this.lw.addPort(this.modal.choices[choice - 1],
                            this.modal.box_idx,
                            this.inputconfig.color);
                        break;
                    }
                    case ModalState.SelectWire: {
                        this.lw.addWire(this.modal.choices[choice - 1],
                            this.src!,
                            this.tgt!)
                        this.src = null;
                        this.tgt = null;
                        break;
                    }
                    case ModalState.SelectBox: {
                        this.lw.addBox(this.modal.choices[choice - 1],
                            this.inputconfig.color,
                            this.cursor)
                        break;
                    }
                }
            } else if (e.key == "Escape") {
                // Do nothing, just exit
            } else {
                return; // skip the reset
            }
            this.modal = { ty: ModalState.Normal };
        }
    }

    handlemouseenterattachment = (e: MouseEvent) => {
        this.overAttachment = eventDataProperty(e, "data-a");
    }

    handlemouseoutattachment = () => {
        this.overAttachment = null;
    }

    handlemousedownbox = (e: MouseEvent) => {
        const a = eventDataProperty(e, "data-a") as Attachment;
        this.clicked = a.box_idx;
        this.selected = a.box_idx;
        const loc = this.lw.getLoc(a)!;
        this.clickedOffset = sub2([], loc, this.eventCoordsSVG(e)) as Vec2Like;
    }

    handlemouseupbox = () => {
        this.clicked = null;
        this.clickedOffset = [0, 0];
    }
}

export interface EditorContext {
    state: EditorState
}

function eventDataProperty(e: Event, prop: string): any {
    return JSON.parse((e.target as HTMLElement).getAttribute(prop)!);
}


export function colorAttachment(state: EditorState, a: Attachment): string {
    if (equiv(state.src, a)) {
        return "lightblue";
    } else if (equiv(state.tgt, a)) {
        return "pink";
    } else if (equiv(state.overAttachment, a)
        || (a.ty == AttachType.Box && a.box_idx == state.clicked)) {
        return "lightgrey";
    } else {
        return "white";
    }
}

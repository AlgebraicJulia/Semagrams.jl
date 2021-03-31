import { Wires, Attachment, Box, Point, AttachType, Edge } from "./Wired";
import m from "mithril";

const BOXRADIUS = 40;
const EDGERADIUS = 20;
const PORTRADIUS = 7;

function makemarker(id: string, color: string) {
    return m("marker",
        {
            id: id,
            viewBox: "0 0 10 10",
            refX: "5",
            refY: "5",
            markerWidth: "12",
            markerHeight: "12",
            orient: "auto",
        },
        m("path",
            {
                style: "stroke-width:1px",
                fill: color,
                stroke: "currentcolor", d: "M 0 0 L 10 5 L 0 10 z"
            }))
}

const svgdefs = m("defs", makemarker("arrow-sel", "lightgrey"), makemarker("arrow", "white"))

interface EditorAttrs {
    state: EditorState
}

class InputConfig {
    boxty: string
    portty: string
    edgety: string
    color: string | undefined

    constructor() {
        this.boxty = 'box';
        this.portty = 'input';
        this.edgety = 'edge';
        this.color = undefined;
    }
}

export class EditorState {
    wired: Wires
    offset: Point
    cursor: Point
    inputconfig: InputConfig
    clicked?: number
    cursorOver?: Attachment
    cursorOverEdge?: number
    src?: Attachment
    tgt?: Attachment
    sendToJl: Function

    handlemousemove = (e: any) => {
        const i = this.clicked;
        const p = new Point(e.pageX - this.offset.x, e.pageY - this.offset.y);
        this.cursor = p;
        if (i != undefined) {
            this.wired.boxes[i].loc = p;
        } else {
            e.redraw = false;
        }
    }

    handlemousedown = (e: MouseEvent) => {
        const a = (e.target as Element).getAttribute("data-a");
        if (a) {
            this.clicked = Attachment.fromJson(JSON.parse(a)).box_idx;
        }
    }

    handlemouseup = (_e: MouseEvent) => {
        this.clicked = undefined;
    }

    handlemouseover = (e: MouseEvent) => {
        const a = (e.target as Element).getAttribute("data-a");
        if (a) {
            this.cursorOver = Attachment.fromJson(JSON.parse(a));
        }
    }

    handlemouseout = (_e: MouseEvent) => {
        this.cursorOver = undefined;
    }

    handlemouseoveredge = (e: MouseEvent) => {
        const i = (e.target as Element).getAttribute("data-i");
        if (i) {
            this.cursorOverEdge = parseInt(i);
        }
    }

    handlemouseoutedge = (_e: MouseEvent) => {
        this.cursorOverEdge = undefined;
    }

    handlekeydown = (e: KeyboardEvent) => {
        switch (e.key) {
            case "a": {
                this.wired.addBox(this.inputconfig.boxty, this.cursor, this.inputconfig.color);
                break;
            }
            case "s": {
                if (this.cursorOver != undefined) {
                    this.src = this.cursorOver;
                }
                break;
            }
            case "t": {
                if (this.cursorOver != undefined) {
                    this.tgt = this.cursorOver;
                }
                break;
            }
            case "e": {
                const s = this.src;
                const t = this.tgt;
                if ((s != undefined) && (t != undefined)) {
                    this.wired.addEdge(this.inputconfig.edgety, s, t);
                    this.src = undefined;
                    this.tgt = undefined;
                }
                break;
            }
            case "i": {
                this.inputconfig.portty = 'input';
                break;
            }
            case "o": {
                this.inputconfig.portty = 'output';
                break;
            }
            case "d": {
                if (this.cursorOver != undefined) {
                    this.wired.remAttachment(this.cursorOver);
                    this.cursorOver = undefined;
                } else if (this.cursorOverEdge != undefined) {
                    this.wired.remEdge(this.cursorOverEdge);
                    this.cursorOverEdge = undefined;
                }
                break;
            }
            case "p": {
                if (this.cursorOver != undefined && this.cursorOver.ty == AttachType.Box) {
                    this.wired.addPort(this.inputconfig.portty,
                        this.cursorOver.box, this.inputconfig.color);
                }
                break;
            }
            case "E": {
                const svgstr = (e.target as HTMLElement).outerHTML;
                const element = document.createElement('a');
                element.setAttribute('href', `data:image/svg+xml;charset=utf-8,${encodeURIComponent(svgstr)}`)
                element.setAttribute('download', 'wired.svg')

                element.style.display = 'none';
                document.body.appendChild(element);
                element.click();
                document.body.removeChild(element);
                break;
            }
            case "P": {
                console.log(this);
                break;
            }
            case "W": {
                console.log(this.wired);
                break;
            }
            case "S": {
                this.sendToJl(this.wired);
                break;
            }
        }
        m.redraw()
    }

    handlecmd = (cmd: string) => {
        const setcmd = cmd.match(/set (\w+) (\w+)/);
        if (setcmd != null) {
            const opt = setcmd[1];
            if (opt in this.inputconfig) {
                (this.inputconfig as any)[opt] = setcmd[2] == 'undefined' ? undefined : setcmd[2];
            }
            return;
        }
        const sendcmd = cmd.match(/send/);
        console.log(sendcmd);
        if (sendcmd != null) {
            this.sendToJl(this.wired);
        }
    }

    handleinputkeydown = (e: KeyboardEvent) => {
        if (e.code == 'Enter') {
            const inputElt = e.target as HTMLInputElement;
            this.handlecmd(inputElt.value);
            inputElt.value = "";
        } else {
            (e as any).redraw = false;
        }
    }

    constructor(initWired: Wires, sendToJl: Function) {
        this.offset = new Point(0, 0);
        this.cursor = new Point(0, 0);
        this.wired = initWired;
        this.sendToJl = sendToJl;
        this.inputconfig = new InputConfig();
    }
}

class BoxAttrs {
    constructor(
        public box: Box,
        public i: number,
        public state: EditorState
    ) { }
}

const EditBox: m.Component<BoxAttrs> = {
    view({ attrs }: m.CVnode<BoxAttrs>) {
        const box = attrs.box;
        const i = attrs.i;
        const state = attrs.state;
        var fillcolor;
        const a = new Attachment(i, AttachType.Box);
        if (a.isEqual(state.src)) {
            fillcolor = 'red';
        } else if (a.isEqual(state.tgt)) {
            fillcolor = 'blue';
        } else if (a.isEqual(state.cursorOver)) {
            fillcolor = 'lightgrey';
        } else {
            fillcolor = 'white';
        }
        const strokecolor = (box.color != undefined) ? box.color : 'black';
        const p = box.loc;
        var mainbox;
        if (box.ty == "circle") {
            mainbox = m("circle", {
                cx: p.x, cy: p.y, r: BOXRADIUS,
                "data-a": JSON.stringify(a.toJson()),
                style: `fill:${fillcolor};stroke:${strokecolor}`,
                onmousedown: state.handlemousedown,
                onmouseover: state.handlemouseover,
                onmouseout: state.handlemouseout
            })
        } else {
            mainbox = m("polygon", {
                points: squarestr(p, BOXRADIUS),
                "data-a": JSON.stringify(a.toJson()),
                style: `fill:${fillcolor};stroke:${strokecolor}`,
                onmousedown: state.handlemousedown,
                onmouseover: state.handlemouseover,
                onmouseout: state.handlemouseout
            });
        }
        const ports = box.ports.map((_port, j) => {
            return m(EditPort, { box, i, j, state })
        });
        return m("g", mainbox, ...ports);
    }
}

class PortAttrs {
    constructor(
        public box: Box,
        public i: number,
        public j: number,
        public state: EditorState
    ) { }
}

const EditPort: m.Component<PortAttrs> = {
    view({ attrs: { box, i, j, state } }: m.CVnode<PortAttrs>) {
        const port = box.ports[j];
        var fillcolor;
        const a = new Attachment(i, AttachType.Port, j);
        if (a.isEqual(state.src)) {
            fillcolor = 'red';
        } else if (a.isEqual(state.tgt)) {
            fillcolor = 'blue';
        } else if (a.isEqual(state.cursorOver)) {
            fillcolor = 'lightgrey';
        } else {
            fillcolor = 'white';
        }
        const strokecolor = (port.color != undefined) ? port.color : 'black';
        const pp = port_pos(box, j);
        return m("circle", {
            cx: pp.x,
            cy: pp.y,
            r: PORTRADIUS,
            "data-a": JSON.stringify(a.toJson()),
            style: `fill: ${fillcolor}; stroke: ${strokecolor}`,
            onmousedown: state.handlemousedown,
            onmouseover: state.handlemouseover,
            onmouseout: state.handlemouseout
        });
    }
}

class WireAttrs {
    constructor(
        public edge: Edge,
        public i: number,
        public state: EditorState
    ) { }
}

export const EditWire: m.Component<WireAttrs> = {
    view({ attrs }: m.CVnode<WireAttrs>) {
        const edge = attrs.edge;
        const i = attrs.i;
        const state = attrs.state
        const s = attachment_pos(state.wired, edge.src);
        const t = attachment_pos(state.wired, edge.tgt);
        const c = new Point((s.x + t.x) / 2, (s.y + t.y) / 2);
        var marker;
        if (state.cursorOverEdge != undefined && state.cursorOverEdge == i) {
            marker = "arrow-sel";
        } else {
            marker = "arrow";
        }
        const strokecolor = (edge.color != undefined) ? edge.color : 'black';
        return m("g",
            m("polyline",
                {
                    points: `${s.x},${s.y} ${c.x},${c.y} ${t.x},${t.y}`,
                    "marker-mid": `url(#${marker})`,
                    stroke: strokecolor,
                }),
            m("circle",
                {
                    cx: c.x,
                    cy: c.y,
                    r: EDGERADIUS,
                    opacity: "0.0",
                    "data-i": i,
                    onmouseover: state.handlemouseoveredge,
                    onmouseout: state.handlemouseoutedge,
                })
        )
    }
}

class StateAttrs {
    constructor(
        public state: EditorState
    ) { }
}

function squareStr(x: number, y: number, w: number, h: number): string {
    return `${x},${y} ${x + w},${y} ${x + w},${y + h} ${x},${y + h}`
}

const COLORS = ["red", "blue", "green", undefined];

const EditColor: m.Component<StateAttrs> = {
    view({ attrs }: m.CVnode<StateAttrs>): m.Vnode<any, any> {
        const state = attrs.state;
        const n = COLORS.length
        const swatches = COLORS.map((color, i) => {
            const stroke = color == state.inputconfig.color ? 'black' : 'none';
            return m("polygon",
                {
                    points: squareStr(10, 35 + i * 30, 20, 20),
                    fill: color,
                    stroke,
                    onclick: () => {
                        state.inputconfig.color = color;
                    }
                }
            )
        });
        return m("g",
            m("polygon",
                {
                    points: squareStr(5, 30, 30, n * 30),
                    stroke: 'black',
                    fill: 'white',
                }),
            ...swatches);
    }
}

const EditTy: m.Component<StateAttrs> = {
    view({ attrs: { state } }: m.CVnode<StateAttrs>): m.Vnode<any, any> {
        return m("text",
            {
                x: 5, y: 15
            }, state.inputconfig.portty)
    }
}

export const Editor: m.Component<EditorAttrs> = {
    oncreate({ attrs, dom }: m.VnodeDOM<EditorAttrs>) {
        const rect = dom.getBoundingClientRect();
        attrs.state.offset = new Point(rect.x, rect.y);
    },

    view({ attrs }): m.Vnode<any, any> {
        const state = attrs.state;
        const boxes = state.wired.boxes.map((box, i) => {
            return m(EditBox, { box, i, state });
        });
        const edges = state.wired.edges.map((edge, i) => {
            return m(EditWire, { edge, i, state })
        })
        const colorpicker = m(EditColor, { state });
        const inout = m(EditTy, { state });
        // const status = m("p", JSON.stringify(state.inputconfig));
        return m("div",
            m("svg",
                {
                    onmousemove: state.handlemousemove,
                    onmouseup: state.handlemouseup,
                    onkeydown: state.handlekeydown,
                    tabindex: "0",
                    style: "border-style:solid;stroke-width:2px",
                    width: "95%",
                    height: "300px",
                },
                [svgdefs, ...edges, ...boxes, inout, colorpicker]),
            m("input",
                { onkeydown: state.handleinputkeydown, }
            )
        )
    }
}

function squarestr(p: Point, radius: any): any {
    return `${p.x - radius},${p.y - radius} ${p.x + radius},${p.y - radius} ${p.x + radius},${p.y + radius} ${p.x - radius},${p.y + radius}`;
}

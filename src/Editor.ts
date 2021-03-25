import { Wired, Attachment, Box, Point, AttachType } from "./Wired";
import m from "mithril";

const BOXRADIUS = 25;
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
    initialWired: Wired
}

function port_pos(box: Box, j: number): Point {
    const t = (j / box.ports.length) * 2 * Math.PI;
    return new Point(box.loc.x + (Math.sin(t) * BOXRADIUS), box.loc.y - (Math.cos(t) * BOXRADIUS))
}

function attachment_pos(wired: Wired, a: Attachment): Point {
    if (a.ty == AttachType.Box) {
        return wired.boxes[a.box_idx].loc;
    } else if (a.ty == AttachType.Port && a.port_idx != undefined) {
        return port_pos(wired.boxes[a.box_idx], a.port_idx);
    } else {
        throw new Error("invalid attachment");
    }
}

class InputConfig {
    boxty: string
    portty: string
    edgety: string
    color: string | undefined

    constructor() {
        this.boxty = 'vertex';
        this.portty = 'port';
        this.edgety = 'edge';
        this.color = undefined;
    }
}

export class Editor implements m.ClassComponent<EditorAttrs> {
    wired: Wired
    offset: Point
    cursor: Point
    inputconfig: InputConfig
    clicked?: number
    cursorOver?: Attachment
    cursorOverEdge?: number
    src?: Attachment
    tgt?: Attachment

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
                this.wired.addBox("vertex", this.cursor, this.inputconfig.color);
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
                    this.wired.addEdge("edge", s, t);
                    this.src = undefined;
                    this.tgt = undefined;
                }
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
                    this.wired.addPort("port", this.cursorOver.box_idx, this.inputconfig.color);
                }
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
        }
        m.redraw()
    }

    handleinputkeydown = (e: KeyboardEvent) => {
        if (e.code == 'Enter') {
            const inputElt = e.target as HTMLInputElement;
            const cmd = inputElt.value;
            const cmdparts = cmd.match(/set (\w+) (\w+)/);
            if (cmdparts != null) {
                switch (cmdparts[1]) {
                    case 'color': {
                        this.inputconfig.color =
                            cmdparts[2] == 'undefined' ? undefined : cmdparts[2];
                        break;
                    }
                }
            }
            inputElt.value = "";
        } else {
            (e as any).redraw = false;
        }
    }

    constructor({ attrs }: m.CVnode<EditorAttrs>) {
        this.offset = new Point(0, 0);
        this.cursor = new Point(0, 0);
        this.wired = attrs.initialWired
        this.inputconfig = new InputConfig();
    }

    oncreate(vnode: m.VnodeDOM<EditorAttrs>) {
        const d = vnode.dom;
        const rect = d.getBoundingClientRect();
        this.offset = new Point(rect.x, rect.y);
    }

    view(): m.Vnode<any, any> {
        const boxes = this.wired.boxes.map((box, i) => {
            var fillcolor;
            const a = new Attachment(i, AttachType.Box);
            if (a.isEqual(this.src)) {
                fillcolor = 'red';
            } else if (a.isEqual(this.tgt)) {
                fillcolor = 'blue';
            } else if (a.isEqual(this.cursorOver)) {
                fillcolor = 'lightgrey';
            } else {
                fillcolor = 'white';
            }
            const strokecolor = (box.color != undefined) ? box.color : 'black';
            const p = box.loc;
            const mainbox = m("circle", {
                cx: p.x, cy: p.y, r: BOXRADIUS,
                "data-a": JSON.stringify(a.toJson()),
                style: `fill:${fillcolor};stroke:${strokecolor}`,
                onmousedown: this.handlemousedown,
                onmouseover: this.handlemouseover,
                onmouseout: this.handlemouseout
            });
            const ports = box.ports.map((port, j) => {
                var fillcolor;
                const a = new Attachment(i, AttachType.Port, j);
                if (a.isEqual(this.src)) {
                    fillcolor = 'red';
                } else if (a.isEqual(this.tgt)) {
                    fillcolor = 'blue';
                } else if (a.isEqual(this.cursorOver)) {
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
                    onmousedown: this.handlemousedown,
                    onmouseover: this.handlemouseover,
                    onmouseout: this.handlemouseout
                });
            });
            return m("g", mainbox, ...ports);
        });
        const edges = this.wired.edges.map((edge, i) => {
            const s = attachment_pos(this.wired, edge.src);
            const t = attachment_pos(this.wired, edge.tgt);
            const c = new Point((s.x + t.x) / 2, (s.y + t.y) / 2);
            var marker;
            if (this.cursorOverEdge != undefined && this.cursorOverEdge == i) {
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
                        r: BOXRADIUS,
                        opacity: "0.0",
                        "data-i": i,
                        onmouseover: this.handlemouseoveredge,
                        onmouseout: this.handlemouseoutedge,
                    })
            )
        })
        const status = m("p", `color: ${this.inputconfig.color}`);
        return m("div",
            m("svg",
                {
                    onmousemove: this.handlemousemove,
                    onmouseup: this.handlemouseup,
                    onkeydown: this.handlekeydown,
                    tabindex: "0",
                    style: "border-style:solid;stroke-width:2px",
                    width: "100%",
                    height: "500px",
                },
                [svgdefs, ...edges, ...boxes]),
            status,
            m("input",
                { onkeydown: this.handleinputkeydown, }
            )

        )
    }
}

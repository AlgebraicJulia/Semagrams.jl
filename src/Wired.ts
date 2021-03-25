export class BoxProperties {
    constructor(
        readonly weights: Array<string>
    ) { }
}

export enum AttachType {
    Port,
    Box
}

export class PortProperties {
    constructor(
        readonly weights: Array<string>,
        readonly attach: string,
        readonly color?: string,
    ) { }
}

export class EdgeProperties {
    constructor(
        readonly weights: Array<string>,
        readonly src: [AttachType, string],
        readonly tgt: [AttachType, string],
    ) { }
}

export class WiredSchema {
    constructor(
        readonly object_types: Record<string, BoxProperties>,
        readonly port_types: Record<string, PortProperties>,
        readonly edge_types: Record<string, EdgeProperties>
    ) { }
}

export const GraphSchema = new WiredSchema(
    { "vertex": new BoxProperties([]) },
    {},
    { "edge": new EdgeProperties([], [AttachType.Box, "vertex"], [AttachType.Box, "vertex"]) }
)

export class Point {
    constructor(
        readonly x: number,
        readonly y: number
    ) { }
}

export class Attachment {
    constructor(
        public box_idx: number,
        public ty: AttachType,
        public port_idx?: number
    ) { }

    isEqual(other: Attachment | undefined): boolean {
        if (other != undefined) {
            return (this.box_idx == other.box_idx &&
                this.ty == other.ty &&
                this.port_idx == other.port_idx)
        } else {
            return false;
        }
    }

    toJson(): object {
        if (this.ty == AttachType.Box) {
            return { box_idx: this.box_idx, ty: "BOX" }
        } else {
            return { box_idx: this.box_idx, ty: "PORT", port_idx: this.port_idx }
        }
    }

    static fromJson(ob: any): Attachment {
        if (ob.ty == "BOX") {
            return new Attachment(ob.box_idx, AttachType.Box)
        } else if (ob.ty == "PORT") {
            return new Attachment(ob.box_idx, AttachType.Port, ob.port_idx);
        } else {
            throw new Error("could not deserialize Attachment");
        }
    }
}

export class Port {
    constructor(
        readonly ty: string,
        public weights: Record<string, string>,
        public color?: string
    ) { }
}

export class Box {
    constructor(
        readonly ty: string,
        public weights: Record<string, string>,
        public loc: Point,
        public ports: Array<Port>,
        public color?: string
    ) { }
}

export class Edge {
    constructor(
        readonly ty: string,
        public weights: Record<string, string>,
        public src: Attachment,
        public tgt: Attachment,
        public color?: string
    ) { }
}

export class EdgeIndex {
    public box_edges: Set<number>
    public port_edges: Array<Set<number>>

    constructor() {
        this.box_edges = new Set<number>();
        this.port_edges = [];
    }

    allEdges(): Set<number> {
        const all = new Set<number>();
        for (let e of this.box_edges) {
            all.add(e);
        }
        for (let es of this.port_edges) {
            for (let e of es) {
                all.add(e);
            }
        }
        return all;
    }

    takePortEdge(i: number): number | undefined {
        if (this.port_edges[i].size == 0) {
            return undefined;
        } else {
            return this.port_edges[i].values().next().value;
        }
    }

    takeEdge(): number | undefined {
        if (this.box_edges.size == 0) {
            return undefined;
        } else {
            return this.box_edges.values().next().value;
        }
    }

    retrieve(a: Attachment): Set<number> {
        if (a.ty == AttachType.Box) {
            return this.box_edges;
        } else if (a.ty == AttachType.Port && a.port_idx != undefined) {
            return this.port_edges[a.port_idx];
        } else {
            throw Error("invalid attachment");
        }
    }

    addEdge(a: Attachment, i: number) {
        if (a.ty == AttachType.Box) {
            this.addBoxEdge(i);
        } else if (a.ty == AttachType.Port && a.port_idx != undefined) {
            this.addPortEdge(i, a.port_idx);
        } else {
            throw Error("invalid attachment");
        }
    }

    remEdge(a: Attachment, i: number) {
        if (a.ty == AttachType.Box) {
            this.remBoxEdge(i);
        } else if (a.ty == AttachType.Port && a.port_idx != undefined) {
            this.remPortEdge(i, a.port_idx);
        } else {
            throw Error("invalid attachment");
        }
    }

    updateEdge(a: Attachment, oldi: number, newi: number) {
        this.remEdge(a, oldi);
        this.addEdge(a, newi);
    }

    addBoxEdge(e: number) {
        this.box_edges.add(e);
    }

    addPortEdge(e: number, port_idx: number) {
        this.port_edges[port_idx].add(e);
    }

    remBoxEdge(e: number) {
        this.box_edges.delete(e);
    }

    remPortEdge(e: number, port_idx: number) {
        this.port_edges[port_idx].delete(e);
    }

    addPort() {
        this.port_edges.push(new Set());
    }
}

export class Wired {
    public boxes: Array<Box>
    public edges: Array<Edge>
    private src_index: Array<EdgeIndex>
    private tgt_index: Array<EdgeIndex>

    constructor(
        readonly schema: WiredSchema,
    ) {
        this.boxes = [];
        this.edges = [];
        this.src_index = [];
        this.tgt_index = [];
    }

    addBox(ty: string, pos: Point, color: string | undefined) {
        const i = this.boxes.length;
        this.boxes.push(new Box(ty, {}, pos, [], color));
        this.src_index.push(new EdgeIndex());
        this.tgt_index.push(new EdgeIndex());
        return new Attachment(i, AttachType.Box);
    }

    getColor(a: Attachment): string | undefined {
        if (a.ty == AttachType.Box) {
            return this.boxes[a.box_idx].color;
        } else if (a.ty == AttachType.Port && a.port_idx != undefined) {
            return this.boxes[a.box_idx].ports[a.port_idx].color;
        }
    }

    addEdge(ty: string, src: Attachment, tgt: Attachment): number | undefined {
        const i = this.edges.length;
        const src_color = this.getColor(src);
        const tgt_color = this.getColor(tgt);
        if (src_color != tgt_color) {
            return undefined;
        }
        this.edges.push(new Edge(ty, {}, src, tgt, src_color));
        this.src_index[src.box_idx].addEdge(src, i);
        this.tgt_index[tgt.box_idx].addEdge(tgt, i)
        return i;
    }

    addPort(ty: string, box_id: number, color: string | undefined) {
        const box = this.boxes[box_id];
        const i = box.ports.length;
        this.boxes[box_id].ports.push(new Port(ty, {}, color));
        this.src_index[box_id].addPort();
        this.tgt_index[box_id].addPort();
        return i;
    }

    inNeighbors(a: Attachment): Set<number> {
        return this.tgt_index[a.box_idx].retrieve(a);
    }

    allInNeighbors(n: number): Set<number> {
        return this.tgt_index[n].allEdges();
    }

    outNeighbors(a: Attachment): Set<number> {
        return this.src_index[a.box_idx].retrieve(a);
    }

    allOutNeighbors(n: number): Set<number> {
        return this.src_index[n].allEdges();
    }

    remEdge(i: number) {
        // We perform the "swap and shrink"
        if (i < 0 || i >= this.edges.length) {
            throw Error("invalid edge id")
        }
        // Remove the edge from the indexes
        const e = this.edges[i];
        const src = e.src;
        const tgt = e.tgt;
        this.src_index[src.box_idx].remEdge(src, i);
        this.tgt_index[tgt.box_idx].remEdge(tgt, i);

        const n = this.edges.length - 1;
        if (i !== n) {
            const end = this.edges[n];
            this.src_index[end.src.box_idx].updateEdge(end.src, n, i);
            this.tgt_index[end.tgt.box_idx].updateEdge(end.tgt, n, i);
            this.edges[i] = end;
        }
        this.edges.pop();
    }

    remPort(box_idx: number, port_idx: number) {
        if (box_idx < 0 || box_idx >= this.boxes.length) {
            throw new Error("invalid box id");
        }
        const box = this.boxes[box_idx];
        const n = box.ports.length - 1;
        if (port_idx < 0 || port_idx > n) {
            throw new Error("invalid port id");
        }

        const si = this.src_index[box_idx];
        var e = si.takePortEdge(port_idx);
        while (e !== undefined) {
            this.remEdge(e);
            e = si.takePortEdge(port_idx);
        }
        const ti = this.tgt_index[box_idx];

        e = ti.takePortEdge(port_idx)
        while (e !== undefined) {
            this.remEdge(e);
            e = ti.takePortEdge(port_idx);
        }

        if (port_idx !== n) {
            for (let e of this.inNeighbors(new Attachment(box_idx, AttachType.Port, n))) {
                this.edges[e].tgt.port_idx = port_idx;
            }
            for (let e of this.outNeighbors(new Attachment(box_idx, AttachType.Port, n))) {
                this.edges[e].src.port_idx = port_idx;
            }
            si.port_edges[port_idx] = si.port_edges[n];
            ti.port_edges[port_idx] = ti.port_edges[n];
            box.ports[port_idx] = box.ports[n];
        }
        box.ports.pop();
        si.port_edges.pop();
        ti.port_edges.pop();
    }

    remBox(i: number) {
        // We perform the "swap and shrink"
        // First we remove all of the constituent edges

        const n = this.boxes.length - 1;
        if (i < 0 || i > n) {
            throw Error("invalid box id");
        }
        const box = this.boxes[i];
        const si = this.src_index[i];
        const ti = this.tgt_index[i];

        while (box.ports.length > 0) {
            this.remPort(i, 0);
        }

        let e = si.takeEdge();
        while (e != undefined) {
            this.remEdge(e);
            e = si.takeEdge();
        }
        e = ti.takeEdge();
        while (e != undefined) {
            this.remEdge(e);
            e = ti.takeEdge();
        }
        if (i !== n) {
            for (let e of this.inNeighbors(new Attachment(n, AttachType.Box))) {
                this.edges[e].tgt.box_idx = i;
            }
            for (let e of this.outNeighbors(new Attachment(n, AttachType.Box))) {
                this.edges[e].src.box_idx = i;
            }
            this.src_index[i] = this.src_index[n];
            this.tgt_index[i] = this.tgt_index[n];
            this.boxes[i] = this.boxes[n];
        }
        this.boxes.pop();
        this.src_index.pop();
        this.tgt_index.pop();
    }

    remAttachment(a: Attachment) {
        if (a.ty == AttachType.Box) {
            this.remBox(a.box_idx);
        } else if (a.ty == AttachType.Port && a.port_idx !== undefined) {
            this.remPort(a.box_idx, a.port_idx)
        } else {
            throw new Error("invalid attachment type")
        }
    }
}

export const empty_graph = new Wired(GraphSchema);

import { HashMap } from "@thi.ng/associative";
import { equiv } from "@thi.ng/equiv";
import { hash } from "@thi.ng/vectors";
import { AttachType, Schema } from "./Schema"

export interface BoxAttachment {
    ty: AttachType.Box
    box_idx: number
}

export interface PortAttachment {
    ty: AttachType.Port
    box_idx: number
    port_idx: number
}

/**
 * Wires can either be attached to boxes or ports.
 * Thus, the src of a wire is an "Attachment"
 */
export type Attachment = BoxAttachment | PortAttachment;

/**
 * We use this to store attachments in hash tables
 */
export function hashAttachment(a: Attachment) {
    switch (a.ty) {
        case AttachType.Box: {
            return hash([0, a.box_idx]);
        }
        case AttachType.Port: {
            return hash([1, a.box_idx, a.port_idx])
        }
    }
}

export function box_attach(i: number): BoxAttachment {
    return { ty: AttachType.Box, box_idx: i };
}

export function port_attach(i: number, j: number): PortAttachment {
    return { ty: AttachType.Port, box_idx: i, port_idx: j };
}

/**
 * The data associated with a port.
 * Note that ports are stored in an array for each box, so we don't need to store
 * the index of the box.
 */
export class Port {
    constructor(
        /** The ty refers to a PortProperties in the schema */
        readonly ty: string,
        public weights: Record<string, string>,
        public color?: string
    ) { }
}

/**
 * The data associated with a box, including the ports.
 * Note that the ports are not necessarily numbered sequentially:
 * this is to make the logic for deleting easier. Port ids NEVER change
 * after creation.
 */
export class Box {
    constructor(
        /* The ty refers to a BoxProperties in the schema */
        readonly ty: string,
        public weights: Record<string, string>,
        public ports: Map<number, Port>,
        public color?: string
    ) { }

    export() {
        return {
            ty: this.ty,
            weights: this.weights,
            ports: Array.from(this.ports.entries()),
            color: this.color
        }
    }
}

/**
 * The data associated with a wire. Note that src and tgt are Attachments.
 */
export class Wire {
    constructor(
        /* The ty refers to a WireProperties in the schema */
        readonly ty: string,
        public weights: Record<string, string>,
        public src: Attachment,
        public tgt: Attachment,
        public color?: string
    ) { }
}


/**
 * Generates new ids for new objects in the Semagram.
 * Right now, assigns them sequentially, but that is an implementation detail.
 */
class IDGen {
    private i: number
    constructor() {
        this.i = 0;
    }

    next() {
        return this.i++;
    }
}

function remove<T>(xs: Array<T>, x: T): void {
    for (var i = 0; i < xs.length; i++) {
        if (xs[i] == x) {
            xs.splice(i, 1);
            break;
        }
    }
}

/**
 * This stores the actual data of a Semagram.
 * All it is is two maps of boxes and wires (ports are stored within boxes),
 * along with a schema and an ID generator.
 * These are maps instead of arrays to simplify deleting logic, and to make sure
 * that we never have to change ids.
 */
export class Semagram {
    public boxes: Map<number, Box>
    public wires: Map<number, Wire>
    private src_tgt_index: HashMap<[Attachment, Attachment], Array<number>>
    private gen: IDGen;

    constructor(
        readonly schema: Schema,
    ) {
        this.boxes = new Map();
        this.wires = new Map();
        this.gen = new IDGen();
        this.src_tgt_index = new HashMap([], {
            hash: ([a1, a2]) => hash([hashAttachment(a1), hashAttachment(a2)])
        });
    }

    /**
     * TODO: this should use colorAttribute from the schema
     */
    getColor(a: Attachment): string | undefined {
        switch (a.ty) {
            case AttachType.Box: {
                return this.boxes.get(a.box_idx)!.color;
            }
            case AttachType.Port: {
                return this.boxes.get(a.box_idx)!.ports.get(a.port_idx)!.color;
            }
        }
    }

    /** The getters are self-explanatory */
    getBox(box_idx: number): Box | undefined {
        return this.boxes.get(box_idx);
    }

    getPort(box_idx: number, port_idx: number): Port | undefined {
        const box = this.boxes.get(box_idx);
        return box && box.ports.get(port_idx);
    }

    getWire(wire_idx: number): Wire | undefined {
        return this.wires.get(wire_idx);
    }

    getAttachment(a: Attachment): Box | Port | undefined {
        switch (a.ty) {
            case AttachType.Box: {
                return this.getBox(a.box_idx);
            }
            case AttachType.Port: {
                return this.getPort(a.box_idx, a.port_idx)
            }
        }
    }

    /** The "type" here refers to the ty parameter of the box/port.
     * TODO: This shouldn't return the ty parameter of the Attachment?
     * Seems a bit redundant
     */
    attachmentType(a: Attachment): [AttachType, string] {
        const node = this.getAttachment(a)!;
        return [a.ty, node.ty];
    }

    /** The addX methods are self-explanatory */

    addWire(ty: string, src: Attachment, tgt: Attachment): number | undefined {
        const i = this.gen.next()
        const wireschema = this.schema.wire_types[ty];
        const src_ob = this.getAttachment(src)!;
        const tgt_ob = this.getAttachment(tgt)!;
        if (!(equiv([src.ty, src_ob.ty], wireschema.src))) {
            throw new Error(`The src of a wire of type ${ty} cannot be ${[src.ty, src_ob.ty]}`);
        }
        if (!(equiv([tgt.ty, tgt_ob.ty], wireschema.tgt))) {
            throw new Error(`The tgt of a wire of type ${ty} cannot be ${[tgt.ty, tgt_ob.ty]}`);
        }
        if (src_ob.color != tgt_ob.color) {
            return undefined;
        }
        this.wires.set(i, new Wire(ty, {}, src, tgt, src_ob.color));
        if (!this.src_tgt_index.has([src, tgt])) {
            this.src_tgt_index.set([src, tgt], [])
        }
        this.src_tgt_index.get([src, tgt])!.push(i);
        return i;
    }

    addPort(ty: string, box_idx: number, color: string | undefined): PortAttachment {
        const box = this.boxes.get(box_idx)!;
        const portschema = this.schema.port_types[ty];
        if (portschema.box != box.ty) {
            throw new Error(`Cannot attach a port of type ${ty} to a box of type ${box.ty}`);
        }
        const i = this.gen.next();
        box.ports.set(i, new Port(ty, {}, color));
        return port_attach(box_idx, i);
    }

    addBox(ty: string, color: string | undefined): BoxAttachment {
        const i = this.gen.next();
        this.boxes.set(i, new Box(ty, {}, new Map(), color));
        return box_attach(i);
    }

    /** The remX methods are self-explanatory */

    remWire(i: number) {
        const wire = this.getWire(i);
        if (wire) {
            remove(this.src_tgt_index.get([wire.src, wire.tgt])!, i);
            this.wires.delete(i);
        }
    }

    remPort(box_idx: number, port_idx: number) {
        const box = this.boxes.get(box_idx)!;
        const a = port_attach(box_idx, port_idx);
        for (const i of this.wires.keys()) {
            const e = this.wires.get(i)!;
            if (equiv(e.src, a) || equiv(e.tgt, a)) {
                this.remWire(i);
            }
        }
        box.ports.delete(port_idx);
    }

    remBox(box_idx: number) {
        const box = this.boxes.get(box_idx)!;

        for (const port_idx of box.ports.keys()) {
            this.remPort(box_idx, port_idx);
        }

        const a = box_attach(box_idx);
        for (const wire_idx of this.wires.keys()) {
            const e = this.wires.get(wire_idx)!;
            if (equiv(e.src, a) || equiv(e.tgt, a)) {
                this.remWire(wire_idx);
            }
        }
        this.boxes.delete(box_idx);
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

    wiresBetween(src: Attachment, tgt: Attachment): Array<number> {
        const wire_list = this.src_tgt_index.get([src, tgt]);
        if (wire_list) {
            return wire_list;
        } else {
            return [];
        }
    }

    export() {
        return {
            boxes: Array.from(this.boxes.entries()).map(([i, box]) => [i, box.export()]),
            wires: Array.from(this.wires.entries())
        }
    }
}

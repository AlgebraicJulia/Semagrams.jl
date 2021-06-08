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

export type Attachment = BoxAttachment | PortAttachment;

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

export class Wire {
    constructor(
        readonly ty: string,
        public weights: Record<string, string>,
        public src: Attachment,
        public tgt: Attachment,
        public color?: string
    ) { }
}

class IDGen {
    private i: number
    constructor() {
        this.i = 0;
    }

    next() {
        return this.i++;
    }
}

export class Semagram {
    public boxes: Map<number, Box>
    public wires: Map<number, Wire>
    private gen: IDGen;

    constructor(
        readonly schema: Schema,
    ) {
        this.boxes = new Map();
        this.wires = new Map();
        this.gen = new IDGen();
    }

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

    attachmentType(a: Attachment): [AttachType, string] {
        const node = this.getAttachment(a)!;
        return [a.ty, node.ty];
    }

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

    remWire(i: number) {
        this.wires.delete(i);
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

    export() {
        return {
            boxes: Array.from(this.boxes.entries()).map(([i, box]) => [i, box.export()]),
            wires: Array.from(this.wires.entries())
        }
    }
}

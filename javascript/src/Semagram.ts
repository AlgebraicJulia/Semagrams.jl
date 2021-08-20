import { equiv } from "@thi.ng/equiv";
import { hash } from "@thi.ng/vectors";
import { iterator, map } from "@thi.ng/transducers";
import { AttachType, AttributeType, EntityType, Schema } from "./Schema"
import { SetMap } from "./Util";

export interface BoxEntity {
    ty: EntityType.Box
    box_idx: number
}

export interface PortEntity {
    ty: EntityType.Port
    box_idx: number
    port_idx: number
}

export interface WireEntity {
    ty: EntityType.Wire
    wire_idx: number
}

/**
 * Wires can either be attached to boxes or ports.
 * Thus, the src of a wire is an "Attachment"
 */
export type Attachment = BoxEntity | PortEntity;

export type Entity = BoxEntity | PortEntity | WireEntity;

/**
 * We use this to store attachments in hash tables
 */
export function hashEntity(a: Entity) {
    switch (a.ty) {
        case EntityType.Box: {
            return hash([0, a.box_idx]);
        }
        case EntityType.Port: {
            return hash([1, a.box_idx, a.port_idx]);
        }
        case EntityType.Wire: {
            return hash([2, a.wire_idx]);
        }
    }
}

export function box_entity(i: number): BoxEntity {
    return { ty: EntityType.Box, box_idx: i };
}

export function port_entity(i: number, j: number): PortEntity {
    return { ty: EntityType.Port, box_idx: i, port_idx: j };
}

export function wire_entity(i: number): WireEntity {
    return { ty: EntityType.Wire, wire_idx: i };
}

export interface ExportedPort {
    ty: string
    weights: Record<string, string>
    homs: Record<string, Entity>
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
        public homs: Record<string, Entity>
    ) { }

    export(): ExportedPort {
        return {
            ty: this.ty,
            weights: this.weights,
            homs: this.homs
        };
    }

    static fromExported(e: ExportedPort): Port {
        return new Port(e.ty, e.weights, e.homs);
    }
}

export interface ExportedBox {
    ty: string
    weights: Record<string, string>
    homs: Record<string, Entity>
    ports: Array<[number, ExportedPort]>
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
        public homs: Record<string, Entity>,
        public ports: Map<number, Port>,
    ) { }

    export(): ExportedBox {
        return {
            ty: this.ty,
            weights: this.weights,
            homs: this.homs,
            ports: Array.from(map(([port_idx, p]) => [port_idx, p.export()], this.ports.entries())),
        }
    }

    static fromExported(e: ExportedBox): Box {
        return new Box(e.ty, e.weights, e.homs, new Map(
            iterator(map(([port_idx, ep]) => [port_idx, Port.fromExported(ep)]), e.ports)));
    }
}

export interface ExportedWire {
    ty: string,
    weights: Record<string, string>
    homs: Record<string, Entity>
    src: Attachment,
    tgt: Attachment
}

/**
 * The data associated with a wire. Note that src and tgt are Attachments.
 */
export class Wire {
    constructor(
        /* The ty refers to a WireProperties in the schema */
        readonly ty: string,
        public weights: Record<string, string>,
        public homs: Record<string, Entity>,
        public src: Attachment,
        public tgt: Attachment,
    ) { }

    export(): ExportedWire {
        return {
            ty: this.ty,
            weights: this.weights,
            homs: this.homs,
            src: this.src,
            tgt: this.tgt
        }
    }

    static fromExported(e: ExportedWire): Wire {
        return new Wire(e.ty, e.weights, e.homs, e.src, e.tgt);
    }
}

export interface ExportedIDGen {
    i: number
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

    export(): ExportedIDGen {
        return { i: this.i }
    }

    static fromExported(e: ExportedIDGen): IDGen {
        return Object.assign(new IDGen(), e);
    }
}

function initStyleFns(schema: Schema): Record<string, Function> {
    const style_fns: Record<string, Function> = {};
    for (const [name, box_props] of Object.entries(schema.box_types)) {
        style_fns[name] = eval(box_props.style_fn) as Function;
    }
    for (const [name, port_props] of Object.entries(schema.port_types)) {
        style_fns[name] = eval(port_props.style_fn) as Function;
    }
    for (const [name, wire_props] of Object.entries(schema.wire_types)) {
        style_fns[name] = eval(wire_props.style_fn) as Function;
    }
    return style_fns;
}

export interface ExportedSemagram {
    boxes: Array<[number, ExportedBox]>,
    wires: Array<[number, ExportedWire]>,
    gen: ExportedIDGen
    schema: Schema
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
    private src_tgt_index: SetMap<[Attachment, Attachment], number>
    private gen: IDGen;
    public style_fns: Record<string, Function>

    constructor(
        readonly schema: Schema,
    ) {
        this.boxes = new Map();
        this.wires = new Map();
        this.gen = new IDGen();
        this.src_tgt_index = new SetMap(
            ([a1, a2]) => hash([hashEntity(a1), hashEntity(a2)]),
            (e1, e2) => Math.sign(e1 - e2)
        );
        this.style_fns = initStyleFns(schema);
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

    getEntity(a: Entity): Box | Port | Wire | undefined {
        switch (a.ty) {
            case EntityType.Box: {
                return this.getBox(a.box_idx);
            }
            case EntityType.Port: {
                return this.getPort(a.box_idx, a.port_idx)
            }
            case EntityType.Wire: {
                return this.getWire(a.wire_idx)
            }
        }
    }

    /** The "type" here refers to the ty parameter of the box/port.
     * TODO: This shouldn't return the ty parameter of the Attachment?
     * Seems a bit redundant
     */
    attachmentType(a: Attachment): [AttachType, string] {
        const node = this.getEntity(a)!;
        return [a.ty, node.ty];
    }

    /** The addX methods are self-explanatory */

    addWire(ty: string, src: Attachment, tgt: Attachment): number | undefined {
        const i = this.gen.next()
        const wireschema = this.schema.wire_types[ty];
        const src_ob = this.getEntity(src)!;
        const tgt_ob = this.getEntity(tgt)!;
        if (!(equiv([src.ty, src_ob.ty], wireschema.src))) {
            throw new Error(`The src of a wire of type ${ty} cannot be ${[src.ty, src_ob.ty]}`);
        }
        if (!(equiv([tgt.ty, tgt_ob.ty], wireschema.tgt))) {
            throw new Error(`The tgt of a wire of type ${ty} cannot be ${[tgt.ty, tgt_ob.ty]}`);
        }
        this.wires.set(i, new Wire(ty, {}, {}, src, tgt));
        this.src_tgt_index.add([src, tgt], i);
        return i;
    }

    addPort(ty: string, box_idx: number): PortEntity {
        const box = this.boxes.get(box_idx)!;
        const portschema = this.schema.port_types[ty];
        if (portschema.box != box.ty) {
            throw new Error(`Cannot attach a port of type ${ty} to a box of type ${box.ty}`);
        }
        const i = this.gen.next();
        box.ports.set(i, new Port(ty, {}, {}));
        return port_entity(box_idx, i);
    }

    addBox(ty: string): BoxEntity {
        const i = this.gen.next();
        this.boxes.set(i, new Box(ty, {}, {}, new Map()));
        return box_entity(i);
    }

    /** The remX methods are self-explanatory */

    remWire(i: number) {
        const wire = this.getWire(i);
        if (wire) {
            this.src_tgt_index.delete([wire.src, wire.tgt], i);
            this.wires.delete(i);
        }
    }

    remPort(box_idx: number, port_idx: number) {
        const box = this.boxes.get(box_idx)!;
        const a = port_entity(box_idx, port_idx);
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

        const a = box_entity(box_idx);
        for (const wire_idx of this.wires.keys()) {
            const e = this.wires.get(wire_idx)!;
            if (equiv(e.src, a) || equiv(e.tgt, a)) {
                this.remWire(wire_idx);
            }
        }
        this.boxes.delete(box_idx);
    }

    remEntity(a: Entity) {
        if (a.ty == EntityType.Box) {
            this.remBox(a.box_idx);
        } else if (a.ty == EntityType.Port && a.port_idx !== undefined) {
            this.remPort(a.box_idx, a.port_idx)
        } else if (a.ty == EntityType.Wire) {
            this.remWire(a.wire_idx)
        } else {
            throw new Error("invalid attachment type")
        }
    }

    wiresBetween(src: Attachment, tgt: Attachment): Array<number> {
        const wire_list = this.src_tgt_index.get([src, tgt]);
        if (wire_list) {
            return [...wire_list.values()];
        } else {
            return [];
        }
    }

    weightTypes(a: Entity): Array<[AttributeType, string]> | undefined {
        const obj = this.getEntity(a);
        if (obj == undefined) {
            return undefined;
        }
        switch (a.ty) {
            case EntityType.Box: {
                return this.schema.box_types[obj.ty].weights;
            }
            case EntityType.Port: {
                return this.schema.port_types[obj.ty].weights;
            }
            case EntityType.Wire: {
                return this.schema.wire_types[obj.ty].weights;
            }
        }
    }

    export(): ExportedSemagram {
        return {
            boxes: Array.from(this.boxes.entries()).map(([i, box]) => [i, box.export()]),
            wires: Array.from(this.wires.entries()),
            gen: this.gen.export(),
            schema: this.schema
        }
    }

    static fromExported(e: ExportedSemagram): Semagram {
        const src_tgt_index = new SetMap<[Attachment, Attachment], number>(
            ([a1, a2]) => hash([hashEntity(a1), hashEntity(a2)]),
            (e1, e2) => Math.sign(e1 - e2)
        );
        for (let [wire_idx, w] of e.wires) {
            src_tgt_index.add([w.src, w.tgt], wire_idx);
        }
        return Object.assign(new Semagram(e.schema), {
            boxes: new Map(map(([box_idx, b]) => [box_idx, Box.fromExported(b)], e.boxes)),
            wires: new Map(map(([wire_idx, w]) => [wire_idx, Wire.fromExported(w)], e.wires)),
            gen: IDGen.fromExported(e.gen),
            src_tgt_index,
            schema: e.schema
        })
    }
}

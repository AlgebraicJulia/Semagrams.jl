import { Semagram, Attachment, PortEntity, hashEntity, Entity, ExportedSemagram } from "./Semagram";
import { Schema, PortStyle, EntityType } from "./Schema";
import { Vec2Like, add2 } from "@thi.ng/vectors";
import { centerIndex, getDefault } from "./Util";

/** TODO: This should be a runtime parameter */
export const BOXRADIUS = 40;

export interface ExportedLocatedSemagram {
    sg: ExportedSemagram,
    boxlocs: Array<[number, Vec2Like]>
}

/**
 * This stores a semagram along with the locations of all of its boxes and ports
 * Note: the fact that this stores the port locations
 * should be considered an implemenation detail, at least for now. The port locations
 * are a pure function of the box locations and the number/ordering of the ports.
 * They are recalculated whenever a box location changes.
 *
 * The wireoffsets are a pure function of the wire ordering, and are also recalculated
 * when wires are added/removed.
 */
export class LocatedSemagram {
    sg: Semagram
    boxlocs: Map<number, Vec2Like>
    portlocs: Map<number, Map<number, Vec2Like>>
    wireoffsets: Map<number, number>

    constructor(schema: Schema) {
        this.sg = new Semagram(schema);
        this.boxlocs = new Map();
        this.portlocs = new Map();
        this.wireoffsets = new Map();
    }

    static fromJSON(dat: ExportedLocatedSemagram) {
        return;
    }

    /* Updates box location, and updates its corresponding ports too. */
    setBoxLoc(box_idx: number, loc: Vec2Like) {
        this.boxlocs.set(box_idx, loc);
        this.updatePortLocs(box_idx);
    }

    /**
     * Updates port locations for a specific box.
     * TODO: This should be more generic; we shouldn't special case for the
     * small number of port layout algorithms we have now, because in the
     * future there might be more port layout algorithms we want.
     */
    updatePortLocs(box_idx: number) {
        const box = this.sg.boxes.get(box_idx)!;
        let boxportlocs = getDefault(this.portlocs, box_idx, new Map());
        const circular = [];
        const input = [];
        const output = [];
        for (const port_idx of box.ports.keys()) {
            const p = box.ports.get(port_idx)!;
            const port_schema = this.sg.schema.port_types[p.ty];
            switch (port_schema.style) {
                case PortStyle.Circular: {
                    circular.push(port_idx);
                    break;
                }
                case PortStyle.Input: {
                    input.push(port_idx);
                    break;
                }
                case PortStyle.Output: {
                    output.push(port_idx);
                    break;
                }
            }
        }
        circular.forEach((port_idx, i) => {
            const t = (i / circular.length) * 2 * Math.PI;
            const v: Vec2Like = [
                (Math.sin(t) * BOXRADIUS),
                - (Math.cos(t) * BOXRADIUS)
            ];
            boxportlocs.set(port_idx, v);
        })
        input.forEach((port_idx, i) => {
            const x = -BOXRADIUS;
            const y = BOXRADIUS * 1.5 * centerIndex(i, input.length);
            boxportlocs.set(port_idx, [x, y]);
        });
        output.forEach((port_idx, i) => {
            const x = BOXRADIUS;
            const y = BOXRADIUS * 1.5 * centerIndex(i, output.length);
            boxportlocs.set(port_idx, [x, y]);
        });
    }

    updateAllPortLocs() {
        for (const box_idx of this.sg.boxes.keys()) {
            this.updatePortLocs(box_idx);
        }
    }

    updateWireOffsets(src: Attachment, tgt: Attachment) {
        if (hashEntity(src) < hashEntity(tgt)) {
            [src, tgt] = [tgt, src];
        }
        const srctgt = this.sg.wiresBetween(src, tgt);
        const tgtsrc = this.sg.wiresBetween(tgt, src);
        const n = srctgt.length + tgtsrc.length;
        for (var i = 0; i < srctgt.length; i++) {
            this.wireoffsets.set(srctgt[i], centerIndex(i, n));
        }
        for (var i = 0; i < tgtsrc.length; i++) {
            this.wireoffsets.set(tgtsrc[i], -centerIndex(srctgt.length + i, n));
        }
    }

    updateAllWireOffsets() {
        for (const w of this.sg.wires.values()) {
            this.updateWireOffsets(w.src, w.tgt);
        }
    }

    /**
     * Gets the location of an attachment (i.e. box or port)
     * Useful for figuring out where a wire starts/ends.
     */
    getLoc(a: Attachment): Vec2Like | undefined {
        const boxloc = this.boxlocs.get(a.box_idx)!;
        switch (a.ty) {
            case EntityType.Box: {
                return boxloc;
            }
            case EntityType.Port: {
                const portloc = this.portlocs.get(a.box_idx)!.get(a.port_idx)!;
                return add2([], boxloc, portloc) as Vec2Like;
            }
        }
    }

    getBoxLoc(box_idx: number): Vec2Like {
        return this.boxlocs.get(box_idx)!;
    }

    getOffset(w: number): number {
        return this.wireoffsets.get(w)!;
    }

    /**
     * Wrappers around the functions on the underlying semagram that also deal with location
     * These should be preferred over accessing the semagram directly.
     * I don't know exactly how things will fail if you access the semagram directly,
     * but probably something will go wrong as things come out of sync.
     */
    addBox(ty: string, loc: Vec2Like): Attachment {
        const a = this.sg.addBox(ty);
        this.portlocs.set(a.box_idx, new Map());
        this.setBoxLoc(a.box_idx, loc);
        return a;
    }

    addPort(ty: string, box_idx: number): Attachment {
        const a = this.sg.addPort(ty, box_idx);
        this.updatePortLocs(box_idx);
        return a;
    }

    addWire(ty: string, src: Attachment, tgt: Attachment): number | undefined {
        const i = this.sg.addWire(ty, src, tgt);
        this.updateWireOffsets(src, tgt);
        return i;
    }

    remWire(i: number) {
        const wire = this.sg.getWire(i);
        if (wire) {
            this.sg.remWire(i);
            this.updateWireOffsets(wire.src, wire.tgt);
        }
    }

    remPort(box_idx: number, port_idx: number) {
        this.sg.remPort(box_idx, port_idx);
        this.portlocs.get(box_idx)!.delete(port_idx);
        this.updatePortLocs(box_idx);
    }

    remBox(box_idx: number) {
        this.boxlocs.delete(box_idx);
        this.portlocs.delete(box_idx);
        this.sg.remBox(box_idx);
    }

    remEntity(a: Entity) {
        switch (a.ty) {
            case (EntityType.Box): {
                this.remBox(a.box_idx);
                break;
            }
            case (EntityType.Port): {
                this.remPort(a.box_idx, (a as PortEntity).port_idx);
                break;
            }
            case (EntityType.Wire): {
                this.remWire(a.wire_idx);
                break;
            }
        }
    }

    setHom(ty: string, src: Entity, tgt: Entity) {
        this.sg.getEntity(src)!.homs[ty] = tgt;
    }

    export(): ExportedLocatedSemagram {
        return {
            sg: this.sg.export(),
            boxlocs: Array.from(this.boxlocs.entries())
        }
    }

    static fromExported(e: ExportedLocatedSemagram): LocatedSemagram {
        const ls = new LocatedSemagram(e.sg.schema);
        ls.sg = Semagram.fromExported(e.sg);
        ls.boxlocs = new Map(e.boxlocs);
        ls.updateAllPortLocs();
        ls.updateAllWireOffsets();
        return ls;
    }
}

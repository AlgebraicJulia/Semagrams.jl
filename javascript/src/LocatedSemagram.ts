import { Semagram, Attachment, BoxAttachment, PortAttachment } from "./Semagram";
import { Schema, AttachType, PortStyle } from "./Schema";
import { Vec2Like, add2 } from "@thi.ng/vectors";
import { centerIndex } from "./Util";

export const BOXRADIUS = 40;

export class LocatedSemagram {
    sg: Semagram
    boxlocs: Map<number, Vec2Like>
    portlocs: Map<number, Map<number, Vec2Like>>

    constructor(schema: Schema) {
        this.sg = new Semagram(schema);
        this.boxlocs = new Map();
        this.portlocs = new Map();
    }

    setBoxLoc(box_idx: number, loc: Vec2Like) {
        this.boxlocs.set(box_idx, loc);
        this.updatePortLocs(box_idx);
    }

    updatePortLocs(box_idx: number) {
        const box = this.sg.boxes.get(box_idx)!;
        const boxportlocs = this.portlocs.get(box_idx)!;
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

    getLoc(a: Attachment): Vec2Like | undefined {
        const boxloc = this.boxlocs.get(a.box_idx)!;
        switch (a.ty) {
            case AttachType.Box: {
                return boxloc;
            }
            case AttachType.Port: {
                const portloc = this.portlocs.get(a.box_idx)!.get(a.port_idx)!;
                return add2([], boxloc, portloc) as Vec2Like;
            }
        }
    }

    getBoxLoc(box_idx: number): Vec2Like {
        return this.boxlocs.get(box_idx)!;
    }

    addBox(ty: string, color: string | undefined, loc: Vec2Like): Attachment {
        const a = this.sg.addBox(ty, color);
        this.portlocs.set(a.box_idx, new Map());
        this.setBoxLoc(a.box_idx, loc);
        return a;
    }

    addPort(ty: string, box_idx: number, color: string | undefined): Attachment {
        const a = this.sg.addPort(ty, box_idx, color);
        this.updatePortLocs(box_idx);
        return a;
    }

    addWire(ty: string, src: Attachment, tgt: Attachment): number | undefined {
        return this.sg.addWire(ty, src, tgt);
    }

    remWire(i: number) {
        this.sg.remWire(i);
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

    remAttachment(a: Attachment) {
        switch (a.ty) {
            case (AttachType.Box): {
                this.remBox(a.box_idx);
                break;
            }
            case (AttachType.Port): {
                this.remPort(a.box_idx, (a as PortAttachment).port_idx);
                break;
            }
        }
    }
}

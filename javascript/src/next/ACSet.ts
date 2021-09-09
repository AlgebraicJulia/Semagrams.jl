
export interface Schema {
    obs: Array<string>,
    homs: Array<string>,
    attrtypes: Array<string>,
    attrs: Array<string>,
    doms: Record<string, string>
    codoms: Record<string, string>
}

enum Indexed {
    Yes,
    No
}

interface UnindexedFinDomFunction {
    ty: Indexed.No
    vals: Map<number, any>
}

interface IndexedFinDomFunction {
    ty: Indexed.Yes
    vals: Map<number, any>
    index: Map<any, Set<number>>
}

export type FinDomFunction = UnindexedFinDomFunction | IndexedFinDomFunction;

function create_findomfunction(indexed: Indexed): FinDomFunction {
    switch (indexed) {
        case Indexed.Yes: {
            return {
                ty: Indexed.Yes,
                vals: new Map(),
                index: new Map()
            }
        }
        case Indexed.No: {
            return {
                ty: Indexed.No,
                vals: new Map(),
            }
        }
    }
}

function add_to_key(index: Map<any, Set<number>>, x: number, v: any) {
    const s = index.get(v);
    if (s) {
        s.add(x);
    } else {
        index.set(v, new Set<number>([x]));
    }
}

function set(f: FinDomFunction, x: number, v: any) {
    f.vals.set(x, v);
    if (f.ty == Indexed.Yes) {
        add_to_key(f.index, x, v);
    }
}

function get(f: FinDomFunction, x: number): any {
    return f.vals.get(x);
}

function incident(f: FinDomFunction, v: any) {
    switch (f.ty) {
        case Indexed.Yes: {
            return f.index.get(v) || new Set<number>();
        }
        case Indexed.No: {
            return new Set<number>([...f.vals.keys()].filter(k => f.vals.get(k) == v));
        }
    }
}

function remove(f: FinDomFunction, x: number) {
    const oldval = f.vals.get(x);
    f.vals.delete(x);
    if (f.ty == Indexed.Yes) {
        if (oldval != undefined) {
            f.index.get(oldval).delete(x);
        }
    }
}

export interface Entity {
    ty: string,
    id: number
}

class IDGen {
    private i: number

    constructor() {
        this.i = 0;
    }

    fresh() {
        return this.i++;
    }
}

export class ACSet {
    public schema: Schema
    private idgen: IDGen
    private parts: Record<string, Set<number>>
    private subparts: Record<string, FinDomFunction>

    constructor(s: Schema, indexed: Map<string, Indexed>) {
        this.schema = s;
        this.idgen = new IDGen();
        this.parts = {};
        for (const x of s.obs) {
            this.parts[x] = new Set();
        }
        this.subparts = {};
        for (const f of s.homs) {
            this.subparts[f] = create_findomfunction(indexed[f]);
        }
    }

    add_entity(ty: string): Entity {
        const id = this.idgen.fresh();
        this.parts[ty].add(id);
        return { ty, id };
    }

    add_entities(ty: string, n: number): Array<Entity> {
        const ents = [];
        for (var i = 0; i < n; i++) {
            ents.push(this.add_entity(ty));
        }
        return ents;
    }

    set_subpart(e: Entity, f: string, v: any): void {
        if (this.schema.doms[f] != e.ty) {
            throw `entity has no part $f`
        }
        set(this.subparts[f], e.id, v);
    }

    subpart(e: Entity, f: string): any {
        if (this.schema.doms[f] != e.ty) {
            throw `entity has no part $f`
        }
        return get(this.subparts[f], e.id);
    }

    rem_entity(e: Entity): void {
        this.parts[e.ty].delete(e.id);
        for (const f of this.schema.homs) {
            if (this.schema.doms[f] == e.ty) {
                remove(this.subparts[f], e.id);
            }
        }
        for (const f of this.schema.attrs) {
            if (this.schema.doms[f] == e.ty) {
                remove(this.subparts[f], e.id);
            }
        }
    }
}

import { HashMap, SortedSet } from "@thi.ng/associative";

/** Linearly interpolates between [0...n-1] and [-1,1] */
export function centerIndex(i: number, n: number) {
    return (i - ((n - 1) / 2)) / n;
}

export class SetMap<K, V> {
    private kvs: HashMap<K, SortedSet<V>>
    private cmp: (v1: V, v2: V) => number

    constructor(hash: (k: K) => number, cmp: (v1: V, v2: V) => number) {
        this.kvs = new HashMap([], { hash });
        this.cmp = cmp;
    }

    get(k: K) {
        return this.kvs.get(k);
    }

    add(k: K, v: V) {
        if (!(this.kvs.has(k))) {
            this.kvs.set(k, new SortedSet<V>([], { compare: this.cmp }));
        }
        this.kvs.get(k)!.add(v);
    }

    delete(k: K, v: V) {
        this.kvs.get(k)!.delete(v);
    }

    has(k: K, v: V) {
        if (this.kvs.has(k)) {
            return this.kvs.get(k)!.has(v);
        } else {
            return false;
        }
    }
}

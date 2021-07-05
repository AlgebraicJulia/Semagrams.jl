import { Vec2Like } from '@thi.ng/vectors';

export class AffineTrans {
    constructor(
        public zoom: number,
        public translate: Vec2Like
    ) { }

    svgExport() {
        return `matrix(${this.zoom} 0 0 ${this.zoom} ${this.translate[0]} ${this.translate[1]})`
    }

    apply(v: Vec2Like): Vec2Like {
        return [v[0] * this.zoom + this.translate[0], v[1] * this.zoom + this.translate[1]];
    }

    apply_inv(w: Vec2Like): Vec2Like {
        return [(w[0] - this.translate[0]) / this.zoom, (w[1] - this.translate[1]) / this.zoom];
    }

    zoomby(factor: number) {
        this.zoom *= factor;
    }

    zoomFrom(q: Vec2Like, factor: number) {
        const newzoom = this.zoom * factor;
        this.translate[0] = (this.zoom - newzoom) * q[0] + this.translate[0];
        this.translate[1] = (this.zoom - newzoom) * q[1] + this.translate[1];
        this.zoom = newzoom;
    }
}

import { add2, dist2, mulN2, rotate, sub2, Vec, Vec2Like, ZERO2 } from "@thi.ng/vectors";

const WIREOFFSETCONSTANT = 60;

/**
 * This stringifies our representation of SVG paths.
 * This should probably be factored out of this file, into `SVGUtils.ts` file
 * or something.
 */
export function svgPath(segs: Array<[string, Array<Vec>]>) {
    return segs.map(seg => {
        const pts = seg[1].map(p => [...p].join(",")).join(" ");
        return `${seg[0]} ${pts}`
    }).join(" ");
}

/**
 * This computes the SVG spec for the points of the spline, so that
 * when we have multiple wires going between two attachments, they can curve away from each other.
 */
export function curvePoints(p1: Vec2Like, p2: Vec2Like, offset: number): Array<[string, Array<Vec>]> {
    offset = WIREOFFSETCONSTANT * offset;
    const c = mulN2([], add2([], p1, p2), 1 / 2);
    const c1 = mulN2([], add2([], p1, c), 1 / 2);
    const c2 = mulN2([], add2([], c, p2), 1 / 2);
    const dp = sub2([], p2, p1);
    const dphat = mulN2([], dp, 1 / dist2(dp, ZERO2));
    const dqhat = rotate([], dphat, Math.PI / 2);
    const c1up = add2([], c1, mulN2([], dqhat, offset));
    const c2up = add2([], c2, mulN2([], dqhat, offset));
    const cup = add2([], c, mulN2([], dqhat, offset));
    return [
        ["M", [p1]],
        ["C", [c1up, c1up, cup]],
        ["C", [c2up, c2up, p2]]
    ];
}

export function midpoint(p1: Vec2Like, p2: Vec2Like, offset: number): Vec {
    offset = WIREOFFSETCONSTANT * offset;
    const c = mulN2([], add2([], p1, p2), 1 / 2);
    const dp = sub2([], p2, p1);
    const dphat = mulN2([], dp, 1 / dist2(dp, ZERO2));
    const dqhat = rotate([], dphat, Math.PI / 2);
    const cup = add2([], c, mulN2([], dqhat, offset));
    return cup;
}

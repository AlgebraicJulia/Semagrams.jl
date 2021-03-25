export function equal(x: any, y: any) {
    if (typeof x == "object" && typeof y == "object") {
        const kx = Object.keys(x).sort();
        const ky = Object.keys(y).sort();
        if (kx.length != ky.length) {
            return false;
        }
        for (var i = 0; i < kx.length; i++) {
            if (kx[i] != ky[i]) {
                return false;
            }
        }
        for (const k in kx) {
            if (!equal(x[k], y[k])) {
                return false;
            }
        }
    } else {
        return x === y;
    }
    return true;
}

/** Linearly interpolates between [0...n-1] and [-1,1] */
export function centerIndex(i:number, n:number) {
    return (i - ((n - 1) / 2)) / n;
}

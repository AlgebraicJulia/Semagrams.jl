// import { Wires, Point, GraphSchema } from "../src/Wires";

// describe("Wired", () => {
//     it("sanity test", () => {
//         const wired = new Wired(GraphSchema);
//         wired.addBox("vertex", new Point(3., 4.), undefined);
//         expect(wired.boxes.length).toBe(1);
//     });

//     it("removing edges", () => {
//         const wired = new Wired(GraphSchema);
//         const b1 = wired.addBox("vertex", new Point(0., 0.), undefined);
//         const b2 = wired.addBox("vertex", new Point(0., 0.), undefined);
//         const b3 = wired.addBox("vertex", new Point(0., 0.), undefined);
//         const e1 = wired.addEdge("edge", b1, b2);
//         const e2 = wired.addEdge("edge", b2, b3);
//         expect(wired.inNeighbors(b2).has(e1)).toBe(true);
//         expect(wired.outNeighbors(b2).has(e2)).toBe(true);
//         wired.remEdge(e1);
//         // e2 should have moved down to 0
//         expect(wired.outNeighbors(b2).has(0)).toBe(true);
//         expect(wired.inNeighbors(b3).has(0)).toBe(true);

//         wired.remEdge(0);

//         expect(wired.outNeighbors(b2).has(0)).toBe(false);
//         expect(wired.inNeighbors(b3).has(0)).toBe(false);
//     })
// });

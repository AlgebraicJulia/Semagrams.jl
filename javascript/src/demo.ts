import { main } from './index';
import blank_dpg from './blank_dpg.json';
// import blank_petri from './blank_petri.json';
import { ExportedLocatedSemagram } from './LocatedSemagram';

main(blank_dpg as unknown as ExportedLocatedSemagram,
    { state: null, element: document.getElementById("semagrams-demo") },
    (_state: any) => { },
    (_state: any) => { })

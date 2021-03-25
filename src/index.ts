import m from 'mithril';
import { empty_graph } from './Wired'
import { Editor } from './Editor';

const Home: m.Component<any, any> = {
    view({ }) {
        return m('.page',
            m('h1', "Wired"),
            m('p', "A totally wired editor"),
            m(Editor, { initialWired: empty_graph })
        )
    }
}

function main(): void {
    m.route(document.body, '/', {
        '/': Home
    });
}

main();

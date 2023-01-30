import { App } from './out/apps/simplepetri/fullLinkJS.dest/main.js'

const sir = `{"S": [{"sname": "S"}, {"sname": "I"}, {"sname": "R"}], "T": [{"tname": "inf"}, {"tname": "rec"}], "I": [{"it": 1, "is": 1}, {"it": 1, "is": 2}, {"it": 2, "is": 2}], "O": [{"ot": 1, "os": 2}, {"ot": 1, "os": 2}, {"ot": 2, "os": 3}]}`

App.main(document.getElementById("app-container"), sir)

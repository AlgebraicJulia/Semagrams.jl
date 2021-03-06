export enum Command {
    SetSrc,
    SetTgt,
    AddBox,
    AddPort,
    AddWire,
    RemHovered,
    SetHom,
    Export,
    Debug,
    Deselect,
    Help,
    Roundtrip,
    ZoomIn,
    ZoomOut,
}

export const DEFAULT_KEYBINDINGS: Map<string, Command> = new Map([
    ["s", Command.SetSrc],
    ["t", Command.SetTgt],
    ["b", Command.AddBox],
    ["p", Command.AddPort],
    ["w", Command.AddWire],
    ["d", Command.RemHovered],
    ["h", Command.SetHom],
    ["e", Command.Export],
    ["D", Command.Debug],
    ["Escape", Command.Deselect],
    ["?", Command.Help],
    ["R", Command.Roundtrip],
    ["+", Command.ZoomIn],
    ["-", Command.ZoomOut]
])

export const COMMAND_DOCS: Map<Command, { long: string, short: string }> =
    new Map([
        [Command.SetSrc, {
            short: "set src",
            long: "sets the current source to whatever the cursor is hovering over"
        }],
        [Command.SetTgt, {
            short: "set tgt",
            long: "sets the current target to whatever the cursor is hovering over"
        }],
        [Command.AddBox, {
            short: "add box",
            long: "adds a box at the location of the cursor"
        }],
        [Command.AddPort, {
            short: "add port",
            long: "adds a port to the box that is hovered over"
        }],
        [Command.AddWire, {
            short: "add wire",
            long: "adds a wire between the current selected source and the current selected target"
        }],
        [Command.RemHovered, {
            short: "delete",
            long: "removes whatever the cursor is hovering over"
        }],
        [Command.SetHom, {
            short: "set hom",
            long: "connects the source and target with a hom relation"
        }],
        [Command.Export, {
            short: "export",
            long: "exports the current picture as an svg"
        }],
        [Command.Debug, {
            short: "debug",
            long: "prints internal state to the console"
        }],
        [Command.Deselect, {
            short: "deselect",
            long: "removes the selection of current source/target"
        }],
        [Command.Help, {
            short: "help",
            long: "toggles the help bar"
        }],
        [Command.Roundtrip, {
            short: "roundtrip",
            long: "exports and imports the current data (testing loading and saving)"
        }],
        [Command.ZoomIn, {
            short: "zoom in",
            long: "zooms in"
        }],
        [Command.ZoomOut, {
            short: "zoom out",
            long: "zooms out"
        }]
    ])


export enum Command {
    SetSrc,
    SetTgt,
    AddBox,
    AddPort,
    AddWire,
    RemHovered,
    Debug,
    Deselect,
    Help
}

export const DEFAULT_KEYBINDINGS: Map<string, Command> = new Map([
    ["s", Command.SetSrc],
    ["t", Command.SetTgt],
    ["b", Command.AddBox],
    ["p", Command.AddPort],
    ["w", Command.AddWire],
    ["d", Command.RemHovered],
    ["D", Command.Debug],
    ["Escape", Command.Deselect],
    ["?", Command.Help]
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
        }]
    ])


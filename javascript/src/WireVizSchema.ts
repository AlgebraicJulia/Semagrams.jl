export enum BoxStyle {
    Square = "Square",
    Circle = "Circle"
}

export enum PortStyle {
    Input = "Input",
    Output = "Output",
    Circular = "Circular"
}

export enum WireStyle {
    Default = "DefaultWire"
}

export interface BoxProperties {
    weights: Array<string>,
    shape: string,
    label: string
}

export enum AttachType {
    Box = "AttachBox",
    Port = "AttachPort"
}

export interface PortProperties {
    weights: Array<string>
    box: string
    box_map: string
    style: PortStyle
}

export interface WireProperties {
    weights: Array<string>
    src: [AttachType, string]
    src_map: string
    tgt: [AttachType, string]
    tgt_map: string
    style: WireStyle
}

export interface WireVizSchema {
    box_types: Record<string, BoxProperties>,
    port_types: Record<string, PortProperties>,
    wire_types: Record<string, WireProperties>
}

/**
 * TODO: Style should be replaced by custom SVGs
 */
export enum PortStyle {
    Input = "Input",
    Output = "Output",
    Circular = "Circular"
}

export enum WireStyle {
    Default = "DefaultWire"
}

/**
 * These are all interfaces (i.e., not structs) because 
 */

export interface BoxProperties {
    /** These are the names of weights.
     *  TODO: should also have the types of the weights
     */
    weights: Array<string>,

    /** This is a svg string */
    shape: string,

    /** This records which weight is a label */
    label: string
}

export enum AttachType {
    Box = "AttachBox",
    Port = "AttachPort"
}

export interface PortProperties {
    /** See comment for box weights */
    weights: Array<string>

    /** The box type that this attaches to */
    box: string

    /** The name of the attaching map */
    box_map: string

    /** TODO: this should be a custom SVG */
    style: PortStyle
}

export interface WireProperties {
    /** See comment for box weights */
    weights: Array<string>

    /** The port/box that is the type for the source of this wire */
    src: [AttachType, string]

    /** The name of the map giving the source */
    src_map: string

    /** The port/box that is the type for the target of this wire */
    tgt: [AttachType, string]

    /** The name of the map giving the target */
    tgt_map: string

    /**  TODO: this should be a custom SVG */
    style: WireStyle
}

/**
 * Ports/wires refer to boxes/ports using the keys in these records.
 */
export interface Schema {
    box_types: Record<string, BoxProperties>,
    port_types: Record<string, PortProperties>,
    wire_types: Record<string, WireProperties>
}

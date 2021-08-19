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

export enum AttributeType {
    Numeric = "Numeric",
    Stringlike = "Stringlike"
}

export enum EntityType {
    Box = "Box",
    Port = "Port",
    Wire = "Wire"
}

export interface OutgoingHom {
    name: string,
    codom: string,
    codom_ty: EntityType
}

/**
 * These are all interfaces (i.e., not structs) because
 */

export interface BoxProperties {
    /** These are the names of weights.
     */
    weights: Array<[AttributeType, string]>

    homs: Array<OutgoingHom>

    /** This is a svg string */
    shape: string,

    /** This records which weight is a label */
    label: string | undefined
}


export type AttachType = EntityType.Box | EntityType.Port

export interface PortProperties {
    /** See comment for box weights */
    weights: Array<[AttributeType, string]>

    homs: Array<OutgoingHom>

    /** The box type that this attaches to */
    box: string

    /** The name of the attaching map */
    box_map: string

    /** TODO: this should be a custom SVG */
    style: PortStyle
}

export interface WireProperties {
    /** See comment for box weights */
    weights: Array<[AttributeType, string]>

    homs: Array<OutgoingHom>

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

export enum BoxStyle {
    Square,
    Circular
}

export enum PortStyle {
    Input,
    Output,
    Circular
}

export class BoxProperties {
    constructor(
        readonly weights: Array<string>,
        readonly style: BoxStyle
    ) { }
}

export enum AttachType {
    Port,
    Box
}

export class PortProperties {
    constructor(
        readonly weights: Array<string>,
        readonly box: string,
        readonly style: PortStyle,
    ) { }
}

export class WireProperties {
    constructor(
        readonly weights: Array<string>,
        readonly src: [AttachType, string],
        readonly tgt: [AttachType, string],
    ) { }
}

export class WiresSchema {
    constructor(
        readonly box_types: Record<string, BoxProperties>,
        readonly port_types: Record<string, PortProperties>,
        readonly wire_types: Record<string, WireProperties>
    ) { }
}

export const DirectedPortGraphSchema = new WiresSchema(
    {
        "box": new BoxProperties(
            [],
            BoxStyle.Square
        )
    },
    {
        "input": new PortProperties([], "box", PortStyle.Input),
        "output": new PortProperties([], "box", PortStyle.Output)
    },
    {
        "wire": new WireProperties(
            [],
            [AttachType.Port, "output"],
            [AttachType.Port, "input"])
    }
)

export const PetriSchema = new WiresSchema(
    {
        "species": new BoxProperties(
            [],
            BoxStyle.Circular,
        ),
        "transition": new BoxProperties(
            [],
            BoxStyle.Square,
        )
    },
    {},
    {
        "input": new WireProperties(
            [],
            [AttachType.Box, "species"],
            [AttachType.Box, "transition"]
        ),
        "output": new WireProperties(
            [],
            [AttachType.Box, "transition"],
            [AttachType.Box, "species"],
        )
    }
)

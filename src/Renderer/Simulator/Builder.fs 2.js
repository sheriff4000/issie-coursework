import { value } from "../fable_modules/fable-library.4.0.0-theta-018/Option.js";
import { toText, printf, toFail } from "../fable_modules/fable-library.4.0.0-theta-018/String.js";
import { append, indexed, tryFind, item, filter, replicate, collect, map as map_2, singleton, fold, empty, cons, head, tail, isEmpty } from "../fable_modules/fable-library.4.0.0-theta-018/List.js";
import { tryFind as tryFind_1, FSharpMap__get_Item, ofList, empty as empty_1, FSharpMap__Add, FSharpMap__TryFind } from "../fable_modules/fable-library.4.0.0-theta-018/Map.js";
import { stringHash, equals, compare } from "../fable_modules/fable-library.4.0.0-theta-018/Util.js";
import { SimulationComponent, Bit, SimulationComponentState } from "./SimulatorTypes.fs.js";
import { convertIntToFastData } from "./NumberHelpers.fs.js";
import { Connection, ComponentType } from "../Common/CommonTypes.fs.js";
import { List_groupBy } from "../fable_modules/fable-library.4.0.0-theta-018/Seq2.js";
import { analyseState } from "./CanvasStateAnalyser.fs.js";
import { FSharpResult$2 } from "../fable_modules/fable-library.4.0.0-theta-018/Choice.js";

function getPortNumberOrFail(port) {
    if (port != null) {
        const p = value(port);
        return p;
    }
    else {
        return toFail(printf("what? Component ports should always have a portNumber"));
    }
}

function getValuesForPorts(inputs, portNumbers) {
    if (!isEmpty(portNumbers)) {
        const portNumbers$0027 = tail(portNumbers);
        const portNumber = head(portNumbers);
        const matchValue = FSharpMap__TryFind(inputs, portNumber);
        if (matchValue != null) {
            const wireData = matchValue;
            const matchValue_1 = getValuesForPorts(inputs, portNumbers$0027);
            if (matchValue_1 != null) {
                const values = matchValue_1;
                return cons(wireData, values);
            }
            else {
                return void 0;
            }
        }
        else {
            return void 0;
        }
    }
    else {
        return empty();
    }
}

function getReducer(componentType, _arg) {
    return toFail(printf("Reducer function is legacy code and should never be called!"));
}

function buildSourceToTargetPortMap(connections) {
    return fold((map, conn) => {
        const key = conn.Source.Id;
        const target = [conn.Target.HostId, conn.Target.Id];
        let newValue;
        const matchValue = FSharpMap__TryFind(map, key);
        if (matchValue != null) {
            const oldValue = matchValue;
            newValue = cons(target, oldValue);
        }
        else {
            newValue = singleton(target);
        }
        return FSharpMap__Add(map, key, newValue);
    }, empty_1({
        Compare: compare,
    }), connections);
}

function mapInputPortIdToPortNumber(components) {
    return fold((map, comp) => fold((map_1, port) => FSharpMap__Add(map_1, port.Id, getPortNumberOrFail(port.PortNumber)), map, comp.InputPorts), empty_1({
        Compare: compare,
    }), components);
}

function getDefaultState(compType) {
    let matchResult, w, memory;
    switch (compType.tag) {
        case 50:
        case 48: {
            matchResult = 0;
            break;
        }
        case 0: {
            matchResult = 1;
            break;
        }
        case 1:
        case 2:
        case 4:
        case 7:
        case 5:
        case 6:
        case 10:
        case 11:
        case 12:
        case 13:
        case 14:
        case 15:
        case 16:
        case 18:
        case 19:
        case 20:
        case 17:
        case 32:
        case 21:
        case 22:
        case 23:
        case 24:
        case 31:
        case 28:
        case 29:
        case 30:
        case 33:
        case 34:
        case 35:
        case 45:
        case 3:
        case 25:
        case 26:
        case 27:
        case 51: {
            matchResult = 2;
            break;
        }
        case 9:
        case 8: {
            matchResult = 3;
            break;
        }
        case 44: {
            matchResult = 4;
            break;
        }
        case 36:
        case 37: {
            matchResult = 5;
            break;
        }
        case 38: {
            matchResult = 6;
            w = compType.fields[0];
            break;
        }
        case 39: {
            matchResult = 6;
            w = compType.fields[0];
            break;
        }
        case 40: {
            matchResult = 6;
            w = compType.fields[0];
            break;
        }
        case 42: {
            matchResult = 6;
            w = compType.fields[0];
            break;
        }
        case 41: {
            matchResult = 6;
            w = compType.fields[0];
            break;
        }
        case 43: {
            matchResult = 6;
            w = compType.fields[0];
            break;
        }
        case 46: {
            matchResult = 7;
            memory = compType.fields[0];
            break;
        }
        case 47: {
            matchResult = 7;
            memory = compType.fields[0];
            break;
        }
        default: matchResult = 0}
    switch (matchResult) {
        case 0: {
            return toFail(printf("What? Legacy RAM component types should never occur"));
        }
        case 1: {
            return toFail(printf("Legacy Input component types should never occur"));
        }
        case 2: {
            return new SimulationComponentState(0, []);
        }
        case 3: {
            return new SimulationComponentState(0, []);
        }
        case 4: {
            return new SimulationComponentState(0, []);
        }
        case 5: {
            return new SimulationComponentState(1, [0]);
        }
        case 6: {
            return new SimulationComponentState(2, [convertIntToFastData(w, 0)]);
        }
        case 7: {
            return new SimulationComponentState(3, [memory]);
        }
    }
}

function buildSimulationComponent(sourceToTargetPort, portIdToPortNumber, comp) {
    const mapPortIdsToPortNumbers = (targets) => map_2((tupledArg) => {
        const compId = tupledArg[0];
        const portId = tupledArg[1];
        const matchValue = FSharpMap__TryFind(portIdToPortNumber, portId);
        if (matchValue != null) {
            const portNumber = value(matchValue);
            return [compId, portNumber];
        }
        else {
            return toFail(printf("what? Input port with portId %A has no portNumber associated"))(portId);
        }
    }, targets);
    const outputs = ofList(collect((port) => {
        const matchValue_1 = FSharpMap__TryFind(sourceToTargetPort, port.Id);
        if (matchValue_1 != null) {
            const targets_1 = matchValue_1;
            return singleton([getPortNumberOrFail(port.PortNumber), mapPortIdsToPortNumbers(targets_1)]);
        }
        else if (equals(comp.Type, new ComponentType(4, []))) {
            return empty();
        }
        else {
            return toFail(printf("what? Unconnected output port %s in comp %s"))(port.Id)(comp.Id);
        }
    }, comp.OutputPorts), {
        Compare: compare,
    });
    let inputs;
    const matchValue_2 = comp.Type;
    if (matchValue_2.tag === 2) {
        const width = matchValue_2.fields[0] | 0;
        inputs = FSharpMap__Add(empty_1({
            Compare: compare,
        }), 0, replicate(width, new Bit(0, [])));
    }
    else {
        inputs = empty_1({
            Compare: compare,
        });
    }
    return new SimulationComponent(comp.Id, comp.Type, comp.Label, inputs, outputs, void 0, getDefaultState(comp.Type));
}

export function getLabelConnections(comps, conns) {
    const labels = filter((co) => equals(co.Type, new ComponentType(4, [])), comps);
    const compIdMap = ofList(map_2((co_1) => [co_1.Id, co_1], labels), {
        Compare: compare,
    });
    const getComp = (n) => FSharpMap__get_Item(compIdMap, n);
    const targetMap = ofList(map_2((conn) => [conn.Target.HostId, conn], conns), {
        Compare: compare,
    });
    const getConnection = (compTarget) => FSharpMap__get_Item(targetMap, compTarget.Id);
    const copyConnection = (conn_1, compTarget_1, tagNum) => {
        const Target = item(0, compTarget_1.InputPorts);
        return new Connection(toText(printf("iolab%d"))(tagNum) + conn_1.Id, conn_1.Source, Target, conn_1.Vertices);
    };
    const getDriverConnection = (comps_1) => {
        const _arg = tryFind((co_2) => (!equals(tryFind_1(co_2.Id, targetMap), void 0)), comps_1);
        if (_arg != null) {
            const comp = _arg;
            return FSharpMap__get_Item(targetMap, comp.Id);
        }
        else {
            return toFail(printf("What? component cannot be found in %A"))(targetMap);
        }
    };
    return collect((tupledArg) => {
        const lab = tupledArg[0];
        const lst = tupledArg[1];
        const dConn = getDriverConnection(lst);
        return map_2((tupledArg_1) => {
            const i = tupledArg_1[0] | 0;
            const co_5 = tupledArg_1[1];
            return copyConnection(dConn, co_5, i);
        }, indexed(filter((co_4) => (co_4.Id !== dConn.Target.HostId), lst)));
    }, List_groupBy((co_3) => co_3.Label, labels, {
        Equals: (x_2, y_2) => (x_2 === y_2),
        GetHashCode: stringHash,
    }));
}

function buildSimulationGraph(canvasState_, canvasState__1) {
    const canvasState = [canvasState_, canvasState__1];
    const connections$0027 = canvasState[1];
    const components = canvasState[0];
    const labConns = getLabelConnections(components, connections$0027);
    const connections = append(labConns, connections$0027);
    const sourceToTargetPort = buildSourceToTargetPortMap(connections);
    const portIdToPortNumber = mapInputPortIdToPortNumber(components);
    const mapper = (comp) => buildSimulationComponent(sourceToTargetPort, portIdToPortNumber, comp);
    return ofList(map_2((comp_1) => [comp_1.Id, mapper(comp_1)], components), {
        Compare: compare,
    });
}

export function runCanvasStateChecksAndBuildGraph(canvasState_, canvasState__1, loadedComponents) {
    const canvasState = [canvasState_, canvasState__1];
    const matchValue = analyseState(canvasState[0], canvasState[1], loadedComponents);
    if (matchValue == null) {
        return new FSharpResult$2(0, [buildSimulationGraph(canvasState[0], canvasState[1])]);
    }
    else {
        const err = matchValue;
        return new FSharpResult$2(1, [err]);
    }
}

//# sourceMappingURL=Builder.fs.js.map

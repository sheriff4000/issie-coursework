import { containsKey, tryFind, add, fold as fold_1, toArray as toArray_2, FSharpMap__get_Item, iterate as iterate_1, FSharpMap__get_Count, ofList, toList as toList_1, empty } from "../../fable_modules/fable-library.4.0.0-theta-018/Map.js";
import { uncurry, equals, equalArrays, createAtom, comparePrimitives, compareArrays, compare } from "../../fable_modules/fable-library.4.0.0-theta-018/Util.js";
import { GatherData__getSheetName_10524082, Driver, GatherData__getFullName_10524082, GatherTemp, FastComponent, extractLabel, SimulationComponentState, Bit, FData, StepArray$1, FastSimulation, GatherData } from "../SimulatorTypes.fs.js";
import { printf, toFail, toConsole } from "../../fable_modules/fable-library.4.0.0-theta-018/String.js";
import { toArray as toArray_3, iterate as iterate_2, indexed, getSlice, fold, find, mapIndexed, filter, append, singleton, collect, map as map_2, sumBy, replicate, length, cons, reverse, head, tail, isEmpty, empty as empty_1 } from "../../fable_modules/fable-library.4.0.0-theta-018/List.js";
import { iterateIndexed, append as append_1, concat, collect as collect_1, mapIndexed as mapIndexed_1, initialize, fill, map } from "../../fable_modules/fable-library.4.0.0-theta-018/Array.js";
import { emptyFastData } from "../NumberHelpers.fs.js";
import { iterate, toList, toArray } from "../../fable_modules/fable-library.4.0.0-theta-018/Seq.js";
import { rangeDouble } from "../../fable_modules/fable-library.4.0.0-theta-018/Range.js";
import { isIOLabel, isCustom, isInput, isOutput, getHybridComponentAsyncOuts, isHybridComponent, couldBeSynchronousComponent } from "../SynchronousUtils.fs.js";
import { value, some, defaultArg, toArray as toArray_1, map as map_1 } from "../../fable_modules/fable-library.4.0.0-theta-018/Option.js";
import { instrumentTime, instrumentInterval, getTimeMs } from "../../Common/TimeHelpers.fs.js";
import { PortType, WaveIndexT } from "../../Common/CommonTypes.fs.js";

export const emptyGather = (() => {
    const Labels = empty({
        Compare: compare,
    });
    return new GatherData(empty({
        Compare: compare,
    }), empty({
        Compare: compareArrays,
    }), empty({
        Compare: compareArrays,
    }), empty({
        Compare: compareArrays,
    }), Labels, empty({
        Compare: compareArrays,
    }));
})();

export function emptyFastSimulation(diagramName) {
    toConsole(`Creating empty simulation: ${diagramName}`);
    return new FastSimulation(0, -1, 0, new Array(0), new Array(0), new Array(0), new Array(0), empty({
        Compare: compareArrays,
    }), empty_1(), empty({
        Compare: compareArrays,
    }), empty({
        Compare: compareArrays,
    }), empty({
        Compare: compareArrays,
    }), empty({
        Compare: compareArrays,
    }), empty({
        Compare: compareArrays,
    }), emptyGather, 0, new Array(0), new Array(0), empty({
        Compare: compare,
    }), empty({
        Compare: comparePrimitives,
    }), empty_1(), diagramName);
}

export const simulationPlaceholder = emptyFastSimulation("");

export function getPathIds(cid, ap) {
    const getPath = (ap_1) => {
        if (!isEmpty(ap_1)) {
            const rest = tail(ap_1);
            const cid_1 = head(ap_1);
            return cons([cid_1, reverse(rest)], getPath(rest));
        }
        else {
            return empty_1();
        }
    };
    return reverse(getPath(reverse(ap)));
}

export function getFid(cid, ap) {
    const ff = (_arg) => {
        const Id = _arg;
        return Id;
    };
    return [cid, ap];
}

export function getPortNumbers(sc) {
    let patternInput;
    const matchValue = sc.Type;
    switch (matchValue.tag) {
        case 8:
        case 43: {
            patternInput = [0, 1];
            break;
        }
        case 1:
        case 2:
        case 3:
        case 7:
        case 5:
        case 6:
        case 10:
        case 36:
        case 38:
        case 4:
        case 45:
        case 44:
        case 30:
        case 32:
        case 41: {
            patternInput = [1, 1];
            break;
        }
        case 34:
        case 28:
        case 31:
        case 29:
        case 39:
        case 37:
        case 42: {
            patternInput = [2, 1];
            break;
        }
        case 35: {
            patternInput = [1, 2];
            break;
        }
        case 18:
        case 26:
        case 40: {
            patternInput = [3, 1];
            break;
        }
        case 19: {
            patternInput = [5, 1];
            break;
        }
        case 20: {
            patternInput = [9, 1];
            break;
        }
        case 24: {
            patternInput = [3, 2];
            break;
        }
        case 25: {
            patternInput = [2, 2];
            break;
        }
        case 27:
        case 51: {
            patternInput = [2, 1];
            break;
        }
        case 47:
        case 46: {
            patternInput = [2, 1];
            break;
        }
        case 17: {
            patternInput = [2, 4];
            break;
        }
        case 21: {
            patternInput = [2, 2];
            break;
        }
        case 22: {
            patternInput = [2, 4];
            break;
        }
        case 23: {
            patternInput = [2, 8];
            break;
        }
        case 11:
        case 12:
        case 13:
        case 14:
        case 15:
        case 16: {
            patternInput = [2, 1];
            break;
        }
        case 33: {
            const ct = matchValue.fields[0];
            patternInput = [length(ct.InputLabels), length(ct.OutputLabels)];
            break;
        }
        case 48:
        case 50:
        case 49: {
            patternInput = toFail(printf("legacy component type is not supported"));
            break;
        }
        case 0: {
            patternInput = toFail(printf("Legacy Input component types should never occur"));
            break;
        }
        default: {
            patternInput = [0, 1];
        }
    }
    const outs = patternInput[1] | 0;
    const ins = patternInput[0] | 0;
    return [ins, outs];
}

export function getOutputWidths(sc, wa) {
    const putW0 = (w) => {
        wa[0] = w;
    };
    const putW1 = (w_1) => {
        wa[1] = w_1;
    };
    const putW2 = (w_2) => {
        wa[2] = w_2;
    };
    const putW3 = (w_3) => {
        wa[3] = w_3;
    };
    const matchValue = sc.Type;
    let matchResult, w_4, w_5, mem;
    switch (matchValue.tag) {
        case 50:
        case 48: {
            matchResult = 0;
            break;
        }
        case 0: {
            matchResult = 1;
            break;
        }
        case 1: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 2: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 3: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 38: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 39: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 40: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 42: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 41: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 43: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 35: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 7: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 9: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 8: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 29: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 30: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 31: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 32: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 28: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 26: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 27: {
            matchResult = 2;
            w_4 = matchValue.fields[0];
            break;
        }
        case 24: {
            matchResult = 3;
            w_5 = matchValue.fields[0];
            break;
        }
        case 25: {
            matchResult = 3;
            w_5 = matchValue.fields[0];
            break;
        }
        case 10:
        case 11:
        case 12:
        case 13:
        case 14:
        case 15:
        case 16:
        case 5:
        case 6: {
            matchResult = 4;
            break;
        }
        case 44: {
            matchResult = 5;
            mem = matchValue.fields[0];
            break;
        }
        case 45: {
            matchResult = 5;
            mem = matchValue.fields[0];
            break;
        }
        case 46: {
            matchResult = 5;
            mem = matchValue.fields[0];
            break;
        }
        case 47: {
            matchResult = 5;
            mem = matchValue.fields[0];
            break;
        }
        case 33: {
            matchResult = 6;
            break;
        }
        case 36:
        case 37: {
            matchResult = 7;
            break;
        }
        case 51: {
            matchResult = 8;
            break;
        }
        case 17: {
            matchResult = 9;
            break;
        }
        case 21:
        case 22:
        case 23:
        case 18:
        case 19:
        case 20:
        case 4:
        case 34: {
            matchResult = 10;
            break;
        }
        default: matchResult = 0}
    switch (matchResult) {
        case 0: {
            toFail(printf("What? Legacy RAM component types should never occur"));
            break;
        }
        case 1: {
            toFail(printf("Legacy Input component types should never occur"));
            break;
        }
        case 2: {
            putW0(w_4);
            break;
        }
        case 3: {
            putW0(w_5);
            putW1(1);
            break;
        }
        case 4: {
            putW0(1);
            break;
        }
        case 5: {
            putW0(mem.WordWidth);
            break;
        }
        case 7: {
            putW0(1);
            break;
        }
        case 8: {
            const n = matchValue.fields[0] | 0;
            putW0(n);
            break;
        }
        case 9: {
            putW0(1);
            putW1(1);
            putW2(1);
            putW3(1);
            break;
        }
    }
    return wa;
}

export let stepArrayIndex = createAtom(-1);

export function makeStepArray(arr) {
    stepArrayIndex(stepArrayIndex() + 1);
    return new StepArray$1(arr, stepArrayIndex());
}

export function createFastComponent(maxArraySize, sComp, accessPath) {
    const patternInput = getPortNumbers(sComp);
    const outPortNum = patternInput[1] | 0;
    const inPortNum = patternInput[0] | 0;
    const ins = map(makeStepArray, map((n) => fill(new Array(maxArraySize), 0, maxArraySize, new FData(0, [emptyFastData])), toArray(rangeDouble(0, 1, inPortNum - 1)), null), null);
    const outs = map(makeStepArray, map((n_1) => fill(new Array(maxArraySize), 0, maxArraySize, new FData(0, [emptyFastData])), toArray(rangeDouble(0, 1, outPortNum - 1)), null), null);
    let inps;
    let dat;
    const matchValue = sComp.Type;
    let matchResult;
    if (isEmpty(accessPath)) {
        if (matchValue.tag === 1) {
            matchResult = 0;
        }
        else {
            matchResult = 1;
        }
    }
    else {
        matchResult = 1;
    }
    switch (matchResult) {
        case 0: {
            const width = matchValue.fields[0] | 0;
            const defaultVal = matchValue.fields[1];
            dat = replicate(width, new Bit(0, []));
            break;
        }
        case 1: {
            dat = empty_1();
            break;
        }
    }
    inps = map((i) => fill(new Array(maxArraySize), 0, maxArraySize, dat), toArray(rangeDouble(0, 1, inPortNum - 1)), null);
    const state = couldBeSynchronousComponent(sComp.Type) ? fill(new Array(maxArraySize - 1), 0, maxArraySize - 1, new SimulationComponentState(0, [])) : (void 0);
    const fId = getFid(sComp.Id, accessPath);
    const reduceIfHybrid = (sc, ipn) => {
        if (isHybridComponent(sc.Type)) {
            return sumBy((ipn_1) => {
                const _arg = getHybridComponentAsyncOuts(sc.Type, ipn_1);
                let matchResult_1;
                if (_arg != null) {
                    if (isEmpty(_arg)) {
                        matchResult_1 = 0;
                    }
                    else {
                        matchResult_1 = 1;
                    }
                }
                else {
                    matchResult_1 = 0;
                }
                switch (matchResult_1) {
                    case 0: {
                        return 0;
                    }
                    case 1: {
                        return 1;
                    }
                }
            }, toList(rangeDouble(0, 1, ipn)), {
                GetZero: () => 0,
                Add: (x, y) => (x + y),
            }) | 0;
        }
        else {
            return ipn | 0;
        }
    };
    const OutputWidth = getOutputWidths(sComp, fill(new Array(outPortNum), 0, outPortNum, void 0));
    const State = map_1(makeStepArray, state);
    const NumMissingInputValues = reduceIfHybrid(sComp, inPortNum) | 0;
    return new FastComponent(fId, sComp.Id, sComp.Type, State, !(sComp.Type.tag === 4), OutputWidth, ins, fill(new Array(inPortNum), 0, inPortNum, void 0), outs, sComp, accessPath, "", extractLabel(sComp.Label), empty_1(), false, empty_1(), NumMissingInputValues, fill(new Array(outPortNum), 0, outPortNum, ""), "");
}

export function extendFastComponent(numSteps, fc) {
    const oldNumSteps = fc.Outputs[0].Step.length | 0;
    if ((numSteps + 1) <= oldNumSteps) {
    }
    else {
        const extendArray = (arr, dat) => {
            const oldArr = arr.Step;
            const a = initialize(numSteps + 1, (i) => ((i < oldArr.length) ? oldArr[i] : dat), null);
            arr.Step = a;
        };
        const patternInput = getPortNumbers(fc.SimComponent);
        const outPortNum = patternInput[1] | 0;
        const inPortNum = patternInput[0] | 0;
        let matchResult;
        if (fc.FType.tag === 1) {
            if (isEmpty(fc.AccessPath)) {
                matchResult = 0;
            }
            else {
                matchResult = 1;
            }
        }
        else {
            matchResult = 1;
        }
        switch (matchResult) {
            case 0: {
                extendArray(fc.InputLinks[0], fc.InputLinks[0].Step[oldNumSteps - 1]);
                break;
            }
        }
        const array = toArray(rangeDouble(0, 1, outPortNum - 1));
        array.forEach((n) => {
            extendArray(fc.Outputs[n], new FData(0, [emptyFastData]));
        });
        iterate((stateArr) => {
            extendArray(stateArr, stateArr.Step[oldNumSteps - 1]);
        }, toArray_1(fc.State));
    }
}

export function extendFastSimulation(numSteps, fs) {
    if ((numSteps + 1) < fs.MaxStepNum) {
    }
    else {
        const array_1 = [fs.FOrderedComps, fs.FConstantComps, fs.FClockedComps.filter((fc) => (!isHybridComponent(fc.FType))), fs.FGlobalInputComps];
        array_1.forEach((array) => {
            array.forEach((fc_1) => {
                extendFastComponent(numSteps, fc_1);
            });
        });
        fs.MaxStepNum = (numSteps | 0);
    }
}

function createFlattenedSimulation(ap, graph) {
    const graphL = toList_1(graph);
    const allComps = map_2((tupledArg) => {
        const cid = tupledArg[0];
        const comp = tupledArg[1];
        return [[cid, ap], [comp, ap]];
    }, graphL);
    const labels = map_2((tupledArg_1) => {
        let s;
        const cid_1 = tupledArg_1[0];
        const comp_1 = tupledArg_1[1];
        return [cid_1, (s = comp_1.Label, s)];
    }, graphL);
    const topGather = new GatherTemp(empty_1(), empty_1(), labels, allComps);
    const customComps = collect((tupledArg_2) => {
        const cid_2 = tupledArg_2[0];
        const comp_2 = tupledArg_2[1];
        const matchValue = comp_2.Type;
        const matchValue_1 = comp_2.CustomSimulationGraph;
        let matchResult, csg, ct;
        if (matchValue.tag === 33) {
            if (matchValue_1 != null) {
                matchResult = 0;
                csg = matchValue_1;
                ct = matchValue.fields[0];
            }
            else {
                matchResult = 1;
            }
        }
        else {
            matchResult = 1;
        }
        switch (matchResult) {
            case 0: {
                return singleton([cid_2, ct, csg]);
            }
            case 1: {
                return empty_1();
            }
        }
    }, graphL);
    const insideCustomGathers = map_2((tupledArg_3) => {
        const cid_3 = tupledArg_3[0];
        const ct_1 = tupledArg_3[1];
        const csg_1 = tupledArg_3[2];
        const ap$0027 = append(ap, singleton(cid_3));
        const gatherT = createFlattenedSimulation(ap$0027, csg_1);
        const compsInCustomComp = map_2((tuple) => tuple[1], toList_1(csg_1));
        const getCustomNameIdsOf = (compSelectFun) => map_2((comp_4) => {
            let matchValue_3, n, n_1;
            return [[comp_4.Label, (matchValue_3 = comp_4.Type, (matchValue_3.tag === 1) ? ((n = (matchValue_3.fields[0] | 0), n)) : ((matchValue_3.tag === 2) ? ((n_1 = (matchValue_3.fields[0] | 0), n_1)) : -1))], comp_4.Id];
        }, filter((comp_3) => compSelectFun(comp_3.Type), compsInCustomComp));
        const outputs = getCustomNameIdsOf(isOutput);
        const outLinks = mapIndexed((i, tupledArg_4) => {
            const lab = tupledArg_4[0];
            const labOutWidth = tupledArg_4[1] | 0;
            const out = find((tupledArg_5) => {
                const k = tupledArg_5[0];
                const v = tupledArg_5[1];
                return equalArrays(k, [lab, labOutWidth]);
            }, outputs)[1];
            return [[out, ap$0027], [[cid_3, ap], i]];
        }, ct_1.OutputLabels);
        const inputs = getCustomNameIdsOf(isInput);
        const inLinks = mapIndexed((i_1, tupledArg_6) => {
            const lab_1 = tupledArg_6[0];
            const labOutWidth_1 = tupledArg_6[1] | 0;
            const inp = find((tupledArg_7) => {
                const k_1 = tupledArg_7[0];
                const v_1 = tupledArg_7[1];
                return equalArrays(k_1, [lab_1, labOutWidth_1]);
            }, inputs)[1];
            return [[[cid_3, ap], i_1], [inp, ap$0027]];
        }, ct_1.InputLabels);
        return new GatherTemp(append(inLinks, gatherT.CustomInputCompLinksT), append(outLinks, gatherT.CustomOutputCompLinksT), append(labels, gatherT.Labels), gatherT.AllCompsT);
    }, customComps);
    return fold((total, thisGather) => (new GatherTemp(append(thisGather.CustomInputCompLinksT, total.CustomInputCompLinksT), append(thisGather.CustomOutputCompLinksT, total.CustomOutputCompLinksT), append(thisGather.Labels, total.Labels), append(thisGather.AllCompsT, total.AllCompsT))), topGather, insideCustomGathers);
}

export function gatherSimulation(graph) {
    let g, CustomInputCompLinks, CustomOutputCompLinks, Labels, AllComps;
    const startTime = getTimeMs();
    return instrumentInterval("gatherGraph", startTime, (g = createFlattenedSimulation(empty_1(), graph), (CustomInputCompLinks = ofList(g.CustomInputCompLinksT, {
        Compare: compareArrays,
    }), (CustomOutputCompLinks = ofList(g.CustomOutputCompLinksT, {
        Compare: compareArrays,
    }), (Labels = ofList(g.Labels, {
        Compare: compare,
    }), (AllComps = ofList(g.AllCompsT, {
        Compare: compareArrays,
    }), new GatherData(graph, CustomInputCompLinks, CustomOutputCompLinks, ofList(map_2((tupledArg) => {
        const k = tupledArg[0];
        const v = tupledArg[1];
        return [v, k];
    }, g.CustomOutputCompLinksT), {
        Compare: compareArrays,
    }), Labels, AllComps)))))));
}

export function printGather(g) {
    const arg = FSharpMap__get_Count(g.AllComps) | 0;
    toConsole(printf("%d components"))(arg);
    iterate_1((tupledArg, tupledArg_1) => {
        const cid = tupledArg[0];
        const ap = tupledArg[1];
        const comp = tupledArg_1[0];
        const ap$0027 = tupledArg_1[1];
        const arg_1 = GatherData__getFullName_10524082(g, cid, ap);
        toConsole(printf("%s (%A:%A): %A"))(arg_1)(cid)(ap)(comp.Outputs);
    }, g.AllComps);
    iterate_1((tupledArg_2, tupledArg_3) => {
        const _arg = tupledArg_2[0];
        const ipn = tupledArg_2[1];
        const cid$0027 = tupledArg_3[0];
        const ap$0027_1 = tupledArg_3[1];
        const cid_1 = _arg[0];
        const ap_1 = _arg[1];
        const tupledArg_4 = [cid$0027, ap$0027_1];
        const arg_5 = GatherData__getFullName_10524082(g, cid_1, ap_1);
        toConsole(printf("inlink: %s -> %A"))(arg_5)([tupledArg_4[0], tupledArg_4[1]]);
    }, g.CustomInputCompLinks);
    iterate_1((tupledArg_5, tupledArg_6) => {
        const cid$0027_1 = tupledArg_5[0];
        const ap$0027_2 = tupledArg_5[1];
        const _arg_1 = tupledArg_6[0];
        const opn = tupledArg_6[1];
        const cid_2 = _arg_1[0];
        const ap_2 = _arg_1[1];
        const arg_10 = GatherData__getFullName_10524082(g, cid_2, ap_2);
        const tupledArg_7 = [cid$0027_1, ap$0027_2];
        toConsole(printf("outlink: %A -> %s"))([tupledArg_7[0], tupledArg_7[1]])(arg_10);
    }, g.CustomOutputCompLinks);
}

export function addComponentWaveDrivers(f, fc, pType) {
    const makeWaveIndex = (index, pn, pType_1, arr) => (new WaveIndexT(index, fc.fId, pType_1, pn));
    const addStepArray = (pn_1, index_1, stepA) => {
        f.Drivers[index_1] = defaultArg(f.Drivers[index_1], new Driver(index_1, 0, stepA));
        const addWidth = (w, optDriver) => map_1((d) => (new Driver(d.Index, w, d.DriverData)), optDriver);
        iterate((w_1) => {
            f.Drivers[index_1] = addWidth(w_1, f.Drivers[index_1]);
        }, toArray_1(fc.OutputWidth[pn_1]));
    };
    const ioLabelIsActive = (fc_1) => (!equalArrays(FSharpMap__get_Item(f.FIOActive, [fc_1.FLabel, fc_1.fId[1]]).fId, fc_1.fId));
    return mapIndexed_1((pn_2, stepA_1) => {
        let this$, this$_1;
        const index_2 = stepA_1.Index | 0;
        let patternInput;
        const matchValue = fc.FType;
        let matchResult;
        if (matchValue.tag === 4) {
            if (pType.tag === 0) {
                matchResult = 0;
            }
            else if (ioLabelIsActive(fc)) {
                matchResult = 2;
            }
            else {
                matchResult = 3;
            }
        }
        else if (matchValue.tag === 1) {
            if (pType.tag === 0) {
                matchResult = 0;
            }
            else {
                matchResult = 3;
            }
        }
        else if (matchValue.tag === 3) {
            if (pType.tag === 0) {
                matchResult = 0;
            }
            else {
                matchResult = 3;
            }
        }
        else if (matchValue.tag === 2) {
            if (pType.tag === 0) {
                matchResult = 0;
            }
            else {
                matchResult = 3;
            }
        }
        else if (matchValue.tag === 9) {
            matchResult = 1;
        }
        else {
            matchResult = 3;
        }
        switch (matchResult) {
            case 0: {
                patternInput = [false, false];
                break;
            }
            case 1: {
                patternInput = [true, false];
                break;
            }
            case 2: {
                patternInput = [false, false];
                break;
            }
            case 3: {
                patternInput = [true, true];
                break;
            }
        }
        const addWave = patternInput[1];
        const addDriver = patternInput[0];
        if (equals(pType, new PortType(1, [])) && addDriver) {
            addStepArray(pn_2, index_2, stepA_1);
        }
        if (addWave) {
            const matchValue_2 = fc.FType;
            let matchResult_1;
            if (matchValue_2.tag === 35) {
                matchResult_1 = 0;
            }
            else if (matchValue_2.tag === 7) {
                matchResult_1 = 0;
            }
            else if (matchValue_2.tag === 34) {
                matchResult_1 = 0;
            }
            else if (matchValue_2.tag === 9) {
                matchResult_1 = 0;
            }
            else if (matchValue_2.tag === 2) {
                if (!equals((this$ = fc, getSlice(0, length(this$.SheetName) - 2, this$.SheetName)), empty_1())) {
                    matchResult_1 = 1;
                }
                else {
                    matchResult_1 = 3;
                }
            }
            else if (matchValue_2.tag === 1) {
                if (!equals((this$_1 = fc, getSlice(0, length(this$_1.SheetName) - 2, this$_1.SheetName)), empty_1())) {
                    matchResult_1 = 2;
                }
                else {
                    matchResult_1 = 3;
                }
            }
            else {
                matchResult_1 = 3;
            }
            switch (matchResult_1) {
                case 0: {
                    return [];
                }
                case 1: {
                    return [];
                }
                case 2: {
                    return [];
                }
                case 3: {
                    return [makeWaveIndex(index_2, pn_2, pType, stepA_1)];
                }
            }
        }
        else {
            return [];
        }
    }, (pType.tag === 0) ? fc.InputLinks : fc.Outputs, null);
}

export function addWaveIndexAndDrivers(waveComps, f) {
    const comps = map((tuple) => tuple[1], toArray_2(waveComps), null);
    const addDrivers = (pType, array_1) => collect_1((fc) => addComponentWaveDrivers(f, fc, pType), array_1, null);
    const outs = addDrivers(new PortType(1, []), comps);
    const ins = addDrivers(new PortType(0, []), comps);
    return concat(append_1(outs, ins, null), null);
}

export function linkFastCustomComponentsToDriverArrays(fs, fid_, fid__1, fc) {
    const fid = [fid_, fid__1];
    const cid = fid[0];
    const ap$0027 = fid[1];
    const ap = append(ap$0027, singleton(cid));
    let ct_1;
    const matchValue = fc.FType;
    if (matchValue.tag === 33) {
        const ct = matchValue.fields[0];
        ct_1 = ct;
    }
    else {
        ct_1 = toFail(printf("linkFastCustomComponent must be called with a custom component"));
    }
    let graph;
    const matchValue_1 = fc.SimComponent.CustomSimulationGraph;
    if (matchValue_1 == null) {
        graph = toFail(printf("What? Can\'t find customSimulationGraph"));
    }
    else {
        const g = matchValue_1;
        graph = g;
    }
    iterate_1((cid_1, sc) => {
        const matchValue_2 = sc.Type;
        switch (matchValue_2.tag) {
            case 1: {
                const w = matchValue_2.fields[0] | 0;
                const portNum = find((tupledArg) => {
                    const i = tupledArg[0] | 0;
                    const lab = tupledArg[1][0];
                    return lab === sc.Label;
                }, indexed(ct_1.InputLabels))[0] | 0;
                fc.InputLinks[portNum] = FSharpMap__get_Item(fs.FComps, [cid_1, ap]).Outputs[0];
                break;
            }
            case 2: {
                const w_1 = matchValue_2.fields[0] | 0;
                const portNum_1 = find((tupledArg_1) => {
                    const i_1 = tupledArg_1[0] | 0;
                    const lab_1 = tupledArg_1[1][0];
                    return lab_1 === sc.Label;
                }, indexed(ct_1.OutputLabels))[0] | 0;
                fc.Outputs[portNum_1] = FSharpMap__get_Item(fs.FComps, [cid_1, ap]).InputLinks[0];
                fc.OutputWidth[portNum_1] = w_1;
                break;
            }
            default: {
            }
        }
    }, graph);
}

export function addWavesToFastSimulation(fs) {
    iterate_1((tupledArg, fc) => {
        linkFastCustomComponentsToDriverArrays(fs, tupledArg[0], tupledArg[1], fc);
    }, fs.FCustomComps);
    const waveComps = fold_1((s, fid, fc_1) => add(fid, fc_1, s), fs.FComps, fs.FCustomComps);
    let fs_1;
    const Drivers = fill(new Array(fs.NumStepArrays), 0, fs.NumStepArrays, void 0);
    fs_1 = (new FastSimulation(fs.ClockTick, fs.MaxStepNum, fs.MaxArraySize, fs.FGlobalInputComps, fs.FConstantComps, fs.FClockedComps, fs.FOrderedComps, fs.FIOActive, fs.FIOLinks, fs.FComps, fs.FCustomComps, waveComps, fs.FSComps, fs.FCustomOutputCompLookup, fs.G, fs.NumStepArrays, Drivers, fs.WaveIndex, fs.ConnectionsByPort, fs.ComponentsById, fs.SimulatedCanvasState, fs.SimulatedTopSheet));
    const WaveIndex = addWaveIndexAndDrivers(waveComps, fs_1);
    return new FastSimulation(fs_1.ClockTick, fs_1.MaxStepNum, fs_1.MaxArraySize, fs_1.FGlobalInputComps, fs_1.FConstantComps, fs_1.FClockedComps, fs_1.FOrderedComps, fs_1.FIOActive, fs_1.FIOLinks, fs_1.FComps, fs_1.FCustomComps, fs_1.WaveComps, fs_1.FSComps, fs_1.FCustomOutputCompLookup, fs_1.G, fs_1.NumStepArrays, fs_1.Drivers, WaveIndex, fs_1.ConnectionsByPort, fs_1.ComponentsById, fs_1.SimulatedCanvasState, fs_1.SimulatedTopSheet);
}

export function createInitFastCompPhase(simulationArraySize, g, f) {
    const numSteps = simulationArraySize | 0;
    stepArrayIndex(-1);
    const start = getTimeMs();
    toConsole(`Creating init fast comp phase of sim with ${numSteps} array size`);
    const makeFastComp = (fid) => {
        const patternInput = FSharpMap__get_Item(g.AllComps, fid);
        const comp = patternInput[0];
        const ap = patternInput[1];
        const fc = createFastComponent(numSteps, comp, ap);
        let fc_1;
        let FullName;
        const tupledArg = fid;
        FullName = GatherData__getFullName_10524082(g, tupledArg[0], tupledArg[1]);
        let SheetName;
        const tupledArg_1 = fid;
        SheetName = GatherData__getSheetName_10524082(g, tupledArg_1[0], tupledArg_1[1]);
        fc_1 = (new FastComponent(fc.fId, fc.cId, fc.FType, fc.State, fc.Active, fc.OutputWidth, fc.InputLinks, fc.InputDrivers, fc.Outputs, fc.SimComponent, fc.AccessPath, FullName, fc.FLabel, SheetName, fc.Touched, fc.DrivenComponents, fc.NumMissingInputValues, fc.VerilogOutputName, fc.VerilogComponentName));
        let outs_1;
        if (isOutput(comp.Type)) {
            const outs = [makeStepArray(fill(new Array(numSteps), 0, numSteps, new FData(0, [emptyFastData])))];
            outs_1 = outs;
        }
        else {
            outs_1 = fc_1.Outputs;
        }
        return new FastComponent(fc_1.fId, fc_1.cId, fc_1.FType, fc_1.State, fc_1.Active, fc_1.OutputWidth, fc_1.InputLinks, fc_1.InputDrivers, outs_1, fc_1.SimComponent, fc_1.AccessPath, fc_1.FullName, fc_1.FLabel, fc_1.SheetName, fc_1.Touched, fc_1.DrivenComponents, fc_1.NumMissingInputValues, fc_1.VerilogOutputName, fc_1.VerilogComponentName);
    };
    const patternInput_1 = fold_1(uncurry(3, (tupledArg_2) => ((cid) => {
        const m = tupledArg_2[0];
        const mc = tupledArg_2[1];
        return (tupledArg_3) => {
            const comp_1 = tupledArg_3[0];
            const ap_1 = tupledArg_3[1];
            return isCustom(comp_1.Type) ? [m, add([comp_1.Id, ap_1], makeFastComp([comp_1.Id, ap_1]), mc)] : [add([comp_1.Id, ap_1], makeFastComp([comp_1.Id, ap_1]), m), mc];
        };
    })), [empty({
        Compare: compareArrays,
    }), empty({
        Compare: compareArrays,
    })], g.AllComps);
    const customComps = patternInput_1[1];
    const comps = patternInput_1[0];
    const customOutLookup = ofList(map_2((tupledArg_4) => {
        const a = tupledArg_4[0];
        const b = tupledArg_4[1];
        return [b, a];
    }, toList_1(g.CustomOutputCompLinks)), {
        Compare: compareArrays,
    });
    instrumentTime("createInitFastCompPhase", start);
    return new FastSimulation(f.ClockTick, 0, simulationArraySize, f.FGlobalInputComps, f.FConstantComps, f.FClockedComps, f.FOrderedComps, f.FIOActive, f.FIOLinks, comps, customComps, f.WaveComps, g.AllComps, customOutLookup, f.G, stepArrayIndex(), new Array(0), f.WaveIndex, f.ConnectionsByPort, f.ComponentsById, f.SimulatedCanvasState, f.SimulatedTopSheet);
}

function reLinkIOLabels(fs) {
    iterate_2((tupledArg) => {
        const _arg = tupledArg[0];
        const ioDriver = tupledArg[1];
        const ipn = _arg[1];
        const fcDriven = _arg[0];
        const labKey = [ioDriver.SimComponent.Label, ioDriver.AccessPath];
        const fcActiveDriver = FSharpMap__get_Item(fs.FIOActive, labKey);
        fcDriven.InputLinks[ipn] = fcActiveDriver.Outputs[0];
        fcDriven.InputDrivers[ipn] = [fcActiveDriver.fId, 0];
        const matchValue = getHybridComponentAsyncOuts(fcDriven.FType, ipn);
        let matchResult;
        if (matchValue != null) {
            if (!isEmpty(matchValue)) {
                matchResult = 0;
            }
            else {
                matchResult = 1;
            }
        }
        else {
            matchResult = 0;
        }
        switch (matchResult) {
            case 0: {
                fcActiveDriver.DrivenComponents = cons(fcDriven, fcActiveDriver.DrivenComponents);
                break;
            }
        }
        ioDriver.Outputs[0] = fcActiveDriver.Outputs[0];
    }, fs.FIOLinks);
}

export function linkFastComponents(g, f) {
    const start = getTimeMs();
    const outer = (arg_1) => reverse(tail(reverse(arg_1)));
    const sComps = g.AllComps;
    const fComps = f.FComps;
    const getSComp = (tupledArg) => {
        const cid = tupledArg[0];
        const ap = tupledArg[1];
        const x = tryFind([cid, ap], sComps);
        if (x != null) {
            const comp = x;
            return comp[0];
        }
        else {
            return toFail(`Error in linkFastComponents: can't find
---${cid}
----${ap}
`);
        }
    };
    const apOf = (fid) => FSharpMap__get_Item(fComps, fid).AccessPath;
    const getLinks = (_arg_mut, opn_mut, ipnOpt_mut) => {
        let array;
        getLinks:
        while (true) {
            const _arg = _arg_mut, opn = opn_mut, ipnOpt = ipnOpt_mut;
            const cid_1 = _arg[0];
            const ap_1 = _arg[1];
            const sComp = getSComp([cid_1, ap_1]);
            const matchValue = isOutput(sComp.Type);
            const matchValue_1 = isCustom(sComp.Type);
            let matchResult, ipn, ipn_1, x_1;
            if (matchValue) {
                if (ipnOpt != null) {
                    if (matchValue_1) {
                        matchResult = 5;
                        x_1 = [matchValue, matchValue_1, ipnOpt];
                    }
                    else {
                        matchResult = 3;
                        ipn_1 = value(ipnOpt);
                    }
                }
                else if (equals(apOf([cid_1, ap_1]), empty_1())) {
                    matchResult = 0;
                }
                else {
                    matchResult = 1;
                }
            }
            else if (matchValue_1) {
                if (ipnOpt == null) {
                    matchResult = 4;
                }
                else {
                    matchResult = 2;
                    ipn = value(ipnOpt);
                }
            }
            else if (ipnOpt == null) {
                matchResult = 4;
            }
            else {
                matchResult = 3;
                ipn_1 = value(ipnOpt);
            }
            switch (matchResult) {
                case 0: {
                    return [];
                }
                case 1: {
                    const patternInput = FSharpMap__get_Item(g.CustomOutputCompLinks, [cid_1, ap_1]);
                    const opn_1 = patternInput[1];
                    const fid_1 = patternInput[0];
                    if (!isCustom(FSharpMap__get_Item(sComps, fid_1)[0].Type)) {
                        toFail(printf("what? assert failed: %s"))("What? this should be a custom component output");
                    }
                    _arg_mut = fid_1;
                    opn_mut = opn_1;
                    ipnOpt_mut = (void 0);
                    continue getLinks;
                }
                case 2: {
                    return [[FSharpMap__get_Item(g.CustomInputCompLinks, [[cid_1, ap_1], ipn]), opn, 0]];
                }
                case 3: {
                    return [[[cid_1, ap_1], opn, ipn_1]];
                }
                case 4: {
                    return collect_1((tupledArg_2) => {
                        const opn_2 = tupledArg_2[0];
                        const lst = tupledArg_2[1];
                        return collect_1((tupledArg_3) => {
                            const cid_2 = tupledArg_3[0];
                            const ipn_2 = tupledArg_3[1];
                            return getLinks([cid_2, ap_1], opn_2, some(ipn_2));
                        }, toArray_3(lst), null);
                    }, (array = toArray_2(sComp.Outputs), array.filter((tupledArg_1) => {
                        const opn$0027 = tupledArg_1[0];
                        return equals(opn$0027, opn);
                    })), null);
                }
                case 5: {
                    const tupledArg_4 = x_1;
                    return toFail(printf("Unexpected link match: %A"))([tupledArg_4[0], tupledArg_4[1], tupledArg_4[2]]);
                }
            }
            break;
        }
    };
    let linkCheck = empty({
        Compare: compareArrays,
    });
    iterate_1((fDriverId, fDriver) => {
        const outs = fDriver.Outputs;
        iterateIndexed((iOut, _arg_2) => {
            const array_4 = map((tupledArg_5) => {
                const fid_2 = tupledArg_5[0];
                const ip = tupledArg_5[2];
                return [fid_2, iOut, ip];
            }, getLinks(fDriverId, iOut, void 0), null);
            array_4.forEach((tupledArg_6) => {
                const fDrivenId = tupledArg_6[0];
                const opn_3 = tupledArg_6[1] | 0;
                const ipn_3 = tupledArg_6[2];
                const linked = tryFind([fDrivenId, ipn_3], linkCheck);
                if (linked != null) {
                    const opn_4 = linked[1];
                    const fid_3 = linked[0];
                    let arg_6;
                    const tupledArg_7 = fid_3;
                    arg_6 = GatherData__getFullName_10524082(g, tupledArg_7[0], tupledArg_7[1]);
                    toFail(printf("Multiple linkage: (previous driver was %A,%A)"))(arg_6)(opn_4);
                }
                linkCheck = add([fDrivenId, ipn_3], [fDriverId, opn_3], linkCheck);
                const fDriven = FSharpMap__get_Item(f.FComps, fDrivenId);
                const ap_2 = fDrivenId[1];
                if (isIOLabel(fDriven.FType)) {
                    const labelKey = [fDriven.SimComponent.Label, ap_2];
                    if (!containsKey(labelKey, f.FIOActive)) {
                        f.FIOActive = add(labelKey, fDriven, f.FIOActive);
                        fDriven.Active = true;
                    }
                }
                if (isIOLabel(fDriver.FType)) {
                    f.FIOLinks = cons([[fDriven, ipn_3], fDriver], f.FIOLinks);
                }
                else {
                    fDriven.InputLinks[ipn_3] = fDriver.Outputs[opn_3];
                    const matchValue_3 = getHybridComponentAsyncOuts(fDriven.FType, ipn_3);
                    let matchResult_1;
                    if (matchValue_3 != null) {
                        if (!isEmpty(matchValue_3)) {
                            matchResult_1 = 0;
                        }
                        else {
                            matchResult_1 = 1;
                        }
                    }
                    else {
                        matchResult_1 = 0;
                    }
                    switch (matchResult_1) {
                        case 0: {
                            fDriver.DrivenComponents = cons(fDriven, fDriver.DrivenComponents);
                            break;
                        }
                    }
                    fDriven.InputDrivers[ipn_3] = [fDriver.fId, opn_3];
                }
            });
        }, fDriver.Outputs);
    }, f.FComps);
    reLinkIOLabels(f);
    instrumentTime("linkFastComponents", start);
    return f;
}

//# sourceMappingURL=FastCreate.fs.js.map

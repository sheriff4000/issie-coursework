import { zip, singleton as singleton_1, item, mapFold, contains, partition, isEmpty, length, fold as fold_1, filter, map, append } from "../../fable_modules/fable-library.4.0.0-theta-018/List.js";
import { TruthTable, TruthTableCell, CellData, ConstraintSet, CellIO } from "../../Simulator/TruthTableTypes.fs.js";
import { FSharpMap__get_Count, ofList, toList } from "../../fable_modules/fable-library.4.0.0-theta-018/Map.js";
import { truncate, mapIndexed, toList as toList_1, initialize, empty, fold, singleton, append as append_1, map as map_1, collect, delay } from "../../fable_modules/fable-library.4.0.0-theta-018/Seq.js";
import { compare, arrayHash, equalArrays, equals } from "../../fable_modules/fable-library.4.0.0-theta-018/Util.js";
import { convertFastDataToWireData, convertWireDataToFastData, convertIntToWireData } from "../../Simulator/NumberHelpers.fs.js";
import { fromInteger } from "../../fable_modules/fable-library.4.0.0-theta-018/Long.js";
import { toString } from "../../fable_modules/fable-library.4.0.0-theta-018/Types.js";
import { buildFastSimulation, extractViewers, extractFastSimulationIOs, changeInputBatch } from "../../Simulator/Fast/FastRun.fs.js";
import { SimulationData, expToString, FastAlgExp, FSInterface } from "../../Simulator/SimulatorTypes.fs.js";
import { printf, toFail } from "../../fable_modules/fable-library.4.0.0-theta-018/String.js";
import { instrumentInterval, getTimeMs } from "../../Common/TimeHelpers.fs.js";
import { hasRedundancies } from "./TruthTableReduce.fs.js";

export function toCellIO(simIOs, viewers) {
    return append(map((io) => (new CellIO(0, [io])), simIOs), map((tupledArg) => {
        const _arg = tupledArg[0];
        const w = tupledArg[1] | 0;
        const l = _arg[0];
        const f = _arg[1];
        return new CellIO(1, [[l, f], w]);
    }, viewers));
}

export function tableAsList(tMap) {
    return map((tupledArg) => {
        const lhs = tupledArg[0];
        const rhs = tupledArg[1];
        return append(lhs, rhs);
    }, toList(tMap));
}

export function product(seq1, seq2) {
    return delay(() => collect((item1) => map_1((item2) => append_1(item2, singleton(item1)), seq2), seq1));
}

export function productn(s) {
    return fold((r, s_1) => product(s_1, r), delay(() => singleton(empty())), s);
}

export function getConstraintsOnIO(io, constraints) {
    const newEqu = filter((e) => equals(e.IO, io), constraints.Equalities);
    const newIneq = filter((i) => equals(i.IO, io), constraints.Inequalities);
    return new ConstraintSet(newEqu, newIneq);
}

export function constrainedValuesandLength(_arg) {
    const ineq = _arg.Inequalities;
    const equ = _arg.Equalities;
    const equValues = map_1((con) => con.Value, equ);
    const patternInput = fold_1((tupledArg, con_1) => {
        const seqAcc = tupledArg[0];
        const count = tupledArg[1] | 0;
        const values = initialize(con_1.Range, (x) => (x + con_1.LowerBound));
        return [append_1(values, seqAcc), count + con_1.Range];
    }, [empty(), 0], ineq);
    const ineqValues = patternInput[0];
    const ineqLength = patternInput[1] | 0;
    return [append_1(equValues, ineqValues), length(equ) + ineqLength];
}

export function tableLHS(inputs, inputConstraints, algebraIOs, bitLimit) {
    const rowLimit = (~(~Math.pow(2, bitLimit))) | 0;
    const inputValuesSeq = (count, io) => {
        const w = io[2] | 0;
        const seqLength = (~(~Math.pow(2, w))) | 0;
        const matchValue = getConstraintsOnIO(new CellIO(0, [io]), inputConstraints);
        let matchResult, conSet;
        if (isEmpty(matchValue.Equalities)) {
            if (isEmpty(matchValue.Inequalities)) {
                matchResult = 0;
            }
            else {
                matchResult = 1;
                conSet = matchValue;
            }
        }
        else {
            matchResult = 1;
            conSet = matchValue;
        }
        switch (matchResult) {
            case 0: {
                return [initialize(seqLength, (x) => x), count * seqLength];
            }
            case 1: {
                const tupledArg = constrainedValuesandLength(conSet);
                const vals = tupledArg[0];
                const seqLength_1 = tupledArg[1] | 0;
                return [vals, count * seqLength_1];
            }
        }
    };
    const patternInput = partition((io_1) => contains(io_1, algebraIOs, {
        Equals: equalArrays,
        GetHashCode: arrayHash,
    }), inputs);
    const numericInputs = patternInput[1];
    const algebraInputs = patternInput[0];
    const patternInput_1 = mapFold(inputValuesSeq, 1, numericInputs);
    const numericVals = patternInput_1[0];
    const constrainedRowCount = patternInput_1[1] | 0;
    const numericRows = toList_1(map_1((l) => toList_1(mapIndexed((i, n) => {
        const w_1 = item(i, numericInputs)[2] | 0;
        return new TruthTableCell(new CellIO(0, [item(i, numericInputs)]), new CellData(0, [convertIntToWireData(w_1, fromInteger(n, false, 2))]));
    }, l)), truncate(rowLimit, productn(numericVals))));
    const algebraRow = map((io_2) => {
        const label = io_2[1];
        return new TruthTableCell(new CellIO(0, [io_2]), new CellData(1, [toString(label)]));
    }, algebraInputs);
    if (isEmpty(numericInputs)) {
        return [singleton_1(algebraRow), 1];
    }
    else {
        return [map((r) => append(algebraRow, r), numericRows), constrainedRowCount];
    }
}

export function simulateInputCombination(rowLHS, outputs, simData) {
    changeInputBatch(simData.ClockTickNumber, simData.FastSim, map((cell) => {
        const matchValue = cell.IO;
        const matchValue_1 = cell.Data;
        let matchResult, cid, wd, exp, io, x, y;
        if (matchValue.tag === 0) {
            if (matchValue_1.tag === 0) {
                matchResult = 0;
                cid = matchValue.fields[0][0];
                wd = matchValue_1.fields[0];
            }
            else if (matchValue_1.tag === 1) {
                matchResult = 1;
                exp = matchValue_1.fields[0];
                io = matchValue.fields[0];
            }
            else {
                matchResult = 2;
                x = matchValue;
                y = matchValue_1;
            }
        }
        else {
            matchResult = 2;
            x = matchValue;
            y = matchValue_1;
        }
        switch (matchResult) {
            case 0: {
                return [cid, new FSInterface(0, [convertWireDataToFastData(wd)])];
            }
            case 1: {
                const cid_1 = io[0];
                return [cid_1, new FSInterface(1, [new FastAlgExp(0, [io])])];
            }
            case 2: {
                return toFail(printf("what? CellData from input rows has IO: %A, and Data: %A."))(x)(y);
            }
        }
    }, rowLHS));
    const outputRow = map((tupledArg) => {
        const comp = tupledArg[0];
        const out = tupledArg[1];
        if (out.tag === 1) {
            const exp_1 = out.fields[0];
            return new TruthTableCell(new CellIO(0, [comp]), new CellData(1, [expToString(exp_1)]));
        }
        else {
            const wd_1 = out.fields[0];
            return new TruthTableCell(new CellIO(0, [comp]), new CellData(0, [convertFastDataToWireData(wd_1)]));
        }
    }, extractFastSimulationIOs(outputs, simData));
    const viewerRow = map((tupledArg_1) => {
        const _arg = tupledArg_1[0];
        const w = tupledArg_1[1] | 0;
        const fs = tupledArg_1[2];
        const l = _arg[0];
        const f = _arg[1];
        if (fs.tag === 1) {
            const exp_2 = fs.fields[0];
            return new TruthTableCell(new CellIO(1, [[l, f], w]), new CellData(1, [expToString(exp_2)]));
        }
        else {
            const wd_2 = fs.fields[0];
            return new TruthTableCell(new CellIO(1, [[l, f], w]), new CellData(0, [convertFastDataToWireData(wd_2)]));
        }
    }, extractViewers(simData));
    return append(outputRow, viewerRow);
}

export function truthTable(simData, inputConstraints, algebraIOs, bitLimit, isRegeneration) {
    let table, tableMap, listRep, hasRed;
    const start = getTimeMs();
    let tableSimData;
    if (isRegeneration) {
        tableSimData = simData;
    }
    else {
        const matchValue = buildFastSimulation(2, "", simData.Graph);
        if (matchValue.tag === 0) {
            const fs = matchValue.fields[0];
            tableSimData = (new SimulationData(fs, simData.Graph, simData.Inputs, simData.Outputs, simData.IsSynchronous, simData.NumberBase, simData.ClockTickNumber));
        }
        else {
            tableSimData = toFail(printf("Error in building fast simulation for Truth Table evaluation"));
        }
    }
    const inputs = map((tuple) => tuple[0], extractFastSimulationIOs(simData.Inputs, tableSimData));
    const outputs = map((tuple_1) => tuple_1[0], extractFastSimulationIOs(simData.Outputs, tableSimData));
    const viewers = extractViewers(simData);
    const patternInput = tableLHS(inputs, inputConstraints, algebraIOs, bitLimit);
    const tCRC = patternInput[1] | 0;
    const lhs = patternInput[0];
    const rhs = map((i) => simulateInputCombination(i, outputs, tableSimData), lhs);
    return instrumentInterval("truthTableGeneration", start, (table = ((tableMap = ofList(zip(lhs, rhs), {
        Compare: compare,
    }), (listRep = tableAsList(tableMap), new TruthTable(tableMap, tableMap, void 0, listRep, FSharpMap__get_Count(tableMap) !== tCRC, tCRC, false, tableSimData, toCellIO(append(inputs, outputs), viewers))))), (table.IsTruncated ? true : (length(algebraIOs) > 0)) ? table : ((hasRed = hasRedundancies(table), new TruthTable(table.TableMap, table.FilteredMap, table.DCMap, table.SortedListRep, table.IsTruncated, table.MaxRowsWithConstraints, hasRed, table.TableSimData, table.IOOrder)))));
}

//# sourceMappingURL=TruthTableCreate.fs.js.map

import * as path_1 from "path";
import * as fs from "fs";
import { toConsole, interpolate, split, toFail, printf, toText, endsWith, join } from "../fable_modules/fable-library.4.0.0-theta-018/String.js";
import { exists, toArray, toList, filter, isEmpty, skip, reverse } from "../fable_modules/fable-library.4.0.0-theta-018/Seq.js";
import { isLetterOrDigit } from "../fable_modules/fable-library.4.0.0-theta-018/Char.js";
import { Result_Bind, Result_Map, FSharpResult$2 } from "../fable_modules/fable-library.4.0.0-theta-018/Choice.js";
import { defaultArg, bind, some } from "../fable_modules/fable-library.4.0.0-theta-018/Option.js";
import { iterate, zip, length, item, fold, head, tail, isEmpty as isEmpty_1, tryHead, map, sortDescending, filter as filter_1, empty } from "../fable_modules/fable-library.4.0.0-theta-018/List.js";
import { StringModule_Contains, StringModule_EndsWith, StringModule_Concat, StringModule_Trim, StringModule_SplitRemoveEmptyEntries, StringModule_StartsWith, StringModule_TryParseWith, StringModule_SplitString, StringModule_ToLower } from "../Common/EEExtensions.fs.js";
import { sortBy, tryFind, mapIndexed, equalsWith, singleton, allPairs, map as map_1, tryItem } from "../fable_modules/fable-library.4.0.0-theta-018/Array.js";
import { tryParse } from "../fable_modules/fable-library.4.0.0-theta-018/Int32.js";
import { Union, FSharpRef } from "../fable_modules/fable-library.4.0.0-theta-018/Types.js";
import { equals as equals_1, equalArrays, fastStructuralHash, defaultOf, compareArrays } from "../fable_modules/fable-library.4.0.0-theta-018/Util.js";
import { tryFindError, JsonHelpers_SavedInfo__get_getSheetInfo, JsonHelpers_SavedInfo__get_getWaveInfo, JsonHelpers_SavedInfo__get_getTimeStamp, JsonHelpers_stateToJsonString, JsonHelpers_SavedInfo, JsonHelpers_jsonStringToState } from "../Common/Helpers.fs.js";
import { equals, compare, fromInteger, op_Addition, fromBits, op_LeftShift, op_Subtraction, op_BitwiseAnd, fromValue } from "../fable_modules/fable-library.4.0.0-theta-018/Long.js";
import { rangeDouble } from "../fable_modules/fable-library.4.0.0-theta-018/Range.js";
import { toArray as toArray_1, ofArray } from "../fable_modules/fable-library.4.0.0-theta-018/Map.js";
import { SimpleJson_tryParse } from "../fable_modules/Fable.SimpleJson.3.24.0/./SimpleJson.fs.js";
import { createTypeInfo } from "../fable_modules/Fable.SimpleJson.3.24.0/./TypeInfo.Converter.fs.js";
import { union_type, class_type, int64_type } from "../fable_modules/fable-library.4.0.0-theta-018/Reflection.js";
import { Convert_fromJson } from "../fable_modules/Fable.SimpleJson.3.24.0/./Json.Converter.fs.js";
import * as remote from "@electron/remote";
import { hex64, strToIntCheckWidth } from "../Simulator/NumberHelpers.fs.js";
import { Array_groupBy } from "../fable_modules/fable-library.4.0.0-theta-018/Seq2.js";
import { LoadedComponent$reflection, LoadedComponent, getMemType, legacyTypesConvert, Component, ComponentType, InitMemData, LegacyCanvas_LegacyComponent, LegacyCanvas_LegacyConnection, SheetInfo, CCForm, Memory1 } from "../Common/CommonTypes.fs.js";
import { compare as compare_1, now } from "../fable_modules/fable-library.4.0.0-theta-018/Date.js";
import { parseDiagramSignature } from "../Simulator/Extractor.fs.js";

export function pathJoin(args) {
    return path_1.join(...args);
}

export function baseName(filePath) {
    return path_1.basename(filePath);
}

export function dirName(filePath) {
    return path_1.dirname(filePath);
}

export function ensureDirectory(dPath) {
    if (!fs.existsSync(dPath)) {
        fs.mkdirSync(dPath);
    }
}

export function pathWithoutExtension(filePath) {
    let source_1;
    const ext = path_1.extname(filePath);
    return join("", reverse((source_1 = reverse(filePath.split("")), skip(ext.length, source_1))));
}

export const baseNameWithoutExtension = (arg) => baseName(pathWithoutExtension(arg));

export function fileNameIsBad(name) {
    return !isEmpty(filter((ch) => {
        let ch_1;
        return !((ch === " ") ? true : ((ch_1 = ch, isLetterOrDigit(ch_1) ? true : (ch_1 === "_"))));
    }, name));
}

export const filePathIsBad = (arg) => fileNameIsBad(baseNameWithoutExtension(arg).split(""));

export function fileExistsWithExtn(extn, folderPath, baseName_1) {
    const path = path_1.join(folderPath, baseName_1 + extn);
    return fs.existsSync(path);
}

export function tryReadFileSync(fPath) {
    if (!fs.existsSync(fPath)) {
        return new FSharpResult$2(1, [`Error: file ${fPath} does not exist`]);
    }
    else {
        return new FSharpResult$2(0, [fs.readFileSync(fPath, "utf8")]);
    }
}

export function writeFileBase64(path, data) {
    const options = some({
        encoding: "base64",
    });
    try {
        fs.writeFileSync(path, data, some(options));
        return new FSharpResult$2(0, [void 0]);
    }
    catch (e) {
        return new FSharpResult$2(1, [`Error '${e.message}' writing file '${path}'`]);
    }
}

export function writeFile(path, data) {
    try {
        const options = some({
            encoding: "utf8",
        });
        fs.writeFileSync(path, data, some(options));
        return new FSharpResult$2(0, [void 0]);
    }
    catch (e) {
        return new FSharpResult$2(1, [`Error '${e.message}' writing file '${path}'`]);
    }
}

export function readFilesFromDirectory(path) {
    if (fs.existsSync(path)) {
        try {
            return toList(fs.readdirSync(path));
        }
        catch (matchValue) {
            return empty();
        }
    }
    else {
        return empty();
    }
}

export function hasExtn(extn, fName) {
    return endsWith(StringModule_ToLower(fName), StringModule_ToLower(extn));
}

export function readFilesFromDirectoryWithExtn(path, extn) {
    return filter_1((name) => hasExtn(extn, name), readFilesFromDirectory(path));
}

export function removeExtn(extn, fName) {
    if (hasExtn(extn, fName)) {
        return fName.slice(0, ((fName.length - extn.length) - 1) + 1);
    }
    else {
        return void 0;
    }
}

export function backupFileData(path, baseName_1) {
    return sortDescending(map((fn_1) => {
        let option;
        return [(option = tryItem(1, StringModule_SplitString(["-"], fn_1)), bind(StringModule_TryParseWith((arg) => {
            let outArg = 0;
            return [tryParse(arg, 511, false, 32, new FSharpRef(() => outArg, (v) => {
                outArg = (v | 0);
            })), outArg];
        }), option)), fn_1];
    }, filter_1((fn) => StringModule_StartsWith(baseName_1 + "-", fn), readFilesFromDirectory(path))), {
        Compare: compareArrays,
    });
}

export function latestBackupFileData(path, baseName_1) {
    return bind((_arg) => {
        if (_arg[0] != null) {
            const n = _arg[0] | 0;
            const fn = _arg[1];
            return [n, fn];
        }
        else {
            return void 0;
        }
    }, tryHead(backupFileData(path, baseName_1)));
}

function tryLoadStateFromPath(filePath) {
    if (!fs.existsSync(filePath)) {
        return new FSharpResult$2(1, [toText(printf("Can\'t read file from %s because it does not seem to exist!"))(filePath)]);
    }
    else {
        const _arg = Result_Map(JsonHelpers_jsonStringToState, (() => {
            try {
                return new FSharpResult$2(0, [fs.readFileSync(filePath, "utf8")]);
            }
            catch (e) {
                return new FSharpResult$2(1, [`Error ${e.message} reading file '${filePath}'`]);
            }
        })());
        if (_arg.tag === 0) {
            const res = _arg.fields[0];
            return new FSharpResult$2(0, [res]);
        }
        else {
            const msg = _arg.fields[0];
            return new FSharpResult$2(1, [toText(printf("could not convert file \'%s\' to a valid issie design sheet. Details: %s"))(filePath)(msg)]);
        }
    }
}

export function makeData(aWidth, dWidth, makeFun) {
    const truncate = (n) => {
        let w;
        return fromValue((dWidth === 64) ? n : ((w = (dWidth | 0), op_BitwiseAnd(op_Subtraction(op_LeftShift(fromBits(1, 0, true), w), fromBits(1, 0, true)), n))), false);
    };
    const a = (~(~(aWidth / 2))) | 0;
    const inp = toArray(rangeDouble(0, 1, (1 << a) - 1));
    return ofArray(map_1((tupledArg) => {
        const x = tupledArg[0] | 0;
        const y = tupledArg[1] | 0;
        return [fromValue(op_Addition(op_LeftShift(fromInteger(x, false, 2), a), fromInteger(y, false, 2)), false), truncate(fromInteger(makeFun(x, y), true, 2))];
    }, allPairs(inp, inp), null), {
        Compare: compare,
    });
}

export function makeFixedROM(addr, data, mem) {
    let d, a, d_1, a_1;
    const signExtend = (w, n) => {
        if ((n & (1 << (w - 1))) !== 0) {
            return (((-1 << w) | n) & -1) | 0;
        }
        else {
            return n | 0;
        }
    };
    const matchValue = mem.Init;
    let matchResult;
    if (matchValue.tag === 4) {
        if ((d = (data | 0), (a = (addr | 0), ((a % 2) === 0) && (a <= 16)))) {
            matchResult = 0;
        }
        else {
            matchResult = 3;
        }
    }
    else if (matchValue.tag === 5) {
        if ((d_1 = (data | 0), (a_1 = (addr | 0), ((a_1 % 2) === 0) && (a_1 <= 16)))) {
            matchResult = 1;
        }
        else {
            matchResult = 3;
        }
    }
    else if (matchValue.tag === 0) {
        matchResult = 2;
    }
    else {
        matchResult = 3;
    }
    switch (matchResult) {
        case 0: {
            const d_2 = data | 0;
            const a_2 = addr | 0;
            return new FSharpResult$2(0, [makeData(a_2, d_2, (x, y) => ((x * y) % (1 << d_2)))]);
        }
        case 1: {
            const d_3 = data | 0;
            const a_3 = addr | 0;
            const w_1 = (~(~(a_3 / 2))) | 0;
            return new FSharpResult$2(0, [makeData(a_3, d_3, (x_1, y_1) => ((signExtend(w_1, x_1) * signExtend(w_1, y_1)) & ((1 << d_3) - 1)))]);
        }
        case 2: {
            return new FSharpResult$2(0, [mem.Data]);
        }
        case 3: {
            return toFail(`addr=${addr}, data=${data}, int=${mem.Init} not allowed in makeFixedROM`);
        }
    }
}

export function jsonStringToMem(jsonString) {
    let matchValue, inputJson, typeInfo;
    try {
        return new FSharpResult$2(0, [(matchValue = SimpleJson_tryParse(jsonString), (matchValue != null) ? ((inputJson = matchValue, (typeInfo = createTypeInfo(class_type("Microsoft.FSharp.Collections.FSharpMap`2", [int64_type, int64_type])), Convert_fromJson(inputJson, typeInfo)))) : (() => {
            throw new Error("Couldn\'t parse the input JSON string because it seems to be invalid");
        })())]);
    }
    catch (ex) {
        return new FSharpResult$2(1, [ex.message]);
    }
}

export function getBaseNameNoExtension(filePath) {
    const name = baseName(filePath);
    const matchValue = toList(split(name, ["."], null, 0));
    if (!isEmpty_1(matchValue)) {
        if (isEmpty_1(tail(matchValue))) {
            const name_1 = head(matchValue);
            return name_1;
        }
        else {
            const firstSplit = head(matchValue);
            const splits = tail(matchValue);
            const rest = fold((baseName_1, i) => ((name + ".") + item(i, splits)), "", toList(rangeDouble(0, 1, length(splits) - 2)));
            return firstSplit + rest;
        }
    }
    else {
        return toFail(printf("what? split at . in a filename should never return empty list"));
    }
}

const projectFileFilters = singleton({
    name: "ISSIE project file",
    extensions: ["dprj"],
}, null);

const ramFileFilters = singleton({
    name: "Memory contents File",
    extensions: ["ram"],
}, null);

const projectFilters = singleton({
    name: "ISSIE project",
    extensions: [""],
}, null);

export function askForExistingProjectPath(defaultPath) {
    const options = {};
    options.filters = Array.from(projectFileFilters);
    options.defaultPath = defaultArg(defaultPath, remote.app.getPath("documents"));
    const w = remote.getCurrentWindow();
    return bind((arg_3) => {
        const _arg = toList(arg_3);
        if (!isEmpty_1(_arg)) {
            const p = head(_arg);
            return path_1.dirname(p);
        }
        else {
            return void 0;
        }
    }, remote.dialog.showOpenDialogSync(w, options));
}

export function askForNewProjectPath(defaultPath) {
    const options = {};
    options.filters = Array.from(projectFilters);
    options.title = "Enter new ISSIE project directory and name";
    options.nameFieldLabel = "New project name";
    options.defaultPath = defaultPath;
    options.buttonLabel = "Create Project";
    options.properties = ["createDirectory", "showOverwriteConfirmation"];
    const w = remote.getCurrentWindow();
    return bind((dPath) => {
        const dir = dirName(dPath);
        const files = fs.readdirSync(dir);
        if (exists((fn) => endsWith(fn, ".dprj"), files)) {
            remote.dialog.showErrorBox("Invalid project directory", "You are trying to create a new Issie project inside an existing project directory. This is not allowed, please choose a different directory");
            return askForNewProjectPath(defaultPath);
        }
        else {
            return dPath;
        }
    }, remote.dialog.showSaveDialogSync(options));
}

export function tryCreateFolder(path) {
    if (exists((arg) => {
        let ch_1;
        return !((ch_1 = arg, isLetterOrDigit(ch_1) ? true : (ch_1 === "_")));
    }, baseName(path).split(""))) {
        return new FSharpResult$2(1, ["File or project names must contain only letters, digits, or underscores"]);
    }
    else {
        try {
            const arg_2 = fs.mkdirSync(path);
            return new FSharpResult$2(0, [void 0]);
        }
        catch (ex) {
            return new FSharpResult$2(1, [`Can't create folder '${path}': ${ex.message}`]);
        }
    }
}

export function removeFileWithExtn(extn, folderPath, baseName_1) {
    const path = path_1.join(folderPath, baseName_1 + extn);
    if (fs.existsSync(path)) {
        try {
            fs.unlink(path, (value) => {
            });
        }
        catch (matchValue) {
        }
    }
}

export function renameFile(extn, folderPath, baseName_1, newBaseName) {
    const oldPath = path_1.join(folderPath, baseName_1 + extn);
    const newPath = path_1.join(folderPath, newBaseName + extn);
    if (fs.existsSync(oldPath)) {
        try {
            const arg_1 = fs.renameSync(oldPath, newPath);
            return new FSharpResult$2(0, [void 0]);
        }
        catch (e) {
            return new FSharpResult$2(1, [`Rename of '${baseName_1}' in '${folderPath}' failed`]);
        }
    }
    else if (extn === ".dgm") {
        return new FSharpResult$2(1, [`Error: The file '${baseName_1}${extn} appears to have been removed`]);
    }
    else {
        return new FSharpResult$2(0, [void 0]);
    }
}

export function removeFile(folderPath, baseName_1) {
    removeFileWithExtn(".dgm", folderPath, baseName_1);
}

export function removeAutoFile(folderPath, baseName_1) {
    const path = path_1.join(folderPath, baseName_1 + ".dgmauto");
    fs.unlink(path, (value) => {
    });
}

export function readMemDefnLine(addressWidth, wordWidth, lineNo, s) {
    const nums = StringModule_SplitRemoveEmptyEntries([" ", "\t", ",", ";", "\""], s);
    if ((!equalsWith((x, y) => (x === y), nums, defaultOf())) && (nums.length === 2)) {
        const data = nums[1];
        const addr = nums[0];
        const addrNum = strToIntCheckWidth(addressWidth, addr);
        const dataNum = strToIntCheckWidth(wordWidth, data);
        const copyOfStruct = addrNum;
        if (copyOfStruct.tag === 1) {
            const aErr = copyOfStruct.fields[0];
            return new FSharpResult$2(1, [`Line ${lineNo}:'${s}' has invalid address (${addr}). ${aErr}`]);
        }
        else {
            const copyOfStruct_1 = dataNum;
            if (copyOfStruct_1.tag === 1) {
                const dErr = copyOfStruct_1.fields[0];
                return new FSharpResult$2(1, [`Line '${s}' has invalid data item (${data}). ${dErr}`]);
            }
            else {
                const a = copyOfStruct.fields[0];
                const d = copyOfStruct_1.fields[0];
                return new FSharpResult$2(0, [[a, d]]);
            }
        }
    }
    else {
        const x_1 = nums;
        return new FSharpResult$2(1, [`Line ${lineNo}:'${s}' has ${x_1.length} items: valid lines consist of two numbers`]);
    }
}

export function readMemLines(addressWidth, wordWidth, lines) {
    let array;
    const parse = mapIndexed((lineNo, s) => readMemDefnLine(addressWidth, wordWidth, lineNo, s), (array = map_1(StringModule_Trim, lines, null), array.filter((y) => ("" !== y))), null);
    const matchValue = tryFind((_arg) => (_arg.tag === 1), parse);
    if (matchValue != null) {
        const copyOfStruct = matchValue;
        if (copyOfStruct.tag === 1) {
            const firstErr = copyOfStruct.fields[0];
            return new FSharpResult$2(1, [firstErr]);
        }
        else {
            return toFail(printf("What? can\'t happen"));
        }
    }
    else {
        const defs = map_1((_arg_1) => {
            if (_arg_1.tag === 0) {
                const x_1 = _arg_1.fields[0];
                return x_1;
            }
            else {
                return toFail(printf("What?"));
            }
        }, parse, null);
        let repeats;
        const array_2 = Array_groupBy((tuple) => tuple[0], defs, {
            Equals: equals,
            GetHashCode: fastStructuralHash,
        });
        repeats = array_2.filter((tupledArg) => {
            const num = tupledArg[0];
            const vals = tupledArg[1];
            return vals.length > 1;
        });
        if (!equalsWith(equalArrays, repeats, [])) {
            return new FSharpResult$2(1, [toText(interpolate("Memory addresses %A%P() are repeated", [map_1((tuple_1) => tuple_1[0], repeats, null)]))]);
        }
        else {
            return new FSharpResult$2(0, [defs]);
        }
    }
}

export function readMemDefns(addressWidth, wordWidth, fPath) {
    let f1_2, f1_1, f1, separator;
    toConsole(printf("starting defn read"));
    return Result_Bind((f1_2 = ((f1_1 = ((f1 = ((separator = ["\n", "\r"], (str) => StringModule_SplitRemoveEmptyEntries(separator, str))), (arg) => readMemLines(addressWidth, wordWidth, f1(arg)))), (arg_1) => {
        const x = f1_1(arg_1);
        toConsole(printf("read lines"));
        return x;
    })), (arg_2) => Result_Map((elements) => ofArray(elements, {
        Compare: compare,
    }), f1_2(arg_2))), tryReadFileSync(fPath));
}

export function writeMemDefns(fPath, mem) {
    try {
        return new FSharpResult$2(0, [writeFile(fPath, StringModule_Concat("\n", map_1((tupledArg) => {
            const a = tupledArg[0];
            const b = tupledArg[1];
            return `${hex64(a)}	${hex64(b)}`;
        }, sortBy((tuple) => tuple[0], toArray_1(mem.Data), {
            Compare: compare,
        }), null)))]);
    }
    catch (e) {
        return new FSharpResult$2(1, [`Error writing file '${fPath}': ${e.message}`]);
    }
}

export function initialiseMem(mem, projectPath) {
    let memResult;
    const matchValue = mem.Init;
    switch (matchValue.tag) {
        case 1: {
            const name = matchValue.fields[0];
            const fPath = pathJoin([projectPath, name + ".ram"]);
            memResult = readMemDefns(mem.AddressWidth, mem.WordWidth, fPath);
            break;
        }
        case 0: {
            memResult = (new FSharpResult$2(0, [mem.Data]));
            break;
        }
        default: {
            memResult = (new FSharpResult$2(1, [`Unsupported legacy memory type '${mem.Init}'`]));
        }
    }
    return Result_Map((data) => (new Memory1(mem.Init, mem.AddressWidth, mem.WordWidth, data)), memResult);
}

export function savePngFile(folderPath, baseName_1, png) {
    const path = pathJoin([folderPath, baseName_1 + ".png"]);
    return writeFileBase64(path, png);
}

export function formatSavedState(canvas, wave) {
    return new JsonHelpers_SavedInfo(1, [canvas, wave, now()]);
}

export function saveStateToFile(folderPath, baseName_1, state_, state__1, state__2) {
    const state = [state_, state__1, state__2];
    const path = pathJoin([folderPath, baseName_1 + ".dgm"]);
    const data = JsonHelpers_stateToJsonString(state[0], state[1], state[2]);
    return writeFile(path, data);
}

export function createEmptyDgmFile(folderPath, baseName_1) {
    return saveStateToFile(folderPath, baseName_1, [empty(), empty()], void 0, new SheetInfo(new CCForm(0, []), void 0));
}

export function stripVertices(conn) {
    return new LegacyCanvas_LegacyConnection(conn.Id, conn.Source, conn.Target, empty());
}

export function magnifySheet(magnification, comp) {
    return new LegacyCanvas_LegacyComponent(comp.Id, comp.Type, comp.Label, comp.InputPorts, comp.OutputPorts, magnification * (comp.X + (comp.W / 2)), magnification * (comp.Y + (comp.H / 2)), -1, -1);
}

export function getLatestComp(comp) {
    const updateMem = (mem) => (new Memory1(new InitMemData(0, []), mem.AddressWidth, mem.WordWidth, mem.Data));
    const matchValue = comp.Type;
    switch (matchValue.tag) {
        case 50: {
            const mem_1 = matchValue.fields[0];
            return new Component(comp.Id, new ComponentType(46, [updateMem(mem_1)]), comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y, comp.H, comp.W, comp.SymbolInfo);
        }
        case 49: {
            const mem_2 = matchValue.fields[0];
            return new Component(comp.Id, new ComponentType(45, [updateMem(mem_2)]), comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y, comp.H, comp.W, comp.SymbolInfo);
        }
        case 48: {
            const mem_3 = matchValue.fields[0];
            return new Component(comp.Id, new ComponentType(44, [updateMem(mem_3)]), comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y, comp.H, comp.W, comp.SymbolInfo);
        }
        case 8: {
            const width = matchValue.fields[0] | 0;
            const cVal = matchValue.fields[1];
            return new Component(comp.Id, new ComponentType(9, [width, cVal, toText(interpolate("%d%P()", [cVal]))]), comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y, comp.H, comp.W, comp.SymbolInfo);
        }
        case 0: {
            const width_1 = matchValue.fields[0] | 0;
            return new Component(comp.Id, new ComponentType(1, [width_1, void 0]), comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y, comp.H, comp.W, comp.SymbolInfo);
        }
        default: {
            return comp;
        }
    }
}

export function getLatestCanvas(state) {
    const oldCircuitMagnification = 1.25;
    const stripConns = (canvas) => {
        const conns = canvas[1];
        const comps = canvas[0];
        const noVertexConns = map(stripVertices, conns);
        const expandedComps = map((comp) => magnifySheet(oldCircuitMagnification, comp), comps);
        const tupledArg = [expandedComps, noVertexConns];
        return legacyTypesConvert(tupledArg[0], tupledArg[1]);
    };
    let patternInput;
    switch (state.tag) {
        case 1: {
            const canvas_2 = state.fields[0];
            patternInput = stripConns(canvas_2);
            break;
        }
        case 2: {
            const canvas_3 = state.fields[0];
            patternInput = legacyTypesConvert(canvas_3[0], canvas_3[1]);
            break;
        }
        case 3: {
            const canvas_4 = state.fields[0];
            patternInput = canvas_4;
            break;
        }
        case 4: {
            const canvas_5 = state.fields[0];
            patternInput = canvas_5;
            break;
        }
        default: {
            const canvas_1 = state.fields[0];
            patternInput = stripConns(canvas_1);
        }
    }
    const conns_1 = patternInput[1];
    const comps_1 = patternInput[0];
    return [map(getLatestComp, comps_1), conns_1];
}

export function checkMemoryContents(projectPath, comp) {
    let mem, mem_1, mem_2, mem_3;
    const matchValue = comp.Type;
    let matchResult, mem_4;
    if (matchValue.tag === 46) {
        if ((mem = matchValue.fields[0], !StringModule_EndsWith("backup", StringModule_ToLower(projectPath)))) {
            matchResult = 0;
            mem_4 = matchValue.fields[0];
        }
        else {
            matchResult = 1;
        }
    }
    else if (matchValue.tag === 45) {
        if ((mem_1 = matchValue.fields[0], !StringModule_EndsWith("backup", StringModule_ToLower(projectPath)))) {
            matchResult = 0;
            mem_4 = matchValue.fields[0];
        }
        else {
            matchResult = 1;
        }
    }
    else if (matchValue.tag === 44) {
        if ((mem_2 = matchValue.fields[0], !StringModule_EndsWith("backup", StringModule_ToLower(projectPath)))) {
            matchResult = 0;
            mem_4 = matchValue.fields[0];
        }
        else {
            matchResult = 1;
        }
    }
    else if (matchValue.tag === 47) {
        if ((mem_3 = matchValue.fields[0], !StringModule_EndsWith("backup", StringModule_ToLower(projectPath)))) {
            matchResult = 0;
            mem_4 = matchValue.fields[0];
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
            const matchValue_1 = mem_4.Init;
            if (matchValue_1.tag === 1) {
                const fName = matchValue_1.fields[0];
                const fPath = pathJoin([projectPath, fName + ".ram"]);
                const memData = readMemDefns(mem_4.AddressWidth, mem_4.WordWidth, fPath);
                if (memData.tag === 1) {
                    const msg = memData.fields[0];
                    toConsole(`Error reloading component ${comp.Label} from its file ${fPath}:
${msg}`);
                    return comp;
                }
                else {
                    const memDat = memData.fields[0];
                    if (!memDat.Equals(mem_4.Data)) {
                        toConsole(printf("%s"))(`Warning! RAM file ${fPath} has changed so component ${comp.Label} is now different`);
                    }
                    const mem_5 = new Memory1(mem_4.Init, mem_4.AddressWidth, mem_4.WordWidth, memDat);
                    return new Component(comp.Id, getMemType(comp.Type)(mem_5), comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y, comp.H, comp.W, comp.SymbolInfo);
                }
            }
            else {
                return comp;
            }
        }
        case 1: {
            return comp;
        }
    }
}

export function makeLoadedComponentFromCanvasData(canvas_, canvas__1, filePath, timeStamp, waveInfo, sheetInfo) {
    const canvas = [canvas_, canvas__1];
    const projectPath = path_1.dirname(filePath);
    const patternInput = parseDiagramSignature(canvas[0], canvas[1]);
    const outputs = patternInput[1];
    const inputs = patternInput[0];
    const conns = canvas[1];
    const comps = canvas[0];
    const comps$0027 = map((comp) => checkMemoryContents(projectPath, comp), comps);
    const canvas_1 = [comps$0027, conns];
    const ramChanges = map((tuple) => tuple[0], filter_1((tupledArg) => {
        const c1 = tupledArg[0];
        const c2 = tupledArg[1];
        return !equals_1(c1.Type, c2.Type);
    }, zip(comps$0027, comps)));
    let patternInput_1;
    if (sheetInfo != null) {
        const sI = sheetInfo;
        patternInput_1 = [sI.Form, sI.Description];
    }
    else {
        patternInput_1 = [new CCForm(0, []), void 0];
    }
    const form = patternInput_1[0];
    const description = patternInput_1[1];
    const ldc = new LoadedComponent(getBaseNameNoExtension(filePath), timeStamp, filePath, waveInfo, canvas_1, inputs, outputs, form, description);
    return [ldc, ramChanges];
}

export function tryLoadComponentFromPath(filePath) {
    let arg_1;
    const matchValue = tryLoadStateFromPath(filePath);
    let matchResult, msg, state;
    if (matchValue.tag === 0) {
        const copyOfStruct = matchValue.fields[0];
        if (copyOfStruct.tag === 0) {
            matchResult = 1;
            state = copyOfStruct.fields[0];
        }
        else {
            matchResult = 0;
            msg = copyOfStruct.fields[0];
        }
    }
    else {
        matchResult = 0;
        msg = matchValue.fields[0];
    }
    switch (matchResult) {
        case 0: {
            return new FSharpResult$2(1, [(arg_1 = getBaseNameNoExtension(filePath), toText(printf("Can\'t load component %s because of Error: %s"))(arg_1)(msg))]);
        }
        case 1: {
            const canvas = getLatestCanvas(state);
            return new FSharpResult$2(0, [makeLoadedComponentFromCanvasData(canvas[0], canvas[1], filePath, JsonHelpers_SavedInfo__get_getTimeStamp(state), JsonHelpers_SavedInfo__get_getWaveInfo(state), JsonHelpers_SavedInfo__get_getSheetInfo(state))[0]]);
        }
    }
}

export class LoadStatus extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Resolve", "OkComp", "OkAuto"];
    }
}

export function LoadStatus$reflection() {
    return union_type("FilesIO.LoadStatus", [], LoadStatus, () => [[["Item1", LoadedComponent$reflection()], ["Item2", LoadedComponent$reflection()]], [["Item", LoadedComponent$reflection()]], [["Item", LoadedComponent$reflection()]]]);
}

export function loadAllComponentFiles(folderPath) {
    let x;
    try {
        x = (new FSharpResult$2(0, [fs.readdirSync(folderPath)]));
    }
    catch (e) {
        x = (new FSharpResult$2(1, [toText(printf("Error reading Issie project directory at \'%s: %A"))(folderPath)(e)]));
    }
    if (x.tag === 0) {
        const x_1 = x.fields[0];
        return tryFindError(map((fileName) => {
            let ldComp_1, autoComp_1;
            if (fileNameIsBad(pathWithoutExtension(fileName).split(""))) {
                return new FSharpResult$2(1, [toText(printf("Can\'t load file name \'%s\' from project \'%s\' because it contains incorrect characters.\\n \\\r\n                    File names used as sheets must contain only alphanumeric and space characters before the \'.dgm\' extension"))(fileName)(folderPath)]);
            }
            else {
                const filePath = path_1.join(folderPath, fileName);
                toConsole(`loading ${fileName}`);
                const ldComp = tryLoadComponentFromPath(filePath);
                const autoComp = tryLoadComponentFromPath(filePath + "auto");
                toConsole(`${fileName} Loaded`);
                let matchResult, autoComp_2, ldComp_2, ldComp_3, autoComp_3, msg_1;
                const copyOfStruct = ldComp;
                if (copyOfStruct.tag === 1) {
                    const copyOfStruct_1 = autoComp;
                    if (copyOfStruct_1.tag === 0) {
                        matchResult = 2;
                        autoComp_3 = copyOfStruct_1.fields[0];
                    }
                    else {
                        matchResult = 3;
                        msg_1 = copyOfStruct.fields[0];
                    }
                }
                else {
                    const copyOfStruct_2 = autoComp;
                    if (copyOfStruct_2.tag === 0) {
                        if ((ldComp_1 = copyOfStruct.fields[0], (autoComp_1 = copyOfStruct_2.fields[0], compare_1(ldComp_1.TimeStamp, autoComp_1.TimeStamp) < 0))) {
                            matchResult = 0;
                            autoComp_2 = copyOfStruct_2.fields[0];
                            ldComp_2 = copyOfStruct.fields[0];
                        }
                        else {
                            matchResult = 1;
                            ldComp_3 = copyOfStruct.fields[0];
                        }
                    }
                    else {
                        matchResult = 1;
                        ldComp_3 = copyOfStruct.fields[0];
                    }
                }
                switch (matchResult) {
                    case 0: {
                        return new FSharpResult$2(0, [new LoadStatus(0, [ldComp_2, autoComp_2])]);
                    }
                    case 1: {
                        return new FSharpResult$2(0, [new LoadStatus(1, [ldComp_3])]);
                    }
                    case 2: {
                        return new FSharpResult$2(0, [new LoadStatus(2, [autoComp_3])]);
                    }
                    case 3: {
                        return new FSharpResult$2(1, [msg_1]);
                    }
                }
            }
        }, filter_1((arg_5) => (".dgm" === path_1.extname(arg_5)), toList(x_1))));
    }
    else {
        const msg = x.fields[0];
        return new FSharpResult$2(1, [msg]);
    }
}

export function askForNewFile(projectPath) {
    const options = {};
    options.filters = Array.from(ramFileFilters);
    options.defaultPath = projectPath;
    options.title = "Enter new file name";
    options.nameFieldLabel = "New file name";
    options.buttonLabel = "Save memory content to file";
    options.properties = ["showOverwriteConfirmation"];
    const w = remote.getCurrentWindow();
    return remote.dialog.showSaveDialogSync(options);
}

export function saveAllProjectFilesFromLoadedComponentsToDisk(proj) {
    iterate((ldc) => {
        const name = ldc.Name;
        const state = ldc.CanvasState;
        const waveInfo = ldc.WaveInfo;
        const sheetInfo = new SheetInfo(ldc.Form, ldc.Description);
        saveStateToFile(proj.ProjectPath, name, state, waveInfo, sheetInfo);
        removeFileWithExtn(".dgmauto", proj.ProjectPath, name);
    }, proj.LoadedComponents);
}

export function openWriteDialogAndWriteMemory(mem, path) {
    const matchValue = askForNewFile(path);
    if (matchValue != null) {
        const fpath = matchValue;
        const fpath$0027 = (!StringModule_Contains(".", fpath)) ? (fpath + ".ram") : fpath;
        writeMemDefns(fpath$0027, mem);
        return fpath$0027;
    }
    else {
        return void 0;
    }
}

//# sourceMappingURL=FilesIO.fs.js.map

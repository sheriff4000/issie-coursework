import * as react from "react";
import { keyValueList } from "../fable_modules/fable-library.4.0.0-theta-018/MapUtil.js";
import { HTMLAttr } from "../fable_modules/Fable.React.8.0.1/Fable.React.Props.fs.js";
import { length, empty as empty_1, tryLast, append, singleton, indexed, fold, item, collect, sortBy, ofArray } from "../fable_modules/fable-library.4.0.0-theta-018/List.js";
import { defaultOf, int32ToString, comparePrimitives } from "../fable_modules/fable-library.4.0.0-theta-018/Util.js";
import { split, printf, toText } from "../fable_modules/fable-library.4.0.0-theta-018/String.js";
import { exists, toList } from "../fable_modules/fable-library.4.0.0-theta-018/Seq.js";
import { rangeDouble } from "../fable_modules/fable-library.4.0.0-theta-018/Range.js";
import { add, tryFind, empty } from "../fable_modules/fable-library.4.0.0-theta-018/Map.js";
import { ErrorInfo, ExtraErrorInfo, ReplaceType } from "./VerilogTypes.fs.js";
import { Option, button } from "../fable_modules/Fulma.2.16.0/Elements/Button.fs.js";
import { Size_ISize } from "../fable_modules/Fulma.2.16.0/Common.fs.js";
import { value } from "../fable_modules/fable-library.4.0.0-theta-018/Option.js";

export function getErrorDiv(errorList) {
    const getUnderLineElement = (marginLeft, _line, message) => {
        let props, props_2;
        return ofArray([(props = [["style", {
            display: "inline-block",
            marginLeft: marginLeft,
            pointerEvents: "stroke",
        }]], react.createElement("span", keyValueList(props, 1))), (props_2 = [new HTMLAttr(65, ["error"]), ["style", {
            pointerEvents: "auto",
            fontSize: 16,
            color: "rgb(255,0,0)",
            background: "rgba(255,0,0,0)",
        }]], react.createElement("span", keyValueList(props_2, 1), _line)), react.createElement("span", {
            className: "hide",
        }, message)]);
    };
    const getErrorLine = (errorLineList) => {
        const sortedErrors = sortBy((e) => e.Col, errorLineList, {
            Compare: comparePrimitives,
        });
        const linechildren = collect((tupledArg) => {
            const index = tupledArg[0] | 0;
            const err = tupledArg[1];
            const prevErrorEnd = (index === 0) ? 0 : (((item(index - 1, sortedErrors).Col + item(index - 1, sortedErrors).Length) - 1) * 8.8);
            let spaces;
            const arg = ((err.Col - 1) * 8.8) - prevErrorEnd;
            spaces = toText(printf("%fpx"))(arg);
            const _line_1 = fold((s_2, v) => (s_2 + "-"), "", toList(rangeDouble(1, 1, err.Length)));
            return getUnderLineElement(spaces, _line_1, err.Message);
        }, indexed(sortedErrors));
        return singleton(react.createElement("p", {}, ...linechildren));
    };
    const getLineToErrorsMap = (sortedErrorList) => {
        const emptyMap = empty({
            Compare: comparePrimitives,
        });
        return fold((state_1, err_1) => {
            const matchValue = tryFind(err_1.Line, state_1);
            if (matchValue == null) {
                return add(err_1.Line, singleton(err_1), state_1);
            }
            else {
                const found = matchValue;
                return add(err_1.Line, append(found, singleton(err_1)), state_1);
            }
        }, emptyMap, sortedErrorList);
    };
    const sortedByLineErrorList = sortBy((err_2) => err_2.Line, errorList, {
        Compare: comparePrimitives,
    });
    const lineToErrorsMap = getLineToErrorsMap(sortedByLineErrorList);
    let childrenElements;
    const matchValue_1 = tryLast(sortedByLineErrorList);
    if (matchValue_1 == null) {
        childrenElements = empty_1();
    }
    else {
        const lastError = matchValue_1;
        childrenElements = collect((line) => {
            const matchValue_2 = tryFind(line, lineToErrorsMap);
            if (matchValue_2 == null) {
                return singleton(react.createElement("br", {}));
            }
            else {
                const errors = matchValue_2;
                return getErrorLine(errors);
            }
        }, toList(rangeDouble(1, 1, lastError.Line)));
    }
    const props_10 = [["style", {
        position: "absolute",
        display: "block",
        width: "100%",
        height: "-13px",
        top: "13px",
        left: "40px",
        right: "0",
        bottom: "0px",
        backgroundColor: "rgba(0,0,0,0)",
        fontWeight: "bold",
        color: "Red",
        zIndex: "2",
        pointerEvents: "none",
        whiteSpace: "pre-line",
    }]];
    return react.createElement("div", keyValueList(props_10, 1), ...childrenElements);
}

export function getSyntaxErrorInfo(error) {
    if (exists((ch) => (ch === ";"), error.Message.split("")) && (!exists((ch_1) => (ch_1 === "."), error.Message.split("")))) {
        return new ErrorInfo(error.Line, error.Col, error.Length, error.Message, [new ExtraErrorInfo("Your previous line is not terminated with a semicolon (;)", false, new ReplaceType(3, []))]);
    }
    else if (exists((ch_2) => (ch_2 === "\'"), error.Message.split(""))) {
        return new ErrorInfo(error.Line, error.Col, error.Length, error.Message, [new ExtraErrorInfo("Numbers must be of format: <size>\'<radix><value>\n  e.g. 16\'h3fa5;", false, new ReplaceType(3, []))]);
    }
    else {
        return new ErrorInfo(error.Line, error.Col, error.Length, error.Message, [new ExtraErrorInfo(error.Message, false, new ReplaceType(3, []))]);
    }
}

export function getErrorTable(errorList, addButton) {
    let children_18, props_18, props_20, children_26, children_24, props_24;
    const getSuggestionLine = (suggestions, replaceType, line) => {
        let props_2;
        const buttons = collect((suggestion) => {
            let props;
            return ofArray([(props = [["style", {
                whiteSpace: "pre",
            }]], react.createElement("span", keyValueList(props, 1), "    ")), button(ofArray([new Option(18, [(_arg) => {
                addButton([suggestion, replaceType, line]);
            }]), new Option(1, [new Size_ISize(0, [])])]), singleton(suggestion))]);
        }, toList(suggestions));
        const line_1 = append(singleton((props_2 = [["style", {
            color: "Red",
            fontStyle: "Italic",
            verticalAlign: "Middle",
        }]], react.createElement("span", keyValueList(props_2, 1), "\tDo you mean:"))), buttons);
        const props_4 = [["style", {
            whiteSpace: "pre",
        }]];
        return react.createElement("td", keyValueList(props_4, 1), ...line_1);
    };
    const getErrorTableLine = (index, extraMessage, line_2) => {
        let children_8, props_6, children_14, props_10, props_12;
        const copyable = extraMessage.Copy;
        const text = extraMessage.Text;
        const showLine = (index === 0) ? ("  Line " + int32ToString(line_2)) : "";
        if (copyable) {
            const suggestions_1 = split(text, ["|"], null, 1);
            return singleton((children_8 = [(props_6 = [["style", {
                color: "Black",
                verticalAlign: "Middle",
                whiteSpace: "pre",
            }]], react.createElement("td", keyValueList(props_6, 1), showLine)), getSuggestionLine(suggestions_1, extraMessage.Replace, line_2)], react.createElement("tr", {}, ...children_8)));
        }
        else {
            return singleton((children_14 = [(props_10 = [["style", {
                color: "Black",
                verticalAlign: "Middle",
                whiteSpace: "pre",
            }]], react.createElement("td", keyValueList(props_10, 1), showLine)), (props_12 = [["style", {
                color: "Black",
                whiteSpace: "pre-wrap",
            }]], react.createElement("td", keyValueList(props_12, 1), text))], react.createElement("tr", {}, ...children_14)));
        }
    };
    const getErrorTableLines = (error) => {
        const line_3 = error.Line | 0;
        if (error.ExtraErrors == null) {
            return defaultOf();
        }
        else {
            const tLine = collect((tupledArg) => {
                const index_1 = tupledArg[0] | 0;
                const mess = tupledArg[1];
                return getErrorTableLine(index_1, mess, line_3);
            }, indexed(ofArray(value(error.ExtraErrors))));
            return react.createElement("tbody", {}, ...tLine);
        }
    };
    const tableFormat = ofArray([(children_18 = [(props_18 = [["style", {
        width: "20%",
    }]], react.createElement("col", keyValueList(props_18, 1))), (props_20 = [["style", {
        width: "80%",
        whiteSpace: "pre-line",
    }]], react.createElement("col", keyValueList(props_20, 1)))], react.createElement("colgroup", {}, ...children_18)), (children_26 = [(children_24 = [(props_24 = [["style", {
        whiteSpace: "pre",
    }]], react.createElement("th", keyValueList(props_24, 1), "  Line")), react.createElement("th", {}, "Message")], react.createElement("tr", {}, ...children_24))], react.createElement("thead", {}, ...children_26))]);
    const tableLines = collect((err_1) => singleton(getErrorTableLines(err_1)), sortBy((err) => err.Line, errorList, {
        Compare: comparePrimitives,
    }));
    const tableChildren = append(tableFormat, tableLines);
    if (length(tableLines) !== 0) {
        const props_32 = [["style", {
            fontSize: "16px",
            tableLayout: "Fixed",
            width: "100%",
            borderRight: "groove",
            borderLeft: "groove",
        }]];
        return react.createElement("table", keyValueList(props_32, 1), ...tableChildren);
    }
    else {
        return react.createElement("table", {});
    }
}

export function getLineCounterDiv(linesNo) {
    let children_2, arg_1;
    const childrenElements = collect((no) => {
        let children;
        return ofArray([(children = [toText(printf("%i"))(no)], react.createElement("span", {}, ...children)), react.createElement("br", {})]);
    }, toList(rangeDouble(1, 1, linesNo)));
    const childrenElements$0027 = append(childrenElements, singleton((children_2 = [(arg_1 = ((linesNo + 1) | 0), toText(printf("%i"))(arg_1))], react.createElement("span", {}, ...children_2))));
    const props_6 = [["style", {
        position: "absolute",
        display: "block",
        width: "3%",
        height: "-5px",
        top: "5px",
        left: "2px",
        right: "0",
        bottom: "0px",
        backgroundColor: "rgba(255,0,0,0)",
        color: "#7f7f7f",
        zIndex: "2",
        pointerEvents: "none",
        textAlign: "right",
        whiteSpace: "pre-line",
    }]];
    return react.createElement("div", keyValueList(props_6, 1), ...childrenElements$0027);
}

export const infoHoverableElement = (() => {
    let children_4, props;
    const example = "\tTHIS IS AN EXAMPLE OF A VALID VERILOG FILE\n----------------------------------------------------------\nmodule decoder(\n\tinput [15:0] instr,\n\tinput n,z,c,\n\toutput mux1sel,jump,\n\toutput [2:0] aluF\n);\n\twire cond = n|z|c;\n\tassign mux1sel = instr[15]&cond | instr[14];\n\tassign j = instr[8]&(n|c);\n\tassign aluF = intr[10:8];\nendmodule";
    const props_6 = [["style", {
        position: "absolute",
        display: "block",
        width: "100%",
        height: "100%",
        top: "-10px",
        left: "104px",
        right: "0",
        bottom: "0",
        backgroundColor: "rgba(0,0,0,0)",
        color: "#7f7f7f",
        zIndex: "2",
        pointerEvents: "none",
        textDecoration: "underline",
        textDecorationColor: "rgb(0,0,255)",
        textAlign: "left",
        whiteSpace: "pre-wrap",
    }]];
    const children_6 = [(children_4 = [(props = [new HTMLAttr(65, ["error"]), ["style", {
        pointerEvents: "auto",
        fontSize: 16,
        color: "rgb(0,0,255)",
        background: "rgba(255,0,0,0)",
    }]], react.createElement("span", keyValueList(props, 1), "example")), react.createElement("span", {
        className: "hide",
    }, example)], react.createElement("p", {
        className: "info",
    }, ...children_4))];
    return react.createElement("div", keyValueList(props_6, 1), ...children_6);
})();

//# sourceMappingURL=CodeEditorHelpers.fs.js.map

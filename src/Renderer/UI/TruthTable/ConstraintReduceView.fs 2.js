import { CSSProp, DOMAttr, HTMLAttr } from "../../fable_modules/Fable.React.8.0.1/Fable.React.Props.fs.js";
import { dataTooltip } from "../../fable_modules/Fulma.Extensions.Wikiki.Tooltip.3.0.0/Tooltip.fs.js";
import * as react_2 from "react";
import { keyValueList } from "../../fable_modules/fable-library.4.0.0-theta-018/MapUtil.js";
import { makeInequalityConstraint, EqualityConstraint, isEqu, ConstraintType, CellIO__get_getWidth, Constraint, CellIO__get_getLabel } from "../../Simulator/TruthTableTypes.fs.js";
import { toFail, toConsole, isNullOrWhiteSpace, printf, toText } from "../../fable_modules/fable-library.4.0.0-theta-018/String.js";
import { filter, isEmpty, head, contains, fold, append, ofArray, singleton, empty, map } from "../../fable_modules/fable-library.4.0.0-theta-018/List.js";
import { Item_Option, right, left, level, item } from "../../fable_modules/Fulma.2.16.0/Layouts/Level.fs.js";
import { list as list_3, Option, tag as tag_52 } from "../../fable_modules/Fulma.2.16.0/Elements/Tag.fs.js";
import { Color_IColor } from "../../fable_modules/Fulma.2.16.0/Common.fs.js";
import { Option as Option_1, delete$ } from "../../fable_modules/Fulma.2.16.0/Elements/Delete.fs.js";
import { Msg } from "../ModelType.fs.js";
import { partialApply, arrayHash, equalArrays, safeHash, equals } from "../../fable_modules/fable-library.4.0.0-theta-018/Util.js";
import { strToIntCheckWidth, convertIntToWireData, convertWireDataToInt } from "../../Simulator/NumberHelpers.fs.js";
import { toInt, compare, equals as equals_1, fromInteger } from "../../fable_modules/fable-library.4.0.0-theta-018/Long.js";
import { FSharpResult$2 } from "../../fable_modules/fable-library.4.0.0-theta-018/Choice.js";
import { button, Option as Option_2 } from "../../fable_modules/Fulma.2.16.0/Elements/Button.fs.js";
import { Option as Option_3, div } from "../../fable_modules/Fulma.2.16.0/Elements/Form/Field.fs.js";
import { div as div_1 } from "../../fable_modules/Fulma.2.16.0/Elements/Form/Control.fs.js";
import { input } from "../../fable_modules/Fulma.2.16.0/Elements/Form/./Input.fs.js";
import { IInputType, Option as Option_4 } from "../../fable_modules/Fulma.2.16.0/Elements/Form/Input.fs.js";
import { baseSelector, viewNum } from "../MemoryEditorView.fs.js";
import { NumberBase } from "../../Common/CommonTypes.fs.js";
import { constraintNumberStyle } from "../Style.fs.js";
import { getTextEventValue } from "../../Interface/JSHelpers.fs.js";
import { makeIOLabel } from "../SimulationView.fs.js";
import { h5, h6 } from "../../fable_modules/Fulma.2.16.0/Elements/Heading.fs.js";
import { toList } from "../../fable_modules/fable-library.4.0.0-theta-018/Map.js";
import { dynamicClosablePopup, dialogPopup } from "../PopupView.fs.js";
import { AlgebraNotImplemented } from "../../Simulator/SimulatorTypes.fs.js";
import { changeInput } from "../../Simulator/Fast/FastRun.fs.js";
import { toString } from "../../fable_modules/fable-library.4.0.0-theta-018/Types.js";

export function addToolTipTop(tip, react) {
    const props = [new HTMLAttr(64, [`${"tooltip"} has-tooltip-top`]), dataTooltip(tip)];
    return react_2.createElement("div", keyValueList(props, 1), react);
}

export function addToolTipRight(tip, react) {
    const props = [new HTMLAttr(64, [`${"tooltip"} has-tooltip-right`]), dataTooltip(tip)];
    return react_2.createElement("div", keyValueList(props, 1), react);
}

export function inCon2str(con) {
    if (con.tag === 1) {
        const i = con.fields[0];
        const arg_3 = CellIO__get_getLabel(i.IO);
        return toText(printf("0x%X ≤ %s ≤ 0x%X"))(i.LowerBound)(arg_3)(i.UpperBound);
    }
    else {
        const e = con.fields[0];
        const arg = CellIO__get_getLabel(e.IO);
        return toText(printf("%s = 0x%X"))(arg)(e.Value);
    }
}

export function makeElementLine(elsLeftAlign, elsRightAlign) {
    const itemListLeft = map((el) => item(empty(), singleton(el)), elsLeftAlign);
    const itemListRight = map((el_1) => item(empty(), singleton(el_1)), elsRightAlign);
    return level(empty(), ofArray([left(empty(), itemListLeft), right(empty(), itemListRight)]));
}

export function viewNumericalConstraints(cons, dispatch) {
    const makeConTag = (con) => {
        const tagText = inCon2str(con);
        return tag_52(ofArray([new Option(1, [new Color_IColor(5, [])]), new Option(3, [])]), ofArray([tagText, delete$(singleton(new Option_1(3, [(_arg) => {
            dispatch(new Msg(33, [con]));
            dispatch(new Msg(34, [con]));
        }])), empty())]));
    };
    const equEls = map((con_1) => makeConTag(new Constraint(0, [con_1])), cons.Equalities);
    const inequEls = map((con_2) => makeConTag(new Constraint(1, [con_2])), cons.Inequalities);
    const tags = append(equEls, inequEls);
    return list_3(empty(), tags);
}

export function constraintsOverlap(con1, con2) {
    const equAndIneqOverlap = (equ, ineq) => {
        if (equals(equ.IO, ineq.IO) && (equ.Value >= ineq.LowerBound)) {
            return equ.Value <= ineq.UpperBound;
        }
        else {
            return false;
        }
    };
    const checkTwoIneq = (in1, in2) => {
        if (equals(in1.IO, in2.IO)) {
            if ((in1.LowerBound >= in2.LowerBound) && (in1.LowerBound <= in2.UpperBound)) {
                return true;
            }
            else if (in1.UpperBound >= in2.LowerBound) {
                return in1.UpperBound <= in2.UpperBound;
            }
            else {
                return false;
            }
        }
        else {
            return false;
        }
    };
    if (con1.tag === 1) {
        if (con2.tag === 1) {
            const c1_3 = con1.fields[0];
            const c2_3 = con2.fields[0];
            if (checkTwoIneq(c1_3, c2_3)) {
                return true;
            }
            else {
                return checkTwoIneq(c2_3, c1_3);
            }
        }
        else {
            const c1_2 = con1.fields[0];
            const c2_2 = con2.fields[0];
            return equAndIneqOverlap(c2_2, c1_2);
        }
    }
    else if (con2.tag === 1) {
        const c1_1 = con1.fields[0];
        const c2_1 = con2.fields[0];
        return equAndIneqOverlap(c1_1, c2_1);
    }
    else {
        const c1 = con1.fields[0];
        const c2 = con2.fields[0];
        return equals(c1, c2);
    }
}

export function validateNumericalConstraint(con, allConstraints) {
    const conText = inCon2str(con);
    if (con.tag === 1) {
        const ineq = con.fields[0];
        const width = CellIO__get_getWidth(ineq.IO) | 0;
        const unsignedLower = convertWireDataToInt(convertIntToWireData(width, fromInteger(ineq.LowerBound, false, 2)));
        const unsignedUpper = convertWireDataToInt(convertIntToWireData(width, fromInteger(ineq.UpperBound, false, 2)));
        if (equals_1(unsignedLower, unsignedUpper)) {
            return new FSharpResult$2(1, ["Lower Bound and Upper Bound cannot have the same value."]);
        }
        else if (compare(unsignedLower, unsignedUpper) > 0) {
            return new FSharpResult$2(1, ["Lower Bound cannot have a greater value than Upper Bound"]);
        }
        else {
            const checkWithEqu = fold((state_2, c_1) => {
                if (state_2.tag === 0) {
                    const ineqc = state_2.fields[0];
                    if (constraintsOverlap(con, new Constraint(0, [c_1]))) {
                        const constr_1 = inCon2str(new Constraint(0, [c_1]));
                        return new FSharpResult$2(1, [toText(printf("This constraint overlaps with another constraint: %s.\n                            Please change your new constraint or delete the old one."))(constr_1)]);
                    }
                    else {
                        return new FSharpResult$2(0, [ineqc]);
                    }
                }
                else {
                    const err_2 = state_2.fields[0];
                    return new FSharpResult$2(1, [err_2]);
                }
            }, new FSharpResult$2(0, [ineq]), allConstraints.Equalities);
            const _arg_1 = fold((state_4, c_2) => {
                if (state_4.tag === 0) {
                    const ineqc_1 = state_4.fields[0];
                    if (constraintsOverlap(con, new Constraint(1, [c_2]))) {
                        const constr_2 = inCon2str(new Constraint(1, [c_2]));
                        return new FSharpResult$2(1, [toText(printf("This constraint overlaps with another constraint: %s.\n                            Please change your new constraint or delete the old one."))(constr_2)]);
                    }
                    else {
                        return new FSharpResult$2(0, [ineqc_1]);
                    }
                }
                else {
                    const err_3 = state_4.fields[0];
                    return new FSharpResult$2(1, [err_3]);
                }
            }, checkWithEqu, allConstraints.Inequalities);
            if (_arg_1.tag === 0) {
                const ineqc_2 = _arg_1.fields[0];
                return new FSharpResult$2(0, [new Constraint(1, [ineqc_2])]);
            }
            else {
                const err_4 = _arg_1.fields[0];
                return new FSharpResult$2(1, [err_4]);
            }
        }
    }
    else {
        const e = con.fields[0];
        if (contains(e, allConstraints.Equalities, {
            Equals: equals,
            GetHashCode: safeHash,
        })) {
            return new FSharpResult$2(1, [toText(printf("Constraint \'%s\' already exists."))(conText)]);
        }
        else {
            const _arg = fold((state, c) => {
                if (state.tag === 0) {
                    const eqc = state.fields[0];
                    if (constraintsOverlap(con, new Constraint(1, [c]))) {
                        const constr = inCon2str(new Constraint(1, [c]));
                        return new FSharpResult$2(1, [toText(printf("This constraint overlaps with another constraint: %s.\n                        Please change your new constraint or delete the old one."))(constr)]);
                    }
                    else {
                        return new FSharpResult$2(0, [eqc]);
                    }
                }
                else {
                    const err = state.fields[0];
                    return new FSharpResult$2(1, [err]);
                }
            }, new FSharpResult$2(0, [e]), allConstraints.Inequalities);
            if (_arg.tag === 0) {
                const eqc_1 = _arg.fields[0];
                return new FSharpResult$2(0, [new Constraint(0, [eqc_1])]);
            }
            else {
                const err_1 = _arg.fields[0];
                return new FSharpResult$2(1, [err_1]);
            }
        }
    }
}

export function dialogPopupNumericalConBody(cellIOs, existingCons, infoMsg, dispatch, dialogData) {
    let arg_29, arg_28;
    const preamble = infoMsg;
    let selected;
    const matchValue = dialogData.ConstraintIOSel;
    if (matchValue != null) {
        const io = matchValue;
        selected = io;
    }
    else {
        dispatch(new Msg(72, [head(cellIOs)]));
        selected = head(cellIOs);
    }
    let ioSelect;
    const buttons = map((io_1) => {
        const action = (_arg) => {
            dispatch(new Msg(72, [io_1]));
            dispatch(new Msg(73, [void 0]));
        };
        const buttonProps = equals(io_1, selected) ? ofArray([new Option_2(0, [new Color_IColor(4, [])]), new Option_2(18, [action])]) : singleton(new Option_2(18, [action]));
        return button(buttonProps, singleton(CellIO__get_getLabel(io_1)));
    }, cellIOs);
    ioSelect = react_2.createElement("div", {}, ...buttons);
    let typeSelect;
    if (CellIO__get_getWidth(selected) === 1) {
        dispatch(new Msg(71, [new ConstraintType(0, [])]));
        typeSelect = item(singleton(new Item_Option(1, [])), singleton(div(singleton(new Option_3(1, [])), singleton(div_1(empty(), singleton(button(ofArray([new Option_2(0, [new Color_IColor(4, [])]), new Option_2(18, [(_arg_1) => {
            dispatch(new Msg(71, [new ConstraintType(0, [])]));
        }])]), singleton("Equality Constraint"))))))));
    }
    else {
        const matchValue_1 = dialogData.ConstraintTypeSel;
        if (matchValue_1 != null) {
            const x = matchValue_1;
            typeSelect = item(singleton(new Item_Option(1, [])), singleton(div(singleton(new Option_3(1, [])), ofArray([div_1(empty(), singleton(button(ofArray([new Option_2(0, [isEqu(x) ? (new Color_IColor(4, [])) : (new Color_IColor(20, []))]), new Option_2(18, [(_arg_2) => {
                dispatch(new Msg(71, [new ConstraintType(0, [])]));
                dispatch(new Msg(73, [void 0]));
            }])]), singleton("Equality Constraint")))), div_1(empty(), singleton(button(ofArray([new Option_2(0, [(!isEqu(x)) ? (new Color_IColor(4, [])) : (new Color_IColor(20, []))]), new Option_2(18, [(_arg_3) => {
                dispatch(new Msg(71, [new ConstraintType(1, [])]));
                dispatch(new Msg(73, [void 0]));
            }])]), singleton("Inequality Constraint"))))]))));
        }
        else {
            dispatch(new Msg(71, [new ConstraintType(0, [])]));
            typeSelect = react_2.createElement("div", {});
        }
    }
    const numField1 = (width) => input(ofArray([new Option_4(1, [new IInputType(0, [])]), new Option_4(9, ["Hex"]), new Option_4(10, [viewNum(new NumberBase(0, []))(fromInteger(0, false, 2))]), new Option_4(15, [ofArray([constraintNumberStyle, new DOMAttr(9, [(arg_20) => {
        const text = getTextEventValue(arg_20);
        const matchValue_2 = strToIntCheckWidth(width, text);
        const matchValue_3 = isNullOrWhiteSpace(text);
        if (matchValue_3) {
            dispatch(new Msg(73, ["Blank constraint field"]));
        }
        else {
            const copyOfStruct = matchValue_2;
            if (copyOfStruct.tag === 0) {
                const num = copyOfStruct.fields[0];
                dispatch(new Msg(73, [void 0]));
                dispatch(new Msg(61, [~(~toInt(num))]));
            }
            else {
                const err = copyOfStruct.fields[0];
                dispatch(new Msg(73, [err]));
            }
        }
    }])])])]));
    const numField2 = (width_1) => input(ofArray([new Option_4(1, [new IInputType(0, [])]), new Option_4(9, ["Hex"]), new Option_4(10, [viewNum(new NumberBase(0, []))(fromInteger(0, false, 2))]), new Option_4(15, [ofArray([constraintNumberStyle, new DOMAttr(9, [(arg_27) => {
        const text_1 = getTextEventValue(arg_27);
        const matchValue_5 = strToIntCheckWidth(width_1, text_1);
        const matchValue_6 = isNullOrWhiteSpace(text_1);
        if (matchValue_6) {
            dispatch(new Msg(73, ["Blank constraint field"]));
        }
        else {
            const copyOfStruct_1 = matchValue_5;
            if (copyOfStruct_1.tag === 0) {
                const num_1 = copyOfStruct_1.fields[0];
                dispatch(new Msg(73, [void 0]));
                dispatch(new Msg(62, [num_1]));
            }
            else {
                const err_1 = copyOfStruct_1.fields[0];
                dispatch(new Msg(73, [err_1]));
            }
        }
    }])])])]));
    let constraintEditor;
    const matchValue_8 = dialogData.ConstraintTypeSel;
    const matchValue_9 = dialogData.ConstraintIOSel;
    if (matchValue_8 != null) {
        if (matchValue_9 != null) {
            if (matchValue_8.tag === 1) {
                const io_3 = matchValue_9;
                const patternInput_1 = [CellIO__get_getLabel(io_3), CellIO__get_getWidth(io_3)];
                const width_3 = patternInput_1[1] | 0;
                const label_1 = patternInput_1[0];
                constraintEditor = makeElementLine(ofArray([numField1(width_3), (arg_29 = makeIOLabel(label_1, width_3), toText(printf("≤ %s ≤"))(arg_29)), numField2(width_3)]), empty());
            }
            else {
                const io_2 = matchValue_9;
                const patternInput = [CellIO__get_getLabel(io_2), CellIO__get_getWidth(io_2)];
                const width_2 = patternInput[1] | 0;
                const label = patternInput[0];
                constraintEditor = makeElementLine(ofArray([(arg_28 = makeIOLabel(label, width_2), toText(printf("%s = "))(arg_28)), numField1(width_2)]), empty());
            }
        }
        else {
            constraintEditor = react_2.createElement("div", {});
        }
    }
    else {
        constraintEditor = react_2.createElement("div", {});
    }
    let errorMsg;
    const matchValue_11 = dialogData.ConstraintErrorMsg;
    if (matchValue_11 != null) {
        const msg = matchValue_11;
        const children_10 = [react_2.createElement("hr", {}), msg];
        errorMsg = react_2.createElement("div", {}, ...children_10);
    }
    else {
        errorMsg = react_2.createElement("div", {});
    }
    const matchValue_12 = dialogData.Int;
    const matchValue_13 = dialogData.Int2;
    const matchValue_15 = dialogData.ConstraintIOSel;
    const matchValue_16 = dialogData.ConstraintTypeSel;
    let matchResult, io_4, v, io_5, lower, upper;
    if (matchValue_12 != null) {
        if (dialogData.ConstraintErrorMsg == null) {
            if (matchValue_15 != null) {
                if (matchValue_16 != null) {
                    if (matchValue_16.tag === 1) {
                        if (matchValue_13 != null) {
                            matchResult = 1;
                            io_5 = matchValue_15;
                            lower = matchValue_12;
                            upper = matchValue_13;
                        }
                        else {
                            matchResult = 2;
                        }
                    }
                    else {
                        matchResult = 0;
                        io_4 = matchValue_15;
                        v = matchValue_12;
                    }
                }
                else {
                    matchResult = 2;
                }
            }
            else {
                matchResult = 2;
            }
        }
        else {
            matchResult = 2;
        }
    }
    else {
        matchResult = 2;
    }
    switch (matchResult) {
        case 0: {
            toConsole(printf("Existing %A"))(existingCons);
            const tentative = new Constraint(0, [new EqualityConstraint(io_4, v)]);
            const arg_31 = inCon2str(tentative);
            toConsole(printf("Tentative: %A"))(arg_31);
            const matchValue_18 = validateNumericalConstraint(tentative, existingCons);
            if (matchValue_18.tag === 0) {
                const c = matchValue_18.fields[0];
                dispatch(new Msg(73, [void 0]));
                dispatch(new Msg(74, [c]));
            }
            else {
                const err_2 = matchValue_18.fields[0];
                dispatch(new Msg(73, [err_2]));
            }
            break;
        }
        case 1: {
            const tentative_1 = new Constraint(1, [makeInequalityConstraint(lower, io_5, ~(~toInt(upper)))]);
            const matchValue_19 = validateNumericalConstraint(tentative_1, existingCons);
            if (matchValue_19.tag === 0) {
                const c_1 = matchValue_19.fields[0];
                dispatch(new Msg(73, [void 0]));
                dispatch(new Msg(74, [c_1]));
            }
            else {
                const err_3 = matchValue_19.fields[0];
                dispatch(new Msg(73, [err_3]));
            }
            break;
        }
        case 2: {
            dispatch(new Msg(74, [void 0]));
            break;
        }
    }
    const children_12 = [preamble, react_2.createElement("hr", {}), h6(empty())(singleton("Select IO")), ioSelect, react_2.createElement("hr", {}), h6(empty())(singleton("Constraint Type")), typeSelect, react_2.createElement("hr", {}), h6(empty())(singleton("Edit Constraint")), constraintEditor, errorMsg];
    return react_2.createElement("div", {}, ...children_12);
}

export function createInputConstraintPopup(model, dispatch) {
    let tt;
    dispatch(new Msg(61, [0]));
    dispatch(new Msg(62, [fromInteger(0, false, 2)]));
    dispatch(new Msg(71, [new ConstraintType(0, [])]));
    dispatch(new Msg(72, [void 0]));
    dispatch(new Msg(74, [void 0]));
    const title = "Add Input Constraint";
    const infoMsg = "The Truth Table is re-generated every time Input Constraints change.";
    let inputs;
    const matchValue = model.CurrentTruthTable;
    if (matchValue != null) {
        const copyOfStruct = matchValue;
        inputs = ((copyOfStruct.tag === 0) ? ((tt = copyOfStruct.fields[0], map((cell) => cell.IO, head(map((tuple) => tuple[0], toList(tt.TableMap)))))) : toFail(printf("what? Constraint add option should not exist when there is TT error")));
    }
    else {
        inputs = toFail(printf("what? No current Truth Table when adding Constraints"));
    }
    const body = (dialogData) => dialogPopupNumericalConBody(inputs, model.TTInputConstraints, infoMsg, dispatch, dialogData);
    const buttonText = "Add";
    const buttonAction = (dialogData_1) => {
        const matchValue_1 = dialogData_1.NewConstraint;
        if (matchValue_1 != null) {
            const con = matchValue_1;
            dispatch(new Msg(31, [con]));
            dispatch(new Msg(56, []));
        }
    };
    const isDisabled = (dialogData_2) => {
        let matchResult;
        if (dialogData_2.ConstraintErrorMsg == null) {
            if (dialogData_2.NewConstraint != null) {
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
                return false;
            }
            case 1: {
                return true;
            }
        }
    };
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, empty(), dispatch);
}

export function createOutputConstraintPopup(model, dispatch) {
    let tt;
    dispatch(new Msg(61, [0]));
    dispatch(new Msg(62, [fromInteger(0, false, 2)]));
    dispatch(new Msg(71, [new ConstraintType(0, [])]));
    dispatch(new Msg(72, [void 0]));
    dispatch(new Msg(74, [void 0]));
    const title = "Add Output Constraint";
    const infoMsg = "If the truth table is truncated, results from applying Output Constraints may not be complete.";
    let outputs;
    const matchValue = model.CurrentTruthTable;
    if (matchValue != null) {
        const copyOfStruct = matchValue;
        outputs = ((copyOfStruct.tag === 0) ? ((tt = copyOfStruct.fields[0], map((cell) => cell.IO, head(map((tuple) => tuple[1], toList(tt.TableMap)))))) : toFail(printf("what? Constraint add option should not exist when there is TT error")));
    }
    else {
        outputs = toFail(printf("what? No current Truth Table when adding Constraints"));
    }
    const body = (dialogData) => dialogPopupNumericalConBody(outputs, model.TTOutputConstraints, infoMsg, dispatch, dialogData);
    const buttonText = "Add";
    const buttonAction = (dialogData_1) => {
        const matchValue_1 = dialogData_1.NewConstraint;
        if (matchValue_1 != null) {
            const con = matchValue_1;
            dispatch(new Msg(32, [con]));
            dispatch(new Msg(56, []));
        }
    };
    const isDisabled = (dialogData_2) => {
        let matchResult;
        if (dialogData_2.ConstraintErrorMsg == null) {
            if (dialogData_2.NewConstraint != null) {
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
                return false;
            }
            case 1: {
                return true;
            }
        }
    };
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, empty(), dispatch);
}

export function viewConstraints(model, dispatch) {
    const inputCons = model.TTInputConstraints;
    const outputCons = model.TTOutputConstraints;
    const addButton = (action) => button(singleton(new Option_2(18, [action])), singleton("Add"));
    const clearButton = (action_1) => button(singleton(new Option_2(18, [action_1])), singleton("Clear All"));
    const children = ["Filter Rows in the Truth Table using Input or Output constraints", react_2.createElement("br", {}), react_2.createElement("br", {}), h6(empty())(singleton("Input Constraints")), viewNumericalConstraints(inputCons, dispatch), react_2.createElement("br", {}), makeElementLine(ofArray([addButton((_arg) => {
        createInputConstraintPopup(model, dispatch);
    }), clearButton((_arg_1) => {
        dispatch(new Msg(29, []));
    })]), empty()), h6(empty())(singleton("Output Constraints")), viewNumericalConstraints(outputCons, dispatch), react_2.createElement("br", {}), makeElementLine(ofArray([addButton((_arg_2) => {
        createOutputConstraintPopup(model, dispatch);
    }), clearButton((_arg_3) => {
        dispatch(new Msg(30, []));
    })]), empty())];
    return react_2.createElement("div", {}, ...children);
}

export function makeOnOffToggle(state, changeAction, onText, offText) {
    return item(singleton(new Item_Option(1, [])), singleton(div(singleton(new Option_3(1, [])), ofArray([div_1(empty(), singleton(button(ofArray([new Option_2(0, [(state === true) ? (new Color_IColor(6, [])) : (new Color_IColor(20, []))]), new Option_2(18, [(state === false) ? changeAction : ((_arg) => {
    })])]), singleton(onText)))), div_1(empty(), singleton(button(ofArray([new Option_2(0, [(state === false) ? (new Color_IColor(8, [])) : (new Color_IColor(20, []))]), new Option_2(18, [(state === true) ? changeAction : ((_arg_1) => {
    })])]), singleton(offText))))]))));
}

export const algebraKey = (() => {
    const props_102 = [["style", {
        overflowY: "scroll",
    }]];
    const children_26 = ["Algebraic expressions in Issie consist of input variables, numeric values,\n                and operators which manipulate them.", react_2.createElement("hr", {}), react_2.createElement("br", {}), h5(empty())(singleton("Variables")), "Variables in algebraic expressions correspond the input components in your\n                schematic, with the name of each variable being derived from the label of\n                each input. Each variable has a width associated with it, this is the same\n                as the width of its corresponding input component.", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Numeric Addition (+)")), react_2.createElement("b", {}, "Usage: A + B, Example: 1 + 2 = 3"), react_2.createElement("br", {}), "Represents the addition of two expressions, just like it would in maths.\n                The width of the result is equal to the width of the operands. If the result\n                exceeds the maximum value for the width, the value will wrap around. For example,\n                0xA + 0xB = 0x6 for a width of 4.", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Numeric Subtraction (-)")), react_2.createElement("b", {}, "Usage: A - B, Example 2 - 1 = 1"), react_2.createElement("br", {}), "Represents the subtraction of two expressions, just like it would in maths.\n                The width of the result is equal to the width of the operands. If the result\n                exceeds the maximum value for the width, the value will wrap around.", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Carry Of (carry())")), react_2.createElement("b", {}, "Usage: carry(X-Y+Z), Example: carry(0xA + 0xB) = 1, when width = 4"), react_2.createElement("br", {}), "Represents the carry-out of an arithmetic operation", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Numeric Negation (-)")), react_2.createElement("b", {}, "Usage: -A, Example: - 5"), react_2.createElement("br", {}), "Shows that an expression is being negated, (times -1), like it would in maths.", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Bitwise And (&)")), react_2.createElement("b", {}, "Usage: A & B, Example 0 & 1 = 0 "), react_2.createElement("br", {}), "Represents an AND operation between two expressions. This is a bitwise operation,\n                meaning that the operator is applied to each bit one-by-one. For example, for inputs\n                with a width of 8 bits: 0xFF & 0xFF = 0xFF", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Bitwise Or (|)")), react_2.createElement("b", {}, "Usage: A | B, Example: 0 | 1 = 1 "), react_2.createElement("br", {}), "Represents an OR operation between two expressions. This is a bitwise operation,\n                meaning that the operator is applied to each bit one-by-one. For example, for inputs\n                with a width of 8 bits: 0xFF | 0x01 = 0xFF.", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Bitwise Xor (⊕)")), react_2.createElement("b", {}, "Usage: A ⊕ B, Example: 0 ⊕ 1 = 1 "), react_2.createElement("br", {}), "Represents an XOR operation between two expressions. This is a bitwise operation,\n                meaning that the operator is applied to each bit one-by-one.", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Bitwise Not (~)")), react_2.createElement("b", {}, "Usage: ~A, Example: ~0 = 1 "), react_2.createElement("br", {}), "Represents an OR operation between two expressions. This is a bitwise operation,\n                meaning that the operator is applied to each bit one-by-one. For example, for an\n                input with a width of 4 bits: ~(0b1101) = ~(0b0010).", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Bit Range ([u:l])")), react_2.createElement("b", {}, "Usage: A[u:l], Example: (0b1101)[2:1] = 0b10"), react_2.createElement("br", {}), "Represents the selection of a specific range of bits from an expression. The upper\n                bound is the first number, and the lower bound is the second number in the brackets.\n                The bit range defined by these two numbers is inclusive. The width of the result is\n                equal to u + l - 1", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Append (::)")), react_2.createElement("b", {}, "Usage: A::B"), react_2.createElement("br", {}), react_2.createElement("b", {}, "Example: 0b1101 :: 0b1001 = 0b11011001"), react_2.createElement("br", {}), "Represents the joining of two expressions together, with the first expression making\n                up the MSBs, and the second the LSBs. The width of the result is the sum of the widths\n                of the two expressions beign appended.", react_2.createElement("br", {}), react_2.createElement("hr", {}), h5(empty())(singleton("Equals (==)")), react_2.createElement("b", {}, "Usage: A == number"), react_2.createElement("br", {}), react_2.createElement("b", {}, "Example: 0x05 == 0x05 = 1, 0x05 == 0x02 = 0"), react_2.createElement("br", {}), "Compares the value of an algebraic expression to a number. If the equality holds (true)\n                then the result is 1. Otherwise (false) the result is 0.", react_2.createElement("br", {})];
    return react_2.createElement("div", keyValueList(props_102, 1), ...children_26);
})();

export function createAlgKeyPopup(dispatch) {
    const title = "Key for Algebraic Expressions";
    const body = (x) => algebraKey;
    const foot = (_arg) => react_2.createElement("div", {});
    dynamicClosablePopup(title, body, foot, ofArray([new CSSProp(189, ["60%"]), new CSSProp(395, ["30%"])]), dispatch);
}

export function validateAlgebraInput(io_, io__1, io__2, fsi, tableSD) {
    const io = [io_, io__1, io__2];
    const cid = io[0];
    try {
        changeInput(cid, fsi, tableSD.ClockTickNumber, tableSD.FastSim);
        return new FSharpResult$2(0, [0]);
    }
    catch (matchValue) {
        if (matchValue instanceof AlgebraNotImplemented) {
            const err = matchValue.Data0;
            return new FSharpResult$2(1, [err]);
        }
        else {
            throw matchValue;
        }
    }
}

export function dialogPopupReductionBody(inputs, tableSD, dispatch, dialogData) {
    const explanation = "Reduce the Truth Table down into something more compact and informative by setting\n            certain inputs to your logic as algebra. The resultant new Truth Table will show\n            outputs as a function of your inputs. As algebraic inputs are directly evaluated by \n            the simulator, they are a great way to avoid Truth Table truncation.";
    let algInputs;
    const matchValue = dialogData.AlgebraInputs;
    if (matchValue == null) {
        algInputs = toFail(printf("what? PopupDialogData.AlgebraInputs is None in popup body"));
    }
    else {
        const l = matchValue;
        algInputs = l;
    }
    const toggleAction = (io, _arg) => {
        dispatch(new Msg(77, [[io, tableSD]]));
    };
    let toggles;
    const children = map((io_1) => {
        const state = !contains(io_1, algInputs, {
            Equals: equalArrays,
            GetHashCode: arrayHash,
        });
        const toggle = makeOnOffToggle(state, partialApply(1, toggleAction, [io_1]), "Values", "Algebra");
        const label = io_1[1];
        return makeElementLine(ofArray([toString(label), toggle]), empty());
    }, inputs);
    toggles = react_2.createElement("div", {}, ...children);
    let error;
    const matchValue_1 = dialogData.AlgebraError;
    if (matchValue_1 != null) {
        if (matchValue_1.InDependency != null) {
            const d = matchValue_1.InDependency;
            const m = matchValue_1.Msg;
            const children_5 = [m, react_2.createElement("br", {}), `Component in question: ${d}`];
            error = react_2.createElement("div", {}, ...children_5);
        }
        else {
            error = toFail(printf("what? SimError for AlgebraNotImplemented should always have an\n                    InDependency field. Error: %A"))(dialogData.AlgebraError);
        }
    }
    else {
        error = react_2.createElement("div", {});
    }
    const children_7 = [explanation, react_2.createElement("hr", {}), toggles, react_2.createElement("hr", {}), error];
    return react_2.createElement("div", {}, ...children_7);
}

export function createAlgReductionPopup(model, dispatch) {
    let tt;
    const title = "Reduction using Algebraic Inputs";
    let patternInput;
    const matchValue = model.CurrentTruthTable;
    if (matchValue != null) {
        const copyOfStruct = matchValue;
        patternInput = ((copyOfStruct.tag === 0) ? ((tt = copyOfStruct.fields[0], [map((cell) => {
            const matchValue_1 = cell.IO;
            if (matchValue_1.tag === 1) {
                return toFail(printf("what? Found viewer in Truth Table inputs"));
            }
            else {
                const s = matchValue_1.fields[0];
                return s;
            }
        }, head(map((tuple) => tuple[0], toList(tt.TableMap)))), tt.TableSimData])) : toFail(printf("what? Reduction option should not exist when there is TT error")));
    }
    else {
        patternInput = toFail(printf("what? No current Truth Table when reducing"));
    }
    const tableSD = patternInput[1];
    const inputs = patternInput[0];
    const body = (dialogData) => dialogPopupReductionBody(inputs, tableSD, dispatch, dialogData);
    const buttonText = "Apply";
    const buttonAction = (dialogData_1) => {
        const matchValue_2 = dialogData_1.AlgebraInputs;
        if (matchValue_2 != null) {
            const l = matchValue_2;
            dispatch(new Msg(41, [l]));
            dispatch(new Msg(56, []));
        }
        else {
            toFail(printf("what? what? PopupDialogData.AlgebraInputs is None in popup body"));
        }
    };
    const isDisabled = (dialogData_2) => {
        const matchValue_3 = dialogData_2.AlgebraInputs;
        let matchResult;
        if (dialogData_2.AlgebraError == null) {
            if (matchValue_3 != null) {
                if (isEmpty(matchValue_3)) {
                    matchResult = 0;
                }
                else {
                    matchResult = 1;
                }
            }
            else {
                matchResult = 0;
            }
        }
        else {
            matchResult = 0;
        }
        switch (matchResult) {
            case 0: {
                return true;
            }
            case 1: {
                return false;
            }
        }
    };
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, empty(), dispatch);
}

export function viewReductions(table, model, dispatch) {
    let goBackButton;
    const matchValue_1 = model.TTAlgebraInputs;
    goBackButton = ((table.DCMap == null) ? (isEmpty(matchValue_1) ? react_2.createElement("div", {}) : button(ofArray([new Option_2(0, [new Color_IColor(5, [])]), new Option_2(18, [(_arg_1) => {
        dispatch(new Msg(41, [empty()]));
        dispatch(new Msg(75, [empty()]));
    }])]), singleton("Back to Numeric Table"))) : (isEmpty(matchValue_1) ? button(ofArray([new Option_2(0, [new Color_IColor(5, [])]), new Option_2(18, [(_arg) => {
        dispatch(new Msg(37, []));
    }])]), singleton("Back to Full Table")) : toFail(printf("what? Table cannot be DC Reduced and Algebraic"))));
    let reduceButton;
    if (table.IsTruncated) {
        const textEl = addToolTipTop("DC Reduction unavailable for truncated tables", "Remove Redundancies");
        reduceButton = button(ofArray([new Option_2(16, [true]), new Option_2(18, [(_arg_2) => {
        }])]), singleton(textEl));
    }
    else if (!table.HasRedundancies) {
        const textEl_1 = addToolTipTop("No redundancies to remove", "Remove Redundancies");
        reduceButton = button(ofArray([new Option_2(16, [true]), new Option_2(18, [(_arg_3) => {
        }])]), singleton(textEl_1));
    }
    else {
        reduceButton = button(ofArray([new Option_2(0, [new Color_IColor(6, [])]), new Option_2(18, [(_arg_4) => {
            dispatch(new Msg(26, []));
        }])]), singleton("Remove Redundancies"));
    }
    const algebraButton = button(ofArray([new Option_2(0, [new Color_IColor(6, [])]), new Option_2(18, [(_arg_5) => {
        createAlgReductionPopup(model, dispatch);
    }])]), singleton("Algebra"));
    const keyButton = button(ofArray([new Option_2(0, [new Color_IColor(5, [])]), new Option_2(18, [(_arg_6) => {
        createAlgKeyPopup(dispatch);
    }])]), singleton("Key"));
    const algebraTags = list_3(empty(), map((tupledArg) => {
        const label = tupledArg[1];
        return tag_52(ofArray([new Option(1, [new Color_IColor(8, [])]), new Option(3, [])]), singleton(toString(label)));
    }, model.TTAlgebraInputs));
    const hasMultiBitOutputs = !isEmpty(filter((tupledArg_1) => {
        const w = tupledArg_1[2] | 0;
        return w > 1;
    }, table.TableSimData.Outputs));
    const maybeBaseSelector = hasMultiBitOutputs ? baseSelector(table.TableSimData.NumberBase, (n) => {
        dispatch(new Msg(42, [n]));
    }) : react_2.createElement("div", {});
    const matchValue_3 = table.DCMap;
    const matchValue_4 = model.TTAlgebraInputs;
    if (matchValue_3 != null) {
        if (!isEmpty(matchValue_4)) {
            return toFail(printf("what? Table cannot be DC Reduced and Algebraic"));
        }
        else {
            const dc = matchValue_3;
            const children_7 = [makeElementLine(singleton(goBackButton), singleton(maybeBaseSelector))];
            return react_2.createElement("div", {}, ...children_7);
        }
    }
    else if (!isEmpty(matchValue_4)) {
        const children_9 = [makeElementLine(ofArray([goBackButton, "   ", algebraButton, "   ", keyButton]), singleton(maybeBaseSelector)), makeElementLine(ofArray(["Algebraic Inputs: ", algebraTags]), empty())];
        return react_2.createElement("div", {}, ...children_9);
    }
    else {
        const children_5 = [makeElementLine(ofArray([reduceButton, "   ", algebraButton]), singleton(maybeBaseSelector))];
        return react_2.createElement("div", {}, ...children_5);
    }
}

//# sourceMappingURL=ConstraintReduceView.fs.js.map

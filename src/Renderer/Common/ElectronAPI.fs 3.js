import { defaultArg } from "../fable_modules/fable-library.4.0.0-theta-018/Option.js";
import * as electron from "electron";

export function jsToBool(b) {
    return defaultArg(b, false);
}

export const Electron_electron = electron;

export const Electron_mainProcess = electron;

//# sourceMappingURL=ElectronAPI.fs.js.map

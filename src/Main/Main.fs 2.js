import { Electron_mainProcess } from "../Renderer/Common/ElectronAPI.fs.js";
import { ofArray, contains, exists, map } from "./fable_modules/fable-library.4.0.0-theta-018/List.js";
import { iterate, toList } from "./fable_modules/fable-library.4.0.0-theta-018/Seq.js";
import { equals, jsOptions, createAtom, stringHash } from "./fable_modules/fable-library.4.0.0-theta-018/Util.js";
import * as main from "@electron/remote/main";
import { sleep, startImmediate } from "./fable_modules/fable-library.4.0.0-theta-018/Async.js";
import { singleton } from "./fable_modules/fable-library.4.0.0-theta-018/AsyncBuilder.js";
import { rangeDouble } from "./fable_modules/fable-library.4.0.0-theta-018/Range.js";
import { toText, printf, toConsole } from "./fable_modules/fable-library.4.0.0-theta-018/String.js";
import * as path from "path";
import { defaultArg, toArray, some } from "./fable_modules/fable-library.4.0.0-theta-018/Option.js";
import * as url_1 from "url";

(() => {
    const objectArg = Electron_mainProcess.systemPreferences;
    return (tupledArg) => {
        objectArg.setUserDefault(tupledArg[0], tupledArg[1], tupledArg[2]);
    };
})()[["NSDisabledDictationMenuItem", "boolean", "true"]];

(() => {
    const objectArg = Electron_mainProcess.systemPreferences;
    return (tupledArg) => {
        objectArg.setUserDefault(tupledArg[0], tupledArg[1], tupledArg[2]);
    };
})()[["NSDisabledCharacterPaletteMenu", "boolean", "true"]];

export const args = map((s) => s.toLocaleLowerCase(), toList(process.argv));

export function argFlagIsOn(flags) {
    const fl = map((s) => s.toLocaleLowerCase(), flags);
    return exists((flag) => contains(flag, args, {
        Equals: (x, y) => (x === y),
        GetHashCode: stringHash,
    }), fl);
}

export function hasDebugArgs() {
    return argFlagIsOn(ofArray(["--debug", "-d"]));
}

main.initialize();

export const debug = false;

export const isMacos = process.platform === "darwin";

export const isWin = process.platform === "win32";

Electron_mainProcess.app.name = "Issie";

export let mainWindow = createAtom(void 0);

export let closeAfterSave = createAtom(false);

export function wait(n, cont) {
    startImmediate(singleton.Delay(() => singleton.TryFinally(singleton.Delay(() => singleton.For(toList(rangeDouble(1, 1, n)), (_arg) => {
        const i = _arg | 0;
        toConsole(printf("%i before"))(i);
        return singleton.Bind(sleep(1000), () => {
            toConsole(printf("%i after"))(i);
            return singleton.Zero();
        });
    })), () => {
        cont();
    })));
}

export function createMainWindow() {
    const options_1 = jsOptions((options) => {
        options.show = true;
        options.autoHideMenuBar = false;
        options.backgroundColor = "#FFFFFF";
        options.opacity = 0.8;
        const isDev = process.defaultApp === true;
        if (isDev) {
            options.icon = some(path.join(__static, "icon-1.png"));
        }
        else {
            options.icon = some("/static/icon-1.png");
        }
        options.title = "issie";
        options.webPreferences = jsOptions((o) => {
            o.nodeIntegration = true;
            o.contextIsolation = false;
            o.devTools = true;
        });
    });
    const window$ = new Electron_mainProcess.BrowserWindow(options_1);
    const webContents = window$.webContents;
    main.enable(webContents);
    mainWindow(window$);
    return window$;
}

export function startRenderer(doAfterReady) {
    Electron_mainProcess.app.on('ready',((_arg, _arg_1) => {
        const window$ = createMainWindow();
        doAfterReady(window$);
    }));
}

export function loadAppIntoWidowWhenReady(window$) {
    const loadWindowContent = (window$_1) => {
        let arg, arg_3;
        if (window$_1.isMinimized()) {
            window$_1.show();
        }
        const isDev = process.defaultApp === true;
        if (isDev) {
            if (debug) {
                window$_1.webContents.openDevTools();
            }
            (arg = toText(printf("http://localhost:9000")), window$_1.loadURL(arg));
            process.on("uncaughtException", (err) => {
                console.error(some(err));
            });
        }
        else {
            let url;
            let arg_2;
            const arg_1 = path.join(__dirname, "index.html");
            arg_2 = toText(printf("file:///%s"))(arg_1);
            url = (new URL(arg_2));
            (arg_3 = url_1.format(url, {}), window$_1.loadURL(arg_3));
        }
    };
    loadWindowContent(window$);
    return window$.webContents.on("did-finish-load", () => {
        window$.setOpacity(1);
        window$.maximize();
    });
}

export function addListeners(window$) {
    let tupledArg, objectArg;
    window$.on('closed',(() => {
        mainWindow(void 0);
    }));
    (tupledArg = ["close", (e) => {
        if (!closeAfterSave()) {
            const value_1 = e.preventDefault();
            window$.webContents.send("closingWindow");
        }
    }], window$.on(tupledArg[0], tupledArg[1]));
    Electron_mainProcess.ipcMain.on("exit-the-app", (_arg_1) => {
        closeAfterSave(true);
        iterate((win) => {
            win.close();
        }, toArray(mainWindow()));
    });
    Electron_mainProcess.ipcMain.on("get-user-data", (event, args_1) => {
        let userAppDirOpt;
        try {
            userAppDirOpt = Electron_mainProcess.app.getPath("userData");
        }
        catch (matchValue) {
            userAppDirOpt = (void 0);
        }
        event.returnValue = defaultArg(userAppDirOpt, "");
    });
    Electron_mainProcess.ipcMain.on("toggle-dev-tools", (_arg_2, _arg_3) => {
        iterate((win_1) => {
            win_1.webContents.toggleDevTools();
        }, toArray(mainWindow()));
    });
    (objectArg = Electron_mainProcess.app, objectArg.on('window-all-closed',(() => {
        Electron_mainProcess.app.quit();
    })));
    Electron_mainProcess.app.on('activate',((_arg_5, _arg_6) => {
        if (mainWindow() == null) {
            equals(window$, createMainWindow());
            mainWindow(window$);
            loadAppIntoWidowWhenReady(addListeners(window$));
            mainWindow(window$);
        }
    }));
    return window$;
}

export function startup() {
    startRenderer((win) => {
        loadAppIntoWidowWhenReady(addListeners(win));
    });
}

startup();

//# sourceMappingURL=Main.fs.js.map

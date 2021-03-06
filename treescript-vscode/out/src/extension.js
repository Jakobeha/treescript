'use strict';
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
// tslint:disable:max-line-length
const child_process = require("child_process");
const vscode_1 = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
// tslint:enable:max-line-length
let isSetupGeneral = false;
const clients = new Map();
// Code derived from vscode-hie-server (https://github.com/alanz/vscode-hie-server/blob/master/src/extension.ts)
function activate(context) {
    return __awaiter(this, void 0, void 0, function* () {
        // Try to activate every time a new file is opened, for multi-root
        // workspaces.
        vscode_1.workspace.onDidOpenTextDocument((document) => __awaiter(this, void 0, void 0, function* () { return yield ensureSetupForDoc(context, document); }));
        vscode_1.workspace.textDocuments.forEach((document) => __awaiter(this, void 0, void 0, function* () { return yield ensureSetupForDoc(context, document); }));
        // Deactivate if workspace is removed.
        vscode_1.workspace.onDidChangeWorkspaceFolders(event => {
            for (const folder of event.removed) {
                const key = folder.uri.toString();
                const client = clients.get(key);
                if (client) {
                    clients.delete(key);
                    client.stop();
                }
            }
        });
    });
}
exports.activate = activate;
/**
 * Sets up the server for the document's workspace if it's not activated
 * and the document is a TreeScript file. Prompts the user if the server's
 * executable isn't installed.
 */
function ensureSetupForDoc(context, document) {
    return __awaiter(this, void 0, void 0, function* () {
        if (document.languageId !== 'treescript') {
            return;
        }
        const uri = document.uri;
        if (uri.scheme !== 'file' && uri.scheme !== 'untitled') {
            return;
        }
        const folder = vscode_1.workspace.getWorkspaceFolder(uri);
        if (!folder) {
            return;
        }
        ensureSetupForFolder(context, folder);
    });
}
/**
 * Sets up the server for the document's workspace if it's not
 * activated and should be. Prompts the user if the server's executable
 * isn't installed.
 */
function ensureSetupForFolder(context, folder) {
    return __awaiter(this, void 0, void 0, function* () {
        const uri = folder.uri;
        const key = uri.toString();
        if (clients.has(key)) {
            return;
        }
        const config = vscode_1.workspace.getConfiguration('treescript', uri);
        if (!config.enable) {
            return;
        }
        if (!(yield isServerInstalled())) {
            const notInstalledMsg = 'TreeScript language server (in CLI) not installed. Install via "stack install treescript"';
            const retry = 'Retry';
            vscode_1.window.showErrorMessage(notInstalledMsg, retry).then(option => {
                if (option === retry) {
                    ensureSetupForFolder(context, folder);
                }
            });
            return;
        }
        setup(context, folder);
    });
}
/**
 * Launches the server for the workspace, and registers general commands
 * if they aren't registered yet.
 */
function setup(context, folder) {
    const uri = folder.uri;
    const key = uri.toString();
    const command = 'treescript';
    // const tempDir = os.tmpdir()
    const genArgs = ['serve'];
    const runArgs = genArgs;
    const debugArgs = genArgs;
    // const debugArgs = genArgs.concat(['-d', '-l', path.join(tempDir, 'treescript.log')])
    const serverOptions = {
        run: { command, transport: vscode_languageclient_1.TransportKind.stdio, args: runArgs },
        debug: { command, transport: vscode_languageclient_1.TransportKind.stdio, args: debugArgs }
    };
    // Set a unique name per workspace folder (useful for multi-root workspaces).
    const langName = 'TreeScript (' + folder.name + ')';
    const outputChannel = vscode_1.window.createOutputChannel(langName);
    const clientOptions = {
        // Use the document selector to only notify the LSP on files inside the folder
        // path for the specific workspace.
        documentSelector: [
            { scheme: 'file', language: 'treescript', pattern: `${uri.fsPath}/**/*` },
        ],
        synchronize: {
            // Synchronize the setting section 'treescript' to the server.
            configurationSection: 'treescript',
            // Notify the server about file changes to '.clientrc files in the workspace.
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/.clientrc')
        },
        diagnosticCollectionName: langName,
        revealOutputChannelOn: vscode_languageclient_1.RevealOutputChannelOn.Info,
        outputChannel,
        outputChannelName: langName,
        // middleware: {
        //  provideHover: DocsBrowser.hoverLinksMiddlewareHook
        // },
        // Set the server's workspace folder to the current workspace folder.
        workspaceFolder: folder
    };
    // Create the LSP client.
    const langClient = new vscode_languageclient_1.LanguageClient(langName, langName, serverOptions, clientOptions, true);
    // Finally start the client and add it to the list of clients.
    langClient.start();
    clients.set(key, langClient);
    if (!isSetupGeneral) {
        setupGeneral(context);
    }
}
/**
 * Activates stuff for all workspaces.
 */
function setupGeneral(context) {
    // Register editor commands if not registered.
    registerPointCommand('rename', context);
    isSetupGeneral = true;
}
/*
 * Deactivate each of the LSP servers.
 */
function deactivate() {
    const promises = [];
    for (const client of clients.values()) {
        promises.push(client.stop());
    }
    return Promise.all(promises).then(() => {
        clients.clear();
    });
}
exports.deactivate = deactivate;
/*
 * Check if the treescript CLI (server) is installed.
 */
function isServerInstalled() {
    return __awaiter(this, void 0, void 0, function* () {
        return new Promise(resolve => {
            const cmd = process.platform === 'win32' ? 'where treescript' : 'which treescript';
            child_process.exec(cmd, error => resolve(!error));
        });
    });
}
/*
 * Create an editor command that calls an action on the active LSP server.
 */
function registerPointCommand(command, context) {
    return __awaiter(this, void 0, void 0, function* () {
        const name = 'treescript.commands.' + command;
        const editorCmd = vscode_1.commands.registerTextEditorCommand(name, editor => {
            const cmd = {
                command,
                arguments: [
                    {
                        file: editor.document.uri.toString(),
                        pos: editor.selections[0].active
                    }
                ]
            };
            // Get the current file and workspace folder.
            const uri = editor.document.uri;
            const folder = vscode_1.workspace.getWorkspaceFolder(uri);
            if (folder === undefined) {
                return;
            }
            const client = clients.get(folder.uri.toString());
            if (client === undefined) {
                return;
            }
            client.sendRequest('workspace/executeCommand', cmd).then(() => {
                return true;
            }, err => {
                console.error(err);
            });
        });
        context.subscriptions.push(editorCmd);
    });
}
//# sourceMappingURL=extension.js.map
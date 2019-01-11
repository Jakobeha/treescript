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
//Derived from https://github.com/Microsoft/vscode/blob/master/extensions/merge-conflict/src/mergeDecorator.ts
const vscode = require("vscode");
class Decorator {
    constructor(context) {
        this.context = context;
        this.decorations = {};
        this.enable = false;
        this.updating = new Map();
        this.sloppyCodeBlockRegexp = /(?:(?:[a-z]\w*)?')|(\))(?:\\'|[^'])*(?:'|\\\()/gm.compile();
    }
    begin() {
        this.registerDecorationTypes();
        // Check if we already have a set of active windows, attempt to track these.
        vscode.window.visibleTextEditors.forEach(e => this.applyDecorations(e));
        vscode.workspace.onDidOpenTextDocument(event => {
            this.applyDecorationsFromEvent(event);
        }, null, this.context.subscriptions);
        vscode.workspace.onDidChangeTextDocument(event => {
            this.applyDecorationsFromEvent(event.document);
        }, null, this.context.subscriptions);
        vscode.window.onDidChangeVisibleTextEditors((e) => {
            // Any of which could be new (not just the active one).
            e.forEach(e => this.applyDecorations(e));
        }, null, this.context.subscriptions);
        this.enable = true;
    }
    end() {
        // TODO: Replace with Map<string, T>
        Object.keys(this.decorations).forEach(name => {
            this.decorations[name].dispose();
        });
        this.decorations = {};
    }
    registerDecorationTypes() {
        // Dispose of existing decorations
        Object.keys(this.decorations).forEach(k => this.decorations[k].dispose());
        this.decorations = {};
        // None of our features are enabled
        if (!this.enable) {
            return;
        }
        // Create decorators
        this.decorations['codeBlock.inline'] = vscode.window.createTextEditorDecorationType(this.generateBlockRenderOptions('treescript.codeBlockBackground', false));
        this.decorations['codeBlock.multiline'] = vscode.window.createTextEditorDecorationType(this.generateBlockRenderOptions('treescript.codeBlockBackground', true));
    }
    generateBlockRenderOptions(backgroundColor, multiline) {
        let renderOptions = {};
        renderOptions.backgroundColor = new vscode.ThemeColor(backgroundColor);
        renderOptions.isWholeLine = multiline;
        return renderOptions;
    }
    applyDecorationsFromEvent(eventDocument) {
        for (const editor of vscode.window.visibleTextEditors) {
            if (editor.document === eventDocument) {
                // Attempt to apply
                this.applyDecorations(editor);
            }
        }
    }
    applyDecorations(editor) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!this.enable || !editor || !editor.document) {
                return;
            }
            // If we have a pending scan from the same origin, exit early. (Cannot use this.tracker.isPending() because decorations are per editor.)
            if (this.updating.get(editor)) {
                return;
            }
            try {
                this.updating.set(editor, true);
                let codeBlocks = yield this.getCodeBlocks(editor.document);
                if (vscode.window.visibleTextEditors.indexOf(editor) === -1) {
                    return;
                }
                if (codeBlocks.length === 0) {
                    this.removeDecorations(editor);
                    return;
                }
                // Store decorations keyed by the type of decoration, set decoration wants a "style"
                // to go with it, which will match this key (see constructor);
                let matchDecorations = {};
                let pushDecoration = (key, d) => {
                    matchDecorations[key] = matchDecorations[key] || [];
                    matchDecorations[key].push(d);
                };
                codeBlocks.forEach(codeBlock => {
                    // TODO, this could be more effective, just call getMatchPositions once with a map of decoration to position
                    if (codeBlock.isSingleLine) {
                        pushDecoration('codeBlock.inline', codeBlock);
                    }
                    else {
                        pushDecoration('codeBlock.multiline', codeBlock);
                    }
                });
                // For each match we've generated, apply the generated decoration with the matching decoration type to the
                // editor instance. Keys in both matches and decorations should match.
                Object.keys(matchDecorations).forEach(decorationKey => {
                    let decorationType = this.decorations[decorationKey];
                    if (decorationType) {
                        editor.setDecorations(decorationType, matchDecorations[decorationKey]);
                    }
                });
            }
            finally {
                this.updating.delete(editor);
            }
        });
    }
    removeDecorations(editor) {
        // Remove all decorations, there might be none
        Object.keys(this.decorations).forEach(decorationKey => {
            // Race condition, while editing the settings, it's possible to
            // generate regions before the configuration has been refreshed
            let decorationType = this.decorations[decorationKey];
            if (decorationType) {
                editor.setDecorations(decorationType, []);
            }
        });
    }
    getCodeBlocks(document) {
        return new Promise((resolve, _) => {
            let res = [];
            let text = document.getText();
            let match = null;
            while ((match = this.sloppyCodeBlockRegexp.exec(text)) != null) {
                let range = new vscode.Range(document.positionAt(match.index), document.positionAt(match.index + match.input.length));
                res.push(range);
            }
            this.sloppyCodeBlockRegexp.lastIndex = 0;
            resolve(res);
        });
    }
}
exports.default = Decorator;
//# sourceMappingURL=decorator.js.map
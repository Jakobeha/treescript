'use strict'
//Derived from https://github.com/Microsoft/vscode/blob/master/extensions/merge-conflict/src/mergeDecorator.ts

import * as vscode from 'vscode';
import { Service } from './interfaces';

export default class Decorator implements Service {
  private decorations: { [key: string]: vscode.TextEditorDecorationType } = {};

  private enable: boolean = false;
  private updating = new Map<vscode.TextEditor, boolean>();

  private sloppyCodeBlockRegexp: RegExp = /(?:(?:[a-z]\w*)?')|(\))(?:\\'|[^'])*(?:'|\\\()/gm.compile();

  constructor(private context: vscode.ExtensionContext) {

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

  private registerDecorationTypes() {
    // Dispose of existing decorations
    Object.keys(this.decorations).forEach(k => this.decorations[k].dispose());
    this.decorations = {};

    // None of our features are enabled
    if (!this.enable) {
      return;
    }

    // Create decorators
    this.decorations['codeBlock.inline'] = vscode.window.createTextEditorDecorationType(
      this.generateBlockRenderOptions('treescript.codeBlockBackground', false)
    );

    this.decorations['codeBlock.multiline'] = vscode.window.createTextEditorDecorationType(
      this.generateBlockRenderOptions('treescript.codeBlockBackground', true)
    );
  }

  private generateBlockRenderOptions(backgroundColor: string, multiline: boolean): vscode.DecorationRenderOptions {
    let renderOptions: vscode.DecorationRenderOptions = {};

    renderOptions.backgroundColor = new vscode.ThemeColor(backgroundColor);
    renderOptions.isWholeLine = multiline;

    return renderOptions;
  }

  private applyDecorationsFromEvent(eventDocument: vscode.TextDocument) {
    for (const editor of vscode.window.visibleTextEditors) {
      if (editor.document === eventDocument) {
        // Attempt to apply
        this.applyDecorations(editor);
      }
    }
  }

  private async applyDecorations(editor: vscode.TextEditor) {
    if (!this.enable || !editor || !editor.document) {
      return;
    }

    // If we have a pending scan from the same origin, exit early. (Cannot use this.tracker.isPending() because decorations are per editor.)
    if (this.updating.get(editor)) {
      return;
    }

    try {
      this.updating.set(editor, true);

      let codeBlocks = await this.getCodeBlocks(editor.document);
      if (vscode.window.visibleTextEditors.indexOf(editor) === -1) {
        return;
      }

      if (codeBlocks.length === 0) {
        this.removeDecorations(editor);
        return;
      }

      // Store decorations keyed by the type of decoration, set decoration wants a "style"
      // to go with it, which will match this key (see constructor);
      let matchDecorations: { [key: string]: vscode.Range[] } = {};

      let pushDecoration = (key: string, d: vscode.Range) => {
        matchDecorations[key] = matchDecorations[key] || [];
        matchDecorations[key].push(d);
      };

      codeBlocks.forEach(codeBlock => {
        // TODO, this could be more effective, just call getMatchPositions once with a map of decoration to position
        if (codeBlock.isSingleLine) {
          pushDecoration('codeBlock.inline', codeBlock);
        } else {
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

    } finally {
      this.updating.delete(editor);
    }
  }

  private removeDecorations(editor: vscode.TextEditor) {
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

  private getCodeBlocks(document: vscode.TextDocument): Promise<vscode.Range[]> {
    return new Promise((resolve, _) => {
      let res: vscode.Range[] = [];
      let text = document.getText();
      let match: RegExpExecArray = null;
      while ((match = this.sloppyCodeBlockRegexp.exec(text)) != null) {
        let range = new vscode.Range(document.positionAt(match.index), document.positionAt(match.index + match.input.length));
        res.push(range);
      }
      this.sloppyCodeBlockRegexp.lastIndex = 0;
      resolve(res);
    })
  }
}

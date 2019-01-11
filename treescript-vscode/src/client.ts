'use strict'
//Derived from vscode-hie-server (https://github.com/alanz/vscode-hie-server/blob/master/src/extension.ts)

import * as child_process from 'child_process';
import { commands, ExtensionContext, OutputChannel, TextDocument, window, workspace, WorkspaceFolder } from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn, ServerOptions, TransportKind } from 'vscode-languageclient';
import { Service } from './interfaces';

export default class Client implements Service {
  isSetupGeneral: Boolean = false
  clients: Map<string, LanguageClient> = new Map()

  constructor(private context: ExtensionContext) {

  }

  begin() {
    // Try to activate every time a new file is opened, for multi-root
    // workspaces.
    workspace.onDidOpenTextDocument(async (document: TextDocument) => await this.ensureSetupForDoc(this.context, document))
    workspace.textDocuments.forEach(async (document: TextDocument) => await this.ensureSetupForDoc(this.context, document))
    // Deactivate if workspace is removed.
    workspace.onDidChangeWorkspaceFolders(event => {
      for (const folder of event.removed) {
        const key = folder.uri.toString()

        const client = this.clients.get(key)
        if (client) {
          this.clients.delete(key)
          client.stop()
        }
      }
    })
  }

  /*
  * Deactivate each of the LSP servers.
  */
  async end() {
    const promises: Array<Thenable<void>> = []
    for (const client of this.clients.values()) {
      promises.push(client.stop())
    }
    return Promise.all(promises).then(() => {
      this.clients.clear()
    })
  }

  /**
   * Sets up the server for the document's workspace if it's not activated
   * and the document is a TreeScript file. Prompts the user if the server's
   * executable isn't installed.
   */
  private async ensureSetupForDoc(context: ExtensionContext, document: TextDocument) {
    if (document.languageId !== 'treescript') {
      return
    }

    const uri = document.uri
    if (uri.scheme !== 'file' && uri.scheme !== 'untitled') {
      return
    }

    const folder = workspace.getWorkspaceFolder(uri)
    if (!folder) {
      return
    }

    this.ensureSetupForFolder(context, folder)
  }

  /**
   * Sets up the server for the document's workspace if it's not
   * activated and should be. Prompts the user if the server's executable
   * isn't installed.
   */
  private async ensureSetupForFolder(context: ExtensionContext, folder: WorkspaceFolder) {
    const uri = folder.uri
    const key = uri.toString()
    if (this.clients.has(key)) {
      return
    }

    const config = workspace.getConfiguration('treescript', uri)
    if (!config.enable) {
      return
    }

    if (!await this.isServerInstalled()) {
      const notInstalledMsg: string = 'TreeScript language server (in CLI) not installed. Install via "stack install treescript"'
      const retry: string = 'Retry'
      window.showErrorMessage(notInstalledMsg, retry).then(option => {
        if (option === retry) {
          this.ensureSetupForFolder(context, folder)
        }
      })
      return
    }

    this.setup(context, folder)
  }

  /**
   * Launches the server for the workspace, and registers general commands
   * if they aren't registered yet.
   */
  private setup(context: ExtensionContext, folder: WorkspaceFolder) {
    const uri = folder.uri
    const key = uri.toString()
    const command = 'treescript'
    //const tempDir = os.tmpdir()
    const genArgs = ['serve']
    const runArgs = genArgs
    const debugArgs = genArgs
    //const debugArgs = genArgs.concat(['-d', '-l', path.join(tempDir, 'treescript.log')])

    const serverOptions: ServerOptions = {
      run: { command: command, transport: TransportKind.stdio, args: runArgs },
      debug: { command: command, transport: TransportKind.stdio, args: debugArgs }
    }

    // Set a unique name per workspace folder (useful for multi-root workspaces).
    const langName = 'TreeScript (' + folder.name + ')'
    const outputChannel: OutputChannel = window.createOutputChannel(langName)
    const clientOptions: LanguageClientOptions = {
      // Use the document selector to only notify the LSP on files inside the folder
      // path for the specific workspace.
      documentSelector: [
        { scheme: 'file', language: 'treescript', pattern: `${uri.fsPath}/**/*` },
      ],
      synchronize: {
        // Synchronize the setting section 'treescript' to the server.
        configurationSection: 'treescript',
        // Notify the server about file changes to '.clientrc files in the workspace.
        fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
      },
      diagnosticCollectionName: langName,
      revealOutputChannelOn: RevealOutputChannelOn.Info,
      outputChannel,
      outputChannelName: langName,
      //middleware: {
      //  provideHover: DocsBrowser.hoverLinksMiddlewareHook
      //},
      // Set the server's workspace folder to the current workspace folder.
      workspaceFolder: folder
    }

    // Create the LSP client.
    const langClient = new LanguageClient(langName, langName, serverOptions, clientOptions, true)

    // Finally start the client and add it to the list of clients.
    langClient.start()
    this.clients.set(key, langClient)

    if (!this.isSetupGeneral) {
      this.setupGeneral(context)
    }
  }

  /**
   * Activates stuff for all workspaces.
   */
  private setupGeneral(context: ExtensionContext) {
    // Register editor commands if not registered.
    this.registerPointCommand('rename', context)
    this.isSetupGeneral = true
  }

  /*
  * Check if the treescript CLI (server) is installed.
  */
  private async isServerInstalled(): Promise<boolean> {
    return new Promise<boolean>((resolve, _reject) => {
      const cmd: string = process.platform === 'win32' ? 'where treescript' : 'which treescript'
      child_process.exec(cmd, (error, _stdout, _stderr) => resolve(!error))
    })
  }

  /*
  * Create an editor command that calls an action on the active LSP server.
  */
  private async registerPointCommand(command: string, context: ExtensionContext) {
    const name = 'treescript.commands.' + command
    const editorCmd = commands.registerTextEditorCommand(name, (editor, _edit) => {
      const cmd = {
        command,
        arguments: [
          {
            file: editor.document.uri.toString(),
            pos: editor.selections[0].active
          }
        ]
      }
      // Get the current file and workspace folder.
      const uri = editor.document.uri
      const folder = workspace.getWorkspaceFolder(uri)
      if (folder === undefined) {
        return
      }
      const client = this.clients.get(folder.uri.toString())
      if (client === undefined) {
        return
      }

      client.sendRequest('workspace/executeCommand', cmd).then(
        _hints => {
          return true
        },
        err => {
          console.error(err)
        }
      )
    })
    context.subscriptions.push(editorCmd)
  }
}

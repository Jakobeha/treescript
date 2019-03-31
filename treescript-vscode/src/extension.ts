'use strict'

// tslint:disable:max-line-length
import * as child_process from 'child_process';
import { commands, ExtensionContext, OutputChannel, TextDocument, window, workspace, WorkspaceFolder } from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn, ServerOptions, TransportKind } from 'vscode-languageclient';
// tslint:enable:max-line-length

let isSetupGeneral: boolean = false
const clients: Map<string, LanguageClient> = new Map()

// Code derived from vscode-hie-server (https://github.com/alanz/vscode-hie-server/blob/master/src/extension.ts)

export async function activate(context: ExtensionContext) {
  // Try to activate every time a new file is opened, for multi-root
  // workspaces.
  workspace.onDidOpenTextDocument(async (document: TextDocument) => await ensureSetupForDoc(context, document))
  workspace.textDocuments.forEach(async (document: TextDocument) => await ensureSetupForDoc(context, document))
  // Deactivate if workspace is removed.
  workspace.onDidChangeWorkspaceFolders(event => {
    for (const folder of event.removed) {
      const key = folder.uri.toString()

      const client = clients.get(key)
      if (client) {
        clients.delete(key)
        client.stop()
      }
    }
  })
}

/**
 * Sets up the server for the document's workspace if it's not activated
 * and the document is a TreeScript file. Prompts the user if the server's
 * executable isn't installed.
 */
async function ensureSetupForDoc(context: ExtensionContext, document: TextDocument) {
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

  ensureSetupForFolder(context, folder)
}

/**
 * Sets up the server for the document's workspace if it's not
 * activated and should be. Prompts the user if the server's executable
 * isn't installed.
 */
async function ensureSetupForFolder(context: ExtensionContext, folder: WorkspaceFolder) {
  const uri = folder.uri
  const key = uri.toString()
  if (clients.has(key)) {
    return
  }

  const config = workspace.getConfiguration('treescript', uri)
  if (!config.enable) {
    return
  }

  if (!await isServerInstalled()) {
    const notInstalledMsg: string =
    'TreeScript language server (in CLI) not installed. Install via "stack install treescript"'
    const retry: string = 'Retry'
    window.showErrorMessage(notInstalledMsg, retry).then(option => {
      if (option === retry) {
        ensureSetupForFolder(context, folder)
      }
    })
    return
  }

  setup(context, folder)
}

/**
 * Launches the server for the workspace, and registers general commands
 * if they aren't registered yet.
 */
function setup(context: ExtensionContext, folder: WorkspaceFolder) {
  const uri = folder.uri
  const key = uri.toString()
  const command = 'treescript'
  // const tempDir = os.tmpdir()
  const genArgs = ['serve']
  const runArgs = genArgs
  const debugArgs = genArgs
  // const debugArgs = genArgs.concat(['-d', '-l', path.join(tempDir, 'treescript.log')])

  const serverOptions: ServerOptions = {
    run: { command, transport: TransportKind.stdio, args: runArgs },
    debug: { command, transport: TransportKind.stdio, args: debugArgs }
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
    // middleware: {
    //  provideHover: DocsBrowser.hoverLinksMiddlewareHook
    // },
    // Set the server's workspace folder to the current workspace folder.
    workspaceFolder: folder
  }

  // Create the LSP client.
  const langClient = new LanguageClient(langName, langName, serverOptions, clientOptions, true)

  // Finally start the client and add it to the list of clients.
  langClient.start()
  clients.set(key, langClient)

  if (!isSetupGeneral) {
    setupGeneral(context)
  }
}

/**
 * Activates stuff for all workspaces.
 */
function setupGeneral(context: ExtensionContext) {
  // Register editor commands if not registered.
  registerPointCommand('rename', context)
  isSetupGeneral = true
}

/*
 * Deactivate each of the LSP servers.
 */
export function deactivate(): Thenable<void> {
  const promises: Array<Thenable<void>> = []
  for (const client of clients.values()) {
    promises.push(client.stop())
  }
  return Promise.all(promises).then(() => {
    clients.clear()
  })
}

/*
 * Check if the treescript CLI (server) is installed.
 */
async function isServerInstalled(): Promise<boolean> {
  return new Promise<boolean>(resolve => {
    const cmd: string = process.platform === 'win32' ? 'where treescript' : 'which treescript'
    child_process.exec(cmd, error => resolve(!error))
  })
}

/*
 * Create an editor command that calls an action on the active LSP server.
 */
async function registerPointCommand(command: string, context: ExtensionContext) {
  const name = 'treescript.commands.' + command
  const editorCmd = commands.registerTextEditorCommand(name, editor => {
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
    const client = clients.get(folder.uri.toString())
    if (client === undefined) {
      return
    }

    client.sendRequest('workspace/executeCommand', cmd).then(
      () => {
        return true
      },
      err => {
        console.error(err)
      }
    )
  })
  context.subscriptions.push(editorCmd)
}

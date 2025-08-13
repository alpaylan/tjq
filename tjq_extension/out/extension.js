"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
// src/extension.ts
const vscode = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    // Get the language server executable path from configuration
    const config = vscode.workspace.getConfiguration('tjq');
    let serverPath = config.get('server.path');
    // If no path is configured, try to find the server in common locations
    if (!serverPath) {
        // You can add logic here to find the server executable
        // For now, assume it's in PATH as 'tjq-lsp'
        serverPath = 'tjq-lsp';
    }
    // The server is implemented as a separate executable
    const serverOptions = {
        run: {
            command: serverPath,
            transport: node_1.TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: node_1.TransportKind.stdio
        }
    };
    // Options to control the language client
    const clientOptions = {
        // Register the server for tjq documents
        documentSelector: [{ scheme: 'file', language: 'tjq' }],
        synchronize: {
            // Notify the server about file changes to '.tjq' files contained in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{tjq,jq}')
        }
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient('tjqLanguageServer', 'TJQ Language Server', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
    // Register commands
    const disposableFormat = vscode.commands.registerCommand('tjq.format', () => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'tjq') {
            vscode.commands.executeCommand('editor.action.formatDocument');
        }
    });
    const disposableHole = vscode.commands.registerCommand('tjq.insertHole', () => {
        const editor = vscode.window.activeTextEditor;
        if (editor) {
            editor.edit(editBuilder => {
                editBuilder.insert(editor.selection.active, '??');
            });
        }
    });
    context.subscriptions.push(disposableFormat, disposableHole);
    // Show status in status bar
    const statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    statusItem.text = "$(check) TJQ LSP";
    statusItem.tooltip = "TJQ Language Server is active";
    statusItem.show();
    context.subscriptions.push(statusItem);
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map
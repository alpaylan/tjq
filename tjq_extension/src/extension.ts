// src/extension.ts
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    // Get the language server executable path from configuration
    const config = vscode.workspace.getConfiguration('tjq');
    let serverPath = config.get<string>('server.path');
    
    if (!serverPath) {
        //tjq lsp binary path
        serverPath = 'tjq-lsp';
    }

    // The server is implemented as a separate executable
    const serverOptions: ServerOptions = {
        run: { 
            command: serverPath, 
            transport: TransportKind.stdio 
        },
        debug: { 
            command: serverPath, 
            transport: TransportKind.stdio 
        }
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for tjq documents
        documentSelector: [{ scheme: 'file', language: 'tjq' }],
        synchronize: {
            // Notify the server about file changes to '.tjq' files contained in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{tjq,jq}')
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'tjqLanguageServer',
        'TJQ Language Server',
        serverOptions,
        clientOptions
    );

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

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
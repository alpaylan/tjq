import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    console.log('TJQ extension is activating...');
    
    // Find the LSP binary
    const lspBinaryPath = findLspBinary();
    
    if (!lspBinaryPath) {
        vscode.window.showErrorMessage(
            'TJQ LSP binary not found. Please build it with `cargo build --release` in tjq directory.'
        );
        return;
    }
    
    console.log(`Found LSP binary at: ${lspBinaryPath}`);
    
    // Server options
    const serverOptions: ServerOptions = {
        run: {
            command: lspBinaryPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: lspBinaryPath,
            transport: TransportKind.stdio,
            options: {
                env: {
                    ...process.env,
                    RUST_LOG: 'tjq_lsp=debug,tower_lsp=debug'
                }
            }
        }
    };
    
    // Client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'tjq' },
            { scheme: 'file', language: 'jq' },
            { scheme: 'file', pattern: '**/*.tjq' },
            { scheme: 'file', pattern: '**/*.jq' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{tjq,jq}')
        },
        outputChannelName: 'TJQ Language Server'
    };
    
    // Create and start the client
    client = new LanguageClient(
        'tjq-lsp',
        'TJQ Language Server',
        serverOptions,
        clientOptions
    );
    
    // Start the client
    client.start();
    
    console.log('TJQ extension activated successfully');
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

function findLspBinary(): string | null {
    // Check user configuration first
    const config = vscode.workspace.getConfiguration('tjq');
    const configuredPath = config.get<string>('lspBinaryPath');
    
    if (configuredPath && fs.existsSync(configuredPath)) {
        return configuredPath;
    }
    
    // Search in common locations
    const searchPaths = [
        // Workspace root
        path.join(vscode.workspace.workspaceFolders?.[0]?.uri.fsPath || '', 'target', 'release', 'tjq-lsp'),
        path.join(vscode.workspace.workspaceFolders?.[0]?.uri.fsPath || '', 'target', 'debug', 'tjq-lsp'),
        // Relative to extension
        path.join(__dirname, '..', '..', 'target', 'release', 'tjq-lsp'),
        path.join(__dirname, '..', '..', 'target', 'debug', 'tjq-lsp'),
        // Absolute path 
        '/Users/cank/Desktop/tjq/target/release/tjq-lsp',
        '/Users/cank/Desktop/tjq/target/debug/tjq-lsp',
        // System paths
        '/usr/local/bin/tjq-lsp',
        '/usr/bin/tjq-lsp',
        // Home directory
        path.join(process.env.HOME || '', '.cargo', 'bin', 'tjq-lsp')
    ];
    
    for (const searchPath of searchPaths) {
        if (fs.existsSync(searchPath)) {
            return searchPath;
        }
    }
    
    return null;
}

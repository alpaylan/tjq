"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const vscode = __importStar(require("vscode"));
const path = __importStar(require("path"));
const fs = __importStar(require("fs"));
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    console.log('TJQ extension is activating...');
    // Find the LSP binary
    const lspBinaryPath = findLspBinary();
    if (!lspBinaryPath) {
        vscode.window.showErrorMessage('TJQ LSP binary not found. Please build it with `cargo build --release` in tjq directory.');
        return;
    }
    console.log(`Found LSP binary at: ${lspBinaryPath}`);
    // Server options
    const serverOptions = {
        run: {
            command: lspBinaryPath,
            transport: node_1.TransportKind.stdio
        },
        debug: {
            command: lspBinaryPath,
            transport: node_1.TransportKind.stdio,
            options: {
                env: {
                    ...process.env,
                    RUST_LOG: 'tjq_lsp=debug,tower_lsp=debug'
                }
            }
        }
    };
    // Client options
    const clientOptions = {
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
    client = new node_1.LanguageClient('tjq-lsp', 'TJQ Language Server', serverOptions, clientOptions);
    // Start the client
    client.start();
    console.log('TJQ extension activated successfully');
}
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
function findLspBinary() {
    // Check user configuration first
    const config = vscode.workspace.getConfiguration('tjq');
    const configuredPath = config.get('lspBinaryPath');
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
//# sourceMappingURL=extension.js.map
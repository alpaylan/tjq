#!/usr/bin/env node

const { spawn } = require('child_process');
const readline = require('readline');
const path = require('path');
const fs = require('fs');

// LSP message helpers
function createMessage(content) {
    const contentLength = Buffer.byteLength(JSON.stringify(content), 'utf8');
    return `Content-Length: ${contentLength}\r\n\r\n${JSON.stringify(content)}`;
}

// Buffer for incomplete messages
let messageBuffer = '';

function parseMessages(data) {
    messageBuffer += data.toString();
    const messages = [];
    
    while (true) {
        // Look for Content-Length header
        const headerMatch = messageBuffer.match(/Content-Length: (\d+)\r\n\r\n/);
        if (!headerMatch) break;
        
        const contentLength = parseInt(headerMatch[1]);
        const headerLength = headerMatch[0].length;
        const totalLength = headerLength + contentLength;
        
        // Check if we have the complete message
        if (messageBuffer.length < totalLength) break;
        
        // Extract the JSON content
        const jsonContent = messageBuffer.substring(headerLength, totalLength);
        messageBuffer = messageBuffer.substring(totalLength);
        
        try {
            const message = JSON.parse(jsonContent);
            messages.push(message);
        } catch (e) {
            console.error('Failed to parse JSON:', e.message);
            console.error('Content:', jsonContent.substring(0, 100) + '...');
        }
    }
    
    return messages;
}

// Find the LSP binary
function findLspBinary() {
    const searchPaths = [
        // Relative to sandbox directory - corrected paths
        path.join(__dirname, '..', '..', 'target', 'release', 'tjq-lsp'),
        path.join(__dirname, '..', '..', 'target', 'debug', 'tjq-lsp'),
        // Absolute path - corrected
        '/Users/cank/Desktop/tjq/target/release/tjq-lsp',
        '/Users/cank/Desktop/tjq/target/debug/tjq-lsp',
    ];
    
    for (const searchPath of searchPaths) {
        if (fs.existsSync(searchPath)) {
            return searchPath;
        }
    }
    
    console.error('LSP binary not found. Please build it with:');
    console.error('  cd /Users/cank/Desktop/tjq');
    console.error('  cargo build --release');
    process.exit(1);
}

// Start the LSP server
const lspPath = findLspBinary();
console.log(`Using LSP binary: ${lspPath}`);

// Disable debug logging by default
const env = { ...process.env };
if (!env.RUST_LOG) {
    env.RUST_LOG = 'error';  // Only show actual errors, not info/debug
}

const lsp = spawn(lspPath, [], {
    stdio: ['pipe', 'pipe', 'pipe'],
    env: env
});

let messageId = 1;

// Handle LSP server output
lsp.stdout.on('data', (data) => {
    const messages = parseMessages(data);
    for (const message of messages) {
        if (message.result && message.id === 3) {
            // Special handling for completion results - just show count
            console.log(`Response: Completion with ${message.result.length} items`);
        } else {
            console.log('Response:', JSON.stringify(message, null, 2));
        }
    }
});

lsp.stderr.on('data', (data) => {
    const output = data.toString();
    // Only show actual errors, not debug/info logs
    if (output.includes('ERROR') || output.includes('error')) {
        console.error('LSP Error:', output);
    } else if (process.env.DEBUG) {
        console.log('LSP Log:', output);
    }
});

lsp.on('close', (code) => {
    console.log(`LSP server exited with code ${code}`);
    process.exit(code);
});

// Send initialize request
function initialize() {
    const initRequest = {
        jsonrpc: '2.0',
        id: messageId++,
        method: 'initialize',
        params: {
            processId: process.pid,
            rootUri: 'file:///Users/cank/Desktop/tjq/tjq_lsp/test.tjq',
            capabilities: {
                textDocument: {
                    completion: {
                        completionItem: {
                            snippetSupport: true
                        }
                    },
                    hover: {
                        contentFormat: ['markdown', 'plaintext']
                    }
                }
            }
        }
    };
    
    console.log('Sending initialize request...');
    lsp.stdin.write(createMessage(initRequest));
}

// Send initialized notification
function initialized() {
    const notification = {
        jsonrpc: '2.0',
        method: 'initialized',
        params: {}
    };
    
    console.log('Sending initialized notification...');
    lsp.stdin.write(createMessage(notification));
}

// Open a test document
function openDocument() {
    const notification = {
        jsonrpc: '2.0',
        method: 'textDocument/didOpen',
        params: {
            textDocument: {
                uri: 'file:///Users/cank/Desktop/tjq/tjq_lsp/test.tjq',
                languageId: 'tjq',
                version: 1,
                text: '[1,2,3] | map(. * 2) | length'
            }
        }
    };
    
    console.log('Opening test document...');
    lsp.stdin.write(createMessage(notification));
}

// Request completions
function requestCompletions() {
    const request = {
        jsonrpc: '2.0',
        id: messageId++,
        method: 'textDocument/completion',
        params: {
            textDocument: {
                uri: 'file:///Users/cank/Desktop/tjq/tjq_lsp/test.tjq'
            },
            position: {
                line: 0,
                character: 10  // After "|"
            }
        }
    };
    
    console.log('Requesting completions...');
    lsp.stdin.write(createMessage(request));
}

// Request hover
function requestHover() {
    const request = {
        jsonrpc: '2.0',
        id: messageId++,
        method: 'textDocument/hover',
        params: {
            textDocument: {
                uri: 'file:///Users/cank/Desktop/tjq/tjq_lsp/test.tjq'
            },
            position: {
                line: 0,
                character: 9  // On "map"
            }
        }
    };
    
    console.log('Requesting hover...');
    lsp.stdin.write(createMessage(request));
}

// Request code actions
function requestCodeActions() {
    const request = {
        jsonrpc: '2.0',
        id: messageId++,
        method: 'textDocument/codeAction',
        params: {
            textDocument: {
                uri: 'file:///Users/cank/Desktop/tjq/tjq_lsp/test.tjq'
            },
            range: {
                start: { line: 0, character: 10 },
                end: { line: 0, character: 20 }
            },
            context: {
                diagnostics: []
            }
        }
    };
    
    console.log('Requesting code actions...');
    lsp.stdin.write(createMessage(request));
}

// Shutdown
function shutdown() {
    const request = {
        jsonrpc: '2.0',
        id: messageId++,
        method: 'shutdown',
        params: null
    };
    
    console.log('Sending shutdown request...');
    lsp.stdin.write(createMessage(request));
    
    setTimeout(() => {
        const notification = {
            jsonrpc: '2.0',
            method: 'exit',
            params: null
        };
        lsp.stdin.write(createMessage(notification));
    }, 1000);
}

// Interactive mode
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

function showMenu() {
    console.log('\n=== TJQ LSP Test Client ===');
    console.log('1. Initialize');
    console.log('2. Open document');
    console.log('3. Request completions');
    console.log('4. Request hover');
    console.log('5. Request code actions');
    console.log('6. Shutdown');
    console.log('q. Quit');
    console.log('========================\n');
}

function handleCommand(cmd) {
    switch(cmd.trim()) {
        case '1':
            initialize();
            setTimeout(initialized, 500);
            break;
        case '2':
            openDocument();
            break;
        case '3':
            requestCompletions();
            break;
        case '4':
            requestHover();
            break;
        case '5':
            requestCodeActions();
            break;
        case '6':
            shutdown();
            break;
        case 'q':
            process.exit(0);
            break;
        default:
            console.log('Invalid command');
    }
}

// Start interactive mode
showMenu();
rl.on('line', (input) => {
    handleCommand(input);
    setTimeout(showMenu, 1000);
});

console.log('TJQ LSP Test Client started. Enter commands:');
console.log('(Set DEBUG=1 to see LSP debug logs)');

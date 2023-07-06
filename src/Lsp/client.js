// Connect to the WebSocket server.
const socket = new WebSocket("ws://localhost:3001");

// Set up Elm app.
const app = Elm.Main.init();

// Set up ports.
app.ports.outgoingMessage.subscribe(sendMessageToServer);
app.ports.incomingMessage.subscribe(handleMessageFromServer);

// Send a message to the LSP server.
function sendMessageToServer(message) {
    if (socket.readyState === WebSocket.OPEN) {
        socket.send(message);
    } else {
        console.error('WebSocket is not open');
    }
}

// Handle incoming messages from the WebSocket and send them to Elm.
socket.onmessage = function(event) {
    app.ports.incomingMessage.send(event.data);
};

// Initialize the LSP handshake when the WebSocket connection is established.
socket.onopen = function() {
    const initializeParams = {
        jsonrpc: '2.0',
        id: 1,
        method: 'initialize',
        params: {
            processId: null,
            rootPath: null,
            capabilities: {},
            // ... additional initialization parameters
        }
    };
    socket.send(JSON.stringify(initializeParams));
};

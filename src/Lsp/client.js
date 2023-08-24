// Connect to the WebSocket server.
const socket = new WebSocket("wss://localhost:3000/api/v1/humio-lsp-raw");

// Set up Elm app.
const app = Elm.Main.init({
    node: document.getElementById('elm-app')
});

// Set up ports.
app.ports.outgoingMessage.subscribe(sendMessageToServer);

// Send a message to the LSP server.
function sendMessageToServer(method, params) {
    if (socket.readyState === WebSocket.OPEN) {
        const message = {
            jsonrpc: '2.0',
            id: 1,
            method: method,
            params: params
        }
        socket.send(JSON.stringify(message));
    } else {
        console.error('WebSocket is not open');
    }
}

// Handle incoming messages from the WebSocket and send them to Elm.
socket.onmessage = function (event) {
    app.ports.incomingMessage.send(event.data);
};

// Initialize the LSP handshake when the WebSocket connection is established.
socket.onopen = function () {
    app.ports.webSocketReady.send();
};



import { Server } from './server';
import { Client } from './client';
import { createCanvas } from './render';

const LATENCY: number = 200;

const serverCanvas = document.getElementById('serverView') as HTMLCanvasElement;
const serverContext = serverCanvas.getContext('2d') as CanvasRenderingContext2D;
const addClientButton = document.getElementById('addClientButton') as HTMLButtonElement;

const server: Server = new Server(serverContext);

setInterval(server.update.bind(server), 50);

addClientButton.onclick = (event) => {
    const client = new Client(createCanvas());
    server.addClient(client.playerUID, gameState => {
        setTimeout(() => client.receiveState(gameState), LATENCY);
    });
    client.bindServer((playerUID, input) => {
        setTimeout(() => server.receiveInput(playerUID, input), LATENCY);
    });
    setInterval(client.update.bind(client), 50);
};
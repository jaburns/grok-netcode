import { Server } from './server';
import { Client } from './client';
import { createCanvas } from './render';

const TICK_RATE: number = 50;
const MIN_LATENCY: number = 200;
const MAX_LATENCY: number = 200;
const DROP_RATE: number = 0;

const lossyInvoke = (fn: ()=>void): void => {
    if (Math.random() < DROP_RATE) return;
    setTimeout(fn, MIN_LATENCY + (MAX_LATENCY - MIN_LATENCY)*Math.random());
}

const server = new Server(createCanvas());
setInterval(server.update.bind(server), TICK_RATE);

const addClientButton = document.getElementById('addClientButton') as HTMLButtonElement;

addClientButton.onclick = event => {
    const client = new Client(createCanvas());

    server.addClient(client.playerUID, gameState =>
        lossyInvoke(() => client.receiveState(gameState)));

    client.bindServer((playerUID, input) =>
        lossyInvoke(() => server.receiveInput(playerUID, input)));

    setInterval(client.update.bind(client), TICK_RATE);
};
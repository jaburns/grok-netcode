import { Server } from './server';
import { Client } from './client';
import { createCanvas } from './render';

const TICK_RATE: number = 50;

let latency: number = 200;
let latencyVariance: number = 0;
let dropRate: number = 0;

const addClientButton = document.getElementById('addClientButton') as HTMLButtonElement;
const latencyInput = document.getElementById('setLatency') as HTMLInputElement;
const latencyVarianceInput = document.getElementById('setLatencyVariance') as HTMLInputElement;

const lossyInvoke = (fn: ()=>void): void => {
    if (Math.random() < dropRate) return;
    setTimeout(fn, latency + latencyVariance*Math.random());
}

latencyInput.onkeyup = event => 
    latency = parseFloat((event.target as HTMLInputElement).value);
latencyVarianceInput.onkeyup = event => 
    latencyVariance = parseFloat((event.target as HTMLInputElement).value);

const server = new Server(createCanvas());
setInterval(server.update.bind(server), TICK_RATE);

addClientButton.onclick = event => {
    const client = new Client(createCanvas());

    server.addClient(client.playerUID, gameState =>
        lossyInvoke(() => client.receiveState(gameState)));

    client.bindServer((playerUID, input) =>
        lossyInvoke(() => server.receiveInput(playerUID, input)));

    setInterval(client.update.bind(client), TICK_RATE);
};
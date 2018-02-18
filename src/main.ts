import { Server } from './server';
import { Client } from './client';
import { createCanvas } from './render';
import cloneDeep = require('lodash/cloneDeep');

const TICK_RATE: number = 50;

const addClientButton = document.getElementById('addClientButton') as HTMLButtonElement;
const latencyInput = document.getElementById('setLatency') as HTMLInputElement;
const latencyVarianceInput = document.getElementById('setLatencyVariance') as HTMLInputElement;

let latency: number = parseFloat(latencyInput.value);
let latencyVariance: number = parseFloat(latencyVarianceInput.value);
let dropRate: number = 0;

latencyInput.onkeyup = event => 
    latency = parseFloat((event.target as HTMLInputElement).value);
latencyVarianceInput.onkeyup = event => 
    latencyVariance = parseFloat((event.target as HTMLInputElement).value);

const server = new Server(createCanvas());
setInterval(server.update.bind(server), TICK_RATE);

const lossyInvoke = (fn: ()=>void): void => {
    if (Math.random() < dropRate) return;
    setTimeout(fn, latency + latencyVariance*Math.random());
}

addClientButton.onclick = event => {
    const client = new Client(createCanvas());

    server.addClient(client.playerUID, gameState =>
        lossyInvoke(() => client.receiveState(cloneDeep(gameState))));

    client.bindServer((playerUID, input) =>
        lossyInvoke(() => server.receiveInput(playerUID, cloneDeep(input))));

    setInterval(client.update.bind(client), TICK_RATE);
};
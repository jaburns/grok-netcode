import { Server } from './server';
import { Client } from './client';
import { createRenderer, renderHistoryBack, renderHistoryForward } from './render';
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

const intervals: number[] = [];

const server = new Server(createRenderer());
intervals.push(setInterval(server.update.bind(server), TICK_RATE));

const lossyInvoke = (fn: ()=>void): void => {
    if (Math.random() < dropRate) return;
    setTimeout(fn, latency + latencyVariance*Math.random());
}

addClientButton.onclick = event => {
    const client = new Client(createRenderer());

    server.addClient(client.playerUID, gameState =>
        lossyInvoke(() => client.receiveState(cloneDeep(gameState))));

    client.bindServer((playerUID, input) =>
        lossyInvoke(() => server.receiveInput(playerUID, cloneDeep(input))));

    intervals.push(setInterval(client.update.bind(client), TICK_RATE));
};

(document.getElementById('inspectButton') as HTMLButtonElement).onclick = event => {
    intervals.forEach(x => clearInterval(x));
    addClientButton.onclick = _ => {};

    (document.getElementById('inspectBackButton') as HTMLButtonElement).onclick = event => {
        renderHistoryBack();
    };

    (document.getElementById('inspectForwardButton') as HTMLButtonElement).onclick = event => {
        renderHistoryForward();
    };
};
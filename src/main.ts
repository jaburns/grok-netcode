import { newGameState, newPlayerState, stepGameState, predictGameState } from './state';
import { createCanvas, renderGameState } from './render';
import { getLatestInputs, PlayerInputScheme, PlayerInput } from './input';
import { UIDMap, generateUID } from './utils';

const canvas = document.getElementById('serverView') as HTMLCanvasElement;
const server_ctx = canvas.getContext('2d') as CanvasRenderingContext2D;
const button = document.getElementById('addClientButton') as HTMLButtonElement;

const delayedInvoke = (fn:any, arg:any):any => {
    const argClone = JSON.parse(JSON.stringify(arg));
    setTimeout(() => fn(argClone), 200);
}

const newServer = () => {
    const ctx = server_ctx;
    let clients :any[] = [];
    let inputMap :UIDMap<PlayerInput> = {};
    let state = newGameState();

    const obj = {
        addClient: (x:any):any => {
            const newPlayerUID = generateUID();
            state.players[newPlayerUID] = newPlayerState();
            clients.push(x);
            inputMap[newPlayerUID] = getLatestInputs(PlayerInputScheme.Nothing);
            return newPlayerUID;
        },
        receiveInput: (x:any) => {
            inputMap[x.id] = x.input;
        },
        update: () => {
            state = stepGameState(inputMap, state);

            for (let i = 0; i < clients.length; ++i) {
                delayedInvoke(clients[i].receiveState, state);
            }

            renderGameState(ctx, state, 'Server');
        }
    };

    return obj;
};

const newClient = (server:any):any => {
    const ctx = createCanvas();
    const inputStack:any = [];
    let state:any = null;
    let clientId:any;

    const obj = {
        receiveState: (serverState:any) => {
            if (state == null) {
                state = serverState;
                return;
            }

            let matchIndex = inputStack.length;
            while (--matchIndex >= 0) {
                if (serverState.players[clientId].lastInput == inputStack[matchIndex].uid) break;
            }

            if (matchIndex >= 0) {
                state = serverState;
                for (let i = matchIndex + 1; i < inputStack.length; ++i) {
                    state = predictGameState(inputStack[i], clientId, state);
                }
            }
        },
        update: () => {
            if (state == null) return;

            const latestInput = getLatestInputs(clientId%2 ? PlayerInputScheme.WASD : PlayerInputScheme.Arrows);
            state = predictGameState(latestInput, clientId, state);
            inputStack.push(latestInput);

            if (inputStack.length > 500) {
                inputStack.splice(0, 1);
            }

            delayedInvoke(server.receiveInput, {input:latestInput, id:clientId});

            renderGameState(ctx, state, 'Client '+clientId+(clientId%2 ? ' (WASD)' : ' (Arrows)'));
        }
    };

    clientId = server.addClient(obj);
    return obj;
};

const server = newServer();
setInterval(server.update, 50);

button.onclick = (event) => {
    const client = newClient(server);
    setInterval(client.update, 50);
};
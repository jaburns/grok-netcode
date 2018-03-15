import { newPlayerState, stepGameState, GameState, newGameState } from './state';
import { GameStateRenderer } from './render';
import { getLatestInputs, PlayerInputScheme, PlayerInput } from './input';
import { UIDMap } from './utils';

export type ClientReceiveState = (gameState: GameState) => void; 

const getCurrentInputs = (buffers: UIDMap<PlayerInput[]>): UIDMap<PlayerInput> => {
    const result: UIDMap<PlayerInput> = {};
    for (var k in buffers) {
        result[k] = (buffers[k].length > 1 ? buffers[k].splice(0,1)[0] : buffers[k][0]) as PlayerInput;
    }
    return result;
};

export class Server {
    private readonly renderer: GameStateRenderer;
    private readonly inputBuffers: UIDMap<PlayerInput[]> = {};
    private readonly clientCallbacks: ClientReceiveState[] = [];
    private readonly stateHistory: GameState[] = [];

    private get curState(): GameState { return this.stateHistory[this.stateHistory.length-1]; }

    constructor(renderer: GameStateRenderer) {
        this.renderer = renderer;
        this.stateHistory.push(newGameState());
    }

    addClient(playerUID: string, cb: ClientReceiveState): void {
        this.curState.players[playerUID] = newPlayerState();
        this.clientCallbacks.push(cb);
        this.inputBuffers[playerUID] = [getLatestInputs(PlayerInputScheme.Nothing, this.curState.frameCount)];
    }

    receiveInput(playerUID: string, input: PlayerInput): void {
        this.inputBuffers[playerUID].push(input);
    }

    update(): void {
        const newState = stepGameState(getCurrentInputs(this.inputBuffers), this.stateHistory);
        this.stateHistory.push(newState);

        if (this.stateHistory.length > 100) {
            this.stateHistory.splice(0, 1);
        }

        this.clientCallbacks.forEach(f => f(newState));
        this.renderer(newState);
    }
}
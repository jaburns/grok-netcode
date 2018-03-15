import { predictGameState, GameState } from './state';
import { GameStateRenderer } from './render';
import { getLatestInputs, PlayerInputScheme, PlayerInput } from './input';
import { generateUID } from './utils';

export type ServerReceiveInput = (playerUID: string, input: PlayerInput) => void; 

let clientCounter: number = 0;

export class Client {
    private readonly renderer: GameStateRenderer;
    private readonly inputScheme: PlayerInputScheme;
    private readonly inputStack: PlayerInput[] = [];
    private readonly _playerUID: string;
    private gameState: GameState | null = null;
    private serverCallback: ServerReceiveInput | null = null;

    get playerUID(): string { return this._playerUID; }

    constructor(renderer: GameStateRenderer) {
        this.renderer = renderer;
        this.inputScheme = clientCounter++ % 2 == 0 ? PlayerInputScheme.Arrows : PlayerInputScheme.WASD;
        this._playerUID = generateUID();
    }

    bindServer(cb: ServerReceiveInput): void {
        this.serverCallback = cb;
    }

    receiveState(serverState: GameState): void {
        if (this.gameState == null) {
            this.gameState = serverState;
            return;
        }

        let matchIndex = this.inputStack.length;
        while (--matchIndex >= 0) {
            if (serverState.players[this._playerUID].lastInputUID === this.inputStack[matchIndex].uid) break;
        }

        if (matchIndex >= 0) {
            this.gameState = serverState;
            for (let i = matchIndex + 1; i < this.inputStack.length; ++i) {
                this.gameState = predictGameState(this.inputStack[i], this._playerUID, this.gameState, false);
            }
        }
    }

    update(): void {
        if (this.gameState === null) return;

        const latestInput = getLatestInputs(this.inputScheme, this.gameState.frameCount);
        this.gameState = predictGameState(latestInput, this._playerUID, this.gameState, true);
        this.inputStack.push(latestInput);

        if (this.inputStack.length > 100) {
            this.inputStack.splice(0, 1);
        }

        if (this.serverCallback !== null) {
            this.serverCallback(this._playerUID, latestInput);
        }

        this.renderer(this.gameState);
    }
}
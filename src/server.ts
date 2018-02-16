import { newPlayerState, stepGameState, GameState, newGameState } from './state';
import { renderGameState } from './render';
import { getLatestInputs, PlayerInputScheme, PlayerInput } from './input';
import { UIDMap } from './utils';

export type ClientReceiveState = (gameState: GameState) => void; 

export class Server {
    private readonly canvasContext: CanvasRenderingContext2D;
    private readonly inputMap: UIDMap<PlayerInput> = {};
    private readonly clientCallbacks: ClientReceiveState[] = [];
    private gameState: GameState;

    constructor(context: CanvasRenderingContext2D) {
        this.canvasContext = context;
        this.gameState = newGameState();
    }

    addClient(playerUID: string, cb: ClientReceiveState): void {
        this.gameState.players[playerUID] = newPlayerState();
        this.clientCallbacks.push(cb);
        this.inputMap[playerUID] = getLatestInputs(PlayerInputScheme.Nothing);
    }

    receiveInput(playerUID: string, input: PlayerInput): void {
        this.inputMap[playerUID] = input;
    }

    update(): void {
        this.gameState = stepGameState(this.inputMap, this.gameState);
        this.clientCallbacks.forEach(f => f(this.gameState));
        renderGameState(this.canvasContext, this.gameState, 'Server');
    }
}
import { PlayerInput } from './input';
import { UIDMap } from './utils';
import cloneDeep = require('lodash/cloneDeep');

export interface Vec2 {
    x: number;
    y: number;
}

export interface PlayerState {
    lastInputUID: string;
    position: Vec2;
    velocity: Vec2;
    rotation: number;
}

export interface GameState {
    players: {[playerUID: string]: PlayerState};
}

export const newGameState = (): GameState => ({ 
    players: {}
});

export const newPlayerState = (): PlayerState => ({
    lastInputUID: "",
    position: {x: 0.25 + Math.random()*0.5, y: 0.25 + Math.random()*0.5},
    velocity: {x: 0, y: 0},
    rotation: 0
});

const stepPlayerState = (input: PlayerInput, playerState: PlayerState): PlayerState => {
    const result = cloneDeep(playerState);

    if (input.left) {
        result.rotation -= 10;
    }

    if (input.right) {
        result.rotation += 10;
    }

    if (input.up) {
        const cos = Math.cos(result.rotation * Math.PI / 180);
        const sin = Math.sin(result.rotation * Math.PI / 180);

        result.position.x += 0.02*cos;
        result.position.y += 0.02*sin;
    }

    result.lastInputUID = input.uid;

    return result;
};

export const stepGameState = (inputMap: UIDMap<PlayerInput>, gameState: GameState): GameState => {
    const result = cloneDeep(gameState);

    for (let playerUID in result.players) {
        result.players[playerUID] = stepPlayerState(inputMap[playerUID], result.players[playerUID]);
    }

    return result;
};

export const predictGameState = (input: PlayerInput, playerUID: string, gameState: GameState): GameState => {
    const result = cloneDeep(gameState);
    result.players[playerUID] = stepPlayerState(input, result.players[playerUID]);
    return result;
};
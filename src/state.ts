import { PlayerInput } from './input';
import { UIDMap } from './utils';
import cloneDeep = require('lodash/cloneDeep');

export const LASER_TOTAL_TIME: number = 10;

const PLAYER_ACCEL: number = 0.001;

export interface Vec2 {
    x: number;
    y: number;
}

export interface LaserState {
    source: Vec2;
    angle: number;
    timeLeft: number;
}

export interface PlayerState {
    lastInputUID: string;
    position: Vec2;
    velocity: Vec2;
    rotation: number;
    lasers: LaserState[];
}

export interface GameState {
    players: UIDMap<PlayerState>;
}

export const newGameState = (): GameState => ({ 
    players: {},
});

export const newPlayerState = (): PlayerState => ({
    lastInputUID: "",
    position: {x: 0.25 + Math.random()*0.5, y: 0.25 + Math.random()*0.5},
    velocity: {x: 0, y: 0},
    rotation: 360 * Math.random(),
    lasers: []
});

const stepPlayerState = (input: PlayerInput, playerState: PlayerState): PlayerState => {
    const state = cloneDeep(playerState);

    if (input.left) {
        state.rotation -= 10;
    }

    if (input.right) {
        state.rotation += 10;
    }

    if (input.up) {
        const cos = Math.cos(state.rotation * Math.PI / 180);
        const sin = Math.sin(state.rotation * Math.PI / 180);

        state.velocity.x += PLAYER_ACCEL * cos;
        state.velocity.y += PLAYER_ACCEL * sin;
    }

    state.position.x += state.velocity.x;
    state.position.y += state.velocity.y;

    if (state.position.x > 1) state.position.x -= 1;
    if (state.position.y > 1) state.position.y -= 1;
    if (state.position.x < 0) state.position.x += 1;
    if (state.position.y < 0) state.position.y += 1;

    for (let i = state.lasers.length - 1; i >= 0; --i) {
        if (--state.lasers[i].timeLeft <= 0) {
            state.lasers.splice(i, 1);
        }
    }

    if (input.shoot) {
        state.lasers.push({
            source: state.position,
            angle: state.rotation,
            timeLeft: LASER_TOTAL_TIME
        });
    }

    state.lastInputUID = input.uid;

    return state;
};

export const stepGameState = (inputMap: UIDMap<PlayerInput>, gameState: GameState): GameState => {
    const result = cloneDeep(gameState);

    for (let playerUID in result.players) {
        const steppedPlayer = stepPlayerState(inputMap[playerUID], result.players[playerUID]);
        result.players[playerUID] = steppedPlayer;
    }

    return result;
};

export const predictGameState = (input: PlayerInput, playerUID: string, gameState: GameState): GameState => {
    const result = cloneDeep(gameState);
    result.players[playerUID] = stepPlayerState(input, result.players[playerUID]);
    return result;
};
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
}

export interface GameState {
    players: {[playerUID: string]: PlayerState};
    lasers: LaserState[];
}

export const newGameState = (): GameState => ({ 
    players: {},
    lasers: []
});

export const newPlayerState = (): PlayerState => ({
    lastInputUID: "",
    position: {x: 0.25 + Math.random()*0.5, y: 0.25 + Math.random()*0.5},
    velocity: {x: 0, y: 0},
    rotation: 0
});

interface PlayerStepResult {
    newPlayerState: PlayerState;
    createdLaser: LaserState | null;
};

const stepPlayerState = (input: PlayerInput, playerState: PlayerState): PlayerStepResult => {
    const state = cloneDeep(playerState);
    let laser: LaserState | null = null;

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

    if (input.shoot) {
        laser = {
            source: state.position,
            angle: state.rotation,
            timeLeft: LASER_TOTAL_TIME
        };
    }

    state.lastInputUID = input.uid;

    return {
        newPlayerState: state,
        createdLaser: laser
    };
};

export const stepGameState = (inputMap: UIDMap<PlayerInput>, gameState: GameState): GameState => {
    const result = cloneDeep(gameState);

    for (let i = result.lasers.length - 1; i >= 0; --i) {
        if (--result.lasers[i].timeLeft <= 0) {
            result.lasers.splice(i, 1);
        }
    }

    for (let playerUID in result.players) {
        const steppedPlayer = stepPlayerState(inputMap[playerUID], result.players[playerUID]);
        result.players[playerUID] = steppedPlayer.newPlayerState;

        if (steppedPlayer.createdLaser !== null) {
            result.lasers.push(steppedPlayer.createdLaser);
        }
    }

    return result;
};

export const predictGameState = (input: PlayerInput, playerUID: string, gameState: GameState): GameState => {
    const result = cloneDeep(gameState);
    result.players[playerUID] = stepPlayerState(input, result.players[playerUID]).newPlayerState;
    return result;
};
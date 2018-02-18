import { PlayerInput } from './input';
import { UIDMap, Vec2, v2add, v2scale, circleLineIntersect, v2fromRadial } from './utils';
import cloneDeep = require('lodash/cloneDeep');

export const LASER_TOTAL_TIME: number = 10;
export const PLAYER_RADIUS: number = 0.05;

const LASER_COOLDOWN: number = 20;
const PLAYER_ACCEL: number = 0.001;

export enum HitStatus {
    Nothing,
    Predicted,
    Confirmed
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
    laserCooldown: number;
    lasers: LaserState[];
    hitStatus: HitStatus;
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
    rotation: 2 * Math.PI * Math.random(),
    laserCooldown: 0,
    lasers: [],
    hitStatus: HitStatus.Nothing
});

const stepPlayerState = (input: PlayerInput, playerState: PlayerState): PlayerState => {
    const state = cloneDeep(playerState);

    if (input.left) {
        state.rotation -= 0.2;
    }

    if (input.right) {
        state.rotation += 0.2;
    }

    const dir = v2fromRadial(1, state.rotation);

    if (input.up) {
        state.velocity = v2add(state.velocity, v2scale(dir, PLAYER_ACCEL));
    }

    state.position = v2add(state.position, state.velocity);

    if (state.position.x > 1 - PLAYER_RADIUS) state.velocity.x = -Math.abs(state.velocity.x);
    if (state.position.y > 1 - PLAYER_RADIUS) state.velocity.y = -Math.abs(state.velocity.y);
    if (state.position.x <     PLAYER_RADIUS) state.velocity.x =  Math.abs(state.velocity.x);
    if (state.position.y <     PLAYER_RADIUS) state.velocity.y =  Math.abs(state.velocity.y);

    if (state.laserCooldown > 0) {
        state.laserCooldown--;
    }

    for (let i = state.lasers.length - 1; i >= 0; --i) {
        if (--state.lasers[i].timeLeft <= 0) {
            state.lasers.splice(i, 1);
        }
    }

    if (input.shoot && state.laserCooldown < 1) {
        state.lasers.push({
            source: v2add(state.position, v2scale(dir, PLAYER_RADIUS)),
            angle: state.rotation,
            timeLeft: LASER_TOTAL_TIME
        });
        state.laserCooldown = LASER_COOLDOWN;
    }

    state.lastInputUID = input.uid;

    return state;
};

const laserCollides = (laser: LaserState, player: PlayerState): boolean =>
    circleLineIntersect(
        laser.source, 
        v2add(laser.source, v2fromRadial(1000, laser.angle)),
        player.position,
        PLAYER_RADIUS
    );

const predictLaserCollisions = (gameState: GameState, playerUID: string): void => {
    for (let b in gameState.players) {
        if (playerUID === b) continue;

        gameState.players[playerUID].lasers.forEach(laser => {
            if (laserCollides(laser, gameState.players[b])) {
                gameState.players[b].hitStatus = HitStatus.Predicted;
            }
        });
    }
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
    predictLaserCollisions(result, playerUID);
    return result;
};
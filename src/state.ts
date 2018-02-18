import { PlayerInput } from './input';
import { UIDMap, Vec2, v2add, v2scale, circleLineIntersect, v2fromRadial } from './utils';
import cloneDeep = require('lodash/cloneDeep');

export const LASER_TOTAL_TIME: number = 10;
export const PLAYER_RADIUS: number = 0.05;

const HIT_COOLDOWN: number = 20;
const LASER_COOLDOWN: number = 20;
const PLAYER_SPEED: number = 0.02;

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
    rotation: number;
    laserCooldown: number;
    lasers: LaserState[];
    hitStatus: HitStatus;
    hitCooldown: number;
}

export interface GameState {
    frameCount: number;
    predictedFrameCount: number;
    players: UIDMap<PlayerState>;
}

export const newGameState = (): GameState => ({ 
    frameCount: 0,
    predictedFrameCount: 0,
    players: {},
});

export const newPlayerState = (): PlayerState => ({
    lastInputUID: "",
    position: {x: 0.25 + Math.random()*0.5, y: 0.25 + Math.random()*0.5},
    rotation: 2 * Math.PI * Math.random(),
    laserCooldown: 0,
    lasers: [],
    hitStatus: HitStatus.Nothing,
    hitCooldown: 0,
});

const stepPlayerState = (input: PlayerInput, playerState: PlayerState): PlayerState => {
    const state = cloneDeep(playerState);

    if (state.hitCooldown > 0) {
        if (--state.hitCooldown <= 0) {
            state.hitStatus = HitStatus.Nothing;
        }
    }

    if (input.left) {
        state.rotation -= 0.2;
    }

    if (input.right) {
        state.rotation += 0.2;
    }

    const dir = v2fromRadial(1, state.rotation);

    if (input.up) {
        state.position = v2add(state.position, v2scale(dir, PLAYER_SPEED));
    }

    if (state.position.x > 1 - PLAYER_RADIUS) state.position.x = 1 - PLAYER_RADIUS;
    if (state.position.y > 1 - PLAYER_RADIUS) state.position.y = 1 - PLAYER_RADIUS;
    if (state.position.x <     PLAYER_RADIUS) state.position.x =     PLAYER_RADIUS;
    if (state.position.y <     PLAYER_RADIUS) state.position.y =     PLAYER_RADIUS; 

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

const laserCollides = (laser: LaserState, playerPos: Vec2): boolean =>
    circleLineIntersect(
        laser.source, 
        v2add(laser.source, v2fromRadial(1000, laser.angle)),
        playerPos,
        PLAYER_RADIUS
    );

const rewindAndCollideLaser = (laser: LaserState, shooterUID: string, targetUID: string, shooterInputFrame: number, historicalStates: GameState[]): boolean => {
    for (let i = historicalStates.length - 1; i >= 0; --i) {
        if (historicalStates[i].frameCount === shooterInputFrame) {
            return laserCollides(laser, historicalStates[i].players[targetUID].position);
        }
    }

    return false;
};

export const stepGameState = (inputMap: UIDMap<PlayerInput>, historicalStates: GameState[]): GameState => {
    const gameState = historicalStates[historicalStates.length - 1];
    const result = cloneDeep(gameState);

    // Update frame counter
    result.frameCount++;
    result.predictedFrameCount = result.frameCount;

    // Step individual player states using latest inputs received
    for (let playerUID in result.players) {
        const steppedPlayer = stepPlayerState(inputMap[playerUID], result.players[playerUID]);
        result.players[playerUID] = steppedPlayer;
    }

    // Resolve laser collisions
    for (let a in result.players) {
        for (let b in result.players) {
            if (a === b) continue;

            result.players[a].lasers.forEach(laser => {
                const shooterInputFrame = inputMap[a].frame;
                if (laser.timeLeft === LASER_TOTAL_TIME && rewindAndCollideLaser(laser, a, b, shooterInputFrame, historicalStates)) {
                    result.players[b].hitStatus = HitStatus.Confirmed;
                    result.players[b].hitCooldown = HIT_COOLDOWN;
                }
            });
        }
    }

    return result;
};

export const predictGameState = (input: PlayerInput, playerUID: string, gameState: GameState, newFrame: boolean): GameState => {
    const result = cloneDeep(gameState);

    result.predictedFrameCount++;

    result.players[playerUID] = stepPlayerState(input, result.players[playerUID]);

    // Predict laser collisions in local reference frame
    if (newFrame) {
        for (let b in result.players) {
            if (playerUID === b) continue;

            result.players[playerUID].lasers.forEach(laser => {
                if (laser.timeLeft === LASER_TOTAL_TIME && laserCollides(laser, result.players[b].position)) {
                    result.players[b].hitStatus = HitStatus.Predicted;
                }
            });
        }
    }

    return result;
};
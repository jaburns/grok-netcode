import { PlayerInput } from './input';
import { UIDMap, Vec2, v2add, v2scale, circleLineIntersect, v2fromRadial } from './utils';
import cloneDeep = require('lodash/cloneDeep');

export const LASER_TOTAL_TIME: number = 10;
export const PLAYER_RADIUS: number = 0.05;

const HIT_COOLDOWN: number = 20;
const LASER_COOLDOWN: number = 20;
const PLAYER_SPEED: number = 0.02;
const BULLET_SPEED: number = 0.03;

export enum HitStatus {
    Nothing,
    Predicted,
    Confirmed
}

export interface LaserState {
    ownerUID: string;
    source: Vec2;
    angle: number;
    timeLeft: number;
}

export interface BulletState {
    ownerUID: string;
    position: Vec2;
    angle: number;
    predicted: boolean;
    age: number;
}

export interface PlayerState {
    lastInputUID: string;
    position: Vec2;
    rotation: number;
    gunCooldown: number;
    hitStatus: HitStatus;
    hitCooldown: number;
}

export interface GameState {
    frameCount: number;
    predictedFrameCount: number;
    players: UIDMap<PlayerState>;
    lasers: LaserState[];
    bullets: BulletState[];
}

export const newGameState = (): GameState => ({ 
    frameCount: 0,
    predictedFrameCount: 0,
    players: {},
    lasers: [],
    bullets: []
});

export const newPlayerState = (): PlayerState => ({
    lastInputUID: "",
    position: {x: 0.25 + Math.random()*0.5, y: 0.25 + Math.random()*0.5},
    rotation: 2 * Math.PI * Math.random(),
    gunCooldown: 0,
    hitStatus: HitStatus.Nothing,
    hitCooldown: 0,
});

interface PlayerUpdateResult {
    newState: PlayerState;
    newLaser: LaserState | null;
    newBullet: BulletState | null;
}

const stepPlayerState = (input: PlayerInput, playerUID: string, playerState: PlayerState): PlayerUpdateResult => {
    const state = cloneDeep(playerState);
    const result: PlayerUpdateResult = { 
        newState: state, 
        newLaser: null, 
        newBullet: null 
    };

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

    if (state.gunCooldown > 0) {
        state.gunCooldown--;
    }

    if (input.bullet && state.gunCooldown < 1) {
        result.newBullet = {
            ownerUID: playerUID,
            position: v2add(state.position, v2scale(dir, PLAYER_RADIUS)),
            angle: state.rotation,
            predicted: false,
            age: 0
        };
        state.gunCooldown = LASER_COOLDOWN;
    }

    if (input.laser && state.gunCooldown < 1) {
        result.newLaser = {
            ownerUID: playerUID,
            source: v2add(state.position, v2scale(dir, PLAYER_RADIUS)),
            angle: state.rotation,
            timeLeft: LASER_TOTAL_TIME
        };
        state.gunCooldown = LASER_COOLDOWN;
    }

    state.lastInputUID = input.uid;

    return result;
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

const updateLasers = (gameState: GameState): void => {
    for (let i = gameState.lasers.length - 1; i >= 0; --i) {
        if (--gameState.lasers[i].timeLeft <= 0) {
            gameState.lasers.splice(i, 1);
        }
    }
};

const updateBullets = (gameState: GameState, predictingForPlayerUID: string | null): void => {
    for (let bullet of gameState.bullets) {
        if (predictingForPlayerUID === null || bullet.predicted || bullet.ownerUID !== predictingForPlayerUID) { 
            bullet.position = v2add(bullet.position, v2fromRadial(BULLET_SPEED, bullet.angle));
        }
        bullet.age++;
    }
};

const updatePlayer = (gameState: GameState, input: PlayerInput, playerUID: string, predicting: boolean): void => {
    const steppedPlayer = stepPlayerState(input, playerUID, gameState.players[playerUID]);
    gameState.players[playerUID] = steppedPlayer.newState;
    if (steppedPlayer.newLaser !== null) {
        gameState.lasers.push(steppedPlayer.newLaser);
    }
    if (steppedPlayer.newBullet !== null) {
        const newBullet = steppedPlayer.newBullet;
        newBullet.predicted = predicting;
        gameState.bullets.push(newBullet);
    }
};

export const stepGameState = (inputMap: UIDMap<PlayerInput>, historicalStates: GameState[]): GameState => {
    const gameState = historicalStates[historicalStates.length - 1];
    const result = cloneDeep(gameState);

    result.frameCount++;
    result.predictedFrameCount = result.frameCount;

    updateLasers(result);
    updateBullets(result, null);
    for (let playerUID in result.players) {
        updatePlayer(result, inputMap[playerUID], playerUID, false);
    }

    // Fast-forward bullets from the shooters' reference frames to the server reference frame
    for (let bullet of result.bullets) {
        if (bullet.age === 0) {
            for (let i = inputMap[bullet.ownerUID].frame; i < result.frameCount; ++i) {
                bullet.position = v2add(bullet.position, v2fromRadial(BULLET_SPEED, bullet.angle));
            }
        }
    }

    // Resolve laser collisions
    for (let laser of result.lasers) {
        for (let b in result.players) {
            if (laser.ownerUID === b) continue;

            const shooterInputFrame = inputMap[laser.ownerUID].frame;
            if (laser.timeLeft === LASER_TOTAL_TIME && rewindAndCollideLaser(laser, laser.ownerUID, b, shooterInputFrame, historicalStates)) {
                result.players[b].hitStatus = HitStatus.Confirmed;
                result.players[b].hitCooldown = HIT_COOLDOWN;
            }
        }
    }

    return result;
};

export const predictGameState = (input: PlayerInput, playerUID: string, gameState: GameState, newFrame: boolean): GameState => {
    const result = cloneDeep(gameState);

    result.predictedFrameCount++;

    updateLasers(result);
    updateBullets(result, playerUID);
    updatePlayer(result, input, playerUID, true);

    // Predict laser collisions in local reference frame
    if (newFrame) {
        for (let b in result.players) {
            if (playerUID === b) continue;

            result.lasers.forEach(laser => {
                if (laser.ownerUID !== playerUID) return;

                if (laser.timeLeft === LASER_TOTAL_TIME && laserCollides(laser, result.players[b].position)) {
                    result.players[b].hitStatus = HitStatus.Predicted;
                }
            });
        }
    }

    return result;
};
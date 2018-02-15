import { PlayerInputUID } from './input';

export interface Vec2 {
    x: number;
    y: number;
}

export interface PlayerState {
    latestInput: PlayerInputUID;
    position: Vec2;
    velocity: Vec2;
    rotation: number;
}

export interface GameState {
    players: PlayerState[];
}

export const newWorldState = (): GameState => ({ 
    players: [] 
});

export const newPlayerState = (): PlayerState => ({
    latestInput: "",
    position: {x: 50 + Math.random()*100, y: 50 + Math.random()*100},
    velocity: {x: 0, y: 0},
    rotation: 0
});

const stepPlayerState = (input:any, playerState:any):any => {
    const result = JSON.parse(JSON.stringify(playerState));

    if (input.left) {
        result.rotation -= 10;
    }

    if (input.right) {
        result.rotation += 10;
    }

    if (input.up) {
        const cos = Math.cos(result.rotation * Math.PI / 180);
        const sin = Math.sin(result.rotation * Math.PI / 180);

        result.position.x += 10*cos;
        result.position.y += 10*sin;
    }

    result.lastInput = input.uid;

    return result;
};

export const stepWorldState = (inputs:any, worldState:any):any => {
    const result = JSON.parse(JSON.stringify(worldState));

    for (let i = 0; i < result.players.length; ++i) {
        result.players[i] = stepPlayerState(inputs[i], result.players[i]);
    }

    return result;
};

export const predictWorldState = (input:any, playerId:any, worldState:any):any => {
    const result = JSON.parse(JSON.stringify(worldState));
    result.players[playerId] = stepPlayerState(input, result.players[playerId]);
    return result;
};
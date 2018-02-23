import { generateUID } from './utils';

export interface PlayerInput {
    uid: string;
    frame: number;
    up: boolean;
    left: boolean;
    right: boolean;
    laser: boolean;
    bullet: boolean;
}

export enum PlayerInputScheme {
    Nothing,
    WASD,
    Arrows
}

const keys: {[keyCode:number]: true} = {};

document.onkeydown = e => keys[e.keyCode] = true;
document.onkeyup = e => delete keys[e.keyCode];

const getLatestWASD = (frame: number): PlayerInput => ({
    up:     keys[87] === true, // W
    left:   keys[65] === true, // A
    right:  keys[68] === true, // D
    laser:  keys[83] === true, // S
    bullet: keys[81] === true, // Q
    uid:    generateUID(),
    frame
});

const getLatestArrows = (frame: number): PlayerInput => ({
    up:     keys[38] === true, // Up
    left:   keys[37] === true, // Left
    right:  keys[39] === true, // Right
    laser:  keys[40] === true, // Down
    bullet: keys[32] === true, // Space
    uid:    generateUID(),
    frame
});

const getEmptyInput = (frame: number): PlayerInput => ({
    up:     false,
    left:   false,
    right:  false,
    laser:  false,
    bullet: false,
    uid:    generateUID(),
    frame
});

export const getLatestInputs = (scheme: PlayerInputScheme, frame: number): PlayerInput => {
    switch (scheme) {
        case PlayerInputScheme.WASD: return getLatestWASD(frame);
        case PlayerInputScheme.Arrows: return getLatestArrows(frame);
    }
    return getEmptyInput(frame);
};
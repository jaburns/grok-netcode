import { generateUID } from './utils';

export interface PlayerInput {
    uid: string;
    frame: number;
    up: boolean;
    left: boolean;
    right: boolean;
    shoot: boolean;
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
    up:    keys[87] === true,
    left:  keys[65] === true,
    right: keys[68] === true,
    shoot: keys[83] === true,
    uid:   generateUID(),
    frame
});

const getLatestArrows = (frame: number): PlayerInput => ({
    up:    keys[38] === true,
    left:  keys[37] === true,
    right: keys[39] === true,
    shoot: keys[40] === true,
    uid:   generateUID(),
    frame
});

const getEmptyInput = (frame: number): PlayerInput => ({
    up:    false,
    left:  false,
    right: false,
    shoot: false,
    uid:   generateUID(),
    frame
});

export const getLatestInputs = (scheme: PlayerInputScheme, frame: number): PlayerInput => {
    switch (scheme) {
        case PlayerInputScheme.WASD: return getLatestWASD(frame);
        case PlayerInputScheme.Arrows: return getLatestArrows(frame);
    }
    return getEmptyInput(frame);
};
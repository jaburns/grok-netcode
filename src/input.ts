import { generateUID } from './utils';

export interface PlayerInput {
    uid: string;
    up: boolean;
    down: boolean;
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

const getLatestWASD = (): PlayerInput => ({
    up:    keys[87] === true,
    down:  false,
    left:  keys[65] === true,
    right: keys[68] === true,
    shoot: false,
    uid:   generateUID()
});

const getLatestArrows = (): PlayerInput => ({
    up:    keys[38] === true,
    down:  false,
    left:  keys[37] === true,
    right: keys[39] === true,
    shoot: false,
    uid:   generateUID()
});

const getEmptyInput = (): PlayerInput => ({
    up:    false,
    down:  false,
    left:  false,
    right: false,
    shoot: false,
    uid:   generateUID()
});

export const getLatestInputs = (scheme: PlayerInputScheme): PlayerInput => {
    switch (scheme) {
        case PlayerInputScheme.WASD: return getLatestWASD();
        case PlayerInputScheme.Arrows: return getLatestArrows();
    }
    return getEmptyInput();
};
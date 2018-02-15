
export type PlayerInputUID = string;

export interface PlayerInput {
    uid: PlayerInputUID;
    up: boolean;
    down: boolean;
    left: boolean;
    right: boolean;
    shoot: boolean;
}

export enum PlayerInputScheme {
    WASD,
    Arrows
}

const keys: {[keyCode:number]: true} = {};

document.onkeydown = e => keys[e.keyCode] = true;
document.onkeyup = e => delete keys[e.keyCode];

const generateUID = (): PlayerInputUID =>
    Math.random().toString(36).substr(2);

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

export const getLatestInputs = (scheme: PlayerInputScheme): PlayerInput =>
    scheme === PlayerInputScheme.WASD
        ? getLatestWASD()
        : getLatestArrows();
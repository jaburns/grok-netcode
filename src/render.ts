import { GameState, PlayerState, LaserState, LASER_TOTAL_TIME } from './state';

const VIEW_WIDTH = 300;
const VIEW_HEIGHT = 300;

const renderPlayerState = (ctx: CanvasRenderingContext2D, playerState: PlayerState): void => {
    const frontX = 15*Math.cos(playerState.rotation * Math.PI / 180);
    const frontY = 15*Math.sin(playerState.rotation * Math.PI / 180);
    const rightX = 15*Math.cos((playerState.rotation + 150) * Math.PI / 180);
    const rightY = 15*Math.sin((playerState.rotation + 150) * Math.PI / 180);
    const leftX = 15*Math.cos((playerState.rotation + 210) * Math.PI / 180);
    const leftY = 15*Math.sin((playerState.rotation + 210) * Math.PI / 180);

    ctx.lineWidth = 2;

    ctx.beginPath();
    ctx.moveTo(VIEW_WIDTH * playerState.position.x + frontX, VIEW_HEIGHT * playerState.position.y + frontY);
    ctx.lineTo(VIEW_WIDTH * playerState.position.x + rightX, VIEW_HEIGHT * playerState.position.y + rightY);
    ctx.lineTo(VIEW_WIDTH * playerState.position.x + leftX,  VIEW_HEIGHT * playerState.position.y + leftY);
    ctx.lineTo(VIEW_WIDTH * playerState.position.x + frontX, VIEW_HEIGHT * playerState.position.y + frontY);
    ctx.strokeStyle = '#f99';
    ctx.stroke();
};

const renderLaser = (ctx: CanvasRenderingContext2D, laser: LaserState): void => {
    const offx = 1000*Math.cos(laser.angle * Math.PI / 180);
    const offy = 1000*Math.sin(laser.angle * Math.PI / 180);

    ctx.lineWidth = 2;

    ctx.beginPath();
    ctx.moveTo(VIEW_WIDTH * laser.source.x, VIEW_HEIGHT * laser.source.y);
    ctx.lineTo(VIEW_WIDTH * laser.source.x + offx, VIEW_HEIGHT * laser.source.y + offy);

    const strength = Math.round(15 * laser.timeLeft / LASER_TOTAL_TIME).toString(16);
    ctx.strokeStyle = '#0'+strength+'0';

    ctx.stroke();
};

export const createCanvas = (): CanvasRenderingContext2D => {
    const canvas = document.createElement('canvas') as HTMLCanvasElement;
    canvas.width = VIEW_WIDTH;
    canvas.height = VIEW_HEIGHT;
    (document.getElementById('views') as HTMLDivElement).appendChild(canvas);
    return canvas.getContext('2d') as CanvasRenderingContext2D;
};

export const renderGameState = (ctx: CanvasRenderingContext2D, gameState: GameState , title: string): void => {
    ctx.clearRect(0, 0, VIEW_WIDTH, VIEW_HEIGHT);

    for (let playerUID in gameState.players) {
        renderPlayerState(ctx, gameState.players[playerUID]);
    }

    for (let laser of gameState.lasers) {
        renderLaser(ctx, laser);
    }

    ctx.font = '12px monospace';
    ctx.fillStyle = '#fff';
    ctx.fillText(title, 2, 12);
};
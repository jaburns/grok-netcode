import { GameState, PlayerState } from './state';

const VIEW_WIDTH = 300;
const VIEW_HEIGHT = 300;

const renderPlayerState = (ctx: CanvasRenderingContext2D, playerState: PlayerState): void => {
    const offx = 15*Math.cos(playerState.rotation * Math.PI / 180);
    const offy = 15*Math.sin(playerState.rotation * Math.PI / 180);

    ctx.lineWidth = 2;

    ctx.beginPath();
    ctx.moveTo(VIEW_WIDTH * playerState.position.x - offx, VIEW_HEIGHT * playerState.position.y - offy);
    ctx.lineTo(VIEW_WIDTH * playerState.position.x, VIEW_HEIGHT * playerState.position.y);
    ctx.strokeStyle = '#f99';
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(VIEW_WIDTH * playerState.position.x, VIEW_HEIGHT * playerState.position.y);
    ctx.lineTo(VIEW_WIDTH * playerState.position.x + offx, VIEW_HEIGHT * playerState.position.y + offy);
    ctx.strokeStyle = '#99f';
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

    ctx.font = '12px monospace';
    ctx.fillStyle = '#fff';
    ctx.fillText(title, 2, 12);
};
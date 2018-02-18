import { GameState, PlayerState, LaserState, LASER_TOTAL_TIME, PLAYER_RADIUS, HitStatus } from './state';

const VIEW_WIDTH = 300;
const VIEW_HEIGHT = 300;

const PLAYER_RENDER_RADIUS = VIEW_WIDTH * PLAYER_RADIUS;

const renderLaser = (ctx: CanvasRenderingContext2D, laser: LaserState): void => {
    const offx = 1000*Math.cos(laser.angle);
    const offy = 1000*Math.sin(laser.angle);

    ctx.lineWidth = 2;

    ctx.beginPath();
    ctx.moveTo(VIEW_WIDTH * laser.source.x, VIEW_HEIGHT * laser.source.y);
    ctx.lineTo(VIEW_WIDTH * laser.source.x + offx, VIEW_HEIGHT * laser.source.y + offy);

    const strength = Math.round(0xF * laser.timeLeft / LASER_TOTAL_TIME).toString(16);
    ctx.strokeStyle = '#0'+strength+'0';

    ctx.stroke();
};

const renderPlayerState = (ctx: CanvasRenderingContext2D, playerState: PlayerState): void => {
    const frontX = PLAYER_RENDER_RADIUS*Math.cos(playerState.rotation);
    const frontY = PLAYER_RENDER_RADIUS*Math.sin(playerState.rotation);
    const rightX = PLAYER_RENDER_RADIUS*Math.cos(playerState.rotation + (150 * Math.PI / 180));
    const rightY = PLAYER_RENDER_RADIUS*Math.sin(playerState.rotation + (150 * Math.PI / 180));
    const leftX  = PLAYER_RENDER_RADIUS*Math.cos(playerState.rotation + (210 * Math.PI / 180));
    const leftY  = PLAYER_RENDER_RADIUS*Math.sin(playerState.rotation + (210 * Math.PI / 180));

    ctx.lineWidth = 2;
    ctx.strokeStyle = 
        playerState.hitStatus === HitStatus.Predicted ? '#fff' :
        playerState.hitStatus === HitStatus.Confirmed ? '#999' : '#f99';

    ctx.beginPath();
    ctx.moveTo(VIEW_WIDTH * playerState.position.x + frontX, VIEW_HEIGHT * playerState.position.y + frontY);
    ctx.lineTo(VIEW_WIDTH * playerState.position.x + rightX, VIEW_HEIGHT * playerState.position.y + rightY);
    ctx.moveTo(VIEW_WIDTH * playerState.position.x + frontX, VIEW_HEIGHT * playerState.position.y + frontY);
    ctx.lineTo(VIEW_WIDTH * playerState.position.x + leftX,  VIEW_HEIGHT * playerState.position.y + leftY);
    ctx.stroke();

    ctx.beginPath();
    ctx.arc(VIEW_WIDTH * playerState.position.x, VIEW_WIDTH * playerState.position.y, PLAYER_RENDER_RADIUS, 0, 2*Math.PI);
    ctx.stroke();

    for (let laser of playerState.lasers) {
        renderLaser(ctx, laser);
    }
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
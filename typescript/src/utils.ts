export type UIDMap<T> = {[uid: string]: T};

export const generateUID = (): string => 
    Math.random().toString(36).substr(2);

export interface Vec2 {
    x: number;
    y: number;
};

export const v2add = (a: Vec2, b: Vec2): Vec2 => ({
    x: a.x + b.x,
    y: a.y + b.y
});

export const v2sub = (a: Vec2, b: Vec2): Vec2 => ({
    x: a.x - b.x,
    y: a.y - b.y
});

export const v2scale = (a: Vec2, b: number): Vec2 => ({
    x: b * a.x,
    y: b * a.y
});

export const v2rotate = (a: Vec2, radians: number): Vec2 => {
    const cos = Math.cos(radians);
    const sin = Math.sin(radians);
    return {
        x: a.x*cos - a.y*sin,
        y: a.x*sin + a.y*cos
    };
};

export const v2sqrMagnitude = (a: Vec2): number =>
    a.x*a.x + a.y*a.y;

export const v2fromRadial = (r: number, theta: number): Vec2 => ({
    x: r * Math.cos(theta),
    y: r * Math.sin(theta)
});

export const circleLineIntersect = (linePtA: Vec2, linePtB: Vec2, circleCenter: Vec2, radius: number): boolean => {
    const relCircle = v2sub(circleCenter, linePtA);

    if (v2sqrMagnitude(relCircle) < radius*radius) return true;
    if (v2sqrMagnitude(v2sub(circleCenter, linePtB)) < radius*radius) return true;

    const theta = Math.atan2(linePtB.y - linePtA.y, linePtB.x - linePtA.x);
    const lineSpaceCircleCenter = v2rotate(relCircle, -theta);

    if (Math.abs(lineSpaceCircleCenter.y) > radius) return false;
    const cx2 = lineSpaceCircleCenter.x * lineSpaceCircleCenter.x;
    if (cx2 < 0) return false;
    if (cx2 > v2sqrMagnitude(v2sub(linePtB, linePtA))) return false;

    return true;
};
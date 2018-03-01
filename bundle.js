!function(t){function e(r){if(n[r])return n[r].exports;var o=n[r]={i:r,l:!1,exports:{}};return t[r].call(o.exports,o,o.exports,e),o.l=!0,o.exports}var n={};e.m=t,e.c=n,e.d=function(t,n,r){e.o(t,n)||Object.defineProperty(t,n,{configurable:!1,enumerable:!0,get:r})},e.n=function(t){var n=t&&t.__esModule?function(){return t.default}:function(){return t};return e.d(n,"a",n),n},e.o=function(t,e){return Object.prototype.hasOwnProperty.call(t,e)},e.p="",e(e.s=42)}([function(t,e,n){var r=n(26),o="object"==typeof self&&self&&self.Object===Object&&self,i=r||o||Function("return this")();t.exports=i},function(t,e,n){function r(t,e){var n=i(t,e);return o(n)?n:void 0}var o=n(56),i=n(62);t.exports=r},function(t,e){function n(t){var e=typeof t;return null!=t&&("object"==e||"function"==e)}t.exports=n},function(t,e){function n(t){return null!=t&&"object"==typeof t}t.exports=n},function(t,e,n){function r(t){var e=-1,n=null==t?0:t.length;for(this.clear();++e<n;){var r=t[e];this.set(r[0],r[1])}}var o=n(46),i=n(47),a=n(48),u=n(49),c=n(50);r.prototype.clear=o,r.prototype.delete=i,r.prototype.get=a,r.prototype.has=u,r.prototype.set=c,t.exports=r},function(t,e,n){function r(t,e){for(var n=t.length;n--;)if(o(t[n][0],e))return n;return-1}var o=n(24);t.exports=r},function(t,e,n){function r(t){return null==t?void 0===t?c:u:s&&s in Object(t)?i(t):a(t)}var o=n(13),i=n(58),a=n(59),u="[object Null]",c="[object Undefined]",s=o?o.toStringTag:void 0;t.exports=r},function(t,e,n){var r=n(1),o=r(Object,"create");t.exports=o},function(t,e,n){function r(t,e){var n=t.__data__;return o(e)?n["string"==typeof e?"string":"hash"]:n.map}var o=n(72);t.exports=r},function(t,e,n){function r(t,e,n,r){var a=!n;n||(n={});for(var u=-1,c=e.length;++u<c;){var s=e[u],f=r?r(n[s],t[s],s,n,t):void 0;void 0===f&&(f=t[s]),a?i(n,s,f):o(n,s,f)}return n}var o=n(28),i=n(29);t.exports=r},function(t,e,n){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var r=n(11),o=n(23);e.LASER_TOTAL_TIME=10,e.PLAYER_RADIUS=.05;var i;!function(t){t[t.Nothing=0]="Nothing",t[t.Predicted=1]="Predicted",t[t.Confirmed=2]="Confirmed"}(i=e.HitStatus||(e.HitStatus={})),e.newGameState=function(){return{frameCount:0,predictedFrameCount:0,players:{},lasers:[],bullets:[]}},e.newPlayerState=function(){return{lastInputUID:"",position:{x:.25+.5*Math.random(),y:.25+.5*Math.random()},rotation:2*Math.PI*Math.random(),gunCooldown:0,hitStatus:i.Nothing,hitCooldown:0}};var a=function(t,n,a){var u=o(a),c={newState:u,newLaser:null,newBullet:null};u.hitCooldown>0&&--u.hitCooldown<=0&&(u.hitStatus=i.Nothing),t.left&&(u.rotation-=.2),t.right&&(u.rotation+=.2);var s=r.v2fromRadial(1,u.rotation);return t.up&&(u.position=r.v2add(u.position,r.v2scale(s,.02))),u.position.x>1-e.PLAYER_RADIUS&&(u.position.x=1-e.PLAYER_RADIUS),u.position.y>1-e.PLAYER_RADIUS&&(u.position.y=1-e.PLAYER_RADIUS),u.position.x<e.PLAYER_RADIUS&&(u.position.x=e.PLAYER_RADIUS),u.position.y<e.PLAYER_RADIUS&&(u.position.y=e.PLAYER_RADIUS),u.gunCooldown>0&&u.gunCooldown--,t.bullet&&u.gunCooldown<1&&(c.newBullet={ownerUID:n,position:r.v2add(u.position,r.v2scale(s,e.PLAYER_RADIUS)),angle:u.rotation,predicted:!1,age:0},u.gunCooldown=20),t.laser&&u.gunCooldown<1&&(c.newLaser={ownerUID:n,source:r.v2add(u.position,r.v2scale(s,e.PLAYER_RADIUS)),angle:u.rotation,timeLeft:e.LASER_TOTAL_TIME},u.gunCooldown=20),u.lastInputUID=t.uid,c},u=function(t,n){return r.circleLineIntersect(t.source,r.v2add(t.source,r.v2fromRadial(1e3,t.angle)),n,e.PLAYER_RADIUS)},c=function(t,e,n,r,o){for(var i=o.length-1;i>=0;--i)if(o[i].frameCount===r)return u(t,o[i].players[n].position);return!1},s=function(t){for(var e=t.lasers.length-1;e>=0;--e)--t.lasers[e].timeLeft<=0&&t.lasers.splice(e,1)},f=function(t,e){for(var n=0,o=t.bullets;n<o.length;n++){var i=o[n];(null===e||i.predicted||i.ownerUID!==e)&&(i.position=r.v2add(i.position,r.v2fromRadial(.03,i.angle))),i.age++}},p=function(t,e,n,r){var o=a(e,n,t.players[n]);if(t.players[n]=o.newState,null!==o.newLaser&&t.lasers.push(o.newLaser),null!==o.newBullet){var i=o.newBullet;i.predicted=r,t.bullets.push(i)}};e.stepGameState=function(t,n){var a=n[n.length-1],u=o(a);u.frameCount++,u.predictedFrameCount=u.frameCount,s(u),f(u,null);for(var l in u.players)p(u,t[l],l,!1);for(var v=0,h=u.bullets;v<h.length;v++){var y=h[v];if(0===y.age)for(var d=t[y.ownerUID].frame;d<u.frameCount;++d)y.position=r.v2add(y.position,r.v2fromRadial(.03,y.angle))}for(var b=0,x=u.lasers;b<x.length;b++){var g=x[b];for(var _ in u.players)if(g.ownerUID!==_){var j=t[g.ownerUID].frame;g.timeLeft===e.LASER_TOTAL_TIME&&c(g,g.ownerUID,_,j,n)&&(u.players[_].hitStatus=i.Confirmed,u.players[_].hitCooldown=20)}}return u},e.predictGameState=function(t,n,r,a){var c=o(r);if(c.predictedFrameCount++,s(c),f(c,n),p(c,t,n,!0),a){for(var l in c.players)!function(t){if(n===t)return"continue";c.lasers.forEach(function(r){r.ownerUID===n&&r.timeLeft===e.LASER_TOTAL_TIME&&u(r,c.players[t].position)&&(c.players[t].hitStatus=i.Predicted)})}(l)}return c}},function(t,e,n){"use strict";Object.defineProperty(e,"__esModule",{value:!0}),e.generateUID=function(){return Math.random().toString(36).substr(2)},e.v2add=function(t,e){return{x:t.x+e.x,y:t.y+e.y}},e.v2sub=function(t,e){return{x:t.x-e.x,y:t.y-e.y}},e.v2scale=function(t,e){return{x:e*t.x,y:e*t.y}},e.v2rotate=function(t,e){var n=Math.cos(e),r=Math.sin(e);return{x:t.x*n-t.y*r,y:t.x*r+t.y*n}},e.v2sqrMagnitude=function(t){return t.x*t.x+t.y*t.y},e.v2fromRadial=function(t,e){return{x:t*Math.cos(e),y:t*Math.sin(e)}},e.circleLineIntersect=function(t,n,r,o){var i=e.v2sub(r,t);if(e.v2sqrMagnitude(i)<o*o)return!0;if(e.v2sqrMagnitude(e.v2sub(r,n))<o*o)return!0;var a=Math.atan2(n.y-t.y,n.x-t.x),u=e.v2rotate(i,-a);if(Math.abs(u.y)>o)return!1;var c=u.x*u.x;return!(c<0)&&!(c>e.v2sqrMagnitude(e.v2sub(n,t)))}},function(t,e,n){var r=n(1),o=n(0),i=r(o,"Map");t.exports=i},function(t,e,n){var r=n(0),o=r.Symbol;t.exports=o},function(t,e,n){function r(t){return a(t)?o(t):i(t)}var o=n(30),i=n(86),a=n(34);t.exports=r},function(t,e){var n=Array.isArray;t.exports=n},function(t,e){t.exports=function(t){return t.webpackPolyfill||(t.deprecate=function(){},t.paths=[],t.children||(t.children=[]),Object.defineProperty(t,"loaded",{enumerable:!0,get:function(){return t.l}}),Object.defineProperty(t,"id",{enumerable:!0,get:function(){return t.i}}),t.webpackPolyfill=1),t}},function(t,e){function n(t){return function(e){return t(e)}}t.exports=n},function(t,e,n){(function(t){var r=n(26),o="object"==typeof e&&e&&!e.nodeType&&e,i=o&&"object"==typeof t&&t&&!t.nodeType&&t,a=i&&i.exports===o,u=a&&r.process,c=function(){try{return u&&u.binding&&u.binding("util")}catch(t){}}();t.exports=c}).call(e,n(16)(t))},function(t,e){function n(t){var e=t&&t.constructor;return t===("function"==typeof e&&e.prototype||r)}var r=Object.prototype;t.exports=n},function(t,e,n){var r=n(94),o=n(36),i=Object.prototype,a=i.propertyIsEnumerable,u=Object.getOwnPropertySymbols,c=u?function(t){return null==t?[]:(t=Object(t),r(u(t),function(e){return a.call(t,e)}))}:o;t.exports=c},function(t,e,n){var r=n(98),o=n(12),i=n(99),a=n(100),u=n(101),c=n(6),s=n(27),f=s(r),p=s(o),l=s(i),v=s(a),h=s(u),y=c;(r&&"[object DataView]"!=y(new r(new ArrayBuffer(1)))||o&&"[object Map]"!=y(new o)||i&&"[object Promise]"!=y(i.resolve())||a&&"[object Set]"!=y(new a)||u&&"[object WeakMap]"!=y(new u))&&(y=function(t){var e=c(t),n="[object Object]"==e?t.constructor:void 0,r=n?s(n):"";if(r)switch(r){case f:return"[object DataView]";case p:return"[object Map]";case l:return"[object Promise]";case v:return"[object Set]";case h:return"[object WeakMap]"}return e}),t.exports=y},function(t,e,n){function r(t){var e=new t.constructor(t.byteLength);return new o(e).set(new o(t)),e}var o=n(104);t.exports=r},function(t,e,n){function r(t){return o(t,i|a)}var o=n(44),i=1,a=4;t.exports=r},function(t,e){function n(t,e){return t===e||t!==t&&e!==e}t.exports=n},function(t,e,n){function r(t){if(!i(t))return!1;var e=o(t);return e==u||e==c||e==a||e==s}var o=n(6),i=n(2),a="[object AsyncFunction]",u="[object Function]",c="[object GeneratorFunction]",s="[object Proxy]";t.exports=r},function(t,e,n){(function(e){var n="object"==typeof e&&e&&e.Object===Object&&e;t.exports=n}).call(e,n(57))},function(t,e){function n(t){if(null!=t){try{return o.call(t)}catch(t){}try{return t+""}catch(t){}}return""}var r=Function.prototype,o=r.toString;t.exports=n},function(t,e,n){function r(t,e,n){var r=t[e];u.call(t,e)&&i(r,n)&&(void 0!==n||e in t)||o(t,e,n)}var o=n(29),i=n(24),a=Object.prototype,u=a.hasOwnProperty;t.exports=r},function(t,e,n){function r(t,e,n){"__proto__"==e&&o?o(t,e,{configurable:!0,enumerable:!0,value:n,writable:!0}):t[e]=n}var o=n(77);t.exports=r},function(t,e,n){function r(t,e){var n=a(t),r=!n&&i(t),f=!n&&!r&&u(t),l=!n&&!r&&!f&&s(t),v=n||r||f||l,h=v?o(t.length,String):[],y=h.length;for(var d in t)!e&&!p.call(t,d)||v&&("length"==d||f&&("offset"==d||"parent"==d)||l&&("buffer"==d||"byteLength"==d||"byteOffset"==d)||c(d,y))||h.push(d);return h}var o=n(79),i=n(80),a=n(15),u=n(31),c=n(83),s=n(84),f=Object.prototype,p=f.hasOwnProperty;t.exports=r},function(t,e,n){(function(t){var r=n(0),o=n(82),i="object"==typeof e&&e&&!e.nodeType&&e,a=i&&"object"==typeof t&&t&&!t.nodeType&&t,u=a&&a.exports===i,c=u?r.Buffer:void 0,s=c?c.isBuffer:void 0,f=s||o;t.exports=f}).call(e,n(16)(t))},function(t,e){function n(t){return"number"==typeof t&&t>-1&&t%1==0&&t<=r}var r=9007199254740991;t.exports=n},function(t,e){function n(t,e){return function(n){return t(e(n))}}t.exports=n},function(t,e,n){function r(t){return null!=t&&i(t.length)&&!o(t)}var o=n(25),i=n(32);t.exports=r},function(t,e,n){function r(t){return a(t)?o(t,!0):i(t)}var o=n(30),i=n(89),a=n(34);t.exports=r},function(t,e){function n(){return[]}t.exports=n},function(t,e,n){var r=n(38),o=n(39),i=n(20),a=n(36),u=Object.getOwnPropertySymbols,c=u?function(t){for(var e=[];t;)r(e,i(t)),t=o(t);return e}:a;t.exports=c},function(t,e){function n(t,e){for(var n=-1,r=e.length,o=t.length;++n<r;)t[o+n]=e[n];return t}t.exports=n},function(t,e,n){var r=n(33),o=r(Object.getPrototypeOf,Object);t.exports=o},function(t,e,n){function r(t,e,n){var r=e(t);return i(t)?r:o(r,n(t))}var o=n(38),i=n(15);t.exports=r},function(t,e,n){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var r,o=n(11);!function(t){t[t.Nothing=0]="Nothing",t[t.WASD=1]="WASD",t[t.Arrows=2]="Arrows"}(r=e.PlayerInputScheme||(e.PlayerInputScheme={}));var i={};document.onkeydown=function(t){return i[t.keyCode]=!0},document.onkeyup=function(t){return delete i[t.keyCode]};var a=function(t){return{up:!0===i[87],left:!0===i[65],right:!0===i[68],laser:!0===i[83],bullet:!0===i[81],uid:o.generateUID(),frame:t}},u=function(t){return{up:!0===i[38],left:!0===i[37],right:!0===i[39],laser:!0===i[40],bullet:!0===i[16],uid:o.generateUID(),frame:t}},c=function(t){return{up:!1,left:!1,right:!1,laser:!1,bullet:!1,uid:o.generateUID(),frame:t}};e.getLatestInputs=function(t,e){switch(t){case r.WASD:return a(e);case r.Arrows:return u(e)}return c(e)}},function(t,e,n){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var r=n(43),o=n(115),i=n(116),a=n(23),u=document.getElementById("addClientButton"),c=document.getElementById("setLatency"),s=document.getElementById("setLatencyVariance"),f=parseFloat(c.value),p=parseFloat(s.value);c.onkeyup=function(t){return f=parseFloat(t.target.value)},s.onkeyup=function(t){return p=parseFloat(t.target.value)};var l=[],v=new r.Server(i.createRenderer());l.push(setInterval(v.update.bind(v),50));var h=function(t){Math.random()<0||setTimeout(t,f+p*Math.random())};u.onclick=function(t){var e=new o.Client(i.createRenderer());v.addClient(e.playerUID,function(t){return h(function(){return e.receiveState(a(t))})}),e.bindServer(function(t,e){return h(function(){return v.receiveInput(t,a(e))})}),l.push(setInterval(e.update.bind(e),50))};var y=!1;document.getElementById("inspectRange").oninput=function(t){y||(l.forEach(function(t){return clearInterval(t)}),u.onclick=function(t){},u.parentElement.removeChild(u),y=!0),i.setRenderHistoryFrame(parseInt(t.target.value))}},function(t,e,n){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var r=n(10),o=n(41),i=function(t){var e={};for(var n in t)e[n]=t[n].length>1?t[n].splice(0,1)[0]:t[n][0];return e},a=function(){function t(t){this.inputBuffers={},this.clientCallbacks=[],this.stateHistory=[],this.renderer=t,this.stateHistory.push(r.newGameState())}return Object.defineProperty(t.prototype,"curState",{get:function(){return this.stateHistory[this.stateHistory.length-1]},enumerable:!0,configurable:!0}),t.prototype.addClient=function(t,e){this.curState.players[t]=r.newPlayerState(),this.clientCallbacks.push(e),this.inputBuffers[t]=[o.getLatestInputs(o.PlayerInputScheme.Nothing,this.curState.frameCount)]},t.prototype.receiveInput=function(t,e){this.inputBuffers[t].push(e)},t.prototype.update=function(){var t=r.stepGameState(i(this.inputBuffers),this.stateHistory);this.stateHistory.push(t),this.stateHistory.length>100&&this.stateHistory.splice(0,1),this.clientCallbacks.forEach(function(e){return e(t)}),this.renderer(t)},t}();e.Server=a},function(t,e,n){function r(t,e,n,R,E,L){var k,T=e&A,F=e&I,B=e&O;if(n&&(k=E?n(t,R,E,L):n(t)),void 0!==k)return k;if(!m(t))return t;var z=g(t);if(z){if(k=d(t),!T)return f(t,k)}else{var H=y(t),Y=H==M||H==U;if(_(t))return s(t,T);if(H==D||H==P||Y&&!E){if(k=F||Y?{}:x(t),!T)return F?l(t,c(k,t)):p(t,u(k,t))}else{if(!C[H])return E?t:{};k=b(t,H,T)}}L||(L=new o);var W=L.get(t);if(W)return W;if(L.set(t,k),S(t))return t.forEach(function(o){k.add(r(o,e,n,o,t,L))}),k;if(j(t))return t.forEach(function(o,i){k.set(i,r(o,e,n,i,t,L))}),k;var N=B?F?h:v:F?keysIn:w,G=z?void 0:N(t);return i(G||t,function(o,i){G&&(i=o,o=t[i]),a(k,i,r(o,e,n,i,t,L))}),k}var o=n(45),i=n(76),a=n(28),u=n(78),c=n(88),s=n(91),f=n(92),p=n(93),l=n(95),v=n(96),h=n(97),y=n(21),d=n(102),b=n(103),x=n(109),g=n(15),_=n(31),j=n(111),m=n(2),S=n(113),w=n(14),A=1,I=2,O=4,P="[object Arguments]",M="[object Function]",U="[object GeneratorFunction]",D="[object Object]",C={};C[P]=C["[object Array]"]=C["[object ArrayBuffer]"]=C["[object DataView]"]=C["[object Boolean]"]=C["[object Date]"]=C["[object Float32Array]"]=C["[object Float64Array]"]=C["[object Int8Array]"]=C["[object Int16Array]"]=C["[object Int32Array]"]=C["[object Map]"]=C["[object Number]"]=C[D]=C["[object RegExp]"]=C["[object Set]"]=C["[object String]"]=C["[object Symbol]"]=C["[object Uint8Array]"]=C["[object Uint8ClampedArray]"]=C["[object Uint16Array]"]=C["[object Uint32Array]"]=!0,C["[object Error]"]=C[M]=C["[object WeakMap]"]=!1,t.exports=r},function(t,e,n){function r(t){var e=this.__data__=new o(t);this.size=e.size}var o=n(4),i=n(51),a=n(52),u=n(53),c=n(54),s=n(55);r.prototype.clear=i,r.prototype.delete=a,r.prototype.get=u,r.prototype.has=c,r.prototype.set=s,t.exports=r},function(t,e){function n(){this.__data__=[],this.size=0}t.exports=n},function(t,e,n){function r(t){var e=this.__data__,n=o(e,t);return!(n<0)&&(n==e.length-1?e.pop():a.call(e,n,1),--this.size,!0)}var o=n(5),i=Array.prototype,a=i.splice;t.exports=r},function(t,e,n){function r(t){var e=this.__data__,n=o(e,t);return n<0?void 0:e[n][1]}var o=n(5);t.exports=r},function(t,e,n){function r(t){return o(this.__data__,t)>-1}var o=n(5);t.exports=r},function(t,e,n){function r(t,e){var n=this.__data__,r=o(n,t);return r<0?(++this.size,n.push([t,e])):n[r][1]=e,this}var o=n(5);t.exports=r},function(t,e,n){function r(){this.__data__=new o,this.size=0}var o=n(4);t.exports=r},function(t,e){function n(t){var e=this.__data__,n=e.delete(t);return this.size=e.size,n}t.exports=n},function(t,e){function n(t){return this.__data__.get(t)}t.exports=n},function(t,e){function n(t){return this.__data__.has(t)}t.exports=n},function(t,e,n){function r(t,e){var n=this.__data__;if(n instanceof o){var r=n.__data__;if(!i||r.length<u-1)return r.push([t,e]),this.size=++n.size,this;n=this.__data__=new a(r)}return n.set(t,e),this.size=n.size,this}var o=n(4),i=n(12),a=n(63),u=200;t.exports=r},function(t,e,n){function r(t){return!(!a(t)||i(t))&&(o(t)?h:s).test(u(t))}var o=n(25),i=n(60),a=n(2),u=n(27),c=/[\\^$.*+?()[\]{}|]/g,s=/^\[object .+?Constructor\]$/,f=Function.prototype,p=Object.prototype,l=f.toString,v=p.hasOwnProperty,h=RegExp("^"+l.call(v).replace(c,"\\$&").replace(/hasOwnProperty|(function).*?(?=\\\()| for .+?(?=\\\])/g,"$1.*?")+"$");t.exports=r},function(t,e){var n;n=function(){return this}();try{n=n||Function("return this")()||(0,eval)("this")}catch(t){"object"==typeof window&&(n=window)}t.exports=n},function(t,e,n){function r(t){var e=a.call(t,c),n=t[c];try{t[c]=void 0;var r=!0}catch(t){}var o=u.call(t);return r&&(e?t[c]=n:delete t[c]),o}var o=n(13),i=Object.prototype,a=i.hasOwnProperty,u=i.toString,c=o?o.toStringTag:void 0;t.exports=r},function(t,e){function n(t){return o.call(t)}var r=Object.prototype,o=r.toString;t.exports=n},function(t,e,n){function r(t){return!!i&&i in t}var o=n(61),i=function(){var t=/[^.]+$/.exec(o&&o.keys&&o.keys.IE_PROTO||"");return t?"Symbol(src)_1."+t:""}();t.exports=r},function(t,e,n){var r=n(0),o=r["__core-js_shared__"];t.exports=o},function(t,e){function n(t,e){return null==t?void 0:t[e]}t.exports=n},function(t,e,n){function r(t){var e=-1,n=null==t?0:t.length;for(this.clear();++e<n;){var r=t[e];this.set(r[0],r[1])}}var o=n(64),i=n(71),a=n(73),u=n(74),c=n(75);r.prototype.clear=o,r.prototype.delete=i,r.prototype.get=a,r.prototype.has=u,r.prototype.set=c,t.exports=r},function(t,e,n){function r(){this.size=0,this.__data__={hash:new o,map:new(a||i),string:new o}}var o=n(65),i=n(4),a=n(12);t.exports=r},function(t,e,n){function r(t){var e=-1,n=null==t?0:t.length;for(this.clear();++e<n;){var r=t[e];this.set(r[0],r[1])}}var o=n(66),i=n(67),a=n(68),u=n(69),c=n(70);r.prototype.clear=o,r.prototype.delete=i,r.prototype.get=a,r.prototype.has=u,r.prototype.set=c,t.exports=r},function(t,e,n){function r(){this.__data__=o?o(null):{},this.size=0}var o=n(7);t.exports=r},function(t,e){function n(t){var e=this.has(t)&&delete this.__data__[t];return this.size-=e?1:0,e}t.exports=n},function(t,e,n){function r(t){var e=this.__data__;if(o){var n=e[t];return n===i?void 0:n}return u.call(e,t)?e[t]:void 0}var o=n(7),i="__lodash_hash_undefined__",a=Object.prototype,u=a.hasOwnProperty;t.exports=r},function(t,e,n){function r(t){var e=this.__data__;return o?void 0!==e[t]:a.call(e,t)}var o=n(7),i=Object.prototype,a=i.hasOwnProperty;t.exports=r},function(t,e,n){function r(t,e){var n=this.__data__;return this.size+=this.has(t)?0:1,n[t]=o&&void 0===e?i:e,this}var o=n(7),i="__lodash_hash_undefined__";t.exports=r},function(t,e,n){function r(t){var e=o(this,t).delete(t);return this.size-=e?1:0,e}var o=n(8);t.exports=r},function(t,e){function n(t){var e=typeof t;return"string"==e||"number"==e||"symbol"==e||"boolean"==e?"__proto__"!==t:null===t}t.exports=n},function(t,e,n){function r(t){return o(this,t).get(t)}var o=n(8);t.exports=r},function(t,e,n){function r(t){return o(this,t).has(t)}var o=n(8);t.exports=r},function(t,e,n){function r(t,e){var n=o(this,t),r=n.size;return n.set(t,e),this.size+=n.size==r?0:1,this}var o=n(8);t.exports=r},function(t,e){function n(t,e){for(var n=-1,r=null==t?0:t.length;++n<r&&!1!==e(t[n],n,t););return t}t.exports=n},function(t,e,n){var r=n(1),o=function(){try{var t=r(Object,"defineProperty");return t({},"",{}),t}catch(t){}}();t.exports=o},function(t,e,n){function r(t,e){return t&&o(e,i(e),t)}var o=n(9),i=n(14);t.exports=r},function(t,e){function n(t,e){for(var n=-1,r=Array(t);++n<t;)r[n]=e(n);return r}t.exports=n},function(t,e,n){var r=n(81),o=n(3),i=Object.prototype,a=i.hasOwnProperty,u=i.propertyIsEnumerable,c=r(function(){return arguments}())?r:function(t){return o(t)&&a.call(t,"callee")&&!u.call(t,"callee")};t.exports=c},function(t,e,n){function r(t){return i(t)&&o(t)==a}var o=n(6),i=n(3),a="[object Arguments]";t.exports=r},function(t,e){function n(){return!1}t.exports=n},function(t,e){function n(t,e){var n=typeof t;return!!(e=null==e?r:e)&&("number"==n||"symbol"!=n&&o.test(t))&&t>-1&&t%1==0&&t<e}var r=9007199254740991,o=/^(?:0|[1-9]\d*)$/;t.exports=n},function(t,e,n){var r=n(85),o=n(17),i=n(18),a=i&&i.isTypedArray,u=a?o(a):r;t.exports=u},function(t,e,n){function r(t){return a(t)&&i(t.length)&&!!u[o(t)]}var o=n(6),i=n(32),a=n(3),u={};u["[object Float32Array]"]=u["[object Float64Array]"]=u["[object Int8Array]"]=u["[object Int16Array]"]=u["[object Int32Array]"]=u["[object Uint8Array]"]=u["[object Uint8ClampedArray]"]=u["[object Uint16Array]"]=u["[object Uint32Array]"]=!0,u["[object Arguments]"]=u["[object Array]"]=u["[object ArrayBuffer]"]=u["[object Boolean]"]=u["[object DataView]"]=u["[object Date]"]=u["[object Error]"]=u["[object Function]"]=u["[object Map]"]=u["[object Number]"]=u["[object Object]"]=u["[object RegExp]"]=u["[object Set]"]=u["[object String]"]=u["[object WeakMap]"]=!1,t.exports=r},function(t,e,n){function r(t){if(!o(t))return i(t);var e=[];for(var n in Object(t))u.call(t,n)&&"constructor"!=n&&e.push(n);return e}var o=n(19),i=n(87),a=Object.prototype,u=a.hasOwnProperty;t.exports=r},function(t,e,n){var r=n(33),o=r(Object.keys,Object);t.exports=o},function(t,e,n){function r(t,e){return t&&o(e,i(e),t)}var o=n(9),i=n(35);t.exports=r},function(t,e,n){function r(t){if(!o(t))return a(t);var e=i(t),n=[];for(var r in t)("constructor"!=r||!e&&c.call(t,r))&&n.push(r);return n}var o=n(2),i=n(19),a=n(90),u=Object.prototype,c=u.hasOwnProperty;t.exports=r},function(t,e){function n(t){var e=[];if(null!=t)for(var n in Object(t))e.push(n);return e}t.exports=n},function(t,e,n){(function(t){function r(t,e){if(e)return t.slice();var n=t.length,r=s?s(n):new t.constructor(n);return t.copy(r),r}var o=n(0),i="object"==typeof e&&e&&!e.nodeType&&e,a=i&&"object"==typeof t&&t&&!t.nodeType&&t,u=a&&a.exports===i,c=u?o.Buffer:void 0,s=c?c.allocUnsafe:void 0;t.exports=r}).call(e,n(16)(t))},function(t,e){function n(t,e){var n=-1,r=t.length;for(e||(e=Array(r));++n<r;)e[n]=t[n];return e}t.exports=n},function(t,e,n){function r(t,e){return o(t,i(t),e)}var o=n(9),i=n(20);t.exports=r},function(t,e){function n(t,e){for(var n=-1,r=null==t?0:t.length,o=0,i=[];++n<r;){var a=t[n];e(a,n,t)&&(i[o++]=a)}return i}t.exports=n},function(t,e,n){function r(t,e){return o(t,i(t),e)}var o=n(9),i=n(37);t.exports=r},function(t,e,n){function r(t){return o(t,a,i)}var o=n(40),i=n(20),a=n(14);t.exports=r},function(t,e,n){function r(t){return o(t,a,i)}var o=n(40),i=n(37),a=n(35);t.exports=r},function(t,e,n){var r=n(1),o=n(0),i=r(o,"DataView");t.exports=i},function(t,e,n){var r=n(1),o=n(0),i=r(o,"Promise");t.exports=i},function(t,e,n){var r=n(1),o=n(0),i=r(o,"Set");t.exports=i},function(t,e,n){var r=n(1),o=n(0),i=r(o,"WeakMap");t.exports=i},function(t,e){function n(t){var e=t.length,n=new t.constructor(e);return e&&"string"==typeof t[0]&&o.call(t,"index")&&(n.index=t.index,n.input=t.input),n}var r=Object.prototype,o=r.hasOwnProperty;t.exports=n},function(t,e,n){function r(t,e,n){var r=t.constructor;switch(e){case b:return o(t);case s:case f:return new r(+t);case x:return i(t,n);case g:case _:case j:case m:case S:case w:case A:case I:case O:return c(t,n);case p:return new r;case l:case y:return new r(t);case v:return a(t);case h:return new r;case d:return u(t)}}var o=n(22),i=n(105),a=n(106),u=n(107),c=n(108),s="[object Boolean]",f="[object Date]",p="[object Map]",l="[object Number]",v="[object RegExp]",h="[object Set]",y="[object String]",d="[object Symbol]",b="[object ArrayBuffer]",x="[object DataView]",g="[object Float32Array]",_="[object Float64Array]",j="[object Int8Array]",m="[object Int16Array]",S="[object Int32Array]",w="[object Uint8Array]",A="[object Uint8ClampedArray]",I="[object Uint16Array]",O="[object Uint32Array]";t.exports=r},function(t,e,n){var r=n(0),o=r.Uint8Array;t.exports=o},function(t,e,n){function r(t,e){var n=e?o(t.buffer):t.buffer;return new t.constructor(n,t.byteOffset,t.byteLength)}var o=n(22);t.exports=r},function(t,e){function n(t){var e=new t.constructor(t.source,r.exec(t));return e.lastIndex=t.lastIndex,e}var r=/\w*$/;t.exports=n},function(t,e,n){function r(t){return a?Object(a.call(t)):{}}var o=n(13),i=o?o.prototype:void 0,a=i?i.valueOf:void 0;t.exports=r},function(t,e,n){function r(t,e){var n=e?o(t.buffer):t.buffer;return new t.constructor(n,t.byteOffset,t.length)}var o=n(22);t.exports=r},function(t,e,n){function r(t){return"function"!=typeof t.constructor||a(t)?{}:o(i(t))}var o=n(110),i=n(39),a=n(19);t.exports=r},function(t,e,n){var r=n(2),o=Object.create,i=function(){function t(){}return function(e){if(!r(e))return{};if(o)return o(e);t.prototype=e;var n=new t;return t.prototype=void 0,n}}();t.exports=i},function(t,e,n){var r=n(112),o=n(17),i=n(18),a=i&&i.isMap,u=a?o(a):r;t.exports=u},function(t,e,n){function r(t){return i(t)&&o(t)==a}var o=n(21),i=n(3),a="[object Map]";t.exports=r},function(t,e,n){var r=n(114),o=n(17),i=n(18),a=i&&i.isSet,u=a?o(a):r;t.exports=u},function(t,e,n){function r(t){return i(t)&&o(t)==a}var o=n(21),i=n(3),a="[object Set]";t.exports=r},function(t,e,n){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var r=n(10),o=n(41),i=n(11),a=0,u=function(){function t(t){this.inputStack=[],this.gameState=null,this.serverCallback=null,this.renderer=t,this.inputScheme=a++%2==0?o.PlayerInputScheme.Arrows:o.PlayerInputScheme.WASD,this._playerUID=i.generateUID()}return Object.defineProperty(t.prototype,"playerUID",{get:function(){return this._playerUID},enumerable:!0,configurable:!0}),t.prototype.bindServer=function(t){this.serverCallback=t},t.prototype.receiveState=function(t){if(null==this.gameState)return void(this.gameState=t);for(var e=this.inputStack.length;--e>=0&&t.players[this._playerUID].lastInputUID!==this.inputStack[e].uid;);if(e>=0){this.gameState=t;for(var n=e+1;n<this.inputStack.length;++n)this.gameState=r.predictGameState(this.inputStack[n],this._playerUID,this.gameState,!1)}},t.prototype.update=function(){if(null!==this.gameState){var t=o.getLatestInputs(this.inputScheme,this.gameState.frameCount);this.gameState=r.predictGameState(t,this._playerUID,this.gameState,!0),this.inputStack.push(t),this.inputStack.length>100&&this.inputStack.splice(0,1),null!==this.serverCallback&&this.serverCallback(this._playerUID,t),this.renderer(this.gameState)}},t}();e.Client=u},function(t,e,n){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var r=n(10),o=300*r.PLAYER_RADIUS,i=[],a=0,u=function(t,e){var n=1e3*Math.cos(e.angle),o=1e3*Math.sin(e.angle);t.lineWidth=2,t.beginPath(),t.moveTo(300*e.source.x,300*e.source.y),t.lineTo(300*e.source.x+n,300*e.source.y+o);var i=Math.round(15*e.timeLeft/r.LASER_TOTAL_TIME).toString(16);t.strokeStyle="#0"+i+"0",t.stroke()},c=function(t,e){t.lineWidth=2,t.strokeStyle="#99f",t.beginPath(),t.arc(300*e.position.x,300*e.position.y,2,0,2*Math.PI),t.stroke()},s=function(t,e){var n=o*Math.cos(e.rotation),i=o*Math.sin(e.rotation),a=o*Math.cos(e.rotation+150*Math.PI/180),u=o*Math.sin(e.rotation+150*Math.PI/180),c=o*Math.cos(e.rotation+210*Math.PI/180),s=o*Math.sin(e.rotation+210*Math.PI/180);t.lineWidth=2,t.strokeStyle=e.hitStatus===r.HitStatus.Predicted?"#fff":e.hitStatus===r.HitStatus.Confirmed?"#999":"#f99",t.beginPath(),t.moveTo(300*e.position.x+n,300*e.position.y+i),t.lineTo(300*e.position.x+a,300*e.position.y+u),t.moveTo(300*e.position.x+n,300*e.position.y+i),t.lineTo(300*e.position.x+c,300*e.position.y+s),t.stroke(),t.beginPath(),t.arc(300*e.position.x,300*e.position.y,o,0,2*Math.PI),t.stroke()},f=function(){var t=document.createElement("canvas");return t.width=300,t.height=300,document.getElementById("views").appendChild(t),t.getContext("2d")},p=function(t,e){t.clearRect(0,0,300,300);for(var n in e.players)s(t,e.players[n]);for(var r=0,o=e.lasers;r<o.length;r++){var i=o[r];u(t,i)}for(var a=0,f=e.bullets;a<f.length;a++){var p=f[a];c(t,p)}t.font="12px monospace",t.fillStyle="#fff",t.fillText(e.frameCount+" : "+e.predictedFrameCount,2,12)};e.createRenderer=function(){var t=f(),e=a++;return function(n){i.push({index:e,ctx:t,state:n}),i.length>300&&i.splice(0,1),p(t,n)}},e.setRenderHistoryFrame=function(t){var e=i.length-1+t;e<0&&(e=0),e>=i.length&&(e=i.length-1);var n=i[e];p(n.ctx,n.state),console.log(n.index,n.state)}}]);
//# sourceMappingURL=bundle.js.map
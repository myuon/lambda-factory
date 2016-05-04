var position = [0,0];
var area_size = 500;
var scroll_margin = 50;
var keys = {};
var stage;
var mychara;
var game;
var factories = [];

function initialize() {
  stage = new createjs.Stage("canvas");
  mychara = new createjs.Text("@", "14px sans-serif");
  mychara.textBaseline = "middle";
  mychara.x = 640/2;
  mychara.y = 400/2;

  stage.addChild(mychara);
  stage.update();

  createjs.Ticker.addEventListener("tick", tick);
  createjs.Ticker.setFPS(30);

  document.onkeydown = keydown;
  document.onkeyup = keyup;

  var pos = new createjs.Text(position, "14px sans-serif");
  pos.x = 0; pos.y = 0;
  stage.addChild(pos);

  game = Haste.initGame();

  var g = new createjs.Graphics();
  g.beginFill(createjs.Graphics.getRGB(255,0,0));
  g.drawRect(0,0,30,90);

  var s = new createjs.Shape(g);
  s.x = 100;
  s.y = 100;

  var s2 = new createjs.Shape(g);
  s2.x = 200;
  s2.y = 300;

  stage.addChild(s);
  stage.addChild(s2);
  stage.update();
}

function keydown(event) {
  keys[event.keyCode] = true;
}

function keyup(event) {
  delete keys[event.keyCode];
}

function tick() {
  var dv = 8;

  if (keys[37] && mychara.x > 0 && position[0] > -area_size) {
    if (mychara.x >= scroll_margin || position[0] < (-area_size + scroll_margin)) mychara.x -= dv;
    position[0] -= dv;
  }
  if (keys[38] && mychara.y > 0 && position[1] > -area_size) {
    if (mychara.y >= scroll_margin || position[1] < (-area_size + scroll_margin)) mychara.y -= dv;
    position[1] -= dv;
  }
  if (keys[39] && mychara.x < 640 && position[0] < area_size) {
    if (mychara.x <= 640 - scroll_margin || position[0] > (area_size - scroll_margin)) mychara.x += dv;
    position[0] += dv;
  }
  if (keys[40] && mychara.y < 400 && position[1] < area_size) {
    if (mychara.y <= 400 - scroll_margin || position[1] > (area_size - scroll_margin)) mychara.y += dv;
    position[1] += dv;
  }
  stage.update();

  if (keys[90]) {
    game = Haste.newMachine(position, game);
  }

  game = Haste.tick(game);
}
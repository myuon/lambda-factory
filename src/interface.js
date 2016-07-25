var position = [0,0];
const area_size = 600;
const box_size = 40;
var keys = {};
var stage;
var mychara;
var game;
var objs = {};
var graphic = {};
var board;

function initialize() {
  game = Haste.initGame();

  stage = new createjs.Stage("canvas");

  board = new createjs.Container();
  board.x = 640/2;
  board.y = 400/2;

  let t = new createjs.Graphics();
  t.beginFill(createjs.Graphics.getRGB(255,240,200));
  t.drawRect(0,0,box_size,box_size);
  t.beginFill(createjs.Graphics.getRGB(255,255,255));
  t.drawRect(1,1,box_size-2,box_size-2);

  let tile = new createjs.Shape(t);
  let num = area_size/box_size;

  for (let row = -num; row < num; row++) {
    for (let col = -num; col < num; col++) {
      let tileClone = tile.clone();
      tileClone.x = col * box_size;
      tileClone.y = row * box_size;

      board.addChild(tileClone);

      let v = (Haste.minemap(game,[col,row]) && Haste.minemap(game,[col,row])[1]) || 0;
      if (v != 0) {
        let tm = new createjs.Graphics();
        tm.beginFill(createjs.Graphics.getRGB(255-v,255-v,255-v));
        tm.drawRect(1,1,box_size-2,box_size-2);

        let skitile = new createjs.Shape(tm);
        skitile.x = col * box_size;
        skitile.y = row * box_size;

        board.addChild(skitile);
      }
    }
  }
  stage.addChild(board);

  mychara = new createjs.Text("@", "14px sans-serif");
  mychara.textBaseline = "middle";
  mychara.x = 640/2;
  mychara.y = 400/2;

  stage.addChild(mychara);

  createjs.Ticker.addEventListener("tick", tick);
  createjs.Ticker.setFPS(30);

  document.onkeydown = keydown;
  document.onkeyup = keyup;

  let pos = new createjs.Text(position, "14px sans-serif");
  pos.x = 0; pos.y = 0;
  stage.addChild(pos);

  let gf = new createjs.Graphics();
  gf.beginFill(createjs.Graphics.getRGB(255,200,200));
  gf.drawRect(5,5,box_size-10,box_size-10);
  graphic['Factory'] = gf

  let gp = new createjs.Graphics();
  gp.beginFill(createjs.Graphics.getRGB(120,255,200));
  gp.drawRect(5,5,box_size-10,box_size-10);
  graphic['Pipe'] = gp

  let gm = new createjs.Graphics();
  gm.beginFill(createjs.Graphics.getRGB(255,200,120));
  gm.drawRect(5,5,box_size-10,box_size-10);
  graphic['Miner'] = gm

  stage.update();
}

function keydown(event) {
  keys[event.keyCode] = true;
}

function keyup(event) {
  delete keys[event.keyCode];
}

function tick() {
  const dv = 8;

  if (keys[37] && mychara.x > 0) {
    if (position[0] < -(area_size - 600/2) || mychara.x > 600/2) {
      mychara.x -= dv;
    } else {
      position[0] -= dv;
      board.x += dv;
    }
  }
  if (keys[38] && mychara.y > 0) {
    if (position[1] < -(area_size - 400/2) || mychara.y > 400/2) {
      mychara.y -= dv;
    } else {
      position[1] -= dv;
      board.y += dv;
    }
  }
  if (keys[39] && mychara.x < 640) {
    if (position[0] > (area_size - 600/2) || mychara.x < 600/2) {
      mychara.x += dv;
    } else {
      position[0] += dv;
      board.x -= dv;
    }
  }
  if (keys[40] && mychara.y < 400) {
    if (position[1] > (area_size - 400/2) || mychara.y < 400/2) {
      mychara.y += dv;
    } else {
      position[1] += dv;
      board.y -= dv;
    }
  }

  const box_index = (function(){
    const step = (function(n) {
      return Math.floor(n / box_size);
    });

    let px = step(position[0] - 600/2 + mychara.x);
    let py = step(position[1] - 400/2 + mychara.y);
    return [px,py];
  });

  const setObj = (function(typ) {
    let p = box_index();
    if (!(p in objs)) {
      objs[p] = typ;
      let s = new createjs.Shape(graphic[typ]);
      s.x = p[0] * box_size;
      s.y = p[1] * box_size;
      board.addChild(s);
      game = Haste.newMachine(typ, p, game);
    }
  });

  if (keys[Haste.fromEnum('Z')]) { setObj('Factory'); }
  if (keys[Haste.fromEnum('X')]) { setObj('Pipe'); }
  if (keys[Haste.fromEnum('C')]) { setObj('Miner'); }

  {
    let p = box_index();
    if (p in objs && objs[p] != 'Pipe') {
      let popup = $('#obj-info')[0];
      $('#obj-info').css("visibility", "visible");
      $('#obj-info #info-in').html(Haste.inA(objs[p]));
      $('#obj-info #info-out').html(Haste.outA(objs[p]));

      if (Haste.connects(game,p)) {
        $('#obj-info #info-header').html('<i class="icon check"></i>' + objs[p]);
      } else {
        $('#obj-info #info-header').html(objs[p]);
      }
    } else {
      let popup = $('#obj-info')[0];
      $('#obj-info').css("visibility", "hidden");
    }
  }

  game = Haste.tick(game);
  stage.update();
}

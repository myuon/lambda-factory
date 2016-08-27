import * as createjs from "createjs";
import * as $ from "jquery";
import * as React from "react";
import * as ReactDOM from "react-dom";

declare var Haste;

const area_size = 600;
const box_size = 40;

function extend<T extends U, U>(itrf: T, part: U): T {
  return $.extend(itrf, part);
}

class TwoMap<K, T> {
  _array: Immutable.Map<K, Immutable.Map<K, T>> = Immutable.Map.of();

  has: (_: [K, K]) => boolean = (p) => {
    return (this._array.has(p[0]) && this._array.get(p[0]).has(p[1]));
  }

  at: (_: [K, K]) => T = (p) => {
    return this._array.get(p[0]).get(p[1]);
  }

  set: (_: [K, K], T) => void = (p, x) => {
    let arr = (this._array.has(p[0]) && this._array.get(p[0])) || Immutable.Map.of();
    this._array = this._array.set(p[0], arr.set(p[1], x));
  }
}

type MachineType = "Factory" | "Pipe" | "Miner"

class Game {
  board: createjs.Container;
  stage: createjs.Stage;
  mychara: createjs.Text;
  graphic: { [index: string]: createjs.Graphics } = {};
  position: [number, number] = [0,0];
  keys: boolean[] = [];
  objs: TwoMap<number, MachineType> = new TwoMap<number, MachineType>();
  haste: any = {};
  message: Message;

  constructor(message) {
    this.message = message;
    this.stage = new createjs.Stage("canvas");

    this.board = new createjs.Container();
    this.board.x = 640/2 - box_size/2;
    this.board.y = 400/2 - box_size/2;

    this.tiling();
    this.stage.addChild(this.board);

    this.mychara = new createjs.Text("@", "14px sans-serif");
    this.mychara.textBaseline = "middle";
    this.mychara.x = 640/2;
    this.mychara.y = 400/2;
    this.stage.addChild(this.mychara);

    document.onkeydown = (event) => {
      this.keys[event.keyCode] = true;
    };
    document.onkeyup = (event) => {
      delete this.keys[event.keyCode];
    };

    let pos = new createjs.Text(this.position.toString(), "14px sans-serif");
    pos.x = 0; pos.y = 0;
    this.stage.addChild(pos);

    this.mkGraphic("Factory", [255,200,200]);
    this.mkGraphic("Pipe", [120,255,200]);
    this.mkGraphic("Miner", [255,200,120]);

    this.stage.update();

    this.haste = Haste.initGame();
  }

  tiling = () => {
    let t: createjs.Graphics = new createjs.Graphics();
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

        this.board.addChild(tileClone);

        let v = 0;
        if (v != 0) {
          let tm = new createjs.Graphics();
          tm.beginFill(createjs.Graphics.getRGB(255-v,255-v,255-v));
          tm.drawRect(1,1,box_size-2,box_size-2);

          let skitile = new createjs.Shape(tm);
          skitile.x = col * box_size;
          skitile.y = row * box_size;

          this.board.addChild(skitile);
        }
      }
    }
  }

  mkGraphic = (mtyp: MachineType, color: [number, number, number]) => {
    let g = new createjs.Graphics();
    g.beginFill(createjs.Graphics.getRGB(color[0], color[1], color[2]));
    g.drawRect(5,5,box_size-10,box_size-10);
    this.graphic[mtyp] = g;
  }

  tick = () => {
    const dv = 8;

    if (this.keys[37] && this.mychara.x > 0) {
      if (this.position[0] < -(area_size - 600/2) || this.mychara.x > 600/2) {
        this.mychara.x -= dv;
      } else {
        this.position[0] -= dv;
        this.board.x += dv;
      }
    }
    if (this.keys[38] && this.mychara.y > 0) {
      if (this.position[1] < -(area_size - 400/2) || this.mychara.y > 400/2) {
        this.mychara.y -= dv;
      } else {
        this.position[1] -= dv;
        this.board.y += dv;
      }
    }
    if (this.keys[39] && this.mychara.x < 640) {
      if (this.position[0] > (area_size - 600/2) || this.mychara.x < 600/2) {
        this.mychara.x += dv;
      } else {
        this.position[0] += dv;
        this.board.x -= dv;
      }
    }
    if (this.keys[40] && this.mychara.y < 400) {
      if (this.position[1] > (area_size - 400/2) || this.mychara.y < 400/2) {
        this.mychara.y += dv;
      } else {
        this.position[1] += dv;
        this.board.y -= dv;
      }
    }

    let box_index: () => [number, number] = (() => {
      const step = (n) => Math.floor(n / box_size);

      let px = step(this.position[0] - 600/2 + this.mychara.x);
      let py = step(this.position[1] - 400/2 + this.mychara.y);
      return [px,py];
    });

    const setObj = ((typ: MachineType) => {
      let p = box_index();
      if (!this.objs.has(p)) {
        this.objs.set(p, typ);
        let s = new createjs.Shape(this.graphic[typ]);
        s.x = p[0] * box_size;
        s.y = p[1] * box_size;
        this.board.addChild(s);
        this.haste = Haste.newMachine(typ, p, this.haste);
      }
    });

    if (this.keys[Haste.fromEnum('Z')]) { setObj('Factory'); }
    if (this.keys[Haste.fromEnum('X')]) { setObj('Pipe'); }
    if (this.keys[Haste.fromEnum('C')]) { setObj('Miner'); }

    let p = box_index();

    if (this.objs.has(p)) {
      if (this.message.state.visibility != true) {
        this.message.setState((state, _) => extend(state, {
          visibility: true,
          mtype: this.objs.at(p),
          connected: Haste.connects(this.haste, p)
        }));
      }

      this.message.setState((state, _) => extend(state, {
        amount: Haste.getAmount(this.haste, p)
      }));
    } else {
      if (this.message.state.visibility == true) {
        this.message.setState((state, _) => extend(state, {
          visibility: false
        }));
      }
    }

    this.stage.update();
    this.haste = Haste.tick(this.haste);
  }

  run = () => {
    createjs.Ticker.addEventListener("tick", this.tick);
    createjs.Ticker.setFPS(30);
  }
}

interface IMessage {
  amount: number,
  visibility: boolean,
  mtype: MachineType,
  connected: boolean
};

class Message extends React.Component<{}, IMessage> {
  constructor() {
    super();
    this.state = {
      amount: 0,
      visibility: false,
      mtype: "Factory",
      connected: false
    }
  }

  render() {
    let mstyle = {visibility: this.state.visibility ? "visible" : "hidden"};

    return (
      <div id="status" className="ui compact message" style={mstyle}>
        <div className="header">
        {this.state.connected ? <i className="icon check"></i> : ""}{this.state.mtype}
        </div>
        <ul className="list">
          <li><strong>Amount</strong> {this.state.amount} [1/f]</li>
        </ul>
      </div>
    );
  }
}

let game;
let main = () => {
  let message = ReactDOM.render(
    <Message />,
    document.getElementById('message')
  );

  game = new Game(message);
  game.run();
}

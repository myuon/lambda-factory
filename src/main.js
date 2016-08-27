"use strict";
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var createjs = require("createjs");
var $ = require("jquery");
var React = require("react");
var ReactDOM = require("react-dom");
var area_size = 600;
var box_size = 40;
function extend(itrf, part) {
    return $.extend(itrf, part);
}
var TwoMap = (function () {
    function TwoMap() {
        var _this = this;
        this._array = Immutable.Map.of();
        this.has = function (p) {
            return (_this._array.has(p[0]) && _this._array.get(p[0]).has(p[1]));
        };
        this.at = function (p) {
            return _this._array.get(p[0]).get(p[1]);
        };
        this.set = function (p, x) {
            var arr = (_this._array.has(p[0]) && _this._array.get(p[0])) || Immutable.Map.of();
            _this._array = _this._array.set(p[0], arr.set(p[1], x));
        };
    }
    return TwoMap;
}());
var Game = (function () {
    function Game(message) {
        var _this = this;
        this.graphic = {};
        this.position = [0, 0];
        this.keys = [];
        this.objs = new TwoMap();
        this.haste = {};
        this.tiling = function () {
            var t = new createjs.Graphics();
            t.beginFill(createjs.Graphics.getRGB(255, 240, 200));
            t.drawRect(0, 0, box_size, box_size);
            t.beginFill(createjs.Graphics.getRGB(255, 255, 255));
            t.drawRect(1, 1, box_size - 2, box_size - 2);
            var tile = new createjs.Shape(t);
            var num = area_size / box_size;
            for (var row = -num; row < num; row++) {
                for (var col = -num; col < num; col++) {
                    var tileClone = tile.clone();
                    tileClone.x = col * box_size;
                    tileClone.y = row * box_size;
                    _this.board.addChild(tileClone);
                    var v = 0;
                    if (v != 0) {
                        var tm = new createjs.Graphics();
                        tm.beginFill(createjs.Graphics.getRGB(255 - v, 255 - v, 255 - v));
                        tm.drawRect(1, 1, box_size - 2, box_size - 2);
                        var skitile = new createjs.Shape(tm);
                        skitile.x = col * box_size;
                        skitile.y = row * box_size;
                        _this.board.addChild(skitile);
                    }
                }
            }
        };
        this.mkGraphic = function (mtyp, color) {
            var g = new createjs.Graphics();
            g.beginFill(createjs.Graphics.getRGB(color[0], color[1], color[2]));
            g.drawRect(5, 5, box_size - 10, box_size - 10);
            _this.graphic[mtyp] = g;
        };
        this.tick = function () {
            var dv = 8;
            if (_this.keys[37] && _this.mychara.x > 0) {
                if (_this.position[0] < -(area_size - 600 / 2) || _this.mychara.x > 600 / 2) {
                    _this.mychara.x -= dv;
                }
                else {
                    _this.position[0] -= dv;
                    _this.board.x += dv;
                }
            }
            if (_this.keys[38] && _this.mychara.y > 0) {
                if (_this.position[1] < -(area_size - 400 / 2) || _this.mychara.y > 400 / 2) {
                    _this.mychara.y -= dv;
                }
                else {
                    _this.position[1] -= dv;
                    _this.board.y += dv;
                }
            }
            if (_this.keys[39] && _this.mychara.x < 640) {
                if (_this.position[0] > (area_size - 600 / 2) || _this.mychara.x < 600 / 2) {
                    _this.mychara.x += dv;
                }
                else {
                    _this.position[0] += dv;
                    _this.board.x -= dv;
                }
            }
            if (_this.keys[40] && _this.mychara.y < 400) {
                if (_this.position[1] > (area_size - 400 / 2) || _this.mychara.y < 400 / 2) {
                    _this.mychara.y += dv;
                }
                else {
                    _this.position[1] += dv;
                    _this.board.y -= dv;
                }
            }
            var box_index = (function () {
                var step = function (n) { return Math.floor(n / box_size); };
                var px = step(_this.position[0] - 600 / 2 + _this.mychara.x);
                var py = step(_this.position[1] - 400 / 2 + _this.mychara.y);
                return [px, py];
            });
            var setObj = (function (typ) {
                var p = box_index();
                if (!_this.objs.has(p)) {
                    _this.objs.set(p, typ);
                    var s = new createjs.Shape(_this.graphic[typ]);
                    s.x = p[0] * box_size;
                    s.y = p[1] * box_size;
                    _this.board.addChild(s);
                    _this.haste = Haste.newMachine(typ, p, _this.haste);
                }
            });
            if (_this.keys[Haste.fromEnum('Z')]) {
                setObj('Factory');
            }
            if (_this.keys[Haste.fromEnum('X')]) {
                setObj('Pipe');
            }
            if (_this.keys[Haste.fromEnum('C')]) {
                setObj('Miner');
            }
            var p = box_index();
            if (_this.objs.has(p)) {
                if (_this.message.state.visibility != true) {
                    _this.message.setState(function (state, _) { return extend(state, {
                        visibility: true,
                        mtype: _this.objs.at(p),
                        connected: Haste.connects(_this.haste, p)
                    }); });
                }
                _this.message.setState(function (state, _) { return extend(state, {
                    amount: Haste.getAmount(_this.haste, p)
                }); });
            }
            else {
                if (_this.message.state.visibility == true) {
                    _this.message.setState(function (state, _) { return extend(state, {
                        visibility: false
                    }); });
                }
            }
            _this.stage.update();
            _this.haste = Haste.tick(_this.haste);
        };
        this.run = function () {
            createjs.Ticker.addEventListener("tick", _this.tick);
            createjs.Ticker.setFPS(30);
        };
        this.message = message;
        this.stage = new createjs.Stage("canvas");
        this.board = new createjs.Container();
        this.board.x = 640 / 2 - box_size / 2;
        this.board.y = 400 / 2 - box_size / 2;
        this.tiling();
        this.stage.addChild(this.board);
        this.mychara = new createjs.Text("@", "14px sans-serif");
        this.mychara.textBaseline = "middle";
        this.mychara.x = 640 / 2;
        this.mychara.y = 400 / 2;
        this.stage.addChild(this.mychara);
        document.onkeydown = function (event) {
            _this.keys[event.keyCode] = true;
        };
        document.onkeyup = function (event) {
            delete _this.keys[event.keyCode];
        };
        var pos = new createjs.Text(this.position.toString(), "14px sans-serif");
        pos.x = 0;
        pos.y = 0;
        this.stage.addChild(pos);
        this.mkGraphic("Factory", [255, 200, 200]);
        this.mkGraphic("Pipe", [120, 255, 200]);
        this.mkGraphic("Miner", [255, 200, 120]);
        this.stage.update();
        this.haste = Haste.initGame();
    }
    return Game;
}());
;
var Message = (function (_super) {
    __extends(Message, _super);
    function Message() {
        _super.call(this);
        this.state = {
            amount: 0,
            visibility: false,
            mtype: "Factory",
            connected: false
        };
    }
    Message.prototype.render = function () {
        var mstyle = { visibility: this.state.visibility ? "visible" : "hidden" };
        return (React.createElement("div", {id: "status", className: "ui compact message", style: mstyle}, 
            React.createElement("div", {className: "header"}, 
                this.state.connected ? React.createElement("i", {className: "icon check"}) : "", 
                this.state.mtype), 
            React.createElement("ul", {className: "list"}, 
                React.createElement("li", null, 
                    React.createElement("strong", null, "Amount"), 
                    " ", 
                    this.state.amount, 
                    " [1/f]")
            )));
    };
    return Message;
}(React.Component));
var game;
var main = function () {
    var message = ReactDOM.render(React.createElement(Message, null), document.getElementById('message'));
    game = new Game(message);
    game.run();
};

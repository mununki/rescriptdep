// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Utils = require("./utils.bs.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

function square(x) {
  return Utils.multiply(x, x);
}

function $$double(x) {
  return Utils.add(x, x);
}

function quadruple(x) {
  return Utils.U.quadruple(x);
}

function triple(x) {
  return Utils.multiply(x, 3);
}

function divide(x, y) {
  return Utils.divide(x, y);
}

var M = {
  triple: triple,
  divide: divide
};

function is_whitelisted(module_name) {
  try {
    return Belt_Array.some(Utils.whitelist, (function (name) {
                  return name === module_name;
                }));
  }
  catch (exn){
    return false;
  }
}

var v0 = f0({
      a: Utils.U.triple(0)
    });

console.log(v0);

exports.square = square;
exports.$$double = $$double;
exports.quadruple = quadruple;
exports.M = M;
exports.is_whitelisted = is_whitelisted;
exports.v0 = v0;
/* v0 Not a pure module */

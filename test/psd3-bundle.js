(() => {
  // output/Control.Bind/foreign.js
  var arrayBind = typeof Array.prototype.flatMap === "function" ? function(arr) {
    return function(f) {
      return arr.flatMap(f);
    };
  } : function(arr) {
    return function(f) {
      var result = [];
      var l = arr.length;
      for (var i = 0; i < l; i++) {
        var xs = f(arr[i]);
        var k = xs.length;
        for (var j = 0; j < k; j++) {
          result.push(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map29 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map29($$const(identity2))(a))(b);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure1 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure1(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure1 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure1(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply14 = apply(dictApplicative.Apply0());
    var pure1 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply14(pure1(f))(a);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eqNumber = {
    eq: eqNumberImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var notEq = function(dictEq) {
    var eq32 = eq(dictEq);
    return function(x) {
      return function(y) {
        return eq2(eq32(x)(y))(false);
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq4) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq4 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ (function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  })();
  var GT = /* @__PURE__ */ (function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  })();
  var EQ = /* @__PURE__ */ (function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  })();
  var eqOrdering = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof LT && v1 instanceof LT) {
          return true;
        }
        ;
        if (v instanceof GT && v1 instanceof GT) {
          return true;
        }
        ;
        if (v instanceof EQ && v1 instanceof EQ) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };
  var add = function(dict) {
    return dict.add;
  };

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ (function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  })();
  var ordNumber = /* @__PURE__ */ (function() {
    return {
      compare: ordNumberImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqNumber;
      }
    };
  })();
  var ordInt = /* @__PURE__ */ (function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  })();
  var compare = function(dict) {
    return dict.compare;
  };
  var comparing = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(f) {
      return function(x) {
        return function(y) {
          return compare3(f(x))(f(y));
        };
      };
    };
  };
  var min = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return x;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return y;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [v.constructor.name]);
      };
    };
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i + 1;
        var empty4 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty4;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ (function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  })();
  var Just = /* @__PURE__ */ (function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  })();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a) {
    return maybe(a)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };
  var forE = function(lo) {
    return function(hi) {
      return function(f) {
        return function() {
          for (var i = lo; i < hi; i++) {
            f(i)();
          }
        };
      };
    };
  };
  var foreachE = function(as) {
    return function(f) {
      return function() {
        for (var i = 0, l = as.length; i < l; i++) {
          f(as[i])();
        }
      };
    };
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind4 = bind(dictMonad.Bind1());
    var pure12 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind4(f)(function(f$prime) {
          return bind4(a)(function(a$prime) {
            return pure12(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Aff/foreign.js
  var Aff = (function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error3) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error3) {
        setTimeout(function() {
          throw error3;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error3) {
        return left(error3);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error3) {
        k(left(error3))();
        return nonCanceler2;
      }
    }
    var Scheduler = (function() {
      var limit = 1024;
      var size5 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size5 !== 0) {
          size5--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i, tmp;
          if (size5 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size5) % limit] = cb;
          size5++;
          if (!draining) {
            drain();
          }
        }
      };
    })();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error3) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step2 = aff;
      var fail = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step2 = bhead(step2);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail = util.left(e);
                step2 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step2)) {
                status = RETURN;
                fail = step2;
                step2 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step2 = util.fromRight(step2);
              }
              break;
            case CONTINUE:
              switch (step2.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step2._2;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step2 = util.right(step2._1);
                  } else {
                    status = STEP_BIND;
                    step2 = step2._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step2 = runSync(util.left, util.right, step2._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step2 = runAsync(util.left, step2._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step2 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail = util.left(step2._1);
                  step2 = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step2._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step2._1) {
                    tmp.run();
                  }
                  step2 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step2 = sequential2(util, supervisor, step2._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step2 = interrupt || fail || step2;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail) {
                      status = CONTINUE;
                      step2 = attempt._2(util.fromLeft(fail));
                      fail = null;
                    }
                    break;
                  // We cannot resume from an unmasked interrupt or exception.
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step2 = util.fromRight(step2);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
                  case BRACKET:
                    bracketCount--;
                    if (fail === null) {
                      result = util.fromRight(step2);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step2 = attempt._3(result);
                      }
                    }
                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step2 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail) {
                      step2 = attempt._1.failed(util.fromLeft(fail))(attempt._2);
                    } else {
                      step2 = attempt._1.completed(util.fromRight(step2))(attempt._2);
                    }
                    fail = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail), attempts, interrupt);
                    status = CONTINUE;
                    step2 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step2 = attempt._1;
                    fail = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step2));
                }
              }
              joins = null;
              if (interrupt && fail) {
                setTimeout(function() {
                  throw util.fromLeft(fail);
                }, 0);
              } else if (util.isLeft(step2) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step2);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step2)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill(error3, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error3);
              status = COMPLETED;
              step2 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error3);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step2(error3)), attempts, interrupt);
                }
                status = RETURN;
                step2 = null;
                fail = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error3);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step2 = null;
                fail = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill(error3, par2, cb2) {
        var step2 = par2;
        var head2 = null;
        var tail = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop: while (true) {
          tmp = null;
          switch (step2.tag) {
            case FORKED:
              if (step2._3 === EMPTY) {
                tmp = fibers[step2._1];
                kills2[count++] = tmp.kill(error3, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head2 === null) {
                break loop;
              }
              step2 = head2._2;
              if (tail === null) {
                head2 = null;
              } else {
                head2 = tail._1;
                tail = tail._2;
              }
              break;
            case MAP:
              step2 = step2._2;
              break;
            case APPLY:
            case ALT:
              if (head2) {
                tail = new Aff2(CONS, head2, tail);
              }
              head2 = step2;
              step2 = step2._1;
              break;
          }
        }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head2, tail) {
        var fail, step2, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail = result;
          step2 = null;
        } else {
          step2 = result;
          fail = null;
        }
        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head2 === null) {
            cb(fail || step2)();
            return;
          }
          if (head2._3 !== EMPTY) {
            return;
          }
          switch (head2.tag) {
            case MAP:
              if (fail === null) {
                head2._3 = util.right(head2._1(util.fromRight(step2)));
                step2 = head2._3;
              } else {
                head2._3 = fail;
              }
              break;
            case APPLY:
              lhs = head2._1._3;
              rhs = head2._2._3;
              if (fail) {
                head2._3 = fail;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail === lhs ? head2._2 : head2._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join3(fail, null, null);
                    } else {
                      join3(fail, tail._1, tail._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                return;
              } else {
                step2 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head2._3 = step2;
              }
              break;
            case ALT:
              lhs = head2._1._3;
              rhs = head2._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail = step2 === lhs ? rhs : lhs;
                step2 = null;
                head2._3 = fail;
              } else {
                head2._3 = step2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, step2 === lhs ? head2._2 : head2._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join3(step2, null, null);
                    } else {
                      join3(step2, tail._1, tail._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              }
              break;
          }
          if (tail === null) {
            head2 = null;
          } else {
            head2 = tail._1;
            tail = tail._2;
          }
        }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step2 = par;
        var head2 = null;
        var tail = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step2.tag) {
                case MAP:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(MAP, step2._1, EMPTY, EMPTY);
                  step2 = step2._2;
                  break;
                case APPLY:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(APPLY, EMPTY, step2._2, EMPTY);
                  step2 = step2._1;
                  break;
                case ALT:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(ALT, EMPTY, step2._2, EMPTY);
                  step2 = step2._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step2;
                  step2 = new Aff2(FORKED, fid, new Aff2(CONS, head2, tail), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step2)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head2 === null) {
                break loop;
              }
              if (head2._1 === EMPTY) {
                head2._1 = step2;
                status = CONTINUE;
                step2 = head2._2;
                head2._2 = EMPTY;
              } else {
                head2._2 = step2;
                step2 = head2;
                if (tail === null) {
                  head2 = null;
                } else {
                  head2 = tail._1;
                  tail = tail._2;
                }
              }
          }
        }
        root = step2;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error3, cb2) {
        interrupt = util.left(error3);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill(error3, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential2(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  })();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value12) {
          return Aff.Pure(f(value12));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  var _liftEffect = Aff.Sync;
  var makeAff = Aff.Async;
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _sequential = Aff.Seq;

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ (function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  })();
  var Right = /* @__PURE__ */ (function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  })();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var map3 = /* @__PURE__ */ map(functorEither);
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map3(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a) {
      return function(f) {
        return f(a);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ (function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  })();

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref) {
      return function() {
        ref.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_ = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a) {
      return function() {
        return f(a());
      };
    };
  };
  var pure_ = function(a) {
    return function() {
      return a;
    };
  };
  var bind_ = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i = 0, l = as.length; i < l; i++) {
          f(as[i])();
        }
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy2 = function(name15, moduleName, init) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init();
      state3 = 2;
      return val;
    };
  };
  var functorST = {
    map: map_
  };
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy2("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ (function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  })();
  var snd = function(v) {
    return v.value1;
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    var eq4 = eq(dictEq);
    return function(dictEq1) {
      var eq12 = eq(dictEq1);
      return {
        eq: function(x) {
          return function(y) {
            return eq4(x.value0)(y.value0) && eq12(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare2 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x) {
          return function(y) {
            var v = compare2(x.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x.value1)(y.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };

  // output/Effect.Class/index.js
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init) {
      return function(xs) {
        var acc = init;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init) {
      return function(xs) {
        var acc = init;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure12 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr2 = foldr(dictFoldable);
      return function(f) {
        return foldr2(function($454) {
          return applySecond2(f($454));
        })(pure12(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_1(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty2;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr2 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append5 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr2(function(x) {
          return function(acc) {
            return append5(f(x))(acc);
          };
        })(mempty2);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = /* @__PURE__ */ (function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply14) {
      return function(map29) {
        return function(pure12) {
          return function(f) {
            return function(array) {
              function go2(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure12([]);
                  case 1:
                    return map29(array1)(f(array[bot]));
                  case 2:
                    return apply14(map29(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply14(apply14(map29(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply14(map29(concat2)(go2(bot, pivot)))(go2(pivot, top2));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  })();

  // output/Data.Traversable/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity4);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };
  var $$for = function(dictApplicative) {
    return function(dictTraversable) {
      var traverse22 = traverse(dictTraversable)(dictApplicative);
      return function(x) {
        return function(f) {
          return traverse22(f)(x);
        };
      };
    };
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy3 = function(name15, moduleName, init) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init();
      state3 = 2;
      return val;
    };
  };
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var functorAff = {
    map: _map
  };
  var ffiUtil = /* @__PURE__ */ (function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  })();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var launchAff_ = function($75) {
    return $$void3(launchAff($75));
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy3("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var pure2 = /* @__PURE__ */ pure(applicativeAff);
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure2(unit));

  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Hylograph.Element.Types/index.js
  var SVGContext = /* @__PURE__ */ (function() {
    function SVGContext2() {
    }
    ;
    SVGContext2.value = new SVGContext2();
    return SVGContext2;
  })();
  var HTMLContext = /* @__PURE__ */ (function() {
    function HTMLContext2() {
    }
    ;
    HTMLContext2.value = new HTMLContext2();
    return HTMLContext2;
  })();
  var Circle = /* @__PURE__ */ (function() {
    function Circle4() {
    }
    ;
    Circle4.value = new Circle4();
    return Circle4;
  })();
  var Rect = /* @__PURE__ */ (function() {
    function Rect2() {
    }
    ;
    Rect2.value = new Rect2();
    return Rect2;
  })();
  var Path = /* @__PURE__ */ (function() {
    function Path2() {
    }
    ;
    Path2.value = new Path2();
    return Path2;
  })();
  var Line = /* @__PURE__ */ (function() {
    function Line2() {
    }
    ;
    Line2.value = new Line2();
    return Line2;
  })();
  var Polygon = /* @__PURE__ */ (function() {
    function Polygon3() {
    }
    ;
    Polygon3.value = new Polygon3();
    return Polygon3;
  })();
  var Text = /* @__PURE__ */ (function() {
    function Text2() {
    }
    ;
    Text2.value = new Text2();
    return Text2;
  })();
  var Group = /* @__PURE__ */ (function() {
    function Group2() {
    }
    ;
    Group2.value = new Group2();
    return Group2;
  })();
  var SVG = /* @__PURE__ */ (function() {
    function SVG2() {
    }
    ;
    SVG2.value = new SVG2();
    return SVG2;
  })();
  var Defs = /* @__PURE__ */ (function() {
    function Defs2() {
    }
    ;
    Defs2.value = new Defs2();
    return Defs2;
  })();
  var LinearGradient = /* @__PURE__ */ (function() {
    function LinearGradient2() {
    }
    ;
    LinearGradient2.value = new LinearGradient2();
    return LinearGradient2;
  })();
  var RadialGradient = /* @__PURE__ */ (function() {
    function RadialGradient2() {
    }
    ;
    RadialGradient2.value = new RadialGradient2();
    return RadialGradient2;
  })();
  var Stop = /* @__PURE__ */ (function() {
    function Stop2() {
    }
    ;
    Stop2.value = new Stop2();
    return Stop2;
  })();
  var PatternFill = /* @__PURE__ */ (function() {
    function PatternFill2() {
    }
    ;
    PatternFill2.value = new PatternFill2();
    return PatternFill2;
  })();
  var Title = /* @__PURE__ */ (function() {
    function Title2() {
    }
    ;
    Title2.value = new Title2();
    return Title2;
  })();
  var Div = /* @__PURE__ */ (function() {
    function Div2() {
    }
    ;
    Div2.value = new Div2();
    return Div2;
  })();
  var Span = /* @__PURE__ */ (function() {
    function Span2() {
    }
    ;
    Span2.value = new Span2();
    return Span2;
  })();
  var Table = /* @__PURE__ */ (function() {
    function Table2() {
    }
    ;
    Table2.value = new Table2();
    return Table2;
  })();
  var Tr = /* @__PURE__ */ (function() {
    function Tr2() {
    }
    ;
    Tr2.value = new Tr2();
    return Tr2;
  })();
  var Td = /* @__PURE__ */ (function() {
    function Td2() {
    }
    ;
    Td2.value = new Td2();
    return Td2;
  })();
  var Th = /* @__PURE__ */ (function() {
    function Th2() {
    }
    ;
    Th2.value = new Th2();
    return Th2;
  })();
  var Tbody = /* @__PURE__ */ (function() {
    function Tbody2() {
    }
    ;
    Tbody2.value = new Tbody2();
    return Tbody2;
  })();
  var Thead = /* @__PURE__ */ (function() {
    function Thead2() {
    }
    ;
    Thead2.value = new Thead2();
    return Thead2;
  })();
  var Code = /* @__PURE__ */ (function() {
    function Code2() {
    }
    ;
    Code2.value = new Code2();
    return Code2;
  })();
  var Var = /* @__PURE__ */ (function() {
    function Var2() {
    }
    ;
    Var2.value = new Var2();
    return Var2;
  })();
  var Dfn = /* @__PURE__ */ (function() {
    function Dfn2() {
    }
    ;
    Dfn2.value = new Dfn2();
    return Dfn2;
  })();
  var Dl = /* @__PURE__ */ (function() {
    function Dl2() {
    }
    ;
    Dl2.value = new Dl2();
    return Dl2;
  })();
  var Dt = /* @__PURE__ */ (function() {
    function Dt2() {
    }
    ;
    Dt2.value = new Dt2();
    return Dt2;
  })();
  var Dd = /* @__PURE__ */ (function() {
    function Dd2() {
    }
    ;
    Dd2.value = new Dd2();
    return Dd2;
  })();
  var Ol = /* @__PURE__ */ (function() {
    function Ol2() {
    }
    ;
    Ol2.value = new Ol2();
    return Ol2;
  })();
  var Ul = /* @__PURE__ */ (function() {
    function Ul2() {
    }
    ;
    Ul2.value = new Ul2();
    return Ul2;
  })();
  var Li = /* @__PURE__ */ (function() {
    function Li2() {
    }
    ;
    Li2.value = new Li2();
    return Li2;
  })();
  var Small = /* @__PURE__ */ (function() {
    function Small2() {
    }
    ;
    Small2.value = new Small2();
    return Small2;
  })();
  var Em = /* @__PURE__ */ (function() {
    function Em2() {
    }
    ;
    Em2.value = new Em2();
    return Em2;
  })();
  var Strong = /* @__PURE__ */ (function() {
    function Strong2() {
    }
    ;
    Strong2.value = new Strong2();
    return Strong2;
  })();
  var Anchor = /* @__PURE__ */ (function() {
    function Anchor2() {
    }
    ;
    Anchor2.value = new Anchor2();
    return Anchor2;
  })();
  var P = /* @__PURE__ */ (function() {
    function P2() {
    }
    ;
    P2.value = new P2();
    return P2;
  })();
  var Pre = /* @__PURE__ */ (function() {
    function Pre2() {
    }
    ;
    Pre2.value = new Pre2();
    return Pre2;
  })();
  var Section = /* @__PURE__ */ (function() {
    function Section2() {
    }
    ;
    Section2.value = new Section2();
    return Section2;
  })();
  var Mark = /* @__PURE__ */ (function() {
    function Mark2() {
    }
    ;
    Mark2.value = new Mark2();
    return Mark2;
  })();
  var Abbr = /* @__PURE__ */ (function() {
    function Abbr2() {
    }
    ;
    Abbr2.value = new Abbr2();
    return Abbr2;
  })();
  var elementContext = function(v) {
    if (v instanceof Circle) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Rect) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Path) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Line) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Polygon) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Text) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Group) {
      return SVGContext.value;
    }
    ;
    if (v instanceof SVG) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Defs) {
      return SVGContext.value;
    }
    ;
    if (v instanceof LinearGradient) {
      return SVGContext.value;
    }
    ;
    if (v instanceof RadialGradient) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Stop) {
      return SVGContext.value;
    }
    ;
    if (v instanceof PatternFill) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Title) {
      return SVGContext.value;
    }
    ;
    if (v instanceof Div) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Span) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Table) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Tr) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Td) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Th) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Tbody) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Thead) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Code) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Var) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Dfn) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Dl) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Dt) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Dd) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Ol) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Ul) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Li) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Small) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Em) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Strong) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Anchor) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof P) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Pre) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Section) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Mark) {
      return HTMLContext.value;
    }
    ;
    if (v instanceof Abbr) {
      return HTMLContext.value;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Element.Types (line 89, column 1 - line 89, column 47): " + [v.constructor.name]);
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value12) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value12);
  };
  var replicatePolyfill = function(count, value12) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value12;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = /* @__PURE__ */ (function() {
    function Cons2(head2, tail) {
      this.head = head2;
      this.tail = tail;
    }
    var emptyList = {};
    function curryCons(head2) {
      return function(tail) {
        return new Cons2(head2, tail);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr2, xs) {
      return listToArray(foldr2(curryCons)(emptyList)(xs));
    };
  })();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty4, next, xs) {
    return xs.length === 0 ? empty4({}) : next(xs[0])(xs.slice(1));
  };
  var indexImpl = function(just, nothing, xs, i) {
    return i < 0 || i >= xs.length ? nothing : just(xs[i]);
  };
  var findMapImpl = function(nothing, isJust2, f, xs) {
    for (var i = 0; i < xs.length; i++) {
      var result = f(xs[i]);
      if (isJust2(result)) return result;
    }
    return nothing;
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i = 0, l = xs.length; i < l; i++) {
      if (f(xs[i])) return just(i);
    }
    return nothing;
  };
  var concat = function(xss) {
    if (xss.length <= 1e4) {
      return Array.prototype.concat.apply([], xss);
    }
    var result = [];
    for (var i = 0, l = xss.length; i < l; i++) {
      var xs = xss[i];
      for (var j = 0, m = xs.length; j < m; j++) {
        result.push(xs[j]);
      }
    }
    return result;
  };
  var filterImpl = function(f, xs) {
    return xs.filter(f);
  };
  var sortByImpl = /* @__PURE__ */ (function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1) mergeFromTo(compare2, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1) mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2, fromOrdering, xs) {
      var out;
      if (xs.length < 2) return xs;
      out = xs.slice(0);
      mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
      return out;
    };
  })();
  var sliceImpl = function(s, e, l) {
    return l.slice(s, e);
  };
  var zipWithImpl = function(f, xs, ys) {
    var l = xs.length < ys.length ? xs.length : ys.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(xs[i])(ys[i]);
    }
    return result;
  };
  var anyImpl = function(p, xs) {
    var len = xs.length;
    for (var i = 0; i < len; i++) {
      if (p(xs[i])) return true;
    }
    return false;
  };
  var unsafeIndexImpl = function(xs, n) {
    return xs[n];
  };

  // output/Data.Array.ST/foreign.js
  var peekImpl = function(just, nothing, i, xs) {
    return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
  };
  var pokeImpl = function(i, a, xs) {
    var ret = i >= 0 && i < xs.length;
    if (ret) xs[i] = a;
    return ret;
  };
  function unsafeFreezeThawImpl(xs) {
    return xs;
  }
  var unsafeFreezeImpl = unsafeFreezeThawImpl;
  var unsafeThawImpl = unsafeFreezeThawImpl;
  function copyImpl(xs) {
    return xs.slice();
  }
  var thawImpl = copyImpl;
  var pushImpl = function(a, xs) {
    return xs.push(a);
  };

  // output/Control.Monad.ST.Uncurried/foreign.js
  var runSTFn1 = function runSTFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
  var runSTFn2 = function runSTFn22(fn) {
    return function(a) {
      return function(b) {
        return function() {
          return fn(a, b);
        };
      };
    };
  };
  var runSTFn3 = function runSTFn32(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function() {
            return fn(a, b, c);
          };
        };
      };
    };
  };
  var runSTFn4 = function runSTFn42(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function() {
              return fn(a, b, c, d);
            };
          };
        };
      };
    };
  };

  // output/Data.Array.ST/index.js
  var unsafeThaw = /* @__PURE__ */ runSTFn1(unsafeThawImpl);
  var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
  var thaw = /* @__PURE__ */ runSTFn1(thawImpl);
  var withArray = function(f) {
    return function(xs) {
      return function __do2() {
        var result = thaw(xs)();
        f(result)();
        return unsafeFreeze(result)();
      };
    };
  };
  var push = /* @__PURE__ */ runSTFn2(pushImpl);
  var poke = /* @__PURE__ */ runSTFn3(pokeImpl);
  var peek = /* @__PURE__ */ (function() {
    return runSTFn4(peekImpl)(Just.create)(Nothing.value);
  })();
  var modify2 = function(i) {
    return function(f) {
      return function(xs) {
        return function __do2() {
          var entry = peek(i)(xs)();
          if (entry instanceof Just) {
            return poke(i)(f(entry.value0))(xs)();
          }
          ;
          if (entry instanceof Nothing) {
            return false;
          }
          ;
          throw new Error("Failed pattern match at Data.Array.ST (line 234, column 3 - line 236, column 26): " + [entry.constructor.name]);
        };
      };
    };
  };

  // output/Data.Function.Uncurried/foreign.js
  var runFn2 = function(fn) {
    return function(a) {
      return function(b) {
        return fn(a, b);
      };
    };
  };
  var runFn3 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return fn(a, b, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(i)(xs[i]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.Array/index.js
  var $$void4 = /* @__PURE__ */ $$void(functorST);
  var map4 = /* @__PURE__ */ map(functorMaybe);
  var map1 = /* @__PURE__ */ map(functorArray);
  var map22 = /* @__PURE__ */ map(functorST);
  var fromJust2 = /* @__PURE__ */ fromJust();
  var when2 = /* @__PURE__ */ when(applicativeST);
  var notEq2 = /* @__PURE__ */ notEq(eqOrdering);
  var zipWith = /* @__PURE__ */ runFn3(zipWithImpl);
  var unsafeIndex = function() {
    return runFn2(unsafeIndexImpl);
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var uncons = /* @__PURE__ */ (function() {
    return runFn3(unconsImpl)($$const(Nothing.value))(function(x) {
      return function(xs) {
        return new Just({
          head: x,
          tail: xs
        });
      };
    });
  })();
  var sortBy = function(comp) {
    return runFn3(sortByImpl)(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 897, column 38 - line 900, column 11): " + [v.constructor.name]);
    });
  };
  var sortWith = function(dictOrd) {
    var comparing3 = comparing(dictOrd);
    return function(f) {
      return sortBy(comparing3(f));
    };
  };
  var sortWith1 = /* @__PURE__ */ sortWith(ordInt);
  var snoc = function(xs) {
    return function(x) {
      return withArray(push(x))(xs)();
    };
  };
  var slice = /* @__PURE__ */ runFn3(sliceImpl);
  var take = function(n) {
    return function(xs) {
      var $152 = n < 1;
      if ($152) {
        return [];
      }
      ;
      return slice(0)(n)(xs);
    };
  };
  var singleton2 = function(a) {
    return [a];
  };
  var replicate = /* @__PURE__ */ runFn2(replicateImpl);
  var $$null = function(xs) {
    return length(xs) === 0;
  };
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var index = /* @__PURE__ */ (function() {
    return runFn4(indexImpl)(Just.create)(Nothing.value);
  })();
  var last = function(xs) {
    return index(xs)(length(xs) - 1 | 0);
  };
  var head = function(xs) {
    return index(xs)(0);
  };
  var nubBy = function(comp) {
    return function(xs) {
      var indexedAndSorted = sortBy(function(x) {
        return function(y) {
          return comp(snd(x))(snd(y));
        };
      })(mapWithIndex2(Tuple.create)(xs));
      var v = head(indexedAndSorted);
      if (v instanceof Nothing) {
        return [];
      }
      ;
      if (v instanceof Just) {
        return map1(snd)(sortWith1(fst)((function __do2() {
          var result = unsafeThaw(singleton2(v.value0))();
          foreach(indexedAndSorted)(function(v1) {
            return function __do3() {
              var lst = map22(/* @__PURE__ */ (function() {
                var $183 = function($185) {
                  return fromJust2(last($185));
                };
                return function($184) {
                  return snd($183($184));
                };
              })())(unsafeFreeze(result))();
              return when2(notEq2(comp(lst)(v1.value1))(EQ.value))($$void4(push(v1)(result)))();
            };
          })();
          return unsafeFreeze(result)();
        })()));
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 1115, column 17 - line 1123, column 28): " + [v.constructor.name]);
    };
  };
  var nub = function(dictOrd) {
    return nubBy(compare(dictOrd));
  };
  var fromFoldable = function(dictFoldable) {
    return runFn2(fromFoldableImpl)(foldr(dictFoldable));
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var foldM = function(dictMonad) {
    var pure1 = pure(dictMonad.Applicative0());
    var bind13 = bind(dictMonad.Bind1());
    return function(f) {
      return function(b) {
        return runFn3(unconsImpl)(function(v) {
          return pure1(b);
        })(function(a) {
          return function(as) {
            return bind13(f(b)(a))(function(b$prime) {
              return foldM(dictMonad)(f)(b$prime)(as);
            });
          };
        });
      };
    };
  };
  var findMap = /* @__PURE__ */ (function() {
    return runFn4(findMapImpl)(Nothing.value)(isJust);
  })();
  var findIndex = /* @__PURE__ */ (function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  })();
  var find2 = function(f) {
    return function(xs) {
      return map4(unsafeIndex1(xs))(findIndex(f)(xs));
    };
  };
  var filter = /* @__PURE__ */ runFn2(filterImpl);
  var drop = function(n) {
    return function(xs) {
      var $173 = n < 1;
      if ($173) {
        return xs;
      }
      ;
      return slice(n)(length(xs))(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap((function() {
      var $189 = maybe([])(singleton2);
      return function($190) {
        return $189(f($190));
      };
    })());
  };
  var catMaybes = /* @__PURE__ */ mapMaybe(/* @__PURE__ */ identity(categoryFn));
  var any2 = /* @__PURE__ */ runFn2(anyImpl);

  // output/Data.Int/foreign.js
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var nan = NaN;
  var isNaNImpl = isNaN;
  var infinity = Infinity;
  var isFiniteImpl = isFinite;
  function fromStringImpl(str, isFinite2, just, nothing) {
    var num = parseFloat(str);
    if (isFinite2(num)) {
      return just(num);
    } else {
      return nothing;
    }
  }
  var abs = Math.abs;
  var cos = Math.cos;
  var floor = Math.floor;
  var pow = function(n) {
    return function(p) {
      return Math.pow(n, p);
    };
  };
  var remainder = function(n) {
    return function(m) {
      return n % m;
    };
  };
  var sin = Math.sin;
  var sqrt = Math.sqrt;

  // output/Data.Number/index.js
  var pi = 3.141592653589793;
  var fromString = function(str) {
    return fromStringImpl(str, isFiniteImpl, Just.create, Nothing.value);
  };

  // output/Data.FoldableWithIndex/index.js
  var foldr8 = /* @__PURE__ */ foldr(foldableArray);
  var mapWithIndex3 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var foldl8 = /* @__PURE__ */ foldl(foldableArray);
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldMapWithIndexDefaultR = function(dictFoldableWithIndex) {
    var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
    return function(dictMonoid) {
      var append5 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldrWithIndex1(function(i) {
          return function(x) {
            return function(acc) {
              return append5(f(i)(x))(acc);
            };
          };
        })(mempty2);
      };
    };
  };
  var foldableWithIndexArray = {
    foldrWithIndex: function(f) {
      return function(z) {
        var $291 = foldr8(function(v) {
          return function(y) {
            return f(v.value0)(v.value1)(y);
          };
        })(z);
        var $292 = mapWithIndex3(Tuple.create);
        return function($293) {
          return $291($292($293));
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        var $294 = foldl8(function(y) {
          return function(v) {
            return f(v.value0)(y)(v.value1);
          };
        })(z);
        var $295 = mapWithIndex3(Tuple.create);
        return function($296) {
          return $294($295($296));
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
    },
    Foldable0: function() {
      return foldableArray;
    }
  };

  // output/Data.TraversableWithIndex/index.js
  var traverseWithIndexDefault = function(dictTraversableWithIndex) {
    var sequence2 = sequence(dictTraversableWithIndex.Traversable2());
    var mapWithIndex4 = mapWithIndex(dictTraversableWithIndex.FunctorWithIndex0());
    return function(dictApplicative) {
      var sequence12 = sequence2(dictApplicative);
      return function(f) {
        var $174 = mapWithIndex4(f);
        return function($175) {
          return sequence12($174($175));
        };
      };
    };
  };
  var traverseWithIndex = function(dict) {
    return dict.traverseWithIndex;
  };
  var traversableWithIndexArray = {
    traverseWithIndex: function(dictApplicative) {
      return traverseWithIndexDefault(traversableWithIndexArray)(dictApplicative);
    },
    FunctorWithIndex0: function() {
      return functorWithIndexArray;
    },
    FoldableWithIndex1: function() {
      return foldableWithIndexArray;
    },
    Traversable2: function() {
      return traversableArray;
    }
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ (function() {
    function Nil2() {
    }
    ;
    Nil2.value = new Nil2();
    return Nil2;
  })();
  var Cons = /* @__PURE__ */ (function() {
    function Cons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons2.create = function(value0) {
      return function(value1) {
        return new Cons2(value0, value1);
      };
    };
    return Cons2;
  })();
  var foldableList = {
    foldr: function(f) {
      return function(b) {
        var rev3 = (function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        })();
        var $284 = foldl(foldableList)(flip(f))(b);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty2);
      };
    }
  };

  // output/Data.Map.Internal/index.js
  var $runtime_lazy4 = function(name15, moduleName, init) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init();
      state3 = 2;
      return val;
    };
  };
  var Leaf = /* @__PURE__ */ (function() {
    function Leaf3() {
    }
    ;
    Leaf3.value = new Leaf3();
    return Leaf3;
  })();
  var Node = /* @__PURE__ */ (function() {
    function Node2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new Node2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return Node2;
  })();
  var Split = /* @__PURE__ */ (function() {
    function Split2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Split2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Split2(value0, value1, value22);
        };
      };
    };
    return Split2;
  })();
  var SplitLast = /* @__PURE__ */ (function() {
    function SplitLast2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    SplitLast2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new SplitLast2(value0, value1, value22);
        };
      };
    };
    return SplitLast2;
  })();
  var unsafeNode = function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return new Node(1, 1, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 702, column 5 - line 706, column 39): " + [r.constructor.name]);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Leaf) {
        return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + (function() {
          var $280 = l.value0 > r.value0;
          if ($280) {
            return l.value0;
          }
          ;
          return r.value0;
        })() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 708, column 5 - line 712, column 68): " + [r.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 700, column 32 - line 712, column 68): " + [l.constructor.name]);
  };
  var singleton4 = function(k) {
    return function(v) {
      return new Node(1, 1, k, v, Leaf.value, Leaf.value);
    };
  };
  var unsafeBalancedNode = /* @__PURE__ */ (function() {
    var height8 = function(v) {
      if (v instanceof Leaf) {
        return 0;
      }
      ;
      if (v instanceof Node) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 757, column 12 - line 759, column 26): " + [v.constructor.name]);
    };
    var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
      if (rl instanceof Node && rl.value0 > height8(rr)) {
        return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
      }
      ;
      return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
    };
    var rotateRight = function(k, v, lk, lv, ll, lr, r) {
      if (lr instanceof Node && height8(ll) <= lr.value0) {
        return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
      }
      ;
      return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
    };
    return function(k, v, l, r) {
      if (l instanceof Leaf) {
        if (r instanceof Leaf) {
          return singleton4(k)(v);
        }
        ;
        if (r instanceof Node && r.value0 > 1) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      if (l instanceof Node) {
        if (r instanceof Node) {
          if (r.value0 > (l.value0 + 1 | 0)) {
            return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
          }
          ;
          if (l.value0 > (r.value0 + 1 | 0)) {
            return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
          }
          ;
        }
        ;
        if (r instanceof Leaf && l.value0 > 1) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 717, column 40 - line 738, column 34): " + [l.constructor.name]);
    };
  })();
  var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy4("unsafeSplit", "Data.Map.Internal", function() {
    return function(comp, k, m) {
      if (m instanceof Leaf) {
        return new Split(Nothing.value, Leaf.value, Leaf.value);
      }
      ;
      if (m instanceof Node) {
        var v = comp(k)(m.value2);
        if (v instanceof LT) {
          var v1 = $lazy_unsafeSplit(793)(comp, k, m.value4);
          return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
        }
        ;
        if (v instanceof GT) {
          var v1 = $lazy_unsafeSplit(796)(comp, k, m.value5);
          return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
        }
        ;
        if (v instanceof EQ) {
          return new Split(new Just(m.value3), m.value4, m.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 791, column 5 - line 799, column 30): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 787, column 34 - line 799, column 30): " + [m.constructor.name]);
    };
  });
  var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(786);
  var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy4("unsafeSplitLast", "Data.Map.Internal", function() {
    return function(k, v, l, r) {
      if (r instanceof Leaf) {
        return new SplitLast(k, v, l);
      }
      ;
      if (r instanceof Node) {
        var v1 = $lazy_unsafeSplitLast(779)(r.value2, r.value3, r.value4, r.value5);
        return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 776, column 37 - line 780, column 57): " + [r.constructor.name]);
    };
  });
  var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(775);
  var unsafeJoinNodes = function(v, v1) {
    if (v instanceof Leaf) {
      return v1;
    }
    ;
    if (v instanceof Node) {
      var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
      return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 764, column 25 - line 768, column 38): " + [v.constructor.name, v1.constructor.name]);
  };
  var $lazy_unsafeDifference = /* @__PURE__ */ $runtime_lazy4("unsafeDifference", "Data.Map.Internal", function() {
    return function(comp, l, r) {
      if (l instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (r instanceof Leaf) {
        return l;
      }
      ;
      if (r instanceof Node) {
        var v = unsafeSplit(comp, r.value2, l);
        var l$prime = $lazy_unsafeDifference(841)(comp, v.value1, r.value4);
        var r$prime = $lazy_unsafeDifference(842)(comp, v.value2, r.value5);
        return unsafeJoinNodes(l$prime, r$prime);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 836, column 39 - line 843, column 33): " + [l.constructor.name, r.constructor.name]);
    };
  });
  var unsafeDifference = /* @__PURE__ */ $lazy_unsafeDifference(835);
  var $lazy_unsafeIntersectionWith = /* @__PURE__ */ $runtime_lazy4("unsafeIntersectionWith", "Data.Map.Internal", function() {
    return function(comp, app, l, r) {
      if (l instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (r instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (r instanceof Node) {
        var v = unsafeSplit(comp, r.value2, l);
        var l$prime = $lazy_unsafeIntersectionWith(825)(comp, app, v.value1, r.value4);
        var r$prime = $lazy_unsafeIntersectionWith(826)(comp, app, v.value2, r.value5);
        if (v.value0 instanceof Just) {
          return unsafeBalancedNode(r.value2, app(v.value0.value0)(r.value3), l$prime, r$prime);
        }
        ;
        if (v.value0 instanceof Nothing) {
          return unsafeJoinNodes(l$prime, r$prime);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 827, column 5 - line 831, column 37): " + [v.value0.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 820, column 49 - line 831, column 37): " + [l.constructor.name, r.constructor.name]);
    };
  });
  var unsafeIntersectionWith = /* @__PURE__ */ $lazy_unsafeIntersectionWith(819);
  var $lazy_unsafeUnionWith = /* @__PURE__ */ $runtime_lazy4("unsafeUnionWith", "Data.Map.Internal", function() {
    return function(comp, app, l, r) {
      if (l instanceof Leaf) {
        return r;
      }
      ;
      if (r instanceof Leaf) {
        return l;
      }
      ;
      if (r instanceof Node) {
        var v = unsafeSplit(comp, r.value2, l);
        var l$prime = $lazy_unsafeUnionWith(809)(comp, app, v.value1, r.value4);
        var r$prime = $lazy_unsafeUnionWith(810)(comp, app, v.value2, r.value5);
        if (v.value0 instanceof Just) {
          return unsafeBalancedNode(r.value2, app(v.value0.value0)(r.value3), l$prime, r$prime);
        }
        ;
        if (v.value0 instanceof Nothing) {
          return unsafeBalancedNode(r.value2, r.value3, l$prime, r$prime);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 811, column 5 - line 815, column 46): " + [v.value0.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 804, column 42 - line 815, column 46): " + [l.constructor.name, r.constructor.name]);
    };
  });
  var unsafeUnionWith = /* @__PURE__ */ $lazy_unsafeUnionWith(803);
  var unionWith = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(app) {
      return function(m1) {
        return function(m2) {
          return unsafeUnionWith(compare2, app, m1, m2);
        };
      };
    };
  };
  var union = function(dictOrd) {
    return unionWith(dictOrd)($$const);
  };
  var member = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return false;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare2(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return true;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 7 - line 462, column 19): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 456, column 8 - line 462, column 19): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var lookup = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare2(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value3);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 283, column 7 - line 286, column 22): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 280, column 8 - line 286, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var intersectionWith = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(app) {
      return function(m1) {
        return function(m2) {
          return unsafeIntersectionWith(compare2, app, m1, m2);
        };
      };
    };
  };
  var intersection = function(dictOrd) {
    return intersectionWith(dictOrd)($$const);
  };
  var insertWith = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(app) {
      return function(k) {
        return function(v) {
          var go2 = function(v1) {
            if (v1 instanceof Leaf) {
              return singleton4(k)(v);
            }
            ;
            if (v1 instanceof Node) {
              var v2 = compare2(k)(v1.value2);
              if (v2 instanceof LT) {
                return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
              }
              ;
              if (v2 instanceof GT) {
                return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
              }
              ;
              if (v2 instanceof EQ) {
                return new Node(v1.value0, v1.value1, k, app(v1.value3)(v), v1.value4, v1.value5);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 486, column 7 - line 489, column 44): " + [v2.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 483, column 8 - line 489, column 44): " + [v1.constructor.name]);
          };
          return go2;
        };
      };
    };
  };
  var insert = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go2 = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton4(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare2(k)(v1.value2);
            if (v2 instanceof LT) {
              return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
            }
            ;
            if (v2 instanceof GT) {
              return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
            }
            ;
            if (v2 instanceof EQ) {
              return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 471, column 7 - line 474, column 35): " + [v2.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 468, column 8 - line 474, column 35): " + [v1.constructor.name]);
        };
        return go2;
      };
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy4("go", "Data.Map.Internal", function() {
          return function(m$prime, z$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(172)(m$prime.value4, f(m$prime.value3)($lazy_go(172)(m$prime.value5, z$prime)));
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 169, column 26 - line 172, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(169);
        return function(m) {
          return go2(m, z);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy4("go", "Data.Map.Internal", function() {
          return function(z$prime, m$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(178)(f($lazy_go(178)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 175, column 26 - line 178, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(175);
        return function(m) {
          return go2(z, m);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append1 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append1(go2(v.value4))(append1(f(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 181, column 10 - line 184, column 28): " + [v.constructor.name]);
        };
        return go2;
      };
    }
  };
  var values = /* @__PURE__ */ (function() {
    return foldr(foldableMap)(Cons.create)(Nil.value);
  })();
  var empty2 = /* @__PURE__ */ (function() {
    return Leaf.value;
  })();
  var fromFoldable2 = function(dictOrd) {
    var insert1 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert1(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };
  var fromFoldableWith = function(dictOrd) {
    var insertWith1 = insertWith(dictOrd);
    return function(dictFoldable) {
      var foldl5 = foldl(dictFoldable);
      return function(f) {
        var f$prime = insertWith1(flip(f));
        return foldl5(function(m) {
          return function(v) {
            return f$prime(v.value0)(v.value1)(m);
          };
        })(empty2);
      };
    };
  };
  var difference = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(m1) {
      return function(m2) {
        return unsafeDifference(compare2, m1, m2);
      };
    };
  };
  var alter = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare2, k, m);
          var v2 = f(v.value0);
          if (v2 instanceof Nothing) {
            return unsafeJoinNodes(v.value1, v.value2);
          }
          ;
          if (v2 instanceof Just) {
            return unsafeBalancedNode(k, v2.value0, v.value1, v.value2);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 514, column 3 - line 518, column 41): " + [v2.constructor.name]);
        };
      };
    };
  };

  // output/Data.List/index.js
  var eq3 = /* @__PURE__ */ eq(eqOrdering);
  var notEq3 = /* @__PURE__ */ notEq(eqOrdering);
  var singleton5 = function(a) {
    return new Cons(a, Nil.value);
  };
  var sortBy2 = function(cmp) {
    var merge = function(v) {
      return function(v1) {
        if (v instanceof Cons && v1 instanceof Cons) {
          if (eq3(cmp(v.value0)(v1.value0))(GT.value)) {
            return new Cons(v1.value0, merge(v)(v1.value1));
          }
          ;
          if (otherwise) {
            return new Cons(v.value0, merge(v.value1)(v1));
          }
          ;
        }
        ;
        if (v instanceof Nil) {
          return v1;
        }
        ;
        if (v1 instanceof Nil) {
          return v;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 466, column 3 - line 466, column 38): " + [v.constructor.name, v1.constructor.name]);
      };
    };
    var mergePairs = function(v) {
      if (v instanceof Cons && v.value1 instanceof Cons) {
        return new Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
      }
      ;
      return v;
    };
    var mergeAll = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Cons && v.value1 instanceof Nil) {
          $tco_done = true;
          return v.value0;
        }
        ;
        $copy_v = mergePairs(v);
        return;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    var sequences = function(v) {
      if (v instanceof Cons && v.value1 instanceof Cons) {
        if (eq3(cmp(v.value0)(v.value1.value0))(GT.value)) {
          return descending(v.value1.value0)(singleton5(v.value0))(v.value1.value1);
        }
        ;
        if (otherwise) {
          return ascending(v.value1.value0)(function(v1) {
            return new Cons(v.value0, v1);
          })(v.value1.value1);
        }
        ;
      }
      ;
      return singleton5(v);
    };
    var descending = function($copy_v) {
      return function($copy_v1) {
        return function($copy_v2) {
          var $tco_var_v = $copy_v;
          var $tco_var_v1 = $copy_v1;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(v, v1, v2) {
            if (v2 instanceof Cons && eq3(cmp(v)(v2.value0))(GT.value)) {
              $tco_var_v = v2.value0;
              $tco_var_v1 = new Cons(v, v1);
              $copy_v2 = v2.value1;
              return;
            }
            ;
            $tco_done1 = true;
            return new Cons(new Cons(v, v1), sequences(v2));
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
          }
          ;
          return $tco_result;
        };
      };
    };
    var ascending = function($copy_v) {
      return function($copy_v1) {
        return function($copy_v2) {
          var $tco_var_v = $copy_v;
          var $tco_var_v1 = $copy_v1;
          var $tco_done2 = false;
          var $tco_result;
          function $tco_loop(v, v1, v2) {
            if (v2 instanceof Cons && notEq3(cmp(v)(v2.value0))(GT.value)) {
              $tco_var_v = v2.value0;
              $tco_var_v1 = function(ys) {
                return v1(new Cons(v, ys));
              };
              $copy_v2 = v2.value1;
              return;
            }
            ;
            $tco_done2 = true;
            return new Cons(v1(singleton5(v)), sequences(v2));
          }
          ;
          while (!$tco_done2) {
            $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
          }
          ;
          return $tco_result;
        };
      };
    };
    return function($444) {
      return mergeAll(sequences($444));
    };
  };
  var reverse2 = /* @__PURE__ */ (function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  })();
  var fromFoldable3 = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
  };

  // output/Data.Set/index.js
  var coerce3 = /* @__PURE__ */ coerce();
  var member2 = function(dictOrd) {
    return coerce3(member(dictOrd));
  };
  var intersection2 = function(dictOrd) {
    return coerce3(intersection(dictOrd));
  };
  var insert2 = function(dictOrd) {
    var insert1 = insert(dictOrd);
    return function(a) {
      return function(v) {
        return insert1(a)(unit)(v);
      };
    };
  };
  var empty3 = empty2;
  var fromFoldable4 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      var insert1 = insert2(dictOrd);
      return foldl22(function(m) {
        return function(a) {
          return insert1(a)(m);
        };
      })(empty3);
    };
  };
  var difference2 = function(dictOrd) {
    return coerce3(difference(dictOrd));
  };

  // output/Hylograph.Forces.Pure.Node/foreign.js
  var writeVelocity = (node, vx, vy) => {
    node.vx = vx;
    node.vy = vy;
  };
  var writePosition = (node, x, y) => {
    node.x = x;
    node.y = y;
  };
  var writeIndex = (node, index3) => {
    node.index = index3;
  };

  // output/Hylograph.Forces.Pure.Center/index.js
  var initialize = function(config) {
    return function(nodes) {
      return {
        nodes,
        config
      };
    };
  };
  var apply2 = function(v) {
    return function __do2() {
      var sum2 = $$new({
        x: 0,
        y: 0
      })();
      foreachE(v.nodes)(function(node) {
        return function __do3() {
          var total2 = read(sum2)();
          return write({
            x: total2.x + node.x,
            y: total2.y + node.y
          })(sum2)();
        };
      })();
      var total = read(sum2)();
      var count = toNumber(length(v.nodes));
      var shiftX = (total.x / count - v.config.x) * v.config.strength;
      var shiftY = (total.y / count - v.config.y) * v.config.strength;
      return foreachE(v.nodes)(function(node) {
        return function() {
          return writePosition(node, node.x - shiftX, node.y - shiftY);
        };
      })();
    };
  };

  // output/Hylograph.Forces.Pure.Group/index.js
  var alter2 = /* @__PURE__ */ alter(ordInt);
  var fromFoldable5 = /* @__PURE__ */ fromFoldable(foldableList);
  var foldl3 = /* @__PURE__ */ foldl(foldableArray);
  var Ungrouped = /* @__PURE__ */ (function() {
    function Ungrouped2() {
    }
    ;
    Ungrouped2.value = new Ungrouped2();
    return Ungrouped2;
  })();
  var InGroup = /* @__PURE__ */ (function() {
    function InGroup2(value0) {
      this.value0 = value0;
    }
    ;
    InGroup2.create = function(value0) {
      return new InGroup2(value0);
    };
    return InGroup2;
  })();
  var ungrouped = /* @__PURE__ */ (function() {
    return Ungrouped.value;
  })();
  var inGroup = /* @__PURE__ */ (function() {
    return InGroup.create;
  })();
  var grouped = function(membership2) {
    return function(items) {
      var add3 = function(item) {
        return function(members) {
          if (members instanceof Nothing) {
            return new Just([item]);
          }
          ;
          if (members instanceof Just) {
            return new Just(snoc(members.value0)(item));
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.Group (line 39, column 22 - line 41, column 45): " + [members.constructor.name]);
        };
      };
      var place = function(groups) {
        return function(item) {
          var v = membership2(item);
          if (v instanceof Ungrouped) {
            return groups;
          }
          ;
          if (v instanceof InGroup) {
            return alter2(add3(item))(v.value0)(groups);
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.Group (line 35, column 23 - line 37, column 55): " + [v.constructor.name]);
        };
      };
      return fromFoldable5(values(foldl3(place)(empty2)(items)));
    };
  };

  // output/Hylograph.Forces.Pure.Cohesion/index.js
  var map5 = /* @__PURE__ */ map(functorArray);
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var initialize2 = function(config) {
    return function(nodes) {
      var member5 = function(node) {
        var strength = config.strength(node);
        return {
          node,
          strength: (function() {
            var $11 = isNaNImpl(strength);
            if ($11) {
              return 0;
            }
            ;
            return strength;
          })()
        };
      };
      return map5(map5(member5))(grouped(config.group)(nodes));
    };
  };
  var apply3 = function(v) {
    return function(alpha) {
      return foreachE(v)(function(members) {
        return function __do2() {
          var sum2 = $$new({
            x: 0,
            y: 0,
            count: 0
          })();
          foreachE(members)(function(member5) {
            return when3(isFiniteImpl(member5.node.x) && isFiniteImpl(member5.node.y))(function __do3() {
              var total2 = read(sum2)();
              return write({
                x: total2.x + member5.node.x,
                y: total2.y + member5.node.y,
                count: total2.count + 1
              })(sum2)();
            });
          })();
          var total = read(sum2)();
          var centroidY = total.y / total.count;
          var centroidX = total.x / total.count;
          return foreachE(members)(function(member5) {
            return when3(isFiniteImpl(member5.node.x) && isFiniteImpl(member5.node.y))(function() {
              return writeVelocity(member5.node, member5.node.vx + (centroidX - member5.node.x) * member5.strength * alpha, member5.node.vy + (centroidY - member5.node.y) * member5.strength * alpha);
            });
          })();
        };
      });
    };
  };

  // output/Hylograph.Forces.Pure.Quadtree/index.js
  var $runtime_lazy5 = function(name15, moduleName, init) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init();
      state3 = 2;
      return val;
    };
  };
  var Empty = /* @__PURE__ */ (function() {
    function Empty4() {
    }
    ;
    Empty4.value = new Empty4();
    return Empty4;
  })();
  var Leaf2 = /* @__PURE__ */ (function() {
    function Leaf3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Leaf3.create = function(value0) {
      return function(value1) {
        return new Leaf3(value0, value1);
      };
    };
    return Leaf3;
  })();
  var Branch = /* @__PURE__ */ (function() {
    function Branch2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Branch2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Branch2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Branch2;
  })();
  var unitSquare = function(x) {
    return function(y) {
      var y0 = floor(y);
      var x0 = floor(x);
      return {
        x0,
        y0,
        x1: x0 + 1,
        y1: y0 + 1
      };
    };
  };
  var leaf = function(newestFirst) {
    return function(first) {
      return function(rest) {
        if (rest instanceof Nil) {
          return new Leaf2(first, Nil.value);
        }
        ;
        if (newestFirst) {
          return new Leaf2(first, rest);
        }
        ;
        var v = reverse2(new Cons(first, rest));
        if (v instanceof Cons) {
          return new Leaf2(v.value0, v.value1);
        }
        ;
        if (v instanceof Nil) {
          return Empty.value;
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.Quadtree (line 168, column 10 - line 170, column 19): " + [v.constructor.name]);
      };
    };
  };
  var elsewhere = function($copy_first) {
    return function($copy_others) {
      var $tco_var_first = $copy_first;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(first, others) {
        if (others instanceof Nil) {
          $tco_done = true;
          return Nil.value;
        }
        ;
        if (others instanceof Cons) {
          var $20 = others.value0.x === first.x && others.value0.y === first.y;
          if ($20) {
            $tco_var_first = first;
            $copy_others = others.value1;
            return;
          }
          ;
          $tco_done = true;
          return others;
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.Quadtree (line 156, column 26 - line 160, column 16): " + [others.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_first, $copy_others);
      }
      ;
      return $tco_result;
    };
  };
  var cover = function(extent) {
    return function(x) {
      return function(y) {
        var width8 = extent.x1 - extent.x0;
        var side = (function() {
          var $23 = width8 < 0 || width8 > 0;
          if ($23) {
            return width8;
          }
          ;
          return 1;
        })();
        var grow = function($copy_x0) {
          return function($copy_y0) {
            return function($copy_x1) {
              return function($copy_y1) {
                return function($copy_size) {
                  var $tco_var_x0 = $copy_x0;
                  var $tco_var_y0 = $copy_y0;
                  var $tco_var_x1 = $copy_x1;
                  var $tco_var_y1 = $copy_y1;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(x0, y0, x1, y1, size5) {
                    var $24 = x0 > x || (x >= x1 || (y0 > y || y >= y1));
                    if ($24) {
                      var doubled = size5 * 2;
                      var $25 = y < y0;
                      if ($25) {
                        var $26 = x < x0;
                        if ($26) {
                          $tco_var_x0 = x1 - doubled;
                          $tco_var_y0 = y1 - doubled;
                          $tco_var_x1 = x1;
                          $tco_var_y1 = y1;
                          $copy_size = doubled;
                          return;
                        }
                        ;
                        $tco_var_x0 = x0;
                        $tco_var_y0 = y1 - doubled;
                        $tco_var_x1 = x0 + doubled;
                        $tco_var_y1 = y1;
                        $copy_size = doubled;
                        return;
                      }
                      ;
                      var $27 = x < x0;
                      if ($27) {
                        $tco_var_x0 = x1 - doubled;
                        $tco_var_y0 = y0;
                        $tco_var_x1 = x1;
                        $tco_var_y1 = y0 + doubled;
                        $copy_size = doubled;
                        return;
                      }
                      ;
                      $tco_var_x0 = x0;
                      $tco_var_y0 = y0;
                      $tco_var_x1 = x0 + doubled;
                      $tco_var_y1 = y0 + doubled;
                      $copy_size = doubled;
                      return;
                    }
                    ;
                    $tco_done = true;
                    return {
                      x0,
                      y0,
                      x1,
                      y1
                    };
                  }
                  ;
                  while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_x0, $tco_var_y0, $tco_var_x1, $tco_var_y1, $copy_size);
                  }
                  ;
                  return $tco_result;
                };
              };
            };
          };
        };
        return grow(extent.x0)(extent.y0)(extent.x1)(extent.y1)(side);
      };
    };
  };
  var coincident = function(first) {
    return function(others) {
      var v = elsewhere(first)(others);
      if (v instanceof Nil) {
        return true;
      }
      ;
      return false;
    };
  };
  var $lazy_cell = /* @__PURE__ */ $runtime_lazy5("cell", "Hylograph.Forces.Pure.Quadtree", function() {
    return function(newestFirst, x0, y0, x1, y1, points) {
      var turned = !newestFirst;
      var middleY = (y0 + y1) / 2;
      var middleX = (x0 + x1) / 2;
      var deal = function($copy_remaining) {
        return function($copy_topLeft) {
          return function($copy_topRight) {
            return function($copy_bottomLeft) {
              return function($copy_bottomRight) {
                var $tco_var_remaining = $copy_remaining;
                var $tco_var_topLeft = $copy_topLeft;
                var $tco_var_topRight = $copy_topRight;
                var $tco_var_bottomLeft = $copy_bottomLeft;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(remaining, topLeft, topRight, bottomLeft, bottomRight) {
                  if (remaining instanceof Nil) {
                    $tco_done = true;
                    return new Branch($lazy_cell(125)(turned, x0, y0, middleX, middleY, topLeft), $lazy_cell(126)(turned, middleX, y0, x1, middleY, topRight), $lazy_cell(127)(turned, x0, middleY, middleX, y1, bottomLeft), $lazy_cell(128)(turned, middleX, middleY, x1, y1, bottomRight));
                  }
                  ;
                  if (remaining instanceof Cons) {
                    var $30 = remaining.value0.y >= middleY;
                    if ($30) {
                      var $31 = remaining.value0.x >= middleX;
                      if ($31) {
                        $tco_var_remaining = remaining.value1;
                        $tco_var_topLeft = topLeft;
                        $tco_var_topRight = topRight;
                        $tco_var_bottomLeft = bottomLeft;
                        $copy_bottomRight = new Cons(remaining.value0, bottomRight);
                        return;
                      }
                      ;
                      $tco_var_remaining = remaining.value1;
                      $tco_var_topLeft = topLeft;
                      $tco_var_topRight = topRight;
                      $tco_var_bottomLeft = new Cons(remaining.value0, bottomLeft);
                      $copy_bottomRight = bottomRight;
                      return;
                    }
                    ;
                    var $32 = remaining.value0.x >= middleX;
                    if ($32) {
                      $tco_var_remaining = remaining.value1;
                      $tco_var_topLeft = topLeft;
                      $tco_var_topRight = new Cons(remaining.value0, topRight);
                      $tco_var_bottomLeft = bottomLeft;
                      $copy_bottomRight = bottomRight;
                      return;
                    }
                    ;
                    $tco_var_remaining = remaining.value1;
                    $tco_var_topLeft = new Cons(remaining.value0, topLeft);
                    $tco_var_topRight = topRight;
                    $tco_var_bottomLeft = bottomLeft;
                    $copy_bottomRight = bottomRight;
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Hylograph.Forces.Pure.Quadtree (line 122, column 62 - line 135, column 78): " + [remaining.constructor.name]);
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_remaining, $tco_var_topLeft, $tco_var_topRight, $tco_var_bottomLeft, $copy_bottomRight);
                }
                ;
                return $tco_result;
              };
            };
          };
        };
      };
      if (points instanceof Nil) {
        return Empty.value;
      }
      ;
      if (points instanceof Cons) {
        var $36 = coincident(points.value0)(points.value1);
        if ($36) {
          return leaf(newestFirst)(points.value0)(points.value1);
        }
        ;
        return deal(points)(Nil.value)(Nil.value)(Nil.value)(Nil.value);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure.Quadtree (line 137, column 5 - line 141, column 41): " + [points.constructor.name]);
    };
  });
  var cell = /* @__PURE__ */ $lazy_cell(115);
  var build = function(newestFirst) {
    var survey = function($copy_remaining) {
      return function($copy_kept) {
        return function($copy_x0) {
          return function($copy_y0) {
            return function($copy_x1) {
              return function($copy_y1) {
                var $tco_var_remaining = $copy_remaining;
                var $tco_var_kept = $copy_kept;
                var $tco_var_x0 = $copy_x0;
                var $tco_var_y0 = $copy_y0;
                var $tco_var_x1 = $copy_x1;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(remaining, kept, x0, y0, x1, y1) {
                  if (remaining instanceof Nil) {
                    var $40 = x0 > x1 || y0 > y1;
                    if ($40) {
                      $tco_done = true;
                      return {
                        extent: {
                          x0,
                          y0,
                          x1,
                          y1
                        },
                        root: Empty.value
                      };
                    }
                    ;
                    var extent = cover(unitSquare(x0)(y0))(x1)(y1);
                    $tco_done = true;
                    return {
                      extent,
                      root: cell(false, extent.x0, extent.y0, extent.x1, extent.y1, kept)
                    };
                  }
                  ;
                  if (remaining instanceof Cons) {
                    var $41 = isNaNImpl(remaining.value0.x) || isNaNImpl(remaining.value0.y);
                    if ($41) {
                      $tco_var_remaining = remaining.value1;
                      $tco_var_kept = kept;
                      $tco_var_x0 = x0;
                      $tco_var_y0 = y0;
                      $tco_var_x1 = x1;
                      $copy_y1 = y1;
                      return;
                    }
                    ;
                    $tco_var_remaining = remaining.value1;
                    $tco_var_kept = new Cons(remaining.value0, kept);
                    $tco_var_x0 = (function() {
                      var $42 = remaining.value0.x < x0;
                      if ($42) {
                        return remaining.value0.x;
                      }
                      ;
                      return x0;
                    })();
                    $tco_var_y0 = (function() {
                      var $43 = remaining.value0.y < y0;
                      if ($43) {
                        return remaining.value0.y;
                      }
                      ;
                      return y0;
                    })();
                    $tco_var_x1 = (function() {
                      var $44 = remaining.value0.x > x1;
                      if ($44) {
                        return remaining.value0.x;
                      }
                      ;
                      return x1;
                    })();
                    $copy_y1 = (function() {
                      var $45 = remaining.value0.y > y1;
                      if ($45) {
                        return remaining.value0.y;
                      }
                      ;
                      return y1;
                    })();
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Hylograph.Forces.Pure.Quadtree (line 62, column 39 - line 76, column 47): " + [remaining.constructor.name]);
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_remaining, $tco_var_kept, $tco_var_x0, $tco_var_y0, $tco_var_x1, $copy_y1);
                }
                ;
                return $tco_result;
              };
            };
          };
        };
      };
    };
    return survey(newestFirst)(Nil.value)(infinity)(infinity)(-infinity)(-infinity);
  };

  // output/Hylograph.Forces.Pure.Random/index.js
  var multiplier = 1664525;
  var modulus = 4294967296;
  var jiggle = function(v) {
    return (v / modulus - 0.5) * 1e-6;
  };
  var initialSeed = 1;
  var lcg = /* @__PURE__ */ $$new(initialSeed);
  var increment = 1013904223;
  var advance = function(v) {
    return remainder(multiplier * v + increment)(modulus);
  };
  var draw = function(source) {
    return function __do2() {
      var seed = read(source)();
      var next = advance(seed);
      write(next)(source)();
      return jiggle(next);
    };
  };

  // output/Hylograph.Forces.Pure.Collide/index.js
  var $runtime_lazy6 = function(name15, moduleName, init) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init();
      state3 = 2;
      return val;
    };
  };
  var pure3 = /* @__PURE__ */ pure(applicativeEffect);
  var when4 = /* @__PURE__ */ when(applicativeEffect);
  var NoPushes = /* @__PURE__ */ (function() {
    function NoPushes2() {
    }
    ;
    NoPushes2.value = new NoPushes2();
    return NoPushes2;
  })();
  var Push = /* @__PURE__ */ (function() {
    function Push2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Push2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Push2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Push2;
  })();
  var Unoccupied = /* @__PURE__ */ (function() {
    function Unoccupied2() {
    }
    ;
    Unoccupied2.value = new Unoccupied2();
    return Unoccupied2;
  })();
  var Occupant = /* @__PURE__ */ (function() {
    function Occupant2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Occupant2.create = function(value0) {
      return function(value1) {
        return new Occupant2(value0, value1);
      };
    };
    return Occupant2;
  })();
  var Region = /* @__PURE__ */ (function() {
    function Region2(value0, value1, value22, value32, value42, value52, value62, value72) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
      this.value7 = value72;
    }
    ;
    Region2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return function(value72) {
                    return new Region2(value0, value1, value22, value32, value42, value52, value62, value72);
                  };
                };
              };
            };
          };
        };
      };
    };
    return Region2;
  })();
  var Finished = /* @__PURE__ */ (function() {
    function Finished4() {
    }
    ;
    Finished4.value = new Finished4();
    return Finished4;
  })();
  var seek = function(index3, radius, nodeX, nodeY, start4) {
    var go2 = function($copy_region) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(region) {
        if (region instanceof Region) {
          var reach = radius + region.value0;
          if (region.value7 instanceof Unoccupied) {
            var $22 = region.value1 > nodeX + reach || (region.value3 < nodeX - reach || (region.value2 > nodeY + reach || region.value4 < nodeY - reach));
            if ($22) {
              $copy_region = region.value6;
              return;
            }
            ;
            $copy_region = region.value5;
            return;
          }
          ;
          if (region.value7 instanceof Occupant) {
            var $23 = region.value7.value0 > index3;
            if ($23) {
              var y = nodeY - region.value7.value1.y - region.value7.value1.vy;
              var x = nodeX - region.value7.value1.x - region.value7.value1.vx;
              var $24 = x * x + y * y < reach * reach;
              if ($24) {
                $tco_done = true;
                return region;
              }
              ;
              $copy_region = region.value6;
              return;
            }
            ;
            $copy_region = region.value6;
            return;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.Collide (line 254, column 11 - line 266, column 28): " + [region.value7.constructor.name]);
        }
        ;
        if (region instanceof Finished) {
          $tco_done = true;
          return Finished.value;
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.Collide (line 249, column 17 - line 267, column 27): " + [region.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_region);
      }
      ;
      return $tco_result;
    };
    return go2(start4);
  };
  var regions = function(planted) {
    var occupantOf = function(body) {
      return new Occupant((function() {
        if (body.included) {
          return body.index;
        }
        ;
        return -1 | 0;
      })(), body.node);
    };
    var larger = function(quarter, region, radius) {
      if (quarter instanceof Empty) {
        return radius;
      }
      ;
      if (region instanceof Region) {
        var $38 = region.value0 > radius;
        if ($38) {
          return region.value0;
        }
        ;
        return radius;
      }
      ;
      if (region instanceof Finished) {
        return radius;
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure.Collide (line 187, column 10 - line 189, column 25): " + [region.constructor.name]);
    };
    var $lazy_thread = $runtime_lazy6("thread", "Hylograph.Forces.Pure.Collide", function() {
      return function(tree, x0, y0, x1, y1, after) {
        if (tree instanceof Empty) {
          return after;
        }
        ;
        if (tree instanceof Leaf2) {
          return new Region(tree.value0.item.radius, x0, y0, x1, y1, after, after, occupantOf(tree.value0.item));
        }
        ;
        if (tree instanceof Branch) {
          var middleY = (y0 + y1) / 2;
          var middleX = (x0 + x1) / 2;
          var fourth = $lazy_thread(168)(tree.value3, middleX, middleY, x1, y1, after);
          var third = $lazy_thread(169)(tree.value2, x0, middleY, middleX, y1, fourth);
          var second = $lazy_thread(170)(tree.value1, middleX, y0, x1, middleY, third);
          var first = $lazy_thread(171)(tree.value0, x0, y0, middleX, middleY, second);
          var radius = larger(tree.value3, fourth, larger(tree.value2, third, larger(tree.value1, second, larger(tree.value0, first, 0))));
          return new Region(radius, x0, y0, x1, y1, first, after, Unoccupied.value);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.Collide (line 160, column 45 - line 176, column 57): " + [tree.constructor.name]);
      };
    });
    var thread = $lazy_thread(159);
    return thread(planted.root, planted.extent.x0, planted.extent.y0, planted.extent.x1, planted.extent.y1, Finished.value);
  };
  var rebound = function(strength) {
    return function(radius) {
      return function(otherRadius) {
        return function(x) {
          return function(y) {
            return function(distance2) {
              return function(seed) {
                var seedX = (function() {
                  var $54 = x === 0;
                  if ($54) {
                    return advance(seed);
                  }
                  ;
                  return seed;
                })();
                var seedY = (function() {
                  var $55 = y === 0;
                  if ($55) {
                    return advance(seedX);
                  }
                  ;
                  return seedX;
                })();
                var otherRadius2 = otherRadius * otherRadius;
                var apartY = (function() {
                  var $56 = y === 0;
                  if ($56) {
                    return jiggle(seedY);
                  }
                  ;
                  return y;
                })();
                var apartX = (function() {
                  var $57 = x === 0;
                  if ($57) {
                    return jiggle(seedX);
                  }
                  ;
                  return x;
                })();
                var lengthX = (function() {
                  var $58 = x === 0;
                  if ($58) {
                    return distance2 + apartX * apartX;
                  }
                  ;
                  return distance2;
                })();
                var lengthY = (function() {
                  var $59 = y === 0;
                  if ($59) {
                    return lengthX + apartY * apartY;
                  }
                  ;
                  return lengthX;
                })();
                var distance = sqrt(lengthY);
                var scale = (radius + otherRadius - distance) / distance * strength;
                return {
                  x: apartX * scale,
                  y: apartY * scale,
                  share: otherRadius2 / (radius * radius + otherRadius2),
                  seed: seedY
                };
              };
            };
          };
        };
      };
    };
  };
  var walk = function(strength, index3, radius, nodeX, nodeY, chain, startVx, startVy, startSeed, startPushes) {
    var go2 = function($copy_start) {
      return function($copy_vx) {
        return function($copy_vy) {
          return function($copy_seed) {
            return function($copy_pushes) {
              var $tco_var_start = $copy_start;
              var $tco_var_vx = $copy_vx;
              var $tco_var_vy = $copy_vy;
              var $tco_var_seed = $copy_seed;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(start4, vx, vy, seed, pushes) {
                var v = seek(index3, radius, nodeX, nodeY, start4);
                if (v instanceof Region && v.value7 instanceof Occupant) {
                  var y = nodeY - v.value7.value1.y - v.value7.value1.vy;
                  var x = nodeX - v.value7.value1.x - v.value7.value1.vx;
                  var bounce = rebound(strength)(radius)(v.value0)(x)(y)(x * x + y * y)(seed);
                  $tco_var_start = v.value6;
                  $tco_var_vx = vx + bounce.x * bounce.share;
                  $tco_var_vy = vy + bounce.y * bounce.share;
                  $tco_var_seed = bounce.seed;
                  $copy_pushes = new Push(v.value7.value1, v.value7.value1.vx - bounce.x * (1 - bounce.share), v.value7.value1.vy - bounce.y * (1 - bounce.share), pushes);
                  return;
                }
                ;
                $tco_done = true;
                return {
                  vx,
                  vy,
                  seed,
                  pushes
                };
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_start, $tco_var_vx, $tco_var_vy, $tco_var_seed, $copy_pushes);
              }
              ;
              return $tco_result;
            };
          };
        };
      };
    };
    return go2(chain)(startVx)(startVy)(startSeed)(startPushes);
  };
  var initialize3 = function(config) {
    return function(random) {
      return function(nodes) {
        return {
          bodies: mapWithIndex2(function(index3) {
            return function(node) {
              return {
                index: index3,
                radius: config.radius(node),
                included: config.include(node),
                node
              };
            };
          })(nodes),
          strength: config.strength,
          iterations: config.iterations,
          alphaScaled: config.alphaScaled,
          random
        };
      };
    };
  };
  var deliver = function(pushes) {
    if (pushes instanceof NoPushes) {
      return pure3(unit);
    }
    ;
    return function __do2() {
      var waiting = $$new(pushes)();
      while (!(function __do3() {
        var remaining = read(waiting)();
        if (remaining instanceof NoPushes) {
          return true;
        }
        ;
        if (remaining instanceof Push) {
          writeVelocity(remaining.value0, remaining.value1, remaining.value2);
          write(remaining.value3)(waiting)();
          return false;
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.Collide (line 313, column 7 - line 318, column 21): " + [remaining.constructor.name]);
      })()) {
      }
      ;
      return {};
    };
  };
  var anticipate = function(bodies) {
    return function __do2() {
      var points = $$new(Nil.value)();
      foreachE(bodies)(function(body) {
        return function __do3() {
          var earlier = read(points)();
          return write(new Cons({
            x: body.node.x + body.node.vx,
            y: body.node.y + body.node.vy,
            item: body
          }, earlier))(points)();
        };
      })();
      return read(points)();
    };
  };
  var apply4 = function(v) {
    return function(alpha) {
      var strength = (function() {
        if (v.alphaScaled) {
          return v.strength * alpha;
        }
        ;
        return v.strength;
      })();
      return forE(0)(v.iterations)(function(v1) {
        return function __do2() {
          var points = anticipate(v.bodies)();
          var chain = regions(build(points));
          return foreachE(v.bodies)(function(body) {
            return when4(body.included)(function __do3() {
              var seed = read(v.random)();
              var moved = walk(strength, body.index, body.radius, body.node.x + body.node.vx, body.node.y + body.node.vy, chain, body.node.vx, body.node.vy, seed, NoPushes.value);
              writeVelocity(body.node, moved.vx, moved.vy);
              write(moved.seed)(v.random)();
              return deliver(moved.pushes)();
            });
          })();
        };
      });
    };
  };

  // output/Hylograph.Forces.Pure.Contain/index.js
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var foldl4 = /* @__PURE__ */ foldl(foldableArray);
  var map6 = /* @__PURE__ */ map(functorArray);
  var fromFoldable6 = /* @__PURE__ */ fromFoldable3(foldableArray);
  var when5 = /* @__PURE__ */ when(applicativeEffect);
  var Rectangle = /* @__PURE__ */ (function() {
    function Rectangle2(value0) {
      this.value0 = value0;
    }
    ;
    Rectangle2.create = function(value0) {
      return new Rectangle2(value0);
    };
    return Rectangle2;
  })();
  var Circle2 = /* @__PURE__ */ (function() {
    function Circle4(value0) {
      this.value0 = value0;
    }
    ;
    Circle4.create = function(value0) {
      return new Circle4(value0);
    };
    return Circle4;
  })();
  var Polygon2 = /* @__PURE__ */ (function() {
    function Polygon3(value0) {
      this.value0 = value0;
    }
    ;
    Polygon3.create = function(value0) {
      return new Polygon3(value0);
    };
    return Polygon3;
  })();
  var Free = /* @__PURE__ */ (function() {
    function Free2() {
    }
    ;
    Free2.value = new Free2();
    return Free2;
  })();
  var Within = /* @__PURE__ */ (function() {
    function Within2(value0) {
      this.value0 = value0;
    }
    ;
    Within2.create = function(value0) {
      return new Within2(value0);
    };
    return Within2;
  })();
  var within = /* @__PURE__ */ (function() {
    return Within.create;
  })();
  var rectangle = function(x0) {
    return function(y0) {
      return function(x1) {
        return function(y1) {
          return new Rectangle({
            x0: (function() {
              var $23 = x1 < x0;
              if ($23) {
                return x1;
              }
              ;
              return x0;
            })(),
            y0: (function() {
              var $24 = y1 < y0;
              if ($24) {
                return y1;
              }
              ;
              return y0;
            })(),
            x1: (function() {
              var $25 = x1 < x0;
              if ($25) {
                return x0;
              }
              ;
              return x1;
            })(),
            y1: (function() {
              var $26 = y1 < y0;
              if ($26) {
                return y0;
              }
              ;
              return y1;
            })()
          });
        };
      };
    };
  };
  var polygon = function(vertices) {
    var sides = zipWith(function(from3) {
      return function(to) {
        return {
          from: from3,
          to
        };
      };
    })(vertices)(append2(drop(1)(vertices))(take(1)(vertices)));
    var doubleArea = foldl4(function(area) {
      return function(side) {
        return area + (side.from.x * side.to.y - side.to.x * side.from.y);
      };
    })(0)(sides);
    var sign2 = (function() {
      var $27 = doubleArea > 0;
      if ($27) {
        return 1;
      }
      ;
      return -1;
    })();
    var edge = function(side) {
      var alongY = side.to.y - side.from.y;
      var alongX = side.to.x - side.from.x;
      var sideLength = sqrt(alongX * alongX + alongY * alongY);
      var $28 = sideLength > 0;
      if ($28) {
        return new Just({
          x: side.from.x,
          y: side.from.y,
          normalX: -alongY / sideLength * sign2,
          normalY: alongX / sideLength * sign2
        });
      }
      ;
      return Nothing.value;
    };
    var count = length(vertices);
    var edges = (function() {
      var $29 = count >= 3 && (doubleArea !== 0 && isFiniteImpl(doubleArea));
      if ($29) {
        return catMaybes(map6(edge)(sides));
      }
      ;
      return [];
    })();
    return new Polygon2(fromFoldable6(edges));
  };
  var pass2 = function($copy_edges) {
    return function($copy_radius) {
      return function($copy_x) {
        return function($copy_y) {
          return function($copy_moved) {
            var $tco_var_edges = $copy_edges;
            var $tco_var_radius = $copy_radius;
            var $tco_var_x = $copy_x;
            var $tco_var_y = $copy_y;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(edges, radius, x, y, moved) {
              if (edges instanceof Nil) {
                $tco_done = true;
                return {
                  x,
                  y,
                  moved
                };
              }
              ;
              if (edges instanceof Cons) {
                var inside = (x - edges.value0.x) * edges.value0.normalX + (y - edges.value0.y) * edges.value0.normalY;
                var $31 = inside < radius;
                if ($31) {
                  $tco_var_edges = edges.value1;
                  $tco_var_radius = radius;
                  $tco_var_x = x + edges.value0.normalX * (radius - inside);
                  $tco_var_y = y + edges.value0.normalY * (radius - inside);
                  $copy_moved = true;
                  return;
                }
                ;
                $tco_var_edges = edges.value1;
                $tco_var_radius = radius;
                $tco_var_x = x;
                $tco_var_y = y;
                $copy_moved = moved;
                return;
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Pure.Contain (line 161, column 31 - line 169, column 38): " + [edges.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_edges, $tco_var_radius, $tco_var_x, $tco_var_y, $copy_moved);
            }
            ;
            return $tco_result;
          };
        };
      };
    };
  };
  var passes = function($copy_edges) {
    return function($copy_radius) {
      return function($copy_remaining) {
        return function($copy_x) {
          return function($copy_y) {
            var $tco_var_edges = $copy_edges;
            var $tco_var_radius = $copy_radius;
            var $tco_var_remaining = $copy_remaining;
            var $tco_var_x = $copy_x;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(edges, radius, remaining, x, y) {
              var $34 = remaining <= 0;
              if ($34) {
                $tco_done = true;
                return {
                  x,
                  y
                };
              }
              ;
              var after = pass2(edges)(radius)(x)(y)(false);
              if (after.moved) {
                $tco_var_edges = edges;
                $tco_var_radius = radius;
                $tco_var_remaining = remaining - 1 | 0;
                $tco_var_x = after.x;
                $copy_y = after.y;
                return;
              }
              ;
              $tco_done = true;
              return {
                x: after.x,
                y: after.y
              };
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_edges, $tco_var_radius, $tco_var_remaining, $tco_var_x, $copy_y);
            }
            ;
            return $tco_result;
          };
        };
      };
    };
  };
  var isHard = function(v) {
    return v.hard;
  };
  var initialize4 = function(config) {
    return function(nodes) {
      var kept = function(node) {
        var v = config.region(node);
        if (v instanceof Free) {
          return Nothing.value;
        }
        ;
        if (v instanceof Within) {
          var radius = config.radius(node);
          return new Just({
            node,
            region: v.value0,
            radius: (function() {
              var $38 = isNaNImpl(radius);
              if ($38) {
                return 0;
              }
              ;
              return radius;
            })()
          });
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.Contain (line 207, column 15 - line 213, column 76): " + [v.constructor.name]);
      };
      return {
        kept: catMaybes(map6(kept)(nodes)),
        strength: config.strength,
        hard: config.hard
      };
    };
  };
  var free = /* @__PURE__ */ (function() {
    return Free.value;
  })();
  var confine = function(radius) {
    return function(low2) {
      return function(high2) {
        return function(value12) {
          var most = high2 - radius;
          var least = low2 + radius;
          var $40 = least > most;
          if ($40) {
            return (low2 + high2) / 2;
          }
          ;
          var $41 = value12 < least;
          if ($41) {
            return least;
          }
          ;
          var $42 = value12 > most;
          if ($42) {
            return most;
          }
          ;
          return value12;
        };
      };
    };
  };
  var project = function(region) {
    return function(radius) {
      return function(x) {
        return function(y) {
          if (region instanceof Rectangle) {
            return {
              x: confine(radius)(region.value0.x0)(region.value0.x1)(x),
              y: confine(radius)(region.value0.y0)(region.value0.y1)(y)
            };
          }
          ;
          if (region instanceof Circle2) {
            var offsetY = y - region.value0.y;
            var offsetX = x - region.value0.x;
            var inner = (function() {
              var $45 = region.value0.radius - radius > 0;
              if ($45) {
                return region.value0.radius - radius;
              }
              ;
              return 0;
            })();
            var distance2 = offsetX * offsetX + offsetY * offsetY;
            var $46 = distance2 > inner * inner;
            if ($46) {
              var scale = inner / sqrt(distance2);
              return {
                x: region.value0.x + offsetX * scale,
                y: region.value0.y + offsetY * scale
              };
            }
            ;
            return {
              x,
              y
            };
          }
          ;
          if (region instanceof Polygon2) {
            return passes(region.value0)(radius)(8)(x)(y);
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.Contain (line 115, column 29 - line 131, column 45): " + [region.constructor.name]);
        };
      };
    };
  };
  var constrain = function(v) {
    return when5(v.hard)(foreachE(v.kept)(function(kept) {
      return when5(isFiniteImpl(kept.node.x) && isFiniteImpl(kept.node.y))((function() {
        var allowed = project(kept.region)(kept.radius)(kept.node.x)(kept.node.y);
        return function __do2() {
          writeVelocity(kept.node, kept.node.vx + (allowed.x - kept.node.x), kept.node.vy + (allowed.y - kept.node.y));
          return writePosition(kept.node, allowed.x, allowed.y);
        };
      })());
    }));
  };
  var circle = function(x) {
    return function(y) {
      return function(radius) {
        return new Circle2({
          x,
          y,
          radius
        });
      };
    };
  };
  var apply5 = function(v) {
    return foreachE(v.kept)(function(kept) {
      var aheadY = kept.node.y + kept.node.vy;
      var aheadX = kept.node.x + kept.node.vx;
      return when5(isFiniteImpl(aheadX) && isFiniteImpl(aheadY))((function() {
        var allowed = project(kept.region)(kept.radius)(aheadX)(aheadY);
        return function() {
          return writeVelocity(kept.node, kept.node.vx + (allowed.x - aheadX) * v.strength, kept.node.vy + (allowed.y - aheadY) * v.strength);
        };
      })());
    });
  };

  // output/Hylograph.Forces.Pure.Linkage/index.js
  var when6 = /* @__PURE__ */ when(applicativeST);
  var map7 = /* @__PURE__ */ map(functorArray);
  var map12 = /* @__PURE__ */ map(functorEither);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeEither);
  var joinPlaces = function(nodes) {
    return function(links) {
      var count = length(nodes);
      var connected = function(link) {
        return link.source >= 0 && (link.source < count && (link.target >= 0 && link.target < count));
      };
      var degrees = (function __do2() {
        var counts = thaw(replicate(count)(0))();
        foreach(links)(function(link) {
          return when6(connected(link))(function __do3() {
            modify2(link.source)(function(v) {
              return v + 1 | 0;
            })(counts)();
            modify2(link.target)(function(v) {
              return v + 1 | 0;
            })(counts)();
            return unit;
          });
        })();
        return unsafeFreeze(counts)();
      })();
      var degree = function(place) {
        return toNumber(fromMaybe(0)(index(degrees)(place)));
      };
      var joined = function(link) {
        var v = index(nodes)(link.target);
        var v1 = index(nodes)(link.source);
        if (v1 instanceof Just && v instanceof Just) {
          return new Just({
            source: v1.value0,
            target: v.value0,
            sourceDegree: degree(link.source),
            targetDegree: degree(link.target),
            bias: degree(link.source) / (degree(link.source) + degree(link.target)),
            link: link.link
          });
        }
        ;
        return Nothing.value;
      };
      return catMaybes(map7(joined)(links));
    };
  };
  var joinById = function(dictOrd) {
    var fromFoldable11 = fromFoldable2(dictOrd)(foldableArray);
    var lookup4 = lookup(dictOrd);
    return function(dictShow) {
      var show6 = show(dictShow);
      return function(identify) {
        return function(nodes) {
          return function(links) {
            var places = fromFoldable11(mapWithIndex2(function(place) {
              return function(node) {
                return new Tuple(identify(node), place);
              };
            })(nodes));
            var find3 = function(id2) {
              var v = lookup4(id2)(places);
              if (v instanceof Just) {
                return new Right(v.value0);
              }
              ;
              if (v instanceof Nothing) {
                return new Left("node not found: " + show6(id2));
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Pure.Linkage (line 72, column 13 - line 74, column 52): " + [v.constructor.name]);
            };
            var placed = function(link) {
              var v = find3(link.source);
              if (v instanceof Left) {
                return new Left(v.value0);
              }
              ;
              if (v instanceof Right) {
                var v1 = find3(link.target);
                if (v1 instanceof Left) {
                  return new Left(v1.value0);
                }
                ;
                if (v1 instanceof Right) {
                  return new Right({
                    source: v.value0,
                    target: v1.value0,
                    link
                  });
                }
                ;
                throw new Error("Failed pattern match at Hylograph.Forces.Pure.Linkage (line 78, column 21 - line 80, column 53): " + [v1.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Pure.Linkage (line 76, column 17 - line 80, column 53): " + [v.constructor.name]);
            };
            return map12(joinPlaces(nodes))(traverse2(placed)(links));
          };
        };
      };
    };
  };

  // output/Hylograph.Forces.Pure.Directional/index.js
  var map8 = /* @__PURE__ */ map(functorArray);
  var map13 = /* @__PURE__ */ map(functorEither);
  var when7 = /* @__PURE__ */ when(applicativeEffect);
  var ordered = function(config) {
    return function(joined) {
      var usable = function(candidate) {
        return !(isNaNImpl(candidate.gap) || isNaNImpl(candidate.strength));
      };
      var link = function(end) {
        return {
          source: end.source,
          target: end.target,
          gap: config.gap(end.link),
          strength: config.strength(end.link),
          bias: end.bias
        };
      };
      var axisLength = sqrt(config.axis.x * config.axis.x + config.axis.y * config.axis.y);
      var proper = axisLength > 0 && isFiniteImpl(axisLength);
      return {
        links: filter(usable)(map8(link)(joined)),
        axisX: (function() {
          if (proper) {
            return config.axis.x / axisLength;
          }
          ;
          return 0;
        })(),
        axisY: (function() {
          if (proper) {
            return config.axis.y / axisLength;
          }
          ;
          return 1;
        })(),
        iterations: config.iterations
      };
    };
  };
  var initializeById = function(dictOrd) {
    var joinById2 = joinById(dictOrd);
    return function(dictShow) {
      var joinById1 = joinById2(dictShow);
      return function(identify) {
        return function(config) {
          return function(nodes) {
            return function(links) {
              return map13(ordered(config))(joinById1(identify)(nodes)(links));
            };
          };
        };
      };
    };
  };
  var apply6 = function(v) {
    return function(alpha) {
      return forE(0)(v.iterations)(function(v1) {
        return foreachE(v.links)(function(link) {
          var y = link.target.y + link.target.vy - link.source.y - link.source.vy;
          var x = link.target.x + link.target.vx - link.source.x - link.source.vx;
          var along = x * v.axisX + y * v.axisY;
          return when7(along < link.gap)((function() {
            var share = 1 - link.bias;
            var push2 = (link.gap - along) * link.strength * alpha;
            return function __do2() {
              writeVelocity(link.target, link.target.vx + v.axisX * push2 * link.bias, link.target.vy + v.axisY * push2 * link.bias);
              return writeVelocity(link.source, link.source.vx - v.axisX * push2 * share, link.source.vy - v.axisY * push2 * share);
            };
          })());
        });
      });
    };
  };

  // output/Hylograph.Forces.Pure.Link/index.js
  var min3 = /* @__PURE__ */ min(ordNumber);
  var map9 = /* @__PURE__ */ map(functorArray);
  var pure4 = /* @__PURE__ */ pure(applicativeEffect);
  var map14 = /* @__PURE__ */ map(functorEither);
  var FromDegree = /* @__PURE__ */ (function() {
    function FromDegree2() {
    }
    ;
    FromDegree2.value = new FromDegree2();
    return FromDegree2;
  })();
  var PerLink = /* @__PURE__ */ (function() {
    function PerLink2(value0) {
      this.value0 = value0;
    }
    ;
    PerLink2.create = function(value0) {
      return new PerLink2(value0);
    };
    return PerLink2;
  })();
  var strengthPerLink = /* @__PURE__ */ (function() {
    return PerLink.create;
  })();
  var strengthFromDegree = /* @__PURE__ */ (function() {
    return FromDegree.value;
  })();
  var springs = function(config) {
    return function(random) {
      return function(joined) {
        var spring = function(end) {
          return {
            source: end.source,
            target: end.target,
            distance: config.distance(end.link),
            strength: (function() {
              if (config.strength instanceof FromDegree) {
                return 1 / min3(end.sourceDegree)(end.targetDegree);
              }
              ;
              if (config.strength instanceof PerLink) {
                return config.strength.value0(end.link);
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Pure.Link (line 119, column 17 - line 121, column 46): " + [config.strength.constructor.name]);
            })(),
            bias: end.bias
          };
        };
        return {
          springs: map9(spring)(joined),
          iterations: config.iterations,
          random
        };
      };
    };
  };
  var pull = function(spring, alpha, x, y) {
    var share = 1 - spring.bias;
    var separation = sqrt(x * x + y * y);
    var scale = (separation - spring.distance) / separation * alpha * spring.strength;
    var pullY = y * scale;
    var pullX = x * scale;
    writeVelocity(spring.target, spring.target.vx - pullX * spring.bias, spring.target.vy - pullY * spring.bias);
    return writeVelocity(spring.source, spring.source.vx + pullX * share, spring.source.vy + pullY * share);
  };
  var orJiggle = function(random) {
    return function(value12) {
      var $24 = value12 < 0 || value12 > 0;
      if ($24) {
        return pure4(value12);
      }
      ;
      return draw(random);
    };
  };
  var initializeById2 = function(dictOrd) {
    var joinById2 = joinById(dictOrd);
    return function(dictShow) {
      var joinById1 = joinById2(dictShow);
      return function(identify) {
        return function(config) {
          return function(random) {
            return function(nodes) {
              return function(links) {
                return map14(springs(config)(random))(joinById1(identify)(nodes)(links));
              };
            };
          };
        };
      };
    };
  };
  var apply7 = function(v) {
    return function(alpha) {
      return forE(0)(v.iterations)(function(v1) {
        return foreachE(v.springs)(function(spring) {
          var y = spring.target.y + spring.target.vy - spring.source.y - spring.source.vy;
          var x = spring.target.x + spring.target.vx - spring.source.x - spring.source.vx;
          var $27 = (x < 0 || x > 0) && (y < 0 || y > 0);
          if ($27) {
            return function() {
              return pull(spring, alpha, x, y);
            };
          }
          ;
          return function __do2() {
            var apartX = orJiggle(v.random)(x)();
            var apartY = orJiggle(v.random)(y)();
            return pull(spring, alpha, apartX, apartY);
          };
        });
      });
    };
  };

  // output/Hylograph.Forces.Pure.ManyBody/index.js
  var $runtime_lazy7 = function(name15, moduleName, init) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init();
      state3 = 2;
      return val;
    };
  };
  var map10 = /* @__PURE__ */ map(functorArray);
  var EveryPair = /* @__PURE__ */ (function() {
    function EveryPair2() {
    }
    ;
    EveryPair2.value = new EveryPair2();
    return EveryPair2;
  })();
  var WithinGroups = /* @__PURE__ */ (function() {
    function WithinGroups4() {
    }
    ;
    WithinGroups4.value = new WithinGroups4();
    return WithinGroups4;
  })();
  var BetweenGroups = /* @__PURE__ */ (function() {
    function BetweenGroups4() {
    }
    ;
    BetweenGroups4.value = new BetweenGroups4();
    return BetweenGroups4;
  })();
  var NoMembers = /* @__PURE__ */ (function() {
    function NoMembers2() {
    }
    ;
    NoMembers2.value = new NoMembers2();
    return NoMembers2;
  })();
  var Member = /* @__PURE__ */ (function() {
    function Member2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Member2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Member2(value0, value1, value22);
        };
      };
    };
    return Member2;
  })();
  var Cluster = /* @__PURE__ */ (function() {
    function Cluster2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    Cluster2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new Cluster2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return Cluster2;
  })();
  var Finished2 = /* @__PURE__ */ (function() {
    function Finished4() {
    }
    ;
    Finished4.value = new Finished4();
    return Finished4;
  })();
  var withinGroups = /* @__PURE__ */ (function() {
    return WithinGroups.value;
  })();
  var weigh = function(cluster) {
    if (cluster instanceof Cluster) {
      var weight = abs(cluster.value0);
      var $23 = weight > 0;
      if ($23) {
        return weight;
      }
      ;
      return 0;
    }
    ;
    if (cluster instanceof Finished2) {
      return 0;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 275, column 17 - line 281, column 18): " + [cluster.constructor.name]);
  };
  var strengthOf = function(cluster) {
    if (cluster instanceof Cluster) {
      return cluster.value0;
    }
    ;
    if (cluster instanceof Finished2) {
      return 0;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 289, column 22 - line 291, column 18): " + [cluster.constructor.name]);
  };
  var snapshot = function(bodies) {
    return function __do2() {
      var points = $$new(Nil.value)();
      foreachE(bodies)(function(body) {
        return function __do3() {
          var earlier = read(points)();
          return write(new Cons({
            x: body.node.x,
            y: body.node.y,
            item: body
          }, earlier))(points)();
        };
      })();
      return read(points)();
    };
  };
  var separate = function(limits) {
    return function(x) {
      return function(y) {
        return function(distance2) {
          return function(seed) {
            var seedX = (function() {
              var $39 = x === 0;
              if ($39) {
                return advance(seed);
              }
              ;
              return seed;
            })();
            var seedY = (function() {
              var $40 = y === 0;
              if ($40) {
                return advance(seedX);
              }
              ;
              return seedX;
            })();
            var apartY = (function() {
              var $41 = y === 0;
              if ($41) {
                return jiggle(seedY);
              }
              ;
              return y;
            })();
            var apartX = (function() {
              var $42 = x === 0;
              if ($42) {
                return jiggle(seedX);
              }
              ;
              return x;
            })();
            var lengthX = (function() {
              var $43 = x === 0;
              if ($43) {
                return distance2 + apartX * apartX;
              }
              ;
              return distance2;
            })();
            var lengthY = (function() {
              var $44 = y === 0;
              if ($44) {
                return lengthX + apartY * apartY;
              }
              ;
              return lengthX;
            })();
            return {
              x: apartX,
              y: apartY,
              distance2: (function() {
                var $45 = lengthY < limits.distanceMin2;
                if ($45) {
                  return sqrt(limits.distanceMin2 * lengthY);
                }
                ;
                return lengthY;
              })(),
              seed: seedY
            };
          };
        };
      };
    };
  };
  var pushFrom = function(nodes) {
    return function(index3) {
      return function(alpha) {
        return function(x) {
          return function(y) {
            return function(distance2) {
              var go2 = function($copy_remaining) {
                return function($copy_vx) {
                  return function($copy_vy) {
                    var $tco_var_remaining = $copy_remaining;
                    var $tco_var_vx = $copy_vx;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(remaining, vx, vy) {
                      if (remaining instanceof NoMembers) {
                        $tco_done = true;
                        return {
                          vx,
                          vy
                        };
                      }
                      ;
                      if (remaining instanceof Member) {
                        var $47 = remaining.value0 === index3;
                        if ($47) {
                          $tco_var_remaining = remaining.value2;
                          $tco_var_vx = vx;
                          $copy_vy = vy;
                          return;
                        }
                        ;
                        var pull2 = remaining.value1 * alpha / distance2;
                        $tco_var_remaining = remaining.value2;
                        $tco_var_vx = vx + x * pull2;
                        $copy_vy = vy + y * pull2;
                        return;
                      }
                      ;
                      throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 443, column 24 - line 451, column 50): " + [remaining.constructor.name]);
                    }
                    ;
                    while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_remaining, $tco_var_vx, $copy_vy);
                    }
                    ;
                    return $tco_result;
                  };
                };
              };
              return go2(nodes);
            };
          };
        };
      };
    };
  };
  var near = function(limits, alpha, index3, members, x, y, distance2, vx, vy, seed) {
    if (members instanceof Member && (members.value2 instanceof NoMembers && members.value0 === index3)) {
      return {
        vx,
        vy,
        seed
      };
    }
    ;
    var apart = separate(limits)(x)(y)(distance2)(seed);
    var pushed = pushFrom(members)(index3)(alpha)(apart.x)(apart.y)(apart.distance2)(vx)(vy);
    return {
      vx: pushed.vx,
      vy: pushed.vy,
      seed: apart.seed
    };
  };
  var initialize5 = function(config) {
    return function(random) {
      return function(nodes) {
        var negated = function(body) {
          return {
            index: body.index,
            node: body.node,
            strength: -1 * body.strength
          };
        };
        var bodies = mapWithIndex2(function(index3) {
          return function(node) {
            return {
              index: index3,
              strength: config.strength(node),
              node
            };
          };
        })(nodes);
        return {
          bodies,
          groups: (function() {
            if (config.scope instanceof EveryPair) {
              return [];
            }
            ;
            if (config.scope instanceof WithinGroups) {
              return grouped(function(body) {
                return config.group(body.node);
              })(bodies);
            }
            ;
            if (config.scope instanceof BetweenGroups) {
              return grouped(function(body) {
                return config.group(body.node);
              })(map10(negated)(bodies));
            }
            ;
            throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 127, column 13 - line 130, column 86): " + [config.scope.constructor.name]);
          })(),
          scope: config.scope,
          limits: {
            theta2: config.theta * config.theta,
            distanceMin2: config.distanceMin * config.distanceMin,
            distanceMax2: config.distanceMax * config.distanceMax
          },
          random
        };
      };
    };
  };
  var everyPair = /* @__PURE__ */ (function() {
    return EveryPair.value;
  })();
  var distant = function(limits, alpha, strength, x, y, distance2, vx, vy, seed) {
    var apart = separate(limits)(x)(y)(distance2)(seed);
    return {
      vx: vx + apart.x * strength * alpha / apart.distance2,
      vy: vy + apart.y * strength * alpha / apart.distance2,
      seed: apart.seed
    };
  };
  var walk2 = function(limits, alpha, index3, nodeX, nodeY, chain, startVx, startVy, startSeed) {
    var go2 = function($copy_cluster) {
      return function($copy_vx) {
        return function($copy_vy) {
          return function($copy_seed) {
            var $tco_var_cluster = $copy_cluster;
            var $tco_var_vx = $copy_vx;
            var $tco_var_vy = $copy_vy;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(cluster, vx, vy, seed) {
              if (cluster instanceof Cluster) {
                var $57 = !(cluster.value0 < 0 || cluster.value0 > 0);
                if ($57) {
                  $tco_var_cluster = cluster.value5;
                  $tco_var_vx = vx;
                  $tco_var_vy = vy;
                  $copy_seed = seed;
                  return;
                }
                ;
                var y = cluster.value2 - nodeY;
                var x = cluster.value1 - nodeX;
                var distance2 = x * x + y * y;
                var $58 = cluster.value3 < distance2;
                if ($58) {
                  var $59 = distance2 < limits.distanceMax2;
                  if ($59) {
                    var $60 = distance2 >= limits.distanceMin2 && (x !== 0 && y !== 0);
                    if ($60) {
                      $tco_var_cluster = cluster.value5;
                      $tco_var_vx = vx + x * cluster.value0 * alpha / distance2;
                      $tco_var_vy = vy + y * cluster.value0 * alpha / distance2;
                      $copy_seed = seed;
                      return;
                    }
                    ;
                    var moved = distant(limits, alpha, cluster.value0, x, y, distance2, vx, vy, seed);
                    $tco_var_cluster = cluster.value5;
                    $tco_var_vx = moved.vx;
                    $tco_var_vy = moved.vy;
                    $copy_seed = moved.seed;
                    return;
                  }
                  ;
                  $tco_var_cluster = cluster.value5;
                  $tco_var_vx = vx;
                  $tco_var_vy = vy;
                  $copy_seed = seed;
                  return;
                }
                ;
                if (cluster.value6 instanceof NoMembers) {
                  $tco_var_cluster = cluster.value4;
                  $tco_var_vx = vx;
                  $tco_var_vy = vy;
                  $copy_seed = seed;
                  return;
                }
                ;
                var $62 = distance2 >= limits.distanceMax2;
                if ($62) {
                  $tco_var_cluster = cluster.value5;
                  $tco_var_vx = vx;
                  $tco_var_vy = vy;
                  $copy_seed = seed;
                  return;
                }
                ;
                var moved = near(limits, alpha, index3, cluster.value6, x, y, distance2, vx, vy, seed);
                $tco_var_cluster = cluster.value5;
                $tco_var_vx = moved.vx;
                $tco_var_vy = moved.vy;
                $copy_seed = moved.seed;
                return;
              }
              ;
              if (cluster instanceof Finished2) {
                $tco_done = true;
                return {
                  vx,
                  vy,
                  seed
                };
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 326, column 29 - line 357, column 35): " + [cluster.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_cluster, $tco_var_vx, $tco_var_vy, $copy_seed);
            }
            ;
            return $tco_result;
          };
        };
      };
    };
    return go2(chain)(startVx)(startVy)(startSeed);
  };
  var counting = function(weight, term, sum2) {
    var $70 = weight > 0;
    if ($70) {
      return sum2 + term;
    }
    ;
    return sum2;
  };
  var centreYOf = function(cluster) {
    if (cluster instanceof Cluster) {
      return cluster.value2;
    }
    ;
    if (cluster instanceof Finished2) {
      return 0;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 299, column 21 - line 301, column 18): " + [cluster.constructor.name]);
  };
  var centreXOf = function(cluster) {
    if (cluster instanceof Cluster) {
      return cluster.value1;
    }
    ;
    if (cluster instanceof Finished2) {
      return 0;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 294, column 21 - line 296, column 18): " + [cluster.constructor.name]);
  };
  var clusters = function(theta2) {
    return function(planted) {
      var total = function($copy_nodes) {
        return function($copy_sum) {
          var $tco_var_nodes = $copy_nodes;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(nodes, sum2) {
            if (nodes instanceof NoMembers) {
              $tco_done = true;
              return sum2;
            }
            ;
            if (nodes instanceof Member) {
              $tco_var_nodes = nodes.value2;
              $copy_sum = sum2 + nodes.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 268, column 21 - line 270, column 58): " + [nodes.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_nodes, $copy_sum);
          }
          ;
          return $tco_result;
        };
      };
      var present = function(quarter) {
        return function(cluster) {
          if (quarter instanceof Empty) {
            return Finished2.value;
          }
          ;
          return cluster;
        };
      };
      var membersOf = function($copy_oldestFirst) {
        return function($copy_newer) {
          var $tco_var_oldestFirst = $copy_oldestFirst;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(oldestFirst, newer) {
            if (oldestFirst instanceof Nil) {
              $tco_done1 = true;
              return newer;
            }
            ;
            if (oldestFirst instanceof Cons) {
              $tco_var_oldestFirst = oldestFirst.value1;
              $copy_newer = new Member(oldestFirst.value0.item.index, oldestFirst.value0.item.strength, newer);
              return;
            }
            ;
            throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 262, column 33 - line 264, column 90): " + [oldestFirst.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_oldestFirst, $copy_newer);
          }
          ;
          return $tco_result;
        };
      };
      var $lazy_thread = $runtime_lazy7("thread", "Hylograph.Forces.Pure.ManyBody", function() {
        return function(tree, x0, x1, after) {
          if (tree instanceof Empty) {
            return after;
          }
          ;
          if (tree instanceof Leaf2) {
            var nodes = membersOf(reverse2(new Cons(tree.value0, tree.value1)))(NoMembers.value);
            return new Cluster(total(nodes)(0), tree.value0.x, tree.value0.y, (x1 - x0) * (x1 - x0) / theta2, after, after, nodes);
          }
          ;
          if (tree instanceof Branch) {
            var middle = (x0 + x1) / 2;
            var fourth = $lazy_thread(215)(tree.value3, middle, x1, after);
            var third = $lazy_thread(216)(tree.value2, x0, middle, fourth);
            var second = $lazy_thread(217)(tree.value1, middle, x1, third);
            var two = present(tree.value1)(second);
            var weightTwo = weigh(two);
            var three = present(tree.value2)(third);
            var weightThree = weigh(three);
            var four = present(tree.value3)(fourth);
            var weightFour = weigh(four);
            var first = $lazy_thread(218)(tree.value0, x0, middle, second);
            var one2 = present(tree.value0)(first);
            var weightOne = weigh(one2);
            var strength = counting(weightFour, strengthOf(four), counting(weightThree, strengthOf(three), counting(weightTwo, strengthOf(two), counting(weightOne, strengthOf(one2), 0))));
            var weight = 0 + weightOne + weightTwo + weightThree + weightFour;
            var x = counting(weightFour, weightFour * centreXOf(four), counting(weightThree, weightThree * centreXOf(three), counting(weightTwo, weightTwo * centreXOf(two), counting(weightOne, weightOne * centreXOf(one2), 0))));
            var y = counting(weightFour, weightFour * centreYOf(four), counting(weightThree, weightThree * centreYOf(three), counting(weightTwo, weightTwo * centreYOf(two), counting(weightOne, weightOne * centreYOf(one2), 0))));
            return new Cluster(strength, x / weight, y / weight, (x1 - x0) * (x1 - x0) / theta2, first, after, NoMembers.value);
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 205, column 39 - line 249, column 106): " + [tree.constructor.name]);
        };
      });
      var thread = $lazy_thread(204);
      return thread(planted.root, planted.extent.x0, planted.extent.x1, Finished2.value);
    };
  };
  var betweenGroups = /* @__PURE__ */ (function() {
    return BetweenGroups.value;
  })();
  var apply8 = function(v) {
    return function(alpha) {
      var among2 = function(bodies) {
        return function __do2() {
          var points = snapshot(bodies)();
          var chain = clusters(v.limits.theta2)(build(points));
          return foreachE(bodies)(function(body) {
            return function __do3() {
              var seed = read(v.random)();
              var moved = walk2(v.limits, alpha, body.index, body.node.x, body.node.y, chain, body.node.vx, body.node.vy, seed);
              writeVelocity(body.node, moved.vx, moved.vy);
              return write(moved.seed)(v.random)();
            };
          })();
        };
      };
      if (v.scope instanceof EveryPair) {
        return among2(v.bodies);
      }
      ;
      if (v.scope instanceof WithinGroups) {
        return foreachE(v.groups)(among2);
      }
      ;
      if (v.scope instanceof BetweenGroups) {
        return function __do2() {
          among2(v.bodies)();
          return foreachE(v.groups)(among2)();
        };
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 146, column 32 - line 151, column 32): " + [v.scope.constructor.name]);
    };
  };

  // output/Hylograph.Forces.Pure.Position/index.js
  var map11 = /* @__PURE__ */ map(functorArray);
  var initialize6 = function(config) {
    return function(nodes) {
      var pulled = function(node) {
        var target5 = config.target(node);
        return {
          node,
          target: target5,
          strength: (function() {
            var $8 = isNaNImpl(target5);
            if ($8) {
              return 0;
            }
            ;
            return config.strength(node);
          })()
        };
      };
      return map11(pulled)(nodes);
    };
  };
  var applyY = function(v) {
    return function(alpha) {
      return foreachE(v)(function(pulled) {
        return function() {
          return writeVelocity(pulled.node, pulled.node.vx, pulled.node.vy + (pulled.target - pulled.node.y) * pulled.strength * alpha);
        };
      });
    };
  };
  var applyX = function(v) {
    return function(alpha) {
      return foreachE(v)(function(pulled) {
        return function() {
          return writeVelocity(pulled.node, pulled.node.vx + (pulled.target - pulled.node.x) * pulled.strength * alpha, pulled.node.vy);
        };
      });
    };
  };

  // output/Hylograph.Forces.Pure.Radial/index.js
  var map15 = /* @__PURE__ */ map(functorArray);
  var orTiny = function(value12) {
    var $11 = value12 < 0 || value12 > 0;
    if ($11) {
      return value12;
    }
    ;
    return 1e-6;
  };
  var initialize7 = function(config) {
    return function(nodes) {
      var pulled = function(node) {
        var radius = config.radius(node);
        return {
          node,
          radius,
          strength: (function() {
            var $12 = isNaNImpl(radius);
            if ($12) {
              return 0;
            }
            ;
            return config.strength(node);
          })()
        };
      };
      return {
        nodes: map15(pulled)(nodes),
        x: config.x,
        y: config.y
      };
    };
  };
  var apply9 = function(v) {
    return function(alpha) {
      return foreachE(v.nodes)(function(pulled) {
        var offsetY = orTiny(pulled.node.y - v.y);
        var offsetX = orTiny(pulled.node.x - v.x);
        var distance = sqrt(offsetX * offsetX + offsetY * offsetY);
        var scale = (pulled.radius - distance) * pulled.strength * alpha / distance;
        return function() {
          return writeVelocity(pulled.node, pulled.node.vx + offsetX * scale, pulled.node.vy + offsetY * scale);
        };
      });
    };
  };

  // output/Hylograph.Forces.Pure.RectCollide/index.js
  var $runtime_lazy8 = function(name15, moduleName, init) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init();
      state3 = 2;
      return val;
    };
  };
  var comparing2 = /* @__PURE__ */ comparing(ordInt);
  var when8 = /* @__PURE__ */ when(applicativeEffect);
  var pure5 = /* @__PURE__ */ pure(applicativeEffect);
  var Cell = /* @__PURE__ */ (function() {
    function Cell2(value0, value1, value22, value32, value42, value52, value62, value72, value82) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
      this.value7 = value72;
      this.value8 = value82;
    }
    ;
    Cell2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return function(value72) {
                    return function(value82) {
                      return new Cell2(value0, value1, value22, value32, value42, value52, value62, value72, value82);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
    return Cell2;
  })();
  var Finished3 = /* @__PURE__ */ (function() {
    function Finished4() {
    }
    ;
    Finished4.value = new Finished4();
    return Finished4;
  })();
  var initialize8 = function(config) {
    return function(nodes) {
      var orZero = function(extent) {
        var $22 = isNaNImpl(extent);
        if ($22) {
          return 0;
        }
        ;
        return extent;
      };
      var box = function(index3) {
        return function(node) {
          return {
            index: index3,
            halfWidth: orZero(config.halfWidth(node)),
            halfHeight: orZero(config.halfHeight(node)),
            node
          };
        };
      };
      return {
        boxes: mapWithIndex2(box)(nodes),
        strength: config.strength,
        iterations: config.iterations
      };
    };
  };
  var inIndexOrder = function(found) {
    if (found instanceof Cons && found.value1 instanceof Nil) {
      return found;
    }
    ;
    return sortBy2(comparing2(function(v) {
      return v.item.index;
    }))(found);
  };
  var direction = function(offset, tie, overlap) {
    var $26 = offset > 0;
    if ($26) {
      return 1 * overlap;
    }
    ;
    var $27 = offset < 0;
    if ($27) {
      return -1 * overlap;
    }
    ;
    return tie * overlap;
  };
  var cells = function(planted) {
    var widest = function($copy_occupants) {
      return function($copy_most) {
        var $tco_var_occupants = $copy_occupants;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(occupants, most) {
          if (occupants instanceof Nil) {
            $tco_done = true;
            return most;
          }
          ;
          if (occupants instanceof Cons) {
            $tco_var_occupants = occupants.value1;
            $copy_most = (function() {
              var $29 = occupants.value0.item.halfWidth > most;
              if ($29) {
                return occupants.value0.item.halfWidth;
              }
              ;
              return most;
            })();
            return;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 197, column 27 - line 199, column 104): " + [occupants.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_occupants, $copy_most);
        }
        ;
        return $tco_result;
      };
    };
    var wider = function(quarter, cell2, most) {
      if (quarter instanceof Empty) {
        return most;
      }
      ;
      if (cell2 instanceof Cell) {
        var $34 = cell2.value0 > most;
        if ($34) {
          return cell2.value0;
        }
        ;
        return most;
      }
      ;
      if (cell2 instanceof Finished3) {
        return most;
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 209, column 10 - line 211, column 23): " + [cell2.constructor.name]);
    };
    var tallest = function($copy_occupants) {
      return function($copy_most) {
        var $tco_var_occupants = $copy_occupants;
        var $tco_done1 = false;
        var $tco_result;
        function $tco_loop(occupants, most) {
          if (occupants instanceof Nil) {
            $tco_done1 = true;
            return most;
          }
          ;
          if (occupants instanceof Cons) {
            $tco_var_occupants = occupants.value1;
            $copy_most = (function() {
              var $45 = occupants.value0.item.halfHeight > most;
              if ($45) {
                return occupants.value0.item.halfHeight;
              }
              ;
              return most;
            })();
            return;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 202, column 28 - line 204, column 107): " + [occupants.constructor.name]);
        }
        ;
        while (!$tco_done1) {
          $tco_result = $tco_loop($tco_var_occupants, $copy_most);
        }
        ;
        return $tco_result;
      };
    };
    var taller = function(quarter, cell2, most) {
      if (quarter instanceof Empty) {
        return most;
      }
      ;
      if (cell2 instanceof Cell) {
        var $50 = cell2.value1 > most;
        if ($50) {
          return cell2.value1;
        }
        ;
        return most;
      }
      ;
      if (cell2 instanceof Finished3) {
        return most;
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 216, column 10 - line 218, column 23): " + [cell2.constructor.name]);
    };
    var $lazy_thread = $runtime_lazy8("thread", "Hylograph.Forces.Pure.RectCollide", function() {
      return function(tree, x0, y0, x1, y1, after) {
        if (tree instanceof Empty) {
          return after;
        }
        ;
        if (tree instanceof Leaf2) {
          var occupants = new Cons(tree.value0, tree.value1);
          return new Cell(widest(occupants)(0), tallest(occupants)(0), x0, y0, x1, y1, after, after, occupants);
        }
        ;
        if (tree instanceof Branch) {
          var middleY = (y0 + y1) / 2;
          var middleX = (x0 + x1) / 2;
          var fourth = $lazy_thread(183)(tree.value3, middleX, middleY, x1, y1, after);
          var third = $lazy_thread(184)(tree.value2, x0, middleY, middleX, y1, fourth);
          var second = $lazy_thread(185)(tree.value1, middleX, y0, x1, middleY, third);
          var first = $lazy_thread(186)(tree.value0, x0, y0, middleX, middleY, second);
          var height8 = taller(tree.value3, fourth, taller(tree.value2, third, taller(tree.value1, second, taller(tree.value0, first, 0))));
          var width8 = wider(tree.value3, fourth, wider(tree.value2, third, wider(tree.value1, second, wider(tree.value0, first, 0))));
          return new Cell(width8, height8, x0, y0, x1, y1, first, after, Nil.value);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 172, column 45 - line 194, column 54): " + [tree.constructor.name]);
      };
    });
    var thread = $lazy_thread(171);
    return thread(planted.root, planted.extent.x0, planted.extent.y0, planted.extent.x1, planted.extent.y1, Finished3.value);
  };
  var anticipate2 = function(boxes) {
    return function __do2() {
      var points = $$new(Nil.value)();
      foreachE(boxes)(function(box) {
        return function __do3() {
          var earlier = read(points)();
          return write(new Cons({
            x: box.node.x + box.node.vx,
            y: box.node.y + box.node.vy,
            item: box
          }, earlier))(points)();
        };
      })();
      return read(points)();
    };
  };
  var among = function(index3, halfWidth, halfHeight, aheadX, aheadY, occupants, start4) {
    var go2 = function($copy_remaining) {
      return function($copy_found) {
        var $tco_var_remaining = $copy_remaining;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(remaining, found) {
          if (remaining instanceof Nil) {
            $tco_done = true;
            return found;
          }
          ;
          if (remaining instanceof Cons) {
            var $68 = remaining.value0.item.index !== index3 && (halfWidth + remaining.value0.item.halfWidth - abs(aheadX - remaining.value0.x) > 0 && halfHeight + remaining.value0.item.halfHeight - abs(aheadY - remaining.value0.y) > 0);
            if ($68) {
              $tco_var_remaining = remaining.value1;
              $copy_found = new Cons(remaining.value0, found);
              return;
            }
            ;
            $tco_var_remaining = remaining.value1;
            $copy_found = found;
            return;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 247, column 26 - line 254, column 27): " + [remaining.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_remaining, $copy_found);
        }
        ;
        return $tco_result;
      };
    };
    return go2(occupants)(start4);
  };
  var overlapping = function(index3, halfWidth, halfHeight, aheadX, aheadY, chain, start4) {
    var go2 = function($copy_cell) {
      return function($copy_found) {
        var $tco_var_cell = $copy_cell;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(cell2, found) {
          if (cell2 instanceof Cell) {
            if (cell2.value8 instanceof Nil) {
              var $73 = cell2.value2 - aheadX > halfWidth + cell2.value0 || (aheadX - cell2.value4 > halfWidth + cell2.value0 || (cell2.value3 - aheadY > halfHeight + cell2.value1 || aheadY - cell2.value5 > halfHeight + cell2.value1));
              if ($73) {
                $tco_var_cell = cell2.value7;
                $copy_found = found;
                return;
              }
              ;
              $tco_var_cell = cell2.value6;
              $copy_found = found;
              return;
            }
            ;
            $tco_var_cell = cell2.value7;
            $copy_found = among(index3, halfWidth, halfHeight, aheadX, aheadY, cell2.value8, found);
            return;
          }
          ;
          if (cell2 instanceof Finished3) {
            $tco_done = true;
            return found;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 228, column 21 - line 237, column 24): " + [cell2.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_cell, $copy_found);
        }
        ;
        return $tco_result;
      };
    };
    return go2(chain)(start4);
  };
  var apply10 = function(v) {
    var settle = function(strength, box, found, aheadX, aheadY, startVx, startVy) {
      var area = box.halfWidth * box.halfHeight;
      var go2 = function($copy_others) {
        return function($copy_vx) {
          return function($copy_vy) {
            var $tco_var_others = $copy_others;
            var $tco_var_vx = $copy_vx;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(others, vx, vy) {
              if (others instanceof Nil) {
                $tco_done = true;
                return {
                  vx,
                  vy
                };
              }
              ;
              if (others instanceof Cons) {
                var tie = (function() {
                  var $85 = box.index < others.value0.item.index;
                  if ($85) {
                    return -1;
                  }
                  ;
                  return 1;
                })();
                var otherArea = others.value0.item.halfWidth * others.value0.item.halfHeight;
                var share = (function() {
                  var $86 = area + otherArea > 0;
                  if ($86) {
                    return otherArea / (area + otherArea);
                  }
                  ;
                  return 0.5;
                })();
                var offsetY = aheadY - others.value0.y;
                var overlapY = box.halfHeight + others.value0.item.halfHeight - abs(offsetY);
                var offsetX = aheadX - others.value0.x;
                var overlapX = box.halfWidth + others.value0.item.halfWidth - abs(offsetX);
                var $87 = overlapX <= overlapY;
                if ($87) {
                  $tco_var_others = others.value1;
                  $tco_var_vx = vx + direction(offsetX, tie, overlapX) * strength * share;
                  $copy_vy = vy;
                  return;
                }
                ;
                $tco_var_others = others.value1;
                $tco_var_vx = vx;
                $copy_vy = vy + direction(offsetY, tie, overlapY) * strength * share;
                return;
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 116, column 25 - line 130, column 92): " + [others.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_others, $tco_var_vx, $copy_vy);
            }
            ;
            return $tco_result;
          };
        };
      };
      return go2(inIndexOrder(found))(startVx)(startVy);
    };
    return forE(0)(v.iterations)(function(v1) {
      return function __do2() {
        var points = anticipate2(v.boxes)();
        var chain = cells(build(points));
        return foreachE(v.boxes)(function(box) {
          var aheadY = box.node.y + box.node.vy;
          var aheadX = box.node.x + box.node.vx;
          return when8(isFiniteImpl(aheadX) && isFiniteImpl(aheadY))((function() {
            var v2 = overlapping(box.index, box.halfWidth, box.halfHeight, aheadX, aheadY, chain, Nil.value);
            if (v2 instanceof Nil) {
              return pure5(unit);
            }
            ;
            var moved = settle(v.strength, box, v2, aheadX, aheadY, box.node.vx, box.node.vy);
            return function() {
              return writeVelocity(box.node, moved.vx, moved.vy);
            };
          })());
        })();
      };
    });
  };

  // output/Hylograph.Forces.Pure.Tick/index.js
  var pure6 = /* @__PURE__ */ pure(applicativeEffect);
  var when9 = /* @__PURE__ */ when(applicativeEffect);
  var pin = function(nodes) {
    return foreachE(nodes)(function(node) {
      var v = toMaybe(node.fy);
      var v1 = toMaybe(node.fx);
      if (v1 instanceof Nothing && v instanceof Nothing) {
        return pure6(unit);
      }
      ;
      if (v1 instanceof Just && v instanceof Nothing) {
        return function __do2() {
          writePosition(node, v1.value0, node.y);
          return writeVelocity(node, 0, node.vy);
        };
      }
      ;
      if (v1 instanceof Nothing && v instanceof Just) {
        return function __do2() {
          writePosition(node, node.x, v.value0);
          return writeVelocity(node, node.vx, 0);
        };
      }
      ;
      if (v1 instanceof Just && v instanceof Just) {
        return function __do2() {
          writePosition(node, v1.value0, v.value0);
          return writeVelocity(node, 0, 0);
        };
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure.Tick (line 125, column 27 - line 135, column 46): " + [v1.constructor.name, v.constructor.name]);
    });
  };
  var integrate = function(velocityRetention) {
    return function(nodes) {
      return foreachE(nodes)(function(node) {
        var v = toMaybe(node.fy);
        var v1 = toMaybe(node.fx);
        if (v1 instanceof Nothing && v instanceof Nothing) {
          return function __do2() {
            writePosition(node, node.x + node.vx * velocityRetention, node.y + node.vy * velocityRetention);
            return writeVelocity(node, node.vx * velocityRetention, node.vy * velocityRetention);
          };
        }
        ;
        if (v1 instanceof Just && v instanceof Nothing) {
          return function __do2() {
            writePosition(node, v1.value0, node.y + node.vy * velocityRetention);
            return writeVelocity(node, 0, node.vy * velocityRetention);
          };
        }
        ;
        if (v1 instanceof Nothing && v instanceof Just) {
          return function __do2() {
            writePosition(node, node.x + node.vx * velocityRetention, v.value0);
            return writeVelocity(node, node.vx * velocityRetention, 0);
          };
        }
        ;
        if (v1 instanceof Just && v instanceof Just) {
          return function __do2() {
            writePosition(node, v1.value0, v.value0);
            return writeVelocity(node, 0, 0);
          };
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.Tick (line 52, column 27 - line 64, column 46): " + [v1.constructor.name, v.constructor.name]);
      });
    };
  };
  var initialAngle = /* @__PURE__ */ (function() {
    return pi * (3 - sqrt(5));
  })();
  var initializeNodes = function(nodes) {
    return foreachE(mapWithIndex2(function(index3) {
      return function(node) {
        return {
          index: index3,
          node
        };
      };
    })(nodes))(function(placed) {
      return function __do2() {
        writeIndex(placed.node, placed.index);
        writePosition(placed.node, (function() {
          var v = toMaybe(placed.node.fx);
          if (v instanceof Nothing) {
            return placed.node.x;
          }
          ;
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.Tick (line 98, column 9 - line 100, column 24): " + [v.constructor.name]);
        })(), (function() {
          var v = toMaybe(placed.node.fy);
          if (v instanceof Nothing) {
            return placed.node.y;
          }
          ;
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Pure.Tick (line 102, column 9 - line 104, column 24): " + [v.constructor.name]);
        })());
        when9(isNaNImpl(placed.node.x) || isNaNImpl(placed.node.y))((function() {
          var radius = 10 * sqrt(0.5 + toNumber(placed.index));
          var angle = toNumber(placed.index) * initialAngle;
          return function() {
            return writePosition(placed.node, radius * cos(angle), radius * sin(angle));
          };
        })())();
        return when9(isNaNImpl(placed.node.vx) || isNaNImpl(placed.node.vy))(function() {
          return writeVelocity(placed.node, 0, 0);
        })();
      };
    });
  };
  var decay = function(settings) {
    return function(alpha) {
      var cooled = alpha + (settings.alphaTarget - alpha) * settings.alphaDecay;
      var $32 = cooled < settings.alphaMin;
      if ($32) {
        return 0;
      }
      ;
      return cooled;
    };
  };

  // output/Hylograph.Forces.Pure.Towards/index.js
  var map16 = /* @__PURE__ */ map(functorArray);
  var initialize9 = function(config) {
    return function(nodes) {
      var pulled = function(node) {
        var target5 = config.target(node);
        var strength = config.strength(node);
        return {
          node,
          x: target5.x,
          y: target5.y,
          strength: (function() {
            var $7 = isNaNImpl(strength);
            if ($7) {
              return 0;
            }
            ;
            return strength;
          })()
        };
      };
      var aimed = function(pulled1) {
        return !(isNaNImpl(pulled1.x) || isNaNImpl(pulled1.y));
      };
      return filter(aimed)(map16(pulled)(nodes));
    };
  };
  var apply11 = function(v) {
    return function(alpha) {
      return foreachE(v)(function(pulled) {
        return function() {
          return writeVelocity(pulled.node, pulled.node.vx + (pulled.x - pulled.node.x) * pulled.strength * alpha, pulled.node.vy + (pulled.y - pulled.node.y) * pulled.strength * alpha);
        };
      });
    };
  };

  // output/Hylograph.Forces.Pure/index.js
  var map17 = /* @__PURE__ */ map(functorEither);
  var when10 = /* @__PURE__ */ when(applicativeEffect);
  var pure7 = /* @__PURE__ */ pure(applicativeEffect);
  var ManyBodyForce = /* @__PURE__ */ (function() {
    function ManyBodyForce3(value0) {
      this.value0 = value0;
    }
    ;
    ManyBodyForce3.create = function(value0) {
      return new ManyBodyForce3(value0);
    };
    return ManyBodyForce3;
  })();
  var LinkForce = /* @__PURE__ */ (function() {
    function LinkForce3(value0) {
      this.value0 = value0;
    }
    ;
    LinkForce3.create = function(value0) {
      return new LinkForce3(value0);
    };
    return LinkForce3;
  })();
  var CollideForce = /* @__PURE__ */ (function() {
    function CollideForce3(value0) {
      this.value0 = value0;
    }
    ;
    CollideForce3.create = function(value0) {
      return new CollideForce3(value0);
    };
    return CollideForce3;
  })();
  var CenterForce = /* @__PURE__ */ (function() {
    function CenterForce3(value0) {
      this.value0 = value0;
    }
    ;
    CenterForce3.create = function(value0) {
      return new CenterForce3(value0);
    };
    return CenterForce3;
  })();
  var PositionXForce = /* @__PURE__ */ (function() {
    function PositionXForce3(value0) {
      this.value0 = value0;
    }
    ;
    PositionXForce3.create = function(value0) {
      return new PositionXForce3(value0);
    };
    return PositionXForce3;
  })();
  var PositionYForce = /* @__PURE__ */ (function() {
    function PositionYForce3(value0) {
      this.value0 = value0;
    }
    ;
    PositionYForce3.create = function(value0) {
      return new PositionYForce3(value0);
    };
    return PositionYForce3;
  })();
  var RadialForce = /* @__PURE__ */ (function() {
    function RadialForce3(value0) {
      this.value0 = value0;
    }
    ;
    RadialForce3.create = function(value0) {
      return new RadialForce3(value0);
    };
    return RadialForce3;
  })();
  var TowardsForce = /* @__PURE__ */ (function() {
    function TowardsForce3(value0) {
      this.value0 = value0;
    }
    ;
    TowardsForce3.create = function(value0) {
      return new TowardsForce3(value0);
    };
    return TowardsForce3;
  })();
  var RectCollideForce = /* @__PURE__ */ (function() {
    function RectCollideForce3(value0) {
      this.value0 = value0;
    }
    ;
    RectCollideForce3.create = function(value0) {
      return new RectCollideForce3(value0);
    };
    return RectCollideForce3;
  })();
  var ContainForce = /* @__PURE__ */ (function() {
    function ContainForce3(value0) {
      this.value0 = value0;
    }
    ;
    ContainForce3.create = function(value0) {
      return new ContainForce3(value0);
    };
    return ContainForce3;
  })();
  var CohesionForce = /* @__PURE__ */ (function() {
    function CohesionForce3(value0) {
      this.value0 = value0;
    }
    ;
    CohesionForce3.create = function(value0) {
      return new CohesionForce3(value0);
    };
    return CohesionForce3;
  })();
  var DirectionalForce = /* @__PURE__ */ (function() {
    function DirectionalForce3(value0) {
      this.value0 = value0;
    }
    ;
    DirectionalForce3.create = function(value0) {
      return new DirectionalForce3(value0);
    };
    return DirectionalForce3;
  })();
  var towards = function(config) {
    return function(nodes) {
      return new TowardsForce(initialize9(config)(nodes));
    };
  };
  var rectCollide = function(config) {
    return function(nodes) {
      return new RectCollideForce(initialize8(config)(nodes));
    };
  };
  var radial = function(config) {
    return function(nodes) {
      return new RadialForce(initialize7(config)(nodes));
    };
  };
  var positionY = function(config) {
    return function(nodes) {
      return new PositionYForce(initialize6(config)(nodes));
    };
  };
  var positionX = function(config) {
    return function(nodes) {
      return new PositionXForce(initialize6(config)(nodes));
    };
  };
  var manyBody = function(config) {
    return function(random) {
      return function(nodes) {
        return new ManyBodyForce(initialize5(config)(random)(nodes));
      };
    };
  };
  var linkById = function(dictOrd) {
    var initializeById3 = initializeById2(dictOrd);
    return function(dictShow) {
      var initializeById1 = initializeById3(dictShow);
      return function(identify) {
        return function(config) {
          return function(random) {
            return function(nodes) {
              return function(links) {
                return map17(LinkForce.create)(initializeById1(identify)(config)(random)(nodes)(links));
              };
            };
          };
        };
      };
    };
  };
  var directionalById = function(dictOrd) {
    var initializeById3 = initializeById(dictOrd);
    return function(dictShow) {
      var initializeById1 = initializeById3(dictShow);
      return function(identify) {
        return function(config) {
          return function(nodes) {
            return function(links) {
              return map17(DirectionalForce.create)(initializeById1(identify)(config)(nodes)(links));
            };
          };
        };
      };
    };
  };
  var contain = function(config) {
    return function(nodes) {
      return new ContainForce(initialize4(config)(nodes));
    };
  };
  var constrain2 = function(forces) {
    return function(nodes) {
      var clamps = function(force) {
        if (force instanceof ContainForce) {
          return isHard(force.value0);
        }
        ;
        return false;
      };
      return when10(any2(clamps)(forces))(function __do2() {
        foreachE(forces)(function(force) {
          if (force instanceof ContainForce) {
            return constrain(force.value0);
          }
          ;
          return pure7(unit);
        })();
        return pin(nodes)();
      });
    };
  };
  var collide = function(config) {
    return function(random) {
      return function(nodes) {
        return new CollideForce(initialize3(config)(random)(nodes));
      };
    };
  };
  var cohesion = function(config) {
    return function(nodes) {
      return new CohesionForce(initialize2(config)(nodes));
    };
  };
  var center = function(config) {
    return function(nodes) {
      return new CenterForce(initialize(config)(nodes));
    };
  };
  var applyForce = function(alpha) {
    return function(force) {
      if (force instanceof ManyBodyForce) {
        return apply8(force.value0)(alpha);
      }
      ;
      if (force instanceof LinkForce) {
        return apply7(force.value0)(alpha);
      }
      ;
      if (force instanceof CollideForce) {
        return apply4(force.value0)(alpha);
      }
      ;
      if (force instanceof CenterForce) {
        return apply2(force.value0);
      }
      ;
      if (force instanceof PositionXForce) {
        return applyX(force.value0)(alpha);
      }
      ;
      if (force instanceof PositionYForce) {
        return applyY(force.value0)(alpha);
      }
      ;
      if (force instanceof RadialForce) {
        return apply9(force.value0)(alpha);
      }
      ;
      if (force instanceof TowardsForce) {
        return apply11(force.value0)(alpha);
      }
      ;
      if (force instanceof RectCollideForce) {
        return apply10(force.value0);
      }
      ;
      if (force instanceof ContainForce) {
        return apply5(force.value0);
      }
      ;
      if (force instanceof CohesionForce) {
        return apply3(force.value0)(alpha);
      }
      ;
      if (force instanceof DirectionalForce) {
        return apply6(force.value0)(alpha);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure (line 156, column 26 - line 168, column 58): " + [force.constructor.name]);
    };
  };

  // output/Hylograph.Interaction.Pointer/foreign.js
  function attachSimpleDrag_(element) {
    return () => () => {
      let isDragging = false;
      let transform = { x: 0, y: 0 };
      function handlePointerDown(event) {
        if (event.button !== 0) return;
        isDragging = true;
        element.setPointerCapture(event.pointerId);
        event.preventDefault();
        event.stopPropagation();
        if (element.parentNode) {
          element.parentNode.appendChild(element);
        }
        element.style.cursor = "grabbing";
      }
      function handlePointerMove(event) {
        if (!isDragging) return;
        const svg = element.ownerSVGElement;
        if (svg) {
          const pt = svg.createSVGPoint();
          const ctm = svg.getScreenCTM();
          if (ctm) {
            pt.x = event.movementX;
            pt.y = event.movementY;
            const scale = ctm.a;
            transform.x += event.movementX / scale;
            transform.y += event.movementY / scale;
          } else {
            transform.x += event.movementX;
            transform.y += event.movementY;
          }
        } else {
          transform.x += event.movementX;
          transform.y += event.movementY;
        }
        element.setAttribute("transform", `translate(${transform.x},${transform.y})`);
      }
      function handlePointerUp(event) {
        if (!isDragging) return;
        isDragging = false;
        element.releasePointerCapture(event.pointerId);
        element.style.cursor = "grab";
      }
      function handlePointerCancel(event) {
        if (!isDragging) return;
        isDragging = false;
        element.releasePointerCapture(event.pointerId);
        element.style.cursor = "grab";
      }
      function handleDragStart(event) {
        event.preventDefault();
      }
      element.addEventListener("pointerdown", handlePointerDown);
      element.addEventListener("pointermove", handlePointerMove);
      element.addEventListener("pointerup", handlePointerUp);
      element.addEventListener("pointercancel", handlePointerCancel);
      element.addEventListener("dragstart", handleDragStart);
      element.style.cursor = "grab";
      element.style.touchAction = "none";
      return element;
    };
  }
  var simulationRegistry = /* @__PURE__ */ new Map();
  function getSimulationReheat(simId) {
    const sim = simulationRegistry.get(simId);
    return sim ? sim.reheat : null;
  }
  function attachSimulationDragById_(element) {
    return (simId) => () => {
      let isDragging = false;
      function handlePointerDown(event) {
        if (event.button !== 0) return;
        isDragging = true;
        element.setPointerCapture(event.pointerId);
        event.preventDefault();
        const node = element.__data__;
        if (!node) {
          console.warn("[PointerDrag] No datum on element");
          return;
        }
        const reheat2 = getSimulationReheat(simId);
        if (reheat2) {
          reheat2();
        } else {
          console.warn(`[PointerDrag] No simulation registered with ID: ${simId}`);
        }
        node.fx = node.x;
        node.fy = node.y;
        element.style.cursor = "grabbing";
      }
      function handlePointerMove(event) {
        if (!isDragging) return;
        const node = element.__data__;
        if (!node) return;
        const svg = element.ownerSVGElement;
        if (svg) {
          const pt = svg.createSVGPoint();
          pt.x = event.clientX;
          pt.y = event.clientY;
          const parentGroup = element.parentElement;
          const ctm = parentGroup?.getScreenCTM?.() || svg.getScreenCTM();
          if (ctm) {
            const svgPt = pt.matrixTransform(ctm.inverse());
            node.fx = svgPt.x;
            node.fy = svgPt.y;
          }
        } else {
          node.fx = event.clientX;
          node.fy = event.clientY;
        }
      }
      function handlePointerUp(event) {
        if (!isDragging) return;
        isDragging = false;
        element.releasePointerCapture(event.pointerId);
        const node = element.__data__;
        if (node) {
          node.fx = null;
          node.fy = null;
        }
        element.style.cursor = "grab";
      }
      function handleDragStart(event) {
        event.preventDefault();
      }
      element.addEventListener("pointerdown", handlePointerDown);
      element.addEventListener("pointermove", handlePointerMove);
      element.addEventListener("pointerup", handlePointerUp);
      element.addEventListener("pointercancel", handlePointerUp);
      element.addEventListener("dragstart", handleDragStart);
      element.style.cursor = "grab";
      element.style.touchAction = "none";
      return element;
    };
  }
  function attachSimulationDragNestedById_(element) {
    return (simId) => () => {
      let isDragging = false;
      function handlePointerDown(event) {
        if (event.button !== 0) return;
        isDragging = true;
        element.setPointerCapture(event.pointerId);
        event.preventDefault();
        const datum = element.__data__;
        const node = datum?.node;
        if (!node) {
          console.warn("[PointerDragNested] No datum.node on element");
          return;
        }
        const reheat2 = getSimulationReheat(simId);
        if (reheat2) {
          reheat2();
        } else {
          console.warn(`[PointerDragNested] No simulation registered with ID: ${simId}`);
        }
        node.fx = node.x;
        node.fy = node.y;
        element.style.cursor = "grabbing";
      }
      function handlePointerMove(event) {
        if (!isDragging) return;
        const datum = element.__data__;
        const node = datum?.node;
        if (!node) return;
        const svg = element.ownerSVGElement;
        if (svg) {
          const pt = svg.createSVGPoint();
          pt.x = event.clientX;
          pt.y = event.clientY;
          const parentGroup = element.parentElement;
          const ctm = parentGroup?.getScreenCTM?.() || svg.getScreenCTM();
          if (ctm) {
            const svgPt = pt.matrixTransform(ctm.inverse());
            node.fx = svgPt.x;
            node.fy = svgPt.y;
          }
        } else {
          node.fx = event.clientX;
          node.fy = event.clientY;
        }
      }
      function handlePointerUp(event) {
        if (!isDragging) return;
        isDragging = false;
        element.releasePointerCapture(event.pointerId);
        const datum = element.__data__;
        const node = datum?.node;
        if (node) {
          node.fx = null;
          node.fy = null;
        }
        element.style.cursor = "grab";
      }
      function handleDragStart(event) {
        event.preventDefault();
      }
      element.addEventListener("pointerdown", handlePointerDown);
      element.addEventListener("pointermove", handlePointerMove);
      element.addEventListener("pointerup", handlePointerUp);
      element.addEventListener("pointercancel", handlePointerUp);
      element.addEventListener("dragstart", handleDragStart);
      element.style.cursor = "grab";
      element.style.touchAction = "none";
      return element;
    };
  }

  // output/Hylograph.ForceEngine.Core/index.js
  var map18 = /* @__PURE__ */ map(functorEither);
  var map19 = /* @__PURE__ */ map(functorArray);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableArray);
  var randomSource = lcg;
  var integratePositions = function(nodes) {
    return function(velocityRetention) {
      return integrate(velocityRetention)(nodes);
    };
  };
  var initializeNodes2 = initializeNodes;
  var initializeForce = function(random) {
    return function(nodes) {
      return function(links) {
        return function(recipe2) {
          return map18(function(force) {
            return {
              recipe: recipe2,
              force
            };
          })(recipe2(random)(nodes)(links));
        };
      };
    };
  };
  var decayAlpha = function(alpha) {
    return function(alphaMin) {
      return function(alphaDecay) {
        return function(alphaTarget) {
          return decay({
            alphaMin,
            alphaDecay,
            alphaTarget
          })(alpha);
        };
      };
    };
  };
  var constrain3 = function(forces) {
    return function(nodes) {
      return constrain2(map19(function(v) {
        return v.force;
      })(forces))(nodes);
    };
  };
  var applyForce2 = function(handle) {
    return function(alpha) {
      return applyForce(alpha)(handle.force);
    };
  };
  var applyForces = function(forces) {
    return function(alpha) {
      return for_2(forces)(function(force) {
        return applyForce2(force)(alpha);
      });
    };
  };

  // output/Hylograph.ForceEngine.Setup/index.js
  var fromFoldable7 = /* @__PURE__ */ fromFoldable2(ordInt)(foldableArray);
  var map20 = /* @__PURE__ */ map(functorArray);
  var lookup2 = /* @__PURE__ */ lookup(ordInt);
  var show2 = /* @__PURE__ */ show(showInt);
  var bind2 = /* @__PURE__ */ bind(bindEither);
  var pure8 = /* @__PURE__ */ pure(applicativeEither);
  var traverse3 = /* @__PURE__ */ traverse(traversableArray)(applicativeEither);
  var apply12 = /* @__PURE__ */ apply(applyMaybe);
  var map110 = /* @__PURE__ */ map(functorMaybe);
  var fromFoldableWith2 = /* @__PURE__ */ fromFoldableWith(ordInt)(foldableArray);
  var add2 = /* @__PURE__ */ add(semiringInt);
  var min4 = /* @__PURE__ */ min(ordInt);
  var linkById2 = /* @__PURE__ */ linkById(ordInt)(showInt);
  var bind1 = /* @__PURE__ */ bind(bindMaybe);
  var directionalById2 = /* @__PURE__ */ directionalById(ordInt)(showInt);
  var map23 = /* @__PURE__ */ map(functorEither);
  var fromFoldable1 = /* @__PURE__ */ fromFoldable2(ordString)(foldableArray);
  var fromFoldable22 = /* @__PURE__ */ fromFoldable4(foldableArray);
  var fromFoldable32 = /* @__PURE__ */ fromFoldable22(ordInt);
  var member3 = /* @__PURE__ */ member2(ordInt);
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordInt)(ordInt);
  var fromFoldable42 = /* @__PURE__ */ fromFoldable22(ordTuple2);
  var difference3 = /* @__PURE__ */ difference2(ordTuple2);
  var member1 = /* @__PURE__ */ member2(ordTuple2);
  var intersection3 = /* @__PURE__ */ intersection2(ordTuple2);
  var Static = /* @__PURE__ */ (function() {
    function Static2(value0) {
      this.value0 = value0;
    }
    ;
    Static2.create = function(value0) {
      return new Static2(value0);
    };
    return Static2;
  })();
  var Dynamic = /* @__PURE__ */ (function() {
    function Dynamic2(value0) {
      this.value0 = value0;
    }
    ;
    Dynamic2.create = function(value0) {
      return new Dynamic2(value0);
    };
    return Dynamic2;
  })();
  var RectRegion = /* @__PURE__ */ (function() {
    function RectRegion2(value0) {
      this.value0 = value0;
    }
    ;
    RectRegion2.create = function(value0) {
      return new RectRegion2(value0);
    };
    return RectRegion2;
  })();
  var CircleRegion = /* @__PURE__ */ (function() {
    function CircleRegion2(value0) {
      this.value0 = value0;
    }
    ;
    CircleRegion2.create = function(value0) {
      return new CircleRegion2(value0);
    };
    return CircleRegion2;
  })();
  var PolygonRegion = /* @__PURE__ */ (function() {
    function PolygonRegion2(value0) {
      this.value0 = value0;
    }
    ;
    PolygonRegion2.create = function(value0) {
      return new PolygonRegion2(value0);
    };
    return PolygonRegion2;
  })();
  var AllPairs = /* @__PURE__ */ (function() {
    function AllPairs3() {
    }
    ;
    AllPairs3.value = new AllPairs3();
    return AllPairs3;
  })();
  var WithinGroups2 = /* @__PURE__ */ (function() {
    function WithinGroups4(value0) {
      this.value0 = value0;
    }
    ;
    WithinGroups4.create = function(value0) {
      return new WithinGroups4(value0);
    };
    return WithinGroups4;
  })();
  var BetweenGroups2 = /* @__PURE__ */ (function() {
    function BetweenGroups4(value0) {
      this.value0 = value0;
    }
    ;
    BetweenGroups4.create = function(value0) {
      return new BetweenGroups4(value0);
    };
    return BetweenGroups4;
  })();
  var LinkStrength = /* @__PURE__ */ (function() {
    function LinkStrength2(value0) {
      this.value0 = value0;
    }
    ;
    LinkStrength2.create = function(value0) {
      return new LinkStrength2(value0);
    };
    return LinkStrength2;
  })();
  var ByDegree = /* @__PURE__ */ (function() {
    function ByDegree2() {
    }
    ;
    ByDegree2.value = new ByDegree2();
    return ByDegree2;
  })();
  var ManyBodyForce2 = /* @__PURE__ */ (function() {
    function ManyBodyForce3(value0) {
      this.value0 = value0;
    }
    ;
    ManyBodyForce3.create = function(value0) {
      return new ManyBodyForce3(value0);
    };
    return ManyBodyForce3;
  })();
  var CollideForce2 = /* @__PURE__ */ (function() {
    function CollideForce3(value0) {
      this.value0 = value0;
    }
    ;
    CollideForce3.create = function(value0) {
      return new CollideForce3(value0);
    };
    return CollideForce3;
  })();
  var LinkForce2 = /* @__PURE__ */ (function() {
    function LinkForce3(value0) {
      this.value0 = value0;
    }
    ;
    LinkForce3.create = function(value0) {
      return new LinkForce3(value0);
    };
    return LinkForce3;
  })();
  var CenterForce2 = /* @__PURE__ */ (function() {
    function CenterForce3(value0) {
      this.value0 = value0;
    }
    ;
    CenterForce3.create = function(value0) {
      return new CenterForce3(value0);
    };
    return CenterForce3;
  })();
  var PositionXForce2 = /* @__PURE__ */ (function() {
    function PositionXForce3(value0) {
      this.value0 = value0;
    }
    ;
    PositionXForce3.create = function(value0) {
      return new PositionXForce3(value0);
    };
    return PositionXForce3;
  })();
  var PositionYForce2 = /* @__PURE__ */ (function() {
    function PositionYForce3(value0) {
      this.value0 = value0;
    }
    ;
    PositionYForce3.create = function(value0) {
      return new PositionYForce3(value0);
    };
    return PositionYForce3;
  })();
  var RadialForce2 = /* @__PURE__ */ (function() {
    function RadialForce3(value0) {
      this.value0 = value0;
    }
    ;
    RadialForce3.create = function(value0) {
      return new RadialForce3(value0);
    };
    return RadialForce3;
  })();
  var TowardsForce2 = /* @__PURE__ */ (function() {
    function TowardsForce3(value0) {
      this.value0 = value0;
    }
    ;
    TowardsForce3.create = function(value0) {
      return new TowardsForce3(value0);
    };
    return TowardsForce3;
  })();
  var RectCollideForce2 = /* @__PURE__ */ (function() {
    function RectCollideForce3(value0) {
      this.value0 = value0;
    }
    ;
    RectCollideForce3.create = function(value0) {
      return new RectCollideForce3(value0);
    };
    return RectCollideForce3;
  })();
  var ContainForce2 = /* @__PURE__ */ (function() {
    function ContainForce3(value0) {
      this.value0 = value0;
    }
    ;
    ContainForce3.create = function(value0) {
      return new ContainForce3(value0);
    };
    return ContainForce3;
  })();
  var CohesionForce2 = /* @__PURE__ */ (function() {
    function CohesionForce3(value0) {
      this.value0 = value0;
    }
    ;
    CohesionForce3.create = function(value0) {
      return new CohesionForce3(value0);
    };
    return CohesionForce3;
  })();
  var DirectionalForce2 = /* @__PURE__ */ (function() {
    function DirectionalForce3(value0) {
      this.value0 = value0;
    }
    ;
    DirectionalForce3.create = function(value0) {
      return new DirectionalForce3(value0);
    };
    return DirectionalForce3;
  })();
  var withEnds = function(nodes) {
    return function(links) {
      var byId = fromFoldable7(map20(function(node) {
        return new Tuple(node.id, node);
      })(nodes));
      var find3 = function(id2) {
        var v = lookup2(id2)(byId);
        if (v instanceof Just) {
          return new Right(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return new Left("node not found: " + show2(id2));
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 1065, column 13 - line 1067, column 52): " + [v.constructor.name]);
      };
      var ended2 = function(v) {
        return bind2(find3(v.value1.source))(function(source) {
          return bind2(find3(v.value1.target))(function(target5) {
            return pure8({
              source: v.value1.source,
              target: v.value1.target,
              ends: {
                source,
                target: target5,
                index: v.value0
              }
            });
          });
        });
      };
      return traverse3(ended2)(mapWithIndex2(Tuple.create)(links));
    };
  };
  var valueToNumber = function(v) {
    if (v instanceof Static) {
      return v.value0;
    }
    ;
    if (v instanceof Dynamic) {
      return 0;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 195, column 17 - line 197, column 19): " + [v.constructor.name]);
  };
  var valueOf = function(v) {
    if (v instanceof Static) {
      return function(v1) {
        return v.value0;
      };
    }
    ;
    if (v instanceof Dynamic) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 1034, column 11 - line 1036, column 17): " + [v.constructor.name]);
  };
  var staticOr = function(fallback) {
    return function(v) {
      if (v instanceof Static) {
        return v.value0;
      }
      ;
      if (v instanceof Dynamic) {
        return fallback;
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 531, column 21 - line 533, column 24): " + [v.constructor.name]);
    };
  };
  var withStrength = function(v) {
    return function(v1) {
      if (v1 instanceof ManyBodyForce2) {
        return new ManyBodyForce2({
          name: v1.value0.name,
          theta: v1.value0.theta,
          distanceMin: v1.value0.distanceMin,
          distanceMax: v1.value0.distanceMax,
          filter: v1.value0.filter,
          scope: v1.value0.scope,
          strength: v
        });
      }
      ;
      if (v1 instanceof PositionXForce2) {
        return new PositionXForce2({
          name: v1.value0.name,
          target: v1.value0.target,
          filter: v1.value0.filter,
          strength: v
        });
      }
      ;
      if (v1 instanceof PositionYForce2) {
        return new PositionYForce2({
          name: v1.value0.name,
          target: v1.value0.target,
          filter: v1.value0.filter,
          strength: v
        });
      }
      ;
      if (v1 instanceof RadialForce2) {
        return new RadialForce2({
          name: v1.value0.name,
          radius: v1.value0.radius,
          x: v1.value0.x,
          y: v1.value0.y,
          filter: v1.value0.filter,
          strength: v
        });
      }
      ;
      if (v1 instanceof TowardsForce2) {
        return new TowardsForce2({
          name: v1.value0.name,
          target: v1.value0.target,
          filter: v1.value0.filter,
          strength: v
        });
      }
      ;
      if (v1 instanceof CohesionForce2) {
        return new CohesionForce2({
          name: v1.value0.name,
          group: v1.value0.group,
          filter: v1.value0.filter,
          strength: v
        });
      }
      ;
      if (v1 instanceof CollideForce2) {
        return new CollideForce2({
          name: v1.value0.name,
          radius: v1.value0.radius,
          iterations: v1.value0.iterations,
          filter: v1.value0.filter,
          alphaScaled: v1.value0.alphaScaled,
          strength: staticOr(v1.value0.strength)(v)
        });
      }
      ;
      if (v1 instanceof CenterForce2) {
        return new CenterForce2({
          name: v1.value0.name,
          x: v1.value0.x,
          y: v1.value0.y,
          strength: staticOr(v1.value0.strength)(v)
        });
      }
      ;
      if (v1 instanceof RectCollideForce2) {
        return new RectCollideForce2({
          name: v1.value0.name,
          halfWidth: v1.value0.halfWidth,
          halfHeight: v1.value0.halfHeight,
          iterations: v1.value0.iterations,
          strength: staticOr(v1.value0.strength)(v)
        });
      }
      ;
      if (v1 instanceof ContainForce2) {
        return new ContainForce2({
          name: v1.value0.name,
          regions: v1.value0.regions,
          region: v1.value0.region,
          radius: v1.value0.radius,
          hard: v1.value0.hard,
          strength: staticOr(v1.value0.strength)(v)
        });
      }
      ;
      if (v1 instanceof LinkForce2) {
        if (v instanceof Static) {
          return new LinkForce2({
            name: v1.value0.name,
            distance: v1.value0.distance,
            iterations: v1.value0.iterations,
            strength: new LinkStrength(new Static(v.value0))
          });
        }
        ;
        if (v instanceof Dynamic) {
          return new LinkForce2(v1.value0);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 522, column 18 - line 524, column 29): " + [v.constructor.name]);
      }
      ;
      if (v1 instanceof DirectionalForce2) {
        if (v instanceof Static) {
          return new DirectionalForce2({
            name: v1.value0.name,
            gap: v1.value0.gap,
            axis: v1.value0.axis,
            iterations: v1.value0.iterations,
            strength: new Static(v.value0)
          });
        }
        ;
        if (v instanceof Dynamic) {
          return new DirectionalForce2(v1.value0);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 525, column 25 - line 527, column 36): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 511, column 18 - line 527, column 36): " + [v1.constructor.name]);
    };
  };
  var withX = function(v) {
    return function(v1) {
      if (v1 instanceof PositionXForce2) {
        return new PositionXForce2({
          name: v1.value0.name,
          strength: v1.value0.strength,
          filter: v1.value0.filter,
          target: v
        });
      }
      ;
      if (v1 instanceof CenterForce2) {
        return new CenterForce2({
          name: v1.value0.name,
          y: v1.value0.y,
          strength: v1.value0.strength,
          x: staticOr(v1.value0.x)(v)
        });
      }
      ;
      if (v1 instanceof RadialForce2) {
        return new RadialForce2({
          name: v1.value0.name,
          radius: v1.value0.radius,
          y: v1.value0.y,
          strength: v1.value0.strength,
          filter: v1.value0.filter,
          x: staticOr(v1.value0.x)(v)
        });
      }
      ;
      return v1;
    };
  };
  var withY = function(v) {
    return function(v1) {
      if (v1 instanceof PositionYForce2) {
        return new PositionYForce2({
          name: v1.value0.name,
          strength: v1.value0.strength,
          filter: v1.value0.filter,
          target: v
        });
      }
      ;
      if (v1 instanceof CenterForce2) {
        return new CenterForce2({
          name: v1.value0.name,
          x: v1.value0.x,
          strength: v1.value0.strength,
          y: staticOr(v1.value0.y)(v)
        });
      }
      ;
      if (v1 instanceof RadialForce2) {
        return new RadialForce2({
          name: v1.value0.name,
          radius: v1.value0.radius,
          x: v1.value0.x,
          strength: v1.value0.strength,
          filter: v1.value0.filter,
          y: staticOr(v1.value0.y)(v)
        });
      }
      ;
      return v1;
    };
  };
  var $$static = /* @__PURE__ */ (function() {
    return Static.create;
  })();
  var resolveLinks = function(nodes) {
    return function(links) {
      var byId = fromFoldable7(map20(function(node) {
        return new Tuple(node.id, node);
      })(nodes));
      var endsOf = function(l) {
        return apply12(map110(function(v) {
          return function(v1) {
            return {
              source: v,
              target: v1
            };
          };
        })(lookup2(l.source)(byId)))(lookup2(l.target)(byId));
      };
      return mapWithIndex2(function(index3) {
        return function(ends) {
          return {
            source: ends.source,
            target: ends.target,
            index: index3
          };
        };
      })(mapMaybe(endsOf)(links));
    };
  };
  var regionOf = function(v) {
    if (v instanceof RectRegion) {
      return rectangle(v.value0.x0)(v.value0.y0)(v.value0.x1)(v.value0.y1);
    }
    ;
    if (v instanceof CircleRegion) {
      return circle(v.value0.x)(v.value0.y)(v.value0.radius);
    }
    ;
    if (v instanceof PolygonRegion) {
      return polygon(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 1050, column 12 - line 1053, column 57): " + [v.constructor.name]);
  };
  var mergeSimulationState = function(current) {
    return function(desired) {
      var $287 = {};
      for (var $288 in desired) {
        if ({}.hasOwnProperty.call(desired, $288)) {
          $287[$288] = desired[$288];
        }
        ;
      }
      ;
      $287.x = current.x;
      $287.y = current.y;
      $287.vx = current.vx;
      $287.vy = current.vy;
      $287.fx = current.fx;
      $287.fy = current.fy;
      return $287;
    };
  };
  var membership = function(group3) {
    return function(node) {
      var v = group3(node);
      if (v instanceof Just) {
        return inGroup(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return ungrouped;
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 1045, column 25 - line 1047, column 33): " + [v.constructor.name]);
    };
  };
  var manyBody2 = function(name15) {
    return new ManyBodyForce2({
      name: name15,
      strength: new Static(-30),
      theta: 0.9,
      distanceMin: 1,
      distanceMax: 1e10,
      filter: Nothing.value,
      scope: AllPairs.value
    });
  };
  var linkValues = function(value12) {
    return function(ends) {
      if (value12 instanceof Static) {
        return map20($$const(value12.value0))(ends);
      }
      ;
      if (value12 instanceof Dynamic) {
        return map20(value12.value0)(ends);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 733, column 25 - line 735, column 26): " + [value12.constructor.name]);
    };
  };
  var linkStrengths = function(strength) {
    return function(ends) {
      var count = fromFoldableWith2(add2)(concatMap(function(e) {
        return [new Tuple(e.source.id, 1), new Tuple(e.target.id, 1)];
      })(ends));
      var degree = function(id2) {
        return fromMaybe(1)(lookup2(id2)(count));
      };
      var byDegree = function(e) {
        return 1 / toNumber(min4(degree(e.source.id))(degree(e.target.id)));
      };
      if (strength instanceof LinkStrength) {
        return linkValues(strength.value0)(ends);
      }
      ;
      if (strength instanceof ByDegree) {
        return map20(byDegree)(ends);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 744, column 31 - line 746, column 32): " + [strength.constructor.name]);
    };
  };
  var getParams = function(s) {
    return s.params;
  };
  var getForces = function(s) {
    return s.forces;
  };
  var forceName = function(v) {
    if (v instanceof ManyBodyForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof CollideForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof LinkForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof CenterForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof PositionXForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof PositionYForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof RadialForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof TowardsForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof RectCollideForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof ContainForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof CohesionForce2) {
      return v.value0.name;
    }
    ;
    if (v instanceof DirectionalForce2) {
      return v.value0.name;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 385, column 13 - line 397, column 31): " + [v.constructor.name]);
  };
  var filtered = function(keep) {
    return function(strength) {
      if (keep instanceof Nothing) {
        return strength;
      }
      ;
      if (keep instanceof Just) {
        return function(node) {
          var $325 = keep.value0(node);
          if ($325) {
            return strength(node);
          }
          ;
          return 0;
        };
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 1040, column 26 - line 1042, column 71): " + [keep.constructor.name]);
    };
  };
  var recipe = function(v) {
    if (v instanceof ManyBodyForce2) {
      return function(random) {
        return function(nodes) {
          return function(v1) {
            return new Right(manyBody({
              strength: filtered(v.value0.filter)(valueOf(v.value0.strength)),
              theta: v.value0.theta,
              distanceMin: v.value0.distanceMin,
              distanceMax: v.value0.distanceMax,
              scope: (function() {
                if (v.value0.scope instanceof AllPairs) {
                  return everyPair;
                }
                ;
                if (v.value0.scope instanceof WithinGroups2) {
                  return withinGroups;
                }
                ;
                if (v.value0.scope instanceof BetweenGroups2) {
                  return betweenGroups;
                }
                ;
                throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 936, column 16 - line 939, column 56): " + [v.value0.scope.constructor.name]);
              })(),
              group: (function() {
                if (v.value0.scope instanceof AllPairs) {
                  return function(v2) {
                    return ungrouped;
                  };
                }
                ;
                if (v.value0.scope instanceof WithinGroups2) {
                  return membership(v.value0.scope.value0);
                }
                ;
                if (v.value0.scope instanceof BetweenGroups2) {
                  return membership(v.value0.scope.value0);
                }
                ;
                throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 940, column 16 - line 943, column 50): " + [v.value0.scope.constructor.name]);
              })()
            })(random)(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof CollideForce2) {
      return function(random) {
        return function(nodes) {
          return function(v1) {
            return new Right(collide({
              radius: valueOf(v.value0.radius),
              strength: v.value0.strength,
              iterations: v.value0.iterations,
              include: fromMaybe(function(v2) {
                return true;
              })(v.value0.filter),
              alphaScaled: v.value0.alphaScaled
            })(random)(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof LinkForce2) {
      return function(random) {
        return function(nodes) {
          return function(links) {
            return bind2(withEnds(nodes)(links))(function(ended2) {
              return linkById2(function(v1) {
                return v1.id;
              })({
                distance: function(l) {
                  return valueOf(v.value0.distance)(l.ends);
                },
                strength: (function() {
                  if (v.value0.strength instanceof ByDegree) {
                    return strengthFromDegree;
                  }
                  ;
                  if (v.value0.strength instanceof LinkStrength) {
                    return strengthPerLink(function(l) {
                      return valueOf(v.value0.strength.value0)(l.ends);
                    });
                  }
                  ;
                  throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 963, column 19 - line 965, column 84): " + [v.value0.strength.constructor.name]);
                })(),
                iterations: v.value0.iterations
              })(random)(nodes)(ended2);
            });
          };
        };
      };
    }
    ;
    if (v instanceof CenterForce2) {
      return function(v1) {
        return function(nodes) {
          return function(v2) {
            return new Right(center({
              x: v.value0.x,
              y: v.value0.y,
              strength: v.value0.strength
            })(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof PositionXForce2) {
      return function(v1) {
        return function(nodes) {
          return function(v2) {
            return new Right(positionX({
              target: valueOf(v.value0.target),
              strength: filtered(v.value0.filter)(valueOf(v.value0.strength))
            })(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof PositionYForce2) {
      return function(v1) {
        return function(nodes) {
          return function(v2) {
            return new Right(positionY({
              target: valueOf(v.value0.target),
              strength: filtered(v.value0.filter)(valueOf(v.value0.strength))
            })(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof RadialForce2) {
      return function(v1) {
        return function(nodes) {
          return function(v2) {
            return new Right(radial({
              radius: valueOf(v.value0.radius),
              strength: filtered(v.value0.filter)(valueOf(v.value0.strength)),
              x: v.value0.x,
              y: v.value0.y
            })(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof TowardsForce2) {
      return function(v1) {
        return function(nodes) {
          return function(v2) {
            return new Right(towards({
              target: function(node) {
                return fromMaybe({
                  x: nan,
                  y: nan
                })(v.value0.target(node));
              },
              strength: filtered(v.value0.filter)(valueOf(v.value0.strength))
            })(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof RectCollideForce2) {
      return function(v1) {
        return function(nodes) {
          return function(v2) {
            return new Right(rectCollide({
              halfWidth: valueOf(v.value0.halfWidth),
              halfHeight: valueOf(v.value0.halfHeight),
              strength: v.value0.strength,
              iterations: v.value0.iterations
            })(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof ContainForce2) {
      return function(v1) {
        return function(nodes) {
          return function(v2) {
            var regions2 = map20(regionOf)(v.value0.regions);
            return new Right(contain({
              region: function(node) {
                var v3 = bind1(v.value0.region(node))(index(regions2));
                if (v3 instanceof Just) {
                  return within(v3.value0);
                }
                ;
                if (v3 instanceof Nothing) {
                  return free;
                }
                ;
                throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 1007, column 28 - line 1009, column 40): " + [v3.constructor.name]);
              },
              radius: valueOf(v.value0.radius),
              strength: v.value0.strength,
              hard: v.value0.hard
            })(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof CohesionForce2) {
      return function(v1) {
        return function(nodes) {
          return function(v2) {
            return new Right(cohesion({
              group: membership(v.value0.group),
              strength: filtered(v.value0.filter)(valueOf(v.value0.strength))
            })(nodes));
          };
        };
      };
    }
    ;
    if (v instanceof DirectionalForce2) {
      return function(v1) {
        return function(nodes) {
          return function(links) {
            return bind2(withEnds(nodes)(links))(function(ended2) {
              return directionalById2(function(v2) {
                return v2.id;
              })({
                gap: function(l) {
                  return valueOf(v.value0.gap)(l.ends);
                },
                strength: function(l) {
                  return valueOf(v.value0.strength)(l.ends);
                },
                axis: v.value0.axis,
                iterations: v.value0.iterations
              })(nodes)(ended2);
            });
          };
        };
      };
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 929, column 10 - line 1030, column 12): " + [v.constructor.name]);
  };
  var tryApplySetup = function(setupConfig) {
    return function(sim) {
      return function __do2() {
        var nodes = read(sim.nodes)();
        var links = read(sim.links)();
        var ids2 = map20(function(l) {
          return {
            source: l.source,
            target: l.target
          };
        })(links);
        var made = traverse3(function(fc) {
          return map23(Tuple.create(forceName(fc)))(initializeForce(sim.random)(nodes)(ids2)(recipe(fc)));
        })(setupConfig.forces);
        if (made instanceof Left) {
          return new Left(made.value0);
        }
        ;
        if (made instanceof Right) {
          write(fromFoldable1(made.value0))(sim.forces)();
          write(setupConfig.params.alpha)(sim.alpha)();
          return new Right(unit);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 910, column 3 - line 919, column 24): " + [made.constructor.name]);
      };
    };
  };
  var defaultParams = {
    alpha: 1,
    alphaMin: 1e-3,
    alphaDecay: 0.01,
    alphaTarget: 0,
    velocityRetention: 0.4
  };
  var setup = function(name15) {
    return function(forces) {
      return {
        name: name15,
        forces,
        params: defaultParams
      };
    };
  };
  var computeNodeGUP = function(currentNodes) {
    return function(desiredNodes) {
      var desiredIds = fromFoldable32(map20(function(v) {
        return v.id;
      })(desiredNodes));
      var exited = filter(function(n) {
        return !member3(n.id)(desiredIds);
      })(currentNodes);
      var currentById = fromFoldable7(map20(function(n) {
        return new Tuple(n.id, n);
      })(currentNodes));
      var processDesired = function(desired) {
        var v = lookup2(desired.id)(currentById);
        if (v instanceof Nothing) {
          return {
            node: desired,
            isEnter: true
          };
        }
        ;
        if (v instanceof Just) {
          return {
            node: mergeSimulationState(v.value0)(desired),
            isEnter: false
          };
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 1231, column 7 - line 1237, column 73): " + [v.constructor.name]);
      };
      var processed = map20(processDesired)(desiredNodes);
      var entered = map20(function(v) {
        return v.node;
      })(filter(function(v) {
        return v.isEnter;
      })(processed));
      var merged = map20(function(v) {
        return v.node;
      })(processed);
      var updated = map20(function(v) {
        return v.node;
      })(filter(function($365) {
        return !(function(v) {
          return v.isEnter;
        })($365);
      })(processed));
      return {
        entered,
        updated,
        exited,
        merged
      };
    };
  };
  var computeLinkGUP = function(currentLinks) {
    return function(desiredLinks) {
      var linkKey = function(l) {
        return new Tuple(l.source, l.target);
      };
      var desiredKeys = fromFoldable42(map20(linkKey)(desiredLinks));
      var currentKeys = fromFoldable42(map20(linkKey)(currentLinks));
      var enterKeys = difference3(desiredKeys)(currentKeys);
      var entered = filter(function(l) {
        return member1(linkKey(l))(enterKeys);
      })(desiredLinks);
      var exitKeys = difference3(currentKeys)(desiredKeys);
      var exited = filter(function(l) {
        return member1(linkKey(l))(exitKeys);
      })(currentLinks);
      var updateKeys = intersection3(desiredKeys)(currentKeys);
      var updated = filter(function(l) {
        return member1(linkKey(l))(updateKeys);
      })(desiredLinks);
      return {
        entered,
        updated,
        exited
      };
    };
  };
  var center2 = function(name15) {
    return new CenterForce2({
      name: name15,
      x: 0,
      y: 0,
      strength: 1
    });
  };
  var applySetup = function(setupConfig) {
    return function(sim) {
      return function __do2() {
        var outcome = tryApplySetup(setupConfig)(sim)();
        if (outcome instanceof Left) {
          return $$throw("Hylograph.ForceEngine.Setup: " + (outcome.value0 + ". Nothing was applied."))();
        }
        ;
        if (outcome instanceof Right) {
          return unit;
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 890, column 3 - line 892, column 25): " + [outcome.constructor.name]);
      };
    };
  };
  var applySetupWithData = function(setupConfig) {
    return function(desiredNodes) {
      return function(desiredLinks) {
        return function(sim) {
          return function __do2() {
            var currentNodes = read(sim.nodes)();
            var currentLinks = read(sim.links)();
            var nodeResult = computeNodeGUP(currentNodes)(desiredNodes);
            write(nodeResult.merged)(sim.nodes)();
            initializeNodes2(nodeResult.merged)();
            var linkResult = computeLinkGUP(currentLinks)(desiredLinks);
            write(desiredLinks)(sim.links)();
            applySetup(setupConfig)(sim)();
            return {
              nodes: {
                entered: nodeResult.entered,
                updated: nodeResult.updated,
                exited: nodeResult.exited
              },
              links: {
                entered: linkResult.entered,
                updated: linkResult.updated,
                exited: linkResult.exited
              }
            };
          };
        };
      };
    };
  };

  // output/Hylograph.ForceEngine.Types/index.js
  var defaultSimParams = {
    alpha: 1,
    alphaMin: 1e-3,
    alphaDecay: 0.01,
    alphaTarget: 0,
    velocityRetention: 0.4
  };

  // output/Hylograph.ForceEngine.Simulation/index.js
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var for_3 = /* @__PURE__ */ for_(applicativeEffect);
  var for_1 = /* @__PURE__ */ for_3(foldableArray);
  var when11 = /* @__PURE__ */ when(applicativeEffect);
  var fromFoldable8 = /* @__PURE__ */ fromFoldable(foldableList);
  var invokeTickCallback = function(sim) {
    if (sim.callbacks instanceof Nothing) {
      return pure9(unit);
    }
    ;
    if (sim.callbacks instanceof Just) {
      return function __do2() {
        var callback = read(sim.callbacks.value0.onTick)();
        return callback();
      };
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 492, column 26 - line 496, column 13): " + [sim.callbacks.constructor.name]);
  };
  var getNodes = function(sim) {
    return read(sim.nodes);
  };
  var defaultConfig = /* @__PURE__ */ (function() {
    return {
      alphaMin: defaultSimParams.alphaMin,
      alphaDecay: defaultSimParams.alphaDecay,
      alphaTarget: defaultSimParams.alphaTarget,
      velocityRetention: defaultSimParams.velocityRetention
    };
  })();
  var create = function(config) {
    return function __do2() {
      var nodesRef = $$new([])();
      var linksRef = $$new([])();
      var forcesRef = $$new(empty2)();
      var alphaRef = $$new(1)();
      var prevAlphaRef = $$new(1)();
      var runningRef = $$new(false)();
      var cancelRef = $$new(pure9(unit))();
      var tickRef = $$new(pure9(unit))();
      var random = randomSource();
      return {
        nodes: nodesRef,
        links: linksRef,
        forces: forcesRef,
        random,
        alpha: alphaRef,
        prevAlpha: prevAlphaRef,
        config,
        running: runningRef,
        cancelAnimation: cancelRef,
        tickCallback: tickRef,
        callbacks: Nothing.value
      };
    };
  };
  var checkAlphaThresholds = function(prevAlpha) {
    return function(newAlpha) {
      return function(sim) {
        var crossedThreshold = function(prev) {
          return function($$new2) {
            return function(thresh) {
              return prev > thresh && $$new2 <= thresh;
            };
          };
        };
        var alphaThresholds = [0.5, 0.1, 0.01];
        if (sim.callbacks instanceof Nothing) {
          return pure9(unit);
        }
        ;
        if (sim.callbacks instanceof Just) {
          return for_1(alphaThresholds)(function(threshold) {
            return when11(crossedThreshold(prevAlpha)(newAlpha)(threshold))(function __do2() {
              var callback = read(sim.callbacks.value0.onAlphaThreshold)();
              return callback(newAlpha)();
            });
          });
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 505, column 47 - line 512, column 26): " + [sim.callbacks.constructor.name]);
      };
    };
  };
  var tick = function(sim) {
    return function __do2() {
      var nodes = read(sim.nodes)();
      var forces = read(sim.forces)();
      var alpha = read(sim.alpha)();
      var prevAlpha = read(sim.prevAlpha)();
      var forceHandles = fromFoldable8(values(forces));
      applyForces(forceHandles)(alpha)();
      integratePositions(nodes)(sim.config.velocityRetention)();
      constrain3(forceHandles)(nodes)();
      var newAlpha = decayAlpha(alpha)(sim.config.alphaMin)(sim.config.alphaDecay)(sim.config.alphaTarget);
      write(newAlpha)(sim.alpha)();
      write(alpha)(sim.prevAlpha)();
      var legacyCallback = read(sim.tickCallback)();
      legacyCallback();
      invokeTickCallback(sim)();
      checkAlphaThresholds(prevAlpha)(newAlpha)(sim)();
      return newAlpha;
    };
  };

  // output/Hylograph.ForceEngine.WASM/foreign.js
  var wasmModule = null;
  var WasmSimulation = null;
  function initWasm_(wasmUrl) {
    return function(onError, onSuccess) {
      (async function() {
        if (wasmModule) {
          console.log("WASM already initialized");
          return;
        }
        const wasm = await import(wasmUrl);
        await wasm.default();
        wasmModule = wasm;
        WasmSimulation = wasm.Simulation;
        console.log("WASM Force Engine initialized");
      })().then(
        // Success handler - EffectFnAff callbacks handle the thunk internally
        () => {
          onSuccess();
        },
        // Error handler - only for actual WASM load failures
        (err) => {
          onError(err);
        }
      );
      return function(cancelError, onCancelError, onCancelSuccess) {
        onCancelSuccess();
      };
    };
  }
  function createSimulation_(nodeCount2) {
    return function() {
      if (!WasmSimulation) {
        throw new Error("WASM not initialized. Call initWasm first.");
      }
      return new WasmSimulation(nodeCount2);
    };
  }
  function setNodeCount_(sim) {
    return function(count) {
      return function() {
        sim.set_node_count(count);
      };
    };
  }
  function setLinks_(sim) {
    return function(links) {
      return function() {
        sim.set_links(new Uint32Array(links));
      };
    };
  }
  function setAlpha_(sim) {
    return function(alpha) {
      return function() {
        sim.set_alpha(alpha);
      };
    };
  }
  function setAlphaDecay_(sim) {
    return function(decay2) {
      return function() {
        sim.set_alpha_decay(decay2);
      };
    };
  }
  function setVelocityRetention_(sim) {
    return function(retention) {
      return function() {
        sim.set_velocity_retention(retention);
      };
    };
  }
  function tick_(sim) {
    return function() {
      return sim.tick();
    };
  }
  function clearForces_(sim) {
    sim.clear_forces();
  }
  function addManyBody_(sim, strength, theta, distanceMin, distanceMax) {
    return sim.add_many_body(strength, theta, distanceMin, distanceMax);
  }
  function addLinks_(sim, distance, strength, iterations) {
    return sim.add_links(distance, strength, iterations);
  }
  function addCenter_(sim, x, y, strength) {
    return sim.add_center(x, y, strength);
  }
  function addForceX_(sim, target5, strength) {
    return sim.add_force_x(target5, strength);
  }
  function addForceY_(sim, target5, strength) {
    return sim.add_force_y(target5, strength);
  }
  function addCollide_(sim, radius, strength, iterations) {
    return sim.add_collide(radius, strength, iterations);
  }
  function addRadial_(sim, radius, x, y, strength) {
    return sim.add_radial(radius, x, y, strength);
  }
  function setForceStrengths_(sim, force, strengths) {
    return sim.set_force_strengths(force, Float32Array.from(strengths));
  }
  function setForceRadii_(sim, force, radii) {
    return sim.set_force_radii(force, Float32Array.from(radii));
  }
  function setForceTargets_(sim, force, targets) {
    return sim.set_force_targets(force, Float32Array.from(targets));
  }
  function setForceDistances_(sim, force, distances) {
    return sim.set_force_distances(force, Float32Array.from(distances));
  }
  function setForceMask_(sim, force, mask) {
    return sim.set_force_mask(force, Uint8Array.from(mask, (keep) => keep ? 1 : 0));
  }
  function enableForcesAll_(sim, manyBody3, links, center3, forceX, forceY, collide2) {
    sim.enable_forces_all(manyBody3, links, center3, forceX, forceY, collide2);
  }
  function setAlphaMin_(sim, alphaMin) {
    sim.set_alpha_min(alphaMin);
  }
  function setAlphaTarget_(sim, alphaTarget) {
    sim.set_alpha_target(alphaTarget);
  }
  function syncNodesToWasm_(sim, nodes) {
    const n = nodes.length;
    const positions = new Float32Array(n * 2);
    const velocities = new Float32Array(n * 2);
    const fixed = new Float32Array(n * 2);
    for (let i = 0; i < n; i++) {
      const node = nodes[i];
      positions[i * 2] = node.x;
      positions[i * 2 + 1] = node.y;
      velocities[i * 2] = node.vx;
      velocities[i * 2 + 1] = node.vy;
      fixed[i * 2] = node.fx == null ? NaN : node.fx;
      fixed[i * 2 + 1] = node.fy == null ? NaN : node.fy;
    }
    sim.set_positions(positions);
    sim.set_velocities(velocities);
    sim.set_fixed(fixed);
  }
  function syncNodesFromWasm_(sim, nodes) {
    const positions = sim.get_positions();
    const velocities = sim.get_velocities();
    const updated = new Array(nodes.length);
    for (let i = 0; i < nodes.length; i++) {
      updated[i] = {
        ...nodes[i],
        x: positions[i * 2],
        y: positions[i * 2 + 1],
        vx: velocities[i * 2],
        vy: velocities[i * 2 + 1]
      };
    }
    return updated;
  }
  var ids = (values2) => Uint32Array.from(values2, (v) => v < 0 ? 4294967295 : v);
  function addTowards_(sim, strength) {
    return sim.add_towards(strength);
  }
  function setForceTargetsXY_(sim, force, targets) {
    return sim.set_force_targets_xy(force, Float32Array.from(targets));
  }
  function addRectCollide_(sim, halfWidth, halfHeight, strength, iterations) {
    return sim.add_rect_collide(halfWidth, halfHeight, strength, iterations);
  }
  function setForceHalfWidths_(sim, force, halfWidths) {
    return sim.set_force_half_widths(force, Float32Array.from(halfWidths));
  }
  function setForceHalfHeights_(sim, force, halfHeights) {
    return sim.set_force_half_heights(force, Float32Array.from(halfHeights));
  }
  function addContainment_(sim, strength, hard) {
    return sim.add_containment(strength, hard);
  }
  function containmentAddRect_(sim, force, x0, y0, x1, y1) {
    return sim.containment_add_rect(force, x0, y0, x1, y1);
  }
  function containmentAddCircle_(sim, force, x, y, radius) {
    return sim.containment_add_circle(force, x, y, radius);
  }
  function containmentAddPolygon_(sim, force, vertices) {
    return sim.containment_add_polygon(force, Float32Array.from(vertices));
  }
  function setForceRegions_(sim, force, regions2) {
    return sim.set_force_regions(force, ids(regions2));
  }
  function addCohesion_(sim, strength) {
    return sim.add_cohesion(strength);
  }
  function setForceGroups_(sim, force, groups) {
    return sim.set_force_groups(force, ids(groups));
  }
  function setManyBodyScope_(sim, force, scope2) {
    return sim.set_many_body_scope(force, scope2);
  }
  function addDirectional_(sim, gap, axisX, axisY, strength, iterations) {
    return sim.add_directional(gap, axisX, axisY, strength, iterations);
  }
  function setForceGaps_(sim, force, gaps) {
    return sim.set_force_gaps(force, Float32Array.from(gaps));
  }
  function setCollideAlphaScaled_(sim, force, scaled) {
    return sim.set_collide_alpha_scaled(force, scaled);
  }
  function setCollideFilter_(sim, force, mask) {
    return sim.set_collide_filter(force, Uint8Array.from(mask, (keep) => keep ? 1 : 0));
  }

  // output/Effect.Uncurried/foreign.js
  var runEffectFn1 = function runEffectFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
  var runEffectFn2 = function runEffectFn22(fn) {
    return function(a) {
      return function(b) {
        return function() {
          return fn(a, b);
        };
      };
    };
  };
  var runEffectFn3 = function runEffectFn32(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function() {
            return fn(a, b, c);
          };
        };
      };
    };
  };

  // output/Effect.Aff.Compat/index.js
  var fromEffectFnAff = function(v) {
    return makeAff(function(k) {
      return function __do2() {
        var v1 = v(function($9) {
          return k(Left.create($9))();
        }, function($10) {
          return k(Right.create($10))();
        });
        return function(e) {
          return makeAff(function(k2) {
            return function __do3() {
              v1(e, function($11) {
                return k2(Left.create($11))();
              }, function($12) {
                return k2(Right.create($12))();
              });
              return nonCanceler;
            };
          });
        };
      };
    });
  };

  // output/Hylograph.ForceEngine.WASM/index.js
  var map21 = /* @__PURE__ */ map(functorArray);
  var AllPairs2 = /* @__PURE__ */ (function() {
    function AllPairs3() {
    }
    ;
    AllPairs3.value = new AllPairs3();
    return AllPairs3;
  })();
  var WithinGroups3 = /* @__PURE__ */ (function() {
    function WithinGroups4() {
    }
    ;
    WithinGroups4.value = new WithinGroups4();
    return WithinGroups4;
  })();
  var BetweenGroups3 = /* @__PURE__ */ (function() {
    function BetweenGroups4() {
    }
    ;
    BetweenGroups4.value = new BetweenGroups4();
    return BetweenGroups4;
  })();
  var tick2 = tick_;
  var syncNodesToWasm = /* @__PURE__ */ runEffectFn2(syncNodesToWasm_);
  var syncNodesFromWasm = /* @__PURE__ */ runEffectFn2(syncNodesFromWasm_);
  var setVelocityRetention = setVelocityRetention_;
  var setNodeCount = setNodeCount_;
  var setManyBodyScope = function(sim) {
    return function(force) {
      return function(scope2) {
        return function() {
          return setManyBodyScope_(sim, force, (function() {
            if (scope2 instanceof AllPairs2) {
              return 0;
            }
            ;
            if (scope2 instanceof WithinGroups3) {
              return 1;
            }
            ;
            if (scope2 instanceof BetweenGroups3) {
              return 2;
            }
            ;
            throw new Error("Failed pattern match at Hylograph.ForceEngine.WASM (line 570, column 77 - line 573, column 21): " + [scope2.constructor.name]);
          })());
        };
      };
    };
  };
  var setLinks = setLinks_;
  var setForceTargetsXY = /* @__PURE__ */ runEffectFn3(setForceTargetsXY_);
  var setForceTargets = /* @__PURE__ */ runEffectFn3(setForceTargets_);
  var setForceStrengths = /* @__PURE__ */ runEffectFn3(setForceStrengths_);
  var setForceRadii = /* @__PURE__ */ runEffectFn3(setForceRadii_);
  var setForceMask = /* @__PURE__ */ runEffectFn3(setForceMask_);
  var setForceHalfWidths = /* @__PURE__ */ runEffectFn3(setForceHalfWidths_);
  var setForceHalfHeights = /* @__PURE__ */ runEffectFn3(setForceHalfHeights_);
  var setForceGaps = /* @__PURE__ */ runEffectFn3(setForceGaps_);
  var setForceDistances = /* @__PURE__ */ runEffectFn3(setForceDistances_);
  var setCollideFilter = /* @__PURE__ */ runEffectFn3(setCollideFilter_);
  var setCollideAlphaScaled = /* @__PURE__ */ runEffectFn3(setCollideAlphaScaled_);
  var setAlphaTarget = /* @__PURE__ */ runEffectFn2(setAlphaTarget_);
  var setAlphaMin = /* @__PURE__ */ runEffectFn2(setAlphaMin_);
  var setAlphaDecay = setAlphaDecay_;
  var setAlpha = setAlpha_;
  var orNone = function(v) {
    if (v instanceof Just && v.value0 >= 0) {
      return v.value0;
    }
    ;
    return -1 | 0;
  };
  var setForceGroups = function(sim) {
    return function(force) {
      return function(groups) {
        return function() {
          return setForceGroups_(sim, force, map21(orNone)(groups));
        };
      };
    };
  };
  var setForceRegions = function(sim) {
    return function(force) {
      return function(regions2) {
        return function() {
          return setForceRegions_(sim, force, map21(orNone)(regions2));
        };
      };
    };
  };
  var initWasm = function(url2) {
    return fromEffectFnAff(initWasm_(url2));
  };
  var enableForcesAll = function(sim) {
    return function(cfg) {
      return function() {
        return enableForcesAll_(sim, cfg.manyBody, cfg.links, cfg.center, cfg.forceX, cfg.forceY, cfg.collide);
      };
    };
  };
  var create2 = createSimulation_;
  var containmentAddRect = function(sim) {
    return function(force) {
      return function(r) {
        return function() {
          return containmentAddRect_(sim, force, r.x0, r.y0, r.x1, r.y1);
        };
      };
    };
  };
  var containmentAddPolygon = function(sim) {
    return function(force) {
      return function(vertices) {
        return function() {
          return containmentAddPolygon_(sim, force, concatMap(function(v) {
            return [v.x, v.y];
          })(vertices));
        };
      };
    };
  };
  var containmentAddCircle = function(sim) {
    return function(force) {
      return function(c) {
        return function() {
          return containmentAddCircle_(sim, force, c.x, c.y, c.radius);
        };
      };
    };
  };
  var clearForces = /* @__PURE__ */ runEffectFn1(clearForces_);
  var addTowards = /* @__PURE__ */ runEffectFn2(addTowards_);
  var addRectCollide = function(sim) {
    return function(cfg) {
      return function() {
        return addRectCollide_(sim, cfg.halfWidth, cfg.halfHeight, cfg.strength, cfg.iterations);
      };
    };
  };
  var addRadial = function(sim) {
    return function(cfg) {
      return function() {
        return addRadial_(sim, cfg.radius, cfg.x, cfg.y, cfg.strength);
      };
    };
  };
  var addManyBody = function(sim) {
    return function(cfg) {
      return function() {
        return addManyBody_(sim, cfg.strength, cfg.theta, cfg.distanceMin, cfg.distanceMax);
      };
    };
  };
  var addLinks = function(sim) {
    return function(cfg) {
      return function() {
        return addLinks_(sim, cfg.distance, cfg.strength, cfg.iterations);
      };
    };
  };
  var addForceY = function(sim) {
    return function(cfg) {
      return function() {
        return addForceY_(sim, cfg.target, cfg.strength);
      };
    };
  };
  var addForceX = function(sim) {
    return function(cfg) {
      return function() {
        return addForceX_(sim, cfg.target, cfg.strength);
      };
    };
  };
  var addDirectional = function(sim) {
    return function(cfg) {
      return function() {
        return addDirectional_(sim, cfg.gap, cfg.axis.x, cfg.axis.y, cfg.strength, cfg.iterations);
      };
    };
  };
  var addContainment = function(sim) {
    return function(cfg) {
      return function() {
        return addContainment_(sim, cfg.strength, cfg.hard);
      };
    };
  };
  var addCollide = function(sim) {
    return function(cfg) {
      return function() {
        return addCollide_(sim, cfg.radius, cfg.strength, cfg.iterations);
      };
    };
  };
  var addCohesion = /* @__PURE__ */ runEffectFn2(addCohesion_);
  var addCenter = function(sim) {
    return function(cfg) {
      return function() {
        return addCenter_(sim, cfg.x, cfg.y, cfg.strength);
      };
    };
  };

  // output/Hylograph.ForceEngine.Setup.WASM/index.js
  var pure10 = /* @__PURE__ */ pure(applicativeEffect);
  var fromFoldable9 = /* @__PURE__ */ fromFoldable2(ordInt)(foldableArray);
  var apply13 = /* @__PURE__ */ apply(applyMaybe);
  var map24 = /* @__PURE__ */ map(functorMaybe);
  var lookup3 = /* @__PURE__ */ lookup(ordInt);
  var when12 = /* @__PURE__ */ when(applicativeEffect);
  var show3 = /* @__PURE__ */ show(showInt);
  var $$void5 = /* @__PURE__ */ $$void(functorEffect);
  var map111 = /* @__PURE__ */ map(functorArray);
  var fromFoldable12 = /* @__PURE__ */ fromFoldable(foldableList);
  var fromFoldable23 = /* @__PURE__ */ fromFoldable2(ordString)(foldableArray);
  var nub2 = /* @__PURE__ */ nub(ordInt);
  var bind12 = /* @__PURE__ */ bind(bindMaybe);
  var for_4 = /* @__PURE__ */ for_(applicativeEffect);
  var for_12 = /* @__PURE__ */ for_4(foldableMaybe);
  var for_22 = /* @__PURE__ */ for_4(foldableArray);
  var traverse4 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var map25 = /* @__PURE__ */ map(functorEffect);
  var tick3 = function(sim) {
    return function __do2() {
      var alpha = tick2(sim.wasmSim)();
      var nodes = read(sim.nodesRef)();
      var updatedNodes = syncNodesFromWasm(sim.wasmSim)(nodes)();
      write(updatedNodes)(sim.nodesRef)();
      return alpha;
    };
  };
  var setLinks2 = function(wasmSim) {
    return function(nodes) {
      return function(links) {
        var indexOfId = fromFoldable9(mapWithIndex2(function(index3) {
          return function(node) {
            return new Tuple(node.id, index3);
          };
        })(nodes));
        var endpoints = function(link) {
          return apply13(map24(function(v) {
            return function(v1) {
              return [v, v1];
            };
          })(lookup3(link.source)(indexOfId)))(lookup3(link.target)(indexOfId));
        };
        var resolved = mapMaybe(endpoints)(links);
        var missing = length(links) - length(resolved) | 0;
        return function __do2() {
          when12(missing > 0)(warn("WASM Setup: " + (show3(missing) + " link(s) name a node id that is not in the simulation; left out")))();
          return setLinks(wasmSim)(concat(resolved))();
        };
      };
    };
  };
  var initWasm2 = initWasm;
  var inApplicationOrder = function(forces) {
    return fromFoldable12(values(fromFoldable23(map111(function(fc) {
      return new Tuple(forceName(fc), fc);
    })(forces))));
  };
  var getNodes2 = function(sim) {
    return function __do2() {
      var nodes = read(sim.nodesRef)();
      return syncNodesFromWasm(sim.wasmSim)(nodes)();
    };
  };
  var flatTargets = function(target5) {
    return concatMap(function(node) {
      var v = target5(node);
      if (v instanceof Just) {
        return [v.value0.x, v.value0.y];
      }
      ;
      if (v instanceof Nothing) {
        return [nan, nan];
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 426, column 47 - line 428, column 26): " + [v.constructor.name]);
    });
  };
  var create3 = function(nodes) {
    return function(links) {
      return function __do2() {
        var wasmSim = create2(length(nodes))();
        initializeNodes2(nodes)();
        syncNodesToWasm(wasmSim)(nodes)();
        setLinks2(wasmSim)(nodes)(links)();
        var nodesRef = $$new(nodes)();
        var linksRef = $$new(links)();
        var forceIds = $$new(empty2)();
        return {
          wasmSim,
          nodesRef,
          linksRef,
          forceIds
        };
      };
    };
  };
  var addForce = function(wasmSim) {
    return function(nodes) {
      return function(ends) {
        var perNode = function(value12) {
          return function(send) {
            if (value12 instanceof Static) {
              return pure10(unit);
            }
            ;
            if (value12 instanceof Dynamic) {
              return $$void5(send(map111(value12.value0)(nodes)));
            }
            ;
            throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 399, column 24 - line 401, column 43): " + [value12.constructor.name]);
          };
        };
        var perLink = function(value12) {
          return function(send) {
            if (value12 instanceof Static) {
              return pure10(unit);
            }
            ;
            if (value12 instanceof Dynamic) {
              return $$void5(send(linkValues(value12)(ends)));
            }
            ;
            throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 405, column 24 - line 407, column 53): " + [value12.constructor.name]);
          };
        };
        var groupsOf = function(group3) {
          var given = map111(group3)(nodes);
          var numbering = fromFoldable9(mapWithIndex2(function(index3) {
            return function(g) {
              return new Tuple(g, index3);
            };
          })(nub2(catMaybes(given))));
          return map111(function(v) {
            return bind12(v)(function(g) {
              return lookup3(g)(numbering);
            });
          })(given);
        };
        var filtered2 = function(filter3) {
          return function(force) {
            return for_12(filter3)(function(keep) {
              return $$void5(setForceMask(wasmSim)(force)(map111(keep)(nodes)));
            });
          };
        };
        return function(v) {
          if (v instanceof ManyBodyForce2) {
            return function __do2() {
              var force = addManyBody(wasmSim)({
                strength: valueToNumber(v.value0.strength),
                theta: v.value0.theta,
                distanceMin: v.value0.distanceMin,
                distanceMax: v.value0.distanceMax
              })();
              perNode(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              filtered2(v.value0.filter)(force)();
              (function() {
                if (v.value0.scope instanceof AllPairs) {
                  return unit;
                }
                ;
                if (v.value0.scope instanceof WithinGroups2) {
                  $$void5(setForceGroups(wasmSim)(force)(groupsOf(v.value0.scope.value0)))();
                  return $$void5(setManyBodyScope(wasmSim)(force)(WithinGroups3.value))();
                }
                ;
                if (v.value0.scope instanceof BetweenGroups2) {
                  $$void5(setForceGroups(wasmSim)(force)(groupsOf(v.value0.scope.value0)))();
                  return $$void5(setManyBodyScope(wasmSim)(force)(BetweenGroups3.value))();
                }
                ;
                throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 278, column 5 - line 285, column 68): " + [v.value0.scope.constructor.name]);
              })();
              return force;
            };
          }
          ;
          if (v instanceof CollideForce2) {
            return function __do2() {
              var force = addCollide(wasmSim)({
                radius: valueToNumber(v.value0.radius),
                strength: v.value0.strength,
                iterations: v.value0.iterations
              })();
              perNode(v.value0.radius)(setForceRadii(wasmSim)(force))();
              for_12(v.value0.filter)(function(keep) {
                return $$void5(setCollideFilter(wasmSim)(force)(map111(keep)(nodes)));
              })();
              when12(v.value0.alphaScaled)($$void5(setCollideAlphaScaled(wasmSim)(force)(true)))();
              return force;
            };
          }
          ;
          if (v instanceof LinkForce2) {
            return function __do2() {
              var force = addLinks(wasmSim)({
                distance: valueToNumber(v.value0.distance),
                strength: 1,
                iterations: v.value0.iterations
              })();
              perLink(v.value0.distance)(setForceDistances(wasmSim)(force))();
              (function() {
                if (v.value0.strength instanceof LinkStrength && v.value0.strength.value0 instanceof Static) {
                  return $$void5(setForceStrengths(wasmSim)(force)(map111($$const(v.value0.strength.value0.value0))(ends)))();
                }
                ;
                return $$void5(setForceStrengths(wasmSim)(force)(linkStrengths(v.value0.strength)(ends)))();
              })();
              return force;
            };
          }
          ;
          if (v instanceof CenterForce2) {
            return addCenter(wasmSim)({
              x: v.value0.x,
              y: v.value0.y,
              strength: v.value0.strength
            });
          }
          ;
          if (v instanceof PositionXForce2) {
            return function __do2() {
              var force = addForceX(wasmSim)({
                target: valueToNumber(v.value0.target),
                strength: valueToNumber(v.value0.strength)
              })();
              perNode(v.value0.target)(setForceTargets(wasmSim)(force))();
              perNode(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              filtered2(v.value0.filter)(force)();
              return force;
            };
          }
          ;
          if (v instanceof PositionYForce2) {
            return function __do2() {
              var force = addForceY(wasmSim)({
                target: valueToNumber(v.value0.target),
                strength: valueToNumber(v.value0.strength)
              })();
              perNode(v.value0.target)(setForceTargets(wasmSim)(force))();
              perNode(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              filtered2(v.value0.filter)(force)();
              return force;
            };
          }
          ;
          if (v instanceof RadialForce2) {
            return function __do2() {
              var force = addRadial(wasmSim)({
                radius: valueToNumber(v.value0.radius),
                x: v.value0.x,
                y: v.value0.y,
                strength: valueToNumber(v.value0.strength)
              })();
              perNode(v.value0.radius)(setForceRadii(wasmSim)(force))();
              perNode(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              filtered2(v.value0.filter)(force)();
              return force;
            };
          }
          ;
          if (v instanceof TowardsForce2) {
            return function __do2() {
              var force = addTowards(wasmSim)(valueToNumber(v.value0.strength))();
              $$void5(setForceTargetsXY(wasmSim)(force)(flatTargets(v.value0.target)(nodes)))();
              perNode(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              filtered2(v.value0.filter)(force)();
              return force;
            };
          }
          ;
          if (v instanceof RectCollideForce2) {
            return function __do2() {
              var force = addRectCollide(wasmSim)({
                halfWidth: valueToNumber(v.value0.halfWidth),
                halfHeight: valueToNumber(v.value0.halfHeight),
                strength: v.value0.strength,
                iterations: v.value0.iterations
              })();
              perNode(v.value0.halfWidth)(setForceHalfWidths(wasmSim)(force))();
              perNode(v.value0.halfHeight)(setForceHalfHeights(wasmSim)(force))();
              return force;
            };
          }
          ;
          if (v instanceof ContainForce2) {
            return function __do2() {
              var force = addContainment(wasmSim)({
                strength: v.value0.strength,
                hard: v.value0.hard
              })();
              for_22(v.value0.regions)(function(v1) {
                if (v1 instanceof RectRegion) {
                  return $$void5(containmentAddRect(wasmSim)(force)(v1.value0));
                }
                ;
                if (v1 instanceof CircleRegion) {
                  return $$void5(containmentAddCircle(wasmSim)(force)(v1.value0));
                }
                ;
                if (v1 instanceof PolygonRegion) {
                  return $$void5(containmentAddPolygon(wasmSim)(force)(v1.value0));
                }
                ;
                throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 368, column 20 - line 371, column 88): " + [v1.constructor.name]);
              })();
              $$void5(setForceRegions(wasmSim)(force)(map111(v.value0.region)(nodes)))();
              $$void5(setForceRadii(wasmSim)(force)((function() {
                if (v.value0.radius instanceof Static) {
                  return map111($$const(v.value0.radius.value0))(nodes);
                }
                ;
                if (v.value0.radius instanceof Dynamic) {
                  return map111(v.value0.radius.value0)(nodes);
                }
                ;
                throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 373, column 45 - line 375, column 31): " + [v.value0.radius.constructor.name]);
              })()))();
              return force;
            };
          }
          ;
          if (v instanceof CohesionForce2) {
            return function __do2() {
              var force = addCohesion(wasmSim)(valueToNumber(v.value0.strength))();
              $$void5(setForceGroups(wasmSim)(force)(groupsOf(v.value0.group)))();
              perNode(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              filtered2(v.value0.filter)(force)();
              return force;
            };
          }
          ;
          if (v instanceof DirectionalForce2) {
            return function __do2() {
              var force = addDirectional(wasmSim)({
                gap: valueToNumber(v.value0.gap),
                axis: v.value0.axis,
                strength: valueToNumber(v.value0.strength),
                iterations: v.value0.iterations
              })();
              perLink(v.value0.gap)(setForceGaps(wasmSim)(force))();
              perLink(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              return force;
            };
          }
          ;
          throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 268, column 31 - line 394, column 15): " + [v.constructor.name]);
        };
      };
    };
  };
  var applySetup2 = function(setupConfig) {
    return function(sim) {
      return function __do2() {
        var nodes = read(sim.nodesRef)();
        var links = read(sim.linksRef)();
        var ends = resolveLinks(nodes)(links);
        enableForcesAll(sim.wasmSim)({
          manyBody: false,
          links: false,
          center: false,
          forceX: false,
          forceY: false,
          collide: false
        })();
        clearForces(sim.wasmSim)();
        var ids2 = traverse4(function(fc) {
          return map25(Tuple.create(forceName(fc)))(addForce(sim.wasmSim)(nodes)(ends)(fc));
        })(inApplicationOrder(getForces(setupConfig)))();
        write(fromFoldable23(ids2))(sim.forceIds)();
        var params = getParams(setupConfig);
        setAlpha(sim.wasmSim)(params.alpha)();
        setAlphaMin(sim.wasmSim)(params.alphaMin)();
        setAlphaDecay(sim.wasmSim)(params.alphaDecay)();
        setAlphaTarget(sim.wasmSim)(params.alphaTarget)();
        return setVelocityRetention(sim.wasmSim)(params.velocityRetention)();
      };
    };
  };
  var applySetupWithData2 = function(setupConfig) {
    return function(desiredNodes) {
      return function(desiredLinks) {
        return function(sim) {
          return function __do2() {
            var currentNodes = read(sim.nodesRef)();
            var currentLinks = read(sim.linksRef)();
            var nodeResult = computeNodeGUP(currentNodes)(desiredNodes);
            write(nodeResult.merged)(sim.nodesRef)();
            var currentCount = length(currentNodes);
            var newCount = length(nodeResult.merged);
            when12(currentCount !== newCount)(setNodeCount(sim.wasmSim)(newCount))();
            initializeNodes2(nodeResult.merged)();
            syncNodesToWasm(sim.wasmSim)(nodeResult.merged)();
            var linkResult = computeLinkGUP(currentLinks)(desiredLinks);
            write(desiredLinks)(sim.linksRef)();
            setLinks2(sim.wasmSim)(nodeResult.merged)(desiredLinks)();
            applySetup2(setupConfig)(sim)();
            return {
              nodes: {
                entered: nodeResult.entered,
                updated: nodeResult.updated,
                exited: nodeResult.exited
              },
              links: {
                entered: linkResult.entered,
                updated: linkResult.updated,
                exited: linkResult.exited
              }
            };
          };
        };
      };
    };
  };

  // output/Hylograph.Behavior.Types/index.js
  var OnHover = /* @__PURE__ */ (function() {
    function OnHover2() {
    }
    ;
    OnHover2.value = new OnHover2();
    return OnHover2;
  })();
  var WhenPrimary = /* @__PURE__ */ (function() {
    function WhenPrimary2() {
    }
    ;
    WhenPrimary2.value = new WhenPrimary2();
    return WhenPrimary2;
  })();
  var WhenRelated = /* @__PURE__ */ (function() {
    function WhenRelated2() {
    }
    ;
    WhenRelated2.value = new WhenRelated2();
    return WhenRelated2;
  })();
  var Primary = /* @__PURE__ */ (function() {
    function Primary3() {
    }
    ;
    Primary3.value = new Primary3();
    return Primary3;
  })();
  var Related = /* @__PURE__ */ (function() {
    function Related3() {
    }
    ;
    Related3.value = new Related3();
    return Related3;
  })();
  var Upstream = /* @__PURE__ */ (function() {
    function Upstream2() {
    }
    ;
    Upstream2.value = new Upstream2();
    return Upstream2;
  })();
  var Downstream = /* @__PURE__ */ (function() {
    function Downstream2() {
    }
    ;
    Downstream2.value = new Downstream2();
    return Downstream2;
  })();
  var Dimmed = /* @__PURE__ */ (function() {
    function Dimmed3() {
    }
    ;
    Dimmed3.value = new Dimmed3();
    return Dimmed3;
  })();
  var Neutral = /* @__PURE__ */ (function() {
    function Neutral3() {
    }
    ;
    Neutral3.value = new Neutral3();
    return Neutral3;
  })();
  var SimpleDrag = /* @__PURE__ */ (function() {
    function SimpleDrag2() {
    }
    ;
    SimpleDrag2.value = new SimpleDrag2();
    return SimpleDrag2;
  })();
  var SimulationDrag = /* @__PURE__ */ (function() {
    function SimulationDrag2(value0) {
      this.value0 = value0;
    }
    ;
    SimulationDrag2.create = function(value0) {
      return new SimulationDrag2(value0);
    };
    return SimulationDrag2;
  })();
  var SimulationDragNested = /* @__PURE__ */ (function() {
    function SimulationDragNested2(value0) {
      this.value0 = value0;
    }
    ;
    SimulationDragNested2.create = function(value0) {
      return new SimulationDragNested2(value0);
    };
    return SimulationDragNested2;
  })();

  // output/Hylograph.HATS/index.js
  var show4 = /* @__PURE__ */ show(showNumber);
  var DepthFirst = /* @__PURE__ */ (function() {
    function DepthFirst2() {
    }
    ;
    DepthFirst2.value = new DepthFirst2();
    return DepthFirst2;
  })();
  var BreadthFirst = /* @__PURE__ */ (function() {
    function BreadthFirst2() {
    }
    ;
    BreadthFirst2.value = new BreadthFirst2();
    return BreadthFirst2;
  })();
  var ThunkedMouseEnter = /* @__PURE__ */ (function() {
    function ThunkedMouseEnter2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedMouseEnter2.create = function(value0) {
      return new ThunkedMouseEnter2(value0);
    };
    return ThunkedMouseEnter2;
  })();
  var ThunkedMouseLeave = /* @__PURE__ */ (function() {
    function ThunkedMouseLeave2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedMouseLeave2.create = function(value0) {
      return new ThunkedMouseLeave2(value0);
    };
    return ThunkedMouseLeave2;
  })();
  var ThunkedClick = /* @__PURE__ */ (function() {
    function ThunkedClick2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedClick2.create = function(value0) {
      return new ThunkedClick2(value0);
    };
    return ThunkedClick2;
  })();
  var ThunkedClickWithModifier = /* @__PURE__ */ (function() {
    function ThunkedClickWithModifier2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ThunkedClickWithModifier2.create = function(value0) {
      return function(value1) {
        return new ThunkedClickWithModifier2(value0, value1);
      };
    };
    return ThunkedClickWithModifier2;
  })();
  var ThunkedDrag = /* @__PURE__ */ (function() {
    function ThunkedDrag2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedDrag2.create = function(value0) {
      return new ThunkedDrag2(value0);
    };
    return ThunkedDrag2;
  })();
  var ThunkedZoom = /* @__PURE__ */ (function() {
    function ThunkedZoom2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedZoom2.create = function(value0) {
      return new ThunkedZoom2(value0);
    };
    return ThunkedZoom2;
  })();
  var ThunkedCoordinatedHighlight = /* @__PURE__ */ (function() {
    function ThunkedCoordinatedHighlight2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedCoordinatedHighlight2.create = function(value0) {
      return new ThunkedCoordinatedHighlight2(value0);
    };
    return ThunkedCoordinatedHighlight2;
  })();
  var ThunkedCoordinatedInteraction = /* @__PURE__ */ (function() {
    function ThunkedCoordinatedInteraction2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedCoordinatedInteraction2.create = function(value0) {
      return new ThunkedCoordinatedInteraction2(value0);
    };
    return ThunkedCoordinatedInteraction2;
  })();
  var ThunkedBrush = /* @__PURE__ */ (function() {
    function ThunkedBrush2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedBrush2.create = function(value0) {
      return new ThunkedBrush2(value0);
    };
    return ThunkedBrush2;
  })();
  var ThunkedPointerDown = /* @__PURE__ */ (function() {
    function ThunkedPointerDown2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkedPointerDown2.create = function(value0) {
      return new ThunkedPointerDown2(value0);
    };
    return ThunkedPointerDown2;
  })();
  var FromArray = /* @__PURE__ */ (function() {
    function FromArray2(value0) {
      this.value0 = value0;
    }
    ;
    FromArray2.create = function(value0) {
      return new FromArray2(value0);
    };
    return FromArray2;
  })();
  var FromTree = /* @__PURE__ */ (function() {
    function FromTree2(value0) {
      this.value0 = value0;
    }
    ;
    FromTree2.create = function(value0) {
      return new FromTree2(value0);
    };
    return FromTree2;
  })();
  var WithContext = /* @__PURE__ */ (function() {
    function WithContext2(value0) {
      this.value0 = value0;
    }
    ;
    WithContext2.create = function(value0) {
      return new WithContext2(value0);
    };
    return WithContext2;
  })();
  var StaticAttr = /* @__PURE__ */ (function() {
    function StaticAttr2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    StaticAttr2.create = function(value0) {
      return function(value1) {
        return new StaticAttr2(value0, value1);
      };
    };
    return StaticAttr2;
  })();
  var ThunkedAttr = /* @__PURE__ */ (function() {
    function ThunkedAttr2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ThunkedAttr2.create = function(value0) {
      return function(value1) {
        return new ThunkedAttr2(value0, value1);
      };
    };
    return ThunkedAttr2;
  })();
  var Siblings = /* @__PURE__ */ (function() {
    function Siblings2() {
    }
    ;
    Siblings2.value = new Siblings2();
    return Siblings2;
  })();
  var Elem = /* @__PURE__ */ (function() {
    function Elem2(value0) {
      this.value0 = value0;
    }
    ;
    Elem2.create = function(value0) {
      return new Elem2(value0);
    };
    return Elem2;
  })();
  var MkFold = /* @__PURE__ */ (function() {
    function MkFold2(value0) {
      this.value0 = value0;
    }
    ;
    MkFold2.create = function(value0) {
      return new MkFold2(value0);
    };
    return MkFold2;
  })();
  var Empty2 = /* @__PURE__ */ (function() {
    function Empty4() {
    }
    ;
    Empty4.value = new Empty4();
    return Empty4;
  })();
  var thunkedNum = function(name15) {
    return function(value12) {
      return new ThunkedAttr(name15, function(v) {
        return show4(value12);
      });
    };
  };
  var staticStr = function(name15) {
    return function(value12) {
      return new StaticAttr(name15, value12);
    };
  };
  var runSomeFold = function(v) {
    return function(k) {
      return v(k);
    };
  };
  var mkSomeFold = function(spec) {
    return function(k) {
      return k(spec);
    };
  };
  var forEach = function(name15) {
    return function(elementType) {
      return function(items) {
        return function(keyFn) {
          return function(template) {
            return new MkFold(mkSomeFold({
              name: name15,
              elementType,
              enumerate: new FromArray(items),
              assemble: Siblings.value,
              keyFn,
              template,
              gup: Nothing.value
            }));
          };
        };
      };
    };
  };
  var elem2 = function(elemType) {
    return function(attrs) {
      return function(children2) {
        return new Elem({
          elemType,
          attrs,
          children: children2,
          behaviors: []
        });
      };
    };
  };

  // output/Hylograph.Interaction.Zoom/foreign.js
  var ZoomTransform = class _ZoomTransform {
    constructor(k, x, y) {
      this.k = k;
      this.x = x;
      this.y = y;
    }
    /**
     * Returns a new transform scaled by k
     */
    scale(k) {
      return k === 1 ? this : new _ZoomTransform(this.k * k, this.x, this.y);
    }
    /**
     * Returns a new transform translated by (x, y) in local coordinates
     */
    translate(x, y) {
      return x === 0 && y === 0 ? this : new _ZoomTransform(this.k, this.x + this.k * x, this.y + this.k * y);
    }
    /**
     * Apply transform to a point [x, y] -> [x', y']
     */
    apply(point) {
      return [point[0] * this.k + this.x, point[1] * this.k + this.y];
    }
    /**
     * Apply inverse transform to get original coordinates
     */
    applyX(x) {
      return x * this.k + this.x;
    }
    applyY(y) {
      return y * this.k + this.y;
    }
    /**
     * Invert: given transformed point, return original point
     */
    invert(point) {
      return [(point[0] - this.x) / this.k, (point[1] - this.y) / this.k];
    }
    invertX(x) {
      return (x - this.x) / this.k;
    }
    invertY(y) {
      return (y - this.y) / this.k;
    }
    /**
     * Rescale a scale's domain according to this transform
     */
    rescaleX(x) {
      return x.copy().domain(x.range().map(this.invertX, this).map(x.invert, x));
    }
    rescaleY(y) {
      return y.copy().domain(y.range().map(this.invertY, this).map(y.invert, y));
    }
    toString() {
      return `translate(${this.x},${this.y}) scale(${this.k})`;
    }
  };
  var identity5 = new ZoomTransform(1, 0, 0);
  function constrain4(transform, extent, translateExtent) {
    const dx0 = transform.invertX(extent[0][0]) - translateExtent[0][0];
    const dx1 = transform.invertX(extent[1][0]) - translateExtent[1][0];
    const dy0 = transform.invertY(extent[0][1]) - translateExtent[0][1];
    const dy1 = transform.invertY(extent[1][1]) - translateExtent[1][1];
    let x = transform.x;
    let y = transform.y;
    if (dx1 > dx0) {
      x += (dx0 + dx1) / 2 * transform.k;
    } else if (dx0 < 0) {
      x -= dx0 * transform.k;
    } else if (dx1 > 0) {
      x -= dx1 * transform.k;
    }
    if (dy1 > dy0) {
      y += (dy0 + dy1) / 2 * transform.k;
    } else if (dy0 < 0) {
      y -= dy0 * transform.k;
    } else if (dy1 > 0) {
      y -= dy1 * transform.k;
    }
    return x === transform.x && y === transform.y ? transform : new ZoomTransform(transform.k, x, y);
  }
  function zoomAround(transform, point, newK) {
    const p = transform.invert(point);
    const t = transform.scale(newK / transform.k);
    return new ZoomTransform(
      t.k,
      point[0] - p[0] * t.k,
      point[1] - p[1] * t.k
    );
  }
  function attachNativeZoom_(element) {
    return (config) => () => {
      const scaleMin = config.scaleMin || 0.1;
      const scaleMax = config.scaleMax || 10;
      const targetSelector = config.targetSelector;
      let transform = config.initialTransform ? new ZoomTransform(
        config.initialTransform.k,
        config.initialTransform.x,
        config.initialTransform.y
      ) : identity5;
      let extent = null;
      function getExtent() {
        if (!extent) {
          const rect = element.getBoundingClientRect();
          extent = [[0, 0], [rect.width, rect.height]];
        }
        return extent;
      }
      function toSVGCoords(clientX2, clientY2) {
        const svg = element.ownerSVGElement || element;
        if (svg.createSVGPoint) {
          const pt = svg.createSVGPoint();
          pt.x = clientX2;
          pt.y = clientY2;
          const ctm = svg.getScreenCTM();
          if (ctm) {
            const inv = ctm.inverse();
            const svgPt = pt.matrixTransform(inv);
            return [svgPt.x, svgPt.y];
          }
        }
        const rect = element.getBoundingClientRect();
        return [clientX2 - rect.left, clientY2 - rect.top];
      }
      function toSVGDelta(screenDx, screenDy) {
        const svg = element.ownerSVGElement || element;
        if (svg.getScreenCTM) {
          const ctm = svg.getScreenCTM();
          if (ctm) {
            const scale = ctm.a;
            return [screenDx / scale, screenDy / scale];
          }
        }
        return [screenDx, screenDy];
      }
      let translateExtent = config.translateExtent || null;
      const infiniteExtent = [[-Infinity, -Infinity], [Infinity, Infinity]];
      function applyTransform() {
        const target5 = element.querySelector(targetSelector);
        if (target5) {
          target5.setAttribute("transform", transform.toString());
        }
        element.__zoom = transform;
        if (config.onZoom) {
          config.onZoom({ k: transform.k, x: transform.x, y: transform.y })();
        }
      }
      function setTransform(newTransform) {
        const k = Math.max(scaleMin, Math.min(scaleMax, newTransform.k));
        newTransform = new ZoomTransform(k, newTransform.x, newTransform.y);
        if (translateExtent) {
          newTransform = constrain4(newTransform, getExtent(), translateExtent);
        }
        transform = newTransform;
        applyTransform();
      }
      function handleWheel(event) {
        event.preventDefault();
        const point = toSVGCoords(event.clientX, event.clientY);
        const factor = Math.pow(2, -event.deltaY * 2e-3);
        const newK = Math.max(scaleMin, Math.min(scaleMax, transform.k * factor));
        if (Math.abs(newK - transform.k) < 1e-3) return;
        const newTransform = zoomAround(transform, point, newK);
        setTransform(newTransform);
      }
      let isPanning = false;
      let panStart = null;
      let transformAtPanStart = null;
      function handlePointerDown(event) {
        if (event.button !== 0) return;
        const target5 = event.target;
        const isPanTarget = target5 === element || // SVG element itself
        target5.tagName === "svg" || target5.classList.contains("zoom-background") || target5.classList.contains("zoom-group");
        if (!isPanTarget) {
          return;
        }
        isPanning = true;
        panStart = [event.clientX, event.clientY];
        transformAtPanStart = transform;
        element.setPointerCapture(event.pointerId);
        element.style.cursor = "grabbing";
      }
      function handlePointerMove(event) {
        if (!isPanning) return;
        const screenDx = event.clientX - panStart[0];
        const screenDy = event.clientY - panStart[1];
        const [dx, dy] = toSVGDelta(screenDx, screenDy);
        const newTransform = new ZoomTransform(
          transformAtPanStart.k,
          transformAtPanStart.x + dx,
          transformAtPanStart.y + dy
        );
        setTransform(newTransform);
      }
      function handlePointerUp(event) {
        if (!isPanning) return;
        isPanning = false;
        element.releasePointerCapture(event.pointerId);
        element.style.cursor = "grab";
      }
      let gestureStartTransform = null;
      let gestureStartCenter = null;
      function handleGestureStart(event) {
        event.preventDefault();
        gestureStartTransform = transform;
        const rect = element.getBoundingClientRect();
        const screenCenterX = rect.left + rect.width / 2;
        const screenCenterY = rect.top + rect.height / 2;
        gestureStartCenter = toSVGCoords(screenCenterX, screenCenterY);
      }
      function handleGestureChange(event) {
        event.preventDefault();
        if (!gestureStartTransform) return;
        const newK = gestureStartTransform.k * event.scale;
        const newTransform = zoomAround(gestureStartTransform, gestureStartCenter, newK);
        setTransform(newTransform);
      }
      function handleGestureEnd(event) {
        event.preventDefault();
        gestureStartTransform = null;
      }
      element.addEventListener("wheel", handleWheel, { passive: false });
      element.addEventListener("pointerdown", handlePointerDown);
      element.addEventListener("pointermove", handlePointerMove);
      element.addEventListener("pointerup", handlePointerUp);
      element.addEventListener("pointercancel", handlePointerUp);
      element.addEventListener("gesturestart", handleGestureStart);
      element.addEventListener("gesturechange", handleGestureChange);
      element.addEventListener("gestureend", handleGestureEnd);
      element.style.cursor = "grab";
      element.style.touchAction = "none";
      if (config.initialTransform) {
        applyTransform();
      }
      const handle = {
        // Get current transform
        getTransform: () => ({ k: transform.k, x: transform.x, y: transform.y }),
        // Set transform directly
        setTransform: (t) => () => {
          setTransform(new ZoomTransform(t.k, t.x, t.y));
        },
        // Reset to identity
        resetZoom: () => {
          setTransform(identity5);
        },
        // Zoom to specific scale at point
        zoomTo: (k) => (point) => () => {
          const newTransform = zoomAround(transform, [point.x, point.y], k);
          setTransform(newTransform);
        },
        // Zoom in/out by factor at center
        zoomBy: (factor) => () => {
          const rect = element.getBoundingClientRect();
          const screenCenterX = rect.left + rect.width / 2;
          const screenCenterY = rect.top + rect.height / 2;
          const center3 = toSVGCoords(screenCenterX, screenCenterY);
          const newK = transform.k * factor;
          const newTransform = zoomAround(transform, center3, newK);
          setTransform(newTransform);
        },
        // Cleanup
        destroy: () => {
          element.removeEventListener("wheel", handleWheel);
          element.removeEventListener("pointerdown", handlePointerDown);
          element.removeEventListener("pointermove", handlePointerMove);
          element.removeEventListener("pointerup", handlePointerUp);
          element.removeEventListener("pointercancel", handlePointerUp);
          element.removeEventListener("gesturestart", handleGestureStart);
          element.removeEventListener("gesturechange", handleGestureChange);
          element.removeEventListener("gestureend", handleGestureEnd);
          element.style.cursor = "";
          element.style.touchAction = "";
        }
      };
      return handle;
    };
  }
  function attachZoomNative_(element) {
    return (scaleMin) => (scaleMax) => (targetSelector) => () => {
      return attachNativeZoom_(element)({
        scaleMin,
        scaleMax,
        targetSelector,
        initialTransform: null,
        translateExtent: null,
        onZoom: null
      })();
    };
  }

  // output/Hylograph.HATS.InterpreterTick/foreign.js
  var selectElement = (selector) => (doc) => () => {
    return doc.querySelector(selector);
  };
  var setAttribute = (el) => (name15) => (value12) => () => {
    if (name15 === "textContent") {
      el.textContent = value12;
    } else if (name15.startsWith("xlink:")) {
      el.setAttributeNS("http://www.w3.org/1999/xlink", name15, value12);
    } else if (name15.startsWith("xml:")) {
      el.setAttributeNS("http://www.w3.org/XML/1998/namespace", name15, value12);
    } else {
      el.setAttribute(name15, value12);
    }
  };
  var getAttribute = (el) => (name15) => () => {
    if (name15 === "textContent") {
      return el.textContent || "";
    }
    return el.getAttribute(name15) || "";
  };
  var bindDatum = (el) => (datum) => () => {
    el.__data__ = datum;
  };
  var setKey = (el) => (key) => () => {
    el.setAttribute("data-hats-key", key);
  };
  var setFoldName = (el) => (foldName) => () => {
    el.setAttribute("data-hats-fold", foldName);
  };
  var getKey = (el) => () => {
    return el.getAttribute("data-hats-key") || "";
  };
  var getChildElements = (parent2) => () => {
    return Array.from(parent2.children);
  };
  var getChildElementsForFold = (parent2) => (tagName2) => (foldName) => () => {
    const children2 = Array.from(parent2.children);
    return children2.filter(
      (el) => el.tagName.toLowerCase() === tagName2 && el.getAttribute("data-hats-fold") === foldName
    );
  };
  var removeElement = (el) => () => {
    if (el.parentNode) {
      el.parentNode.removeChild(el);
    }
  };
  var setCursorPointer = (el) => () => {
    el.style.cursor = "pointer";
  };
  var attachMouseEnterThunked = (el) => (handler) => () => {
    el.addEventListener("mouseenter", function(event) {
      handler()();
    });
  };
  var attachMouseLeaveThunked = (el) => (handler) => () => {
    el.addEventListener("mouseleave", function(event) {
      handler()();
    });
  };
  var attachClickThunked = (el) => (handler) => () => {
    el.addEventListener("click", function(event) {
      event.stopPropagation();
      handler()();
    });
    el.style.cursor = "pointer";
  };
  var attachPointerDownThunked = (el) => (handler) => () => {
    el.addEventListener("pointerdown", function(event) {
      event.stopPropagation();
      handler()();
    });
  };
  var attachZoom = (el) => (scaleMin) => (scaleMax) => (targetSelector) => () => {
    attachZoomNative_(el)(scaleMin)(scaleMax)(targetSelector)();
  };
  var attachSimpleDrag = (el) => () => {
    attachSimpleDrag_(el)()();
  };
  var attachSimulationDragById = (el) => (simId) => () => {
    attachSimulationDragById_(el)(simId)();
  };
  var attachSimulationDragNestedById = (el) => (simId) => () => {
    attachSimulationDragNestedById_(el)(simId)();
  };
  var hatsHighlightRegistry = /* @__PURE__ */ new Map();
  var HIGHLIGHT_PRIMARY = "highlight-primary";
  var HIGHLIGHT_RELATED = "highlight-related";
  var HIGHLIGHT_DIMMED = "highlight-dimmed";
  var HIGHLIGHT_UPSTREAM = "highlight-upstream";
  var HIGHLIGHT_DOWNSTREAM = "highlight-downstream";
  var ALL_HIGHLIGHT_CLASSES = [HIGHLIGHT_PRIMARY, HIGHLIGHT_RELATED, HIGHLIGHT_DIMMED, HIGHLIGHT_UPSTREAM, HIGHLIGHT_DOWNSTREAM];
  var HC_PRIMARY = 0;
  var HC_RELATED = 1;
  var HC_DIMMED = 2;
  var HC_NEUTRAL = 3;
  var HC_UPSTREAM = 4;
  var HC_DOWNSTREAM = 5;
  function getHatsHighlightGroup(groupName) {
    const key = groupName || "_global";
    if (!hatsHighlightRegistry.has(key)) {
      hatsHighlightRegistry.set(key, []);
    }
    return hatsHighlightRegistry.get(key);
  }
  var TT_ON_HOVER = 0;
  var TT_WHEN_PRIMARY = 1;
  var TT_WHEN_RELATED = 2;
  var hatsTooltipContainer = null;
  var hatsElementTooltips = /* @__PURE__ */ new Map();
  function getHatsTooltipContainer() {
    if (!hatsTooltipContainer) {
      hatsTooltipContainer = document.createElement("div");
      hatsTooltipContainer.className = "hats-tooltip-container";
      hatsTooltipContainer.style.cssText = "position: fixed; top: 0; left: 0; pointer-events: none; z-index: 10000;";
      document.body.appendChild(hatsTooltipContainer);
    }
    return hatsTooltipContainer;
  }
  function createHatsTooltip() {
    const tooltip = document.createElement("div");
    tooltip.className = "hats-tooltip";
    tooltip.style.cssText = `
    position: absolute;
    background: rgba(45, 45, 45, 0.85);
    color: #f0f0f0;
    padding: 10px 16px;
    font-size: 13px;
    font-weight: 500;
    font-family: 'Courier New', Courier, monospace;
    white-space: pre-line;
    pointer-events: none;
    opacity: 0;
    transition: opacity 0.15s ease-in-out;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
    border: none;
    border-radius: 4px;
    min-width: 280px;
    max-width: 480px;
    line-height: 1.6;
    backdrop-filter: blur(8px);
    -webkit-backdrop-filter: blur(8px);
  `;
    getHatsTooltipContainer().appendChild(tooltip);
    return tooltip;
  }
  function positionHatsTooltip(tooltip, event) {
    const vw = window.innerWidth;
    const vh = window.innerHeight;
    const tooltipWidth = tooltip.offsetWidth || 300;
    const tooltipHeight = tooltip.offsetHeight || 40;
    let x = event.clientX + 15;
    if (x + tooltipWidth > vw - 10) {
      x = event.clientX - tooltipWidth - 15;
    }
    let y = event.clientY - 10;
    if (y + tooltipHeight > vh - 10) {
      y = vh - tooltipHeight - 10;
    }
    if (y < 10) y = 10;
    tooltip.style.left = `${x}px`;
    tooltip.style.top = `${y}px`;
  }
  function showHatsTooltip(tooltip, content3) {
    tooltip.textContent = content3;
    tooltip.style.opacity = "1";
  }
  function hideHatsTooltip(tooltip) {
    if (tooltip) {
      tooltip.style.opacity = "0";
    }
  }
  var attachCoordinatedHighlightThunked = (element) => (identifyThunk) => (classifyFn) => (groupName) => (tooltipContentThunk) => (tooltipTrigger) => (tooltipBorderColor) => () => {
    const group3 = groupName;
    const entry = { element, identifyThunk, classifyFn, tooltipContentThunk, tooltipTrigger, tooltipBorderColor };
    getHatsHighlightGroup(group3).push(entry);
    element.addEventListener("mouseenter", function(event) {
      const hoveredId = identifyThunk();
      applyHatsHighlightsWithTooltips(group3, hoveredId, element, event);
    });
    element.addEventListener("mouseleave", function(event) {
      clearHatsHighlightsInGroupWithTooltips(group3);
    });
  };
  function applyHatsHighlightsWithTooltips(groupName, hoveredId, triggerElement, event) {
    const group3 = getHatsHighlightGroup(groupName);
    group3.forEach((entry) => {
      const { element, classifyFn, tooltipContentThunk, tooltipTrigger } = entry;
      ALL_HIGHLIGHT_CLASSES.forEach((cls) => element.classList.remove(cls));
      const classification = classifyFn(hoveredId);
      switch (classification) {
        case HC_PRIMARY:
          element.classList.add("highlight-primary");
          break;
        case HC_RELATED:
          element.classList.add("highlight-related");
          break;
        case HC_UPSTREAM:
          element.classList.add("highlight-upstream");
          break;
        case HC_DOWNSTREAM:
          element.classList.add("highlight-downstream");
          break;
        case HC_DIMMED:
          element.classList.add("highlight-dimmed");
          break;
        case HC_NEUTRAL:
        default:
          break;
      }
      if (tooltipContentThunk) {
        const shouldShow = tooltipTrigger === TT_ON_HOVER && element === triggerElement || tooltipTrigger === TT_WHEN_PRIMARY && classification === HC_PRIMARY || tooltipTrigger === TT_WHEN_RELATED && (classification === HC_PRIMARY || classification === HC_RELATED);
        if (shouldShow) {
          let tooltip = hatsElementTooltips.get(element);
          if (!tooltip) {
            tooltip = createHatsTooltip();
            hatsElementTooltips.set(element, tooltip);
          }
          const content3 = tooltipContentThunk();
          showHatsTooltip(tooltip, content3);
          if (entry.tooltipBorderColor) {
            tooltip.style.borderLeft = `3px solid ${entry.tooltipBorderColor}`;
          } else {
            tooltip.style.borderLeft = "none";
          }
          if (tooltipTrigger === TT_ON_HOVER && event) {
            positionHatsTooltip(tooltip, event);
          }
        } else {
          const tooltip = hatsElementTooltips.get(element);
          if (tooltip) {
            hideHatsTooltip(tooltip);
          }
        }
      }
    });
  }
  function clearHatsHighlightsInGroupWithTooltips(groupName) {
    const group3 = getHatsHighlightGroup(groupName);
    group3.forEach((entry) => {
      ALL_HIGHLIGHT_CLASSES.forEach((cls) => entry.element.classList.remove(cls));
      const tooltip = hatsElementTooltips.get(entry.element);
      if (tooltip) {
        hideHatsTooltip(tooltip);
      }
    });
  }
  var coordInteractionRegistry = /* @__PURE__ */ new Map();
  var IS_PRIMARY = 0;
  var IS_RELATED = 1;
  var IS_SELECTED = 2;
  var IS_DIMMED = 3;
  var IS_NEUTRAL = 4;
  var COORD_PRIMARY = "coord-primary";
  var COORD_RELATED = "coord-related";
  var COORD_SELECTED = "coord-selected";
  var COORD_DIMMED = "coord-dimmed";
  var ALL_COORD_CLASSES = [COORD_PRIMARY, COORD_RELATED, COORD_SELECTED, COORD_DIMMED];
  function getCoordInteractionGroup(groupName) {
    const key = groupName || "_global";
    if (!coordInteractionRegistry.has(key)) {
      coordInteractionRegistry.set(key, []);
    }
    return coordInteractionRegistry.get(key);
  }
  function applyStateClass(element, state3) {
    ALL_COORD_CLASSES.forEach((cls) => element.classList.remove(cls));
    switch (state3) {
      case IS_PRIMARY:
        element.classList.add(COORD_PRIMARY);
        break;
      case IS_RELATED:
        element.classList.add(COORD_RELATED);
        break;
      case IS_SELECTED:
        element.classList.add(COORD_SELECTED);
        break;
      case IS_DIMMED:
        element.classList.add(COORD_DIMMED);
        break;
      case IS_NEUTRAL:
      default:
        break;
    }
  }
  function applyHoverInteraction(groupName, hoveredId) {
    const group3 = getCoordInteractionGroup(groupName);
    group3.forEach((entry) => {
      const state3 = entry.respondToHover(hoveredId);
      applyStateClass(entry.element, state3);
    });
  }
  function applyBrushInteraction(groupName, box, sourceCell) {
    const group3 = getCoordInteractionGroup(groupName);
    const selectedIds = /* @__PURE__ */ new Set();
    group3.forEach((entry) => {
      if (entry.getPosition) {
        const pos = entry.getPosition();
        const elementCell = entry.element.closest('g[id^="cell-"]');
        if (elementCell === sourceCell) {
          if (pos.x >= box.x0 && pos.x <= box.x1 && pos.y >= box.y0 && pos.y <= box.y1) {
            const id2 = entry.identifyThunk();
            selectedIds.add(id2);
          }
        }
      }
    });
    group3.forEach((entry) => {
      const id2 = entry.identifyThunk();
      let state3;
      if (selectedIds.size === 0) {
        state3 = IS_NEUTRAL;
      } else if (selectedIds.has(id2)) {
        state3 = IS_SELECTED;
      } else {
        state3 = IS_DIMMED;
      }
      applyStateClass(entry.element, state3);
    });
  }
  function applyClearInteraction(groupName) {
    const group3 = getCoordInteractionGroup(groupName);
    group3.forEach((entry) => {
      const state3 = entry.respondToClear();
      applyStateClass(entry.element, state3);
    });
  }
  var attachCoordinatedInteractionThunked = (element) => (identifyThunk) => (respondToHover) => (respondToBrush) => (respondToClear) => (positionThunk) => (groupName) => () => {
    const group3 = groupName;
    const getPosition = positionThunk && positionThunk.value0 ? positionThunk.value0 : null;
    const entry = { element, identifyThunk, respondToHover, respondToBrush, respondToClear, getPosition };
    getCoordInteractionGroup(group3).push(entry);
    element.addEventListener("mouseenter", function(event) {
      const hoveredId = identifyThunk();
      applyHoverInteraction(group3, hoveredId);
    });
    element.addEventListener("mouseleave", function(event) {
      applyClearInteraction(group3);
    });
  };
  var activeBrushes = /* @__PURE__ */ new Map();
  var attachCoordinatedBrushThunked = (element) => (extent) => (groupName) => () => {
    const group3 = groupName;
    let brushRect = null;
    function getBrushRect() {
      if (!brushRect) {
        brushRect = element.querySelector(".brush-background");
        if (brushRect) {
          brushRect.style.fill = "transparent";
          brushRect.style.stroke = "transparent";
          brushRect.style.strokeWidth = "1";
        }
      }
      return brushRect;
    }
    const ns = "http://www.w3.org/2000/svg";
    const selectionRect = document.createElementNS(ns, "rect");
    selectionRect.setAttribute("class", "brush-selection-overlay");
    selectionRect.setAttribute("fill", "rgba(100, 100, 200, 0.2)");
    selectionRect.setAttribute("stroke", "#6666cc");
    selectionRect.setAttribute("stroke-width", "1");
    selectionRect.setAttribute("visibility", "hidden");
    selectionRect.setAttribute("pointer-events", "none");
    const cellGroup = element.parentElement;
    if (cellGroup) {
      cellGroup.appendChild(selectionRect);
    }
    let isDragging = false;
    let startX = 0;
    let startY = 0;
    let lastBrushTime = 0;
    let pendingBrushFrame = null;
    const BRUSH_THROTTLE_MS = 150;
    function getLocalCoords(event) {
      const svg = element.closest("svg");
      if (!svg) return { x: event.clientX, y: event.clientY };
      const pt = svg.createSVGPoint();
      pt.x = event.clientX;
      pt.y = event.clientY;
      const parent2 = element.parentElement;
      const ctm = parent2 && parent2.getScreenCTM ? parent2.getScreenCTM() : svg.getScreenCTM();
      if (!ctm) return { x: event.clientX, y: event.clientY };
      const localP = pt.matrixTransform(ctm.inverse());
      return { x: localP.x, y: localP.y };
    }
    element.addEventListener("pointerdown", function(event) {
      if (event.button !== 0) return;
      const rect = getBrushRect();
      if (event.target !== element && event.target !== rect) return;
      isDragging = true;
      const coords2 = getLocalCoords(event);
      startX = Math.max(extent.x0, Math.min(extent.x1, coords2.x));
      startY = Math.max(extent.y0, Math.min(extent.y1, coords2.y));
      selectionRect.setAttribute("x", startX);
      selectionRect.setAttribute("y", startY);
      selectionRect.setAttribute("width", 0);
      selectionRect.setAttribute("height", 0);
      selectionRect.setAttribute("visibility", "visible");
      element.setPointerCapture(event.pointerId);
      event.preventDefault();
    });
    element.addEventListener("pointermove", function(event) {
      if (!isDragging) return;
      const coords2 = getLocalCoords(event);
      const currentX = Math.max(extent.x0, Math.min(extent.x1, coords2.x));
      const currentY = Math.max(extent.y0, Math.min(extent.y1, coords2.y));
      const x = Math.min(startX, currentX);
      const y = Math.min(startY, currentY);
      const width8 = Math.abs(currentX - startX);
      const height8 = Math.abs(currentY - startY);
      selectionRect.setAttribute("x", x);
      selectionRect.setAttribute("y", y);
      selectionRect.setAttribute("width", width8);
      selectionRect.setAttribute("height", height8);
      const box = { x0: x, y0: y, x1: x + width8, y1: y + height8 };
      const sourceCell = element.closest('g[id^="cell-"]');
      const now = performance.now();
      if (now - lastBrushTime >= BRUSH_THROTTLE_MS) {
        lastBrushTime = now;
        if (pendingBrushFrame) {
          cancelAnimationFrame(pendingBrushFrame);
          pendingBrushFrame = null;
        }
        applyBrushInteraction(group3, box, sourceCell);
      } else if (!pendingBrushFrame) {
        pendingBrushFrame = requestAnimationFrame(() => {
          pendingBrushFrame = null;
          lastBrushTime = performance.now();
          applyBrushInteraction(group3, box, sourceCell);
        });
      }
    });
    element.addEventListener("pointerup", function(event) {
      if (!isDragging) return;
      isDragging = false;
      element.releasePointerCapture(event.pointerId);
    });
    element.addEventListener("click", function(event) {
      const rect = getBrushRect();
      if (event.target !== element && event.target !== rect) return;
      selectionRect.setAttribute("visibility", "hidden");
      applyClearInteraction(group3);
    });
    const handle = { element, selectionRect, group: group3 };
    if (!activeBrushes.has(group3)) {
      activeBrushes.set(group3, []);
    }
    activeBrushes.get(group3).push(handle);
  };

  // output/Hylograph.Transition.Tick/index.js
  var linear = function(t) {
    return t;
  };
  var elasticC5 = /* @__PURE__ */ (function() {
    return 2 * pi / 4.5;
  })();
  var elasticC4 = /* @__PURE__ */ (function() {
    return 2 * pi / 3;
  })();
  var easeOutSin = function(t) {
    return sin(t * pi / 2);
  };
  var easeOutQuad = function(t) {
    return 1 - (1 - t) * (1 - t);
  };
  var easeOutExp = function(t) {
    var $21 = t === 1;
    if ($21) {
      return 1;
    }
    ;
    return 1 - pow(2)(-10 * t);
  };
  var easeOutElastic = function(t) {
    var $22 = t === 0;
    if ($22) {
      return 0;
    }
    ;
    var $23 = t === 1;
    if ($23) {
      return 1;
    }
    ;
    return pow(2)(-10 * t) * sin((t * 10 - 0.75) * elasticC4) + 1;
  };
  var easeOutCubic = function(t) {
    return 1 - (1 - t) * (1 - t) * (1 - t);
  };
  var easeOutCircle = function(t) {
    return sqrt(1 - (t - 1) * (t - 1));
  };
  var easeInSin = function(t) {
    return 1 - cos(t * pi / 2);
  };
  var easeInQuad = function(t) {
    return t * t;
  };
  var easeInOutSin = function(t) {
    return -(cos(pi * t) - 1) / 2;
  };
  var easeInOutQuad = function(t) {
    var $24 = t < 0.5;
    if ($24) {
      return 2 * t * t;
    }
    ;
    return 1 - (-2 * t + 2) * (-2 * t + 2) / 2;
  };
  var easeInOutExp = function(t) {
    var $25 = t === 0;
    if ($25) {
      return 0;
    }
    ;
    var $26 = t === 1;
    if ($26) {
      return 1;
    }
    ;
    var $27 = t < 0.5;
    if ($27) {
      return pow(2)(20 * t - 10) / 2;
    }
    ;
    return (2 - pow(2)(-20 * t + 10)) / 2;
  };
  var easeInOutElastic = function(t) {
    var $28 = t === 0;
    if ($28) {
      return 0;
    }
    ;
    var $29 = t === 1;
    if ($29) {
      return 1;
    }
    ;
    var $30 = t < 0.5;
    if ($30) {
      return -(pow(2)(20 * t - 10) * sin((20 * t - 11.125) * elasticC5)) / 2;
    }
    ;
    return pow(2)(-20 * t + 10) * sin((20 * t - 11.125) * elasticC5) / 2 + 1;
  };
  var easeInOutCubic = function(t) {
    var $31 = t < 0.5;
    if ($31) {
      return 4 * t * t * t;
    }
    ;
    return 1 - (-2 * t + 2) * (-2 * t + 2) * (-2 * t + 2) / 2;
  };
  var easeInOutCircle = function(t) {
    var $32 = t < 0.5;
    if ($32) {
      return (1 - sqrt(1 - 2 * t * (2 * t))) / 2;
    }
    ;
    return (sqrt(1 - (-2 * t + 2) * (-2 * t + 2)) + 1) / 2;
  };
  var easeInExp = function(t) {
    var $33 = t === 0;
    if ($33) {
      return 0;
    }
    ;
    return pow(2)(10 * (t - 1));
  };
  var easeInElastic = function(t) {
    var $34 = t === 0;
    if ($34) {
      return 0;
    }
    ;
    var $35 = t === 1;
    if ($35) {
      return 1;
    }
    ;
    return -pow(2)(10 * t - 10) * sin((t * 10 - 10.75) * elasticC4);
  };
  var easeInCubic = function(t) {
    return t * t * t;
  };
  var easeInCircle = function(t) {
    return 1 - sqrt(1 - t * t);
  };
  var bounceN1 = 7.5625;
  var bounceD1 = 2.75;
  var easeOutBounce = function(t) {
    var $36 = t < 1 / bounceD1;
    if ($36) {
      return bounceN1 * t * t;
    }
    ;
    var $37 = t < 2 / bounceD1;
    if ($37) {
      var t$prime = t - 1.5 / bounceD1;
      return bounceN1 * t$prime * t$prime + 0.75;
    }
    ;
    var $38 = t < 2.5 / bounceD1;
    if ($38) {
      var t$prime = t - 2.25 / bounceD1;
      return bounceN1 * t$prime * t$prime + 0.9375;
    }
    ;
    var t$prime = t - 2.625 / bounceD1;
    return bounceN1 * t$prime * t$prime + 0.984375;
  };
  var easeInBounce = function(t) {
    return 1 - easeOutBounce(1 - t);
  };
  var easeInOutBounce = function(t) {
    var $39 = t < 0.5;
    if ($39) {
      return (1 - easeOutBounce(1 - 2 * t)) / 2;
    }
    ;
    return (1 + easeOutBounce(2 * t - 1)) / 2;
  };
  var backC1 = 1.70158;
  var backC2 = /* @__PURE__ */ (function() {
    return backC1 * 1.525;
  })();
  var easeInOutBack = function(t) {
    var $40 = t < 0.5;
    if ($40) {
      return 2 * t * (2 * t) * ((backC2 + 1) * 2 * t - backC2) / 2;
    }
    ;
    return ((2 * t - 2) * (2 * t - 2) * ((backC2 + 1) * (t * 2 - 2) + backC2) + 2) / 2;
  };
  var backC3 = /* @__PURE__ */ (function() {
    return backC1 + 1;
  })();
  var easeInBack = function(t) {
    return backC3 * t * t * t - backC1 * t * t;
  };
  var easeOutBack = function(t) {
    var t$prime = t - 1;
    return 1 + backC3 * t$prime * t$prime * t$prime + backC1 * t$prime * t$prime;
  };

  // output/Hylograph.Transition.Types/index.js
  var Linear = /* @__PURE__ */ (function() {
    function Linear2() {
    }
    ;
    Linear2.value = new Linear2();
    return Linear2;
  })();
  var Cubic = /* @__PURE__ */ (function() {
    function Cubic2() {
    }
    ;
    Cubic2.value = new Cubic2();
    return Cubic2;
  })();
  var CubicIn = /* @__PURE__ */ (function() {
    function CubicIn2() {
    }
    ;
    CubicIn2.value = new CubicIn2();
    return CubicIn2;
  })();
  var CubicOut = /* @__PURE__ */ (function() {
    function CubicOut2() {
    }
    ;
    CubicOut2.value = new CubicOut2();
    return CubicOut2;
  })();
  var CubicInOut = /* @__PURE__ */ (function() {
    function CubicInOut2() {
    }
    ;
    CubicInOut2.value = new CubicInOut2();
    return CubicInOut2;
  })();
  var Quad = /* @__PURE__ */ (function() {
    function Quad2() {
    }
    ;
    Quad2.value = new Quad2();
    return Quad2;
  })();
  var QuadIn = /* @__PURE__ */ (function() {
    function QuadIn2() {
    }
    ;
    QuadIn2.value = new QuadIn2();
    return QuadIn2;
  })();
  var QuadOut = /* @__PURE__ */ (function() {
    function QuadOut3() {
    }
    ;
    QuadOut3.value = new QuadOut3();
    return QuadOut3;
  })();
  var QuadInOut = /* @__PURE__ */ (function() {
    function QuadInOut2() {
    }
    ;
    QuadInOut2.value = new QuadInOut2();
    return QuadInOut2;
  })();
  var Sin = /* @__PURE__ */ (function() {
    function Sin2() {
    }
    ;
    Sin2.value = new Sin2();
    return Sin2;
  })();
  var SinIn = /* @__PURE__ */ (function() {
    function SinIn2() {
    }
    ;
    SinIn2.value = new SinIn2();
    return SinIn2;
  })();
  var SinOut = /* @__PURE__ */ (function() {
    function SinOut2() {
    }
    ;
    SinOut2.value = new SinOut2();
    return SinOut2;
  })();
  var SinInOut = /* @__PURE__ */ (function() {
    function SinInOut2() {
    }
    ;
    SinInOut2.value = new SinInOut2();
    return SinInOut2;
  })();
  var Exp = /* @__PURE__ */ (function() {
    function Exp2() {
    }
    ;
    Exp2.value = new Exp2();
    return Exp2;
  })();
  var ExpIn = /* @__PURE__ */ (function() {
    function ExpIn2() {
    }
    ;
    ExpIn2.value = new ExpIn2();
    return ExpIn2;
  })();
  var ExpOut = /* @__PURE__ */ (function() {
    function ExpOut2() {
    }
    ;
    ExpOut2.value = new ExpOut2();
    return ExpOut2;
  })();
  var ExpInOut = /* @__PURE__ */ (function() {
    function ExpInOut2() {
    }
    ;
    ExpInOut2.value = new ExpInOut2();
    return ExpInOut2;
  })();
  var Circle3 = /* @__PURE__ */ (function() {
    function Circle4() {
    }
    ;
    Circle4.value = new Circle4();
    return Circle4;
  })();
  var CircleIn = /* @__PURE__ */ (function() {
    function CircleIn2() {
    }
    ;
    CircleIn2.value = new CircleIn2();
    return CircleIn2;
  })();
  var CircleOut = /* @__PURE__ */ (function() {
    function CircleOut2() {
    }
    ;
    CircleOut2.value = new CircleOut2();
    return CircleOut2;
  })();
  var CircleInOut = /* @__PURE__ */ (function() {
    function CircleInOut2() {
    }
    ;
    CircleInOut2.value = new CircleInOut2();
    return CircleInOut2;
  })();
  var Elastic = /* @__PURE__ */ (function() {
    function Elastic2() {
    }
    ;
    Elastic2.value = new Elastic2();
    return Elastic2;
  })();
  var ElasticIn = /* @__PURE__ */ (function() {
    function ElasticIn2() {
    }
    ;
    ElasticIn2.value = new ElasticIn2();
    return ElasticIn2;
  })();
  var ElasticOut = /* @__PURE__ */ (function() {
    function ElasticOut2() {
    }
    ;
    ElasticOut2.value = new ElasticOut2();
    return ElasticOut2;
  })();
  var ElasticInOut = /* @__PURE__ */ (function() {
    function ElasticInOut2() {
    }
    ;
    ElasticInOut2.value = new ElasticInOut2();
    return ElasticInOut2;
  })();
  var Back = /* @__PURE__ */ (function() {
    function Back2() {
    }
    ;
    Back2.value = new Back2();
    return Back2;
  })();
  var BackIn = /* @__PURE__ */ (function() {
    function BackIn2() {
    }
    ;
    BackIn2.value = new BackIn2();
    return BackIn2;
  })();
  var BackOut = /* @__PURE__ */ (function() {
    function BackOut2() {
    }
    ;
    BackOut2.value = new BackOut2();
    return BackOut2;
  })();
  var BackInOut = /* @__PURE__ */ (function() {
    function BackInOut2() {
    }
    ;
    BackInOut2.value = new BackInOut2();
    return BackInOut2;
  })();
  var Bounce = /* @__PURE__ */ (function() {
    function Bounce2() {
    }
    ;
    Bounce2.value = new Bounce2();
    return Bounce2;
  })();
  var BounceIn = /* @__PURE__ */ (function() {
    function BounceIn2() {
    }
    ;
    BounceIn2.value = new BounceIn2();
    return BounceIn2;
  })();
  var BounceOut = /* @__PURE__ */ (function() {
    function BounceOut2() {
    }
    ;
    BounceOut2.value = new BounceOut2();
    return BounceOut2;
  })();
  var BounceInOut = /* @__PURE__ */ (function() {
    function BounceInOut2() {
    }
    ;
    BounceInOut2.value = new BounceInOut2();
    return BounceInOut2;
  })();

  // output/Hylograph.HATS.Transitions/index.js
  var toTickEasing = function(v) {
    if (v instanceof Nothing) {
      return easeOutCubic;
    }
    ;
    if (v instanceof Just) {
      if (v.value0 instanceof Linear) {
        return linear;
      }
      ;
      if (v.value0 instanceof Cubic) {
        return easeOutCubic;
      }
      ;
      if (v.value0 instanceof CubicIn) {
        return easeInCubic;
      }
      ;
      if (v.value0 instanceof CubicOut) {
        return easeOutCubic;
      }
      ;
      if (v.value0 instanceof CubicInOut) {
        return easeInOutCubic;
      }
      ;
      if (v.value0 instanceof Quad) {
        return easeOutQuad;
      }
      ;
      if (v.value0 instanceof QuadIn) {
        return easeInQuad;
      }
      ;
      if (v.value0 instanceof QuadOut) {
        return easeOutQuad;
      }
      ;
      if (v.value0 instanceof QuadInOut) {
        return easeInOutQuad;
      }
      ;
      if (v.value0 instanceof Sin) {
        return easeOutSin;
      }
      ;
      if (v.value0 instanceof SinIn) {
        return easeInSin;
      }
      ;
      if (v.value0 instanceof SinOut) {
        return easeOutSin;
      }
      ;
      if (v.value0 instanceof SinInOut) {
        return easeInOutSin;
      }
      ;
      if (v.value0 instanceof Exp) {
        return easeOutExp;
      }
      ;
      if (v.value0 instanceof ExpIn) {
        return easeInExp;
      }
      ;
      if (v.value0 instanceof ExpOut) {
        return easeOutExp;
      }
      ;
      if (v.value0 instanceof ExpInOut) {
        return easeInOutExp;
      }
      ;
      if (v.value0 instanceof Circle3) {
        return easeOutCircle;
      }
      ;
      if (v.value0 instanceof CircleIn) {
        return easeInCircle;
      }
      ;
      if (v.value0 instanceof CircleOut) {
        return easeOutCircle;
      }
      ;
      if (v.value0 instanceof CircleInOut) {
        return easeInOutCircle;
      }
      ;
      if (v.value0 instanceof Elastic) {
        return easeOutElastic;
      }
      ;
      if (v.value0 instanceof ElasticIn) {
        return easeInElastic;
      }
      ;
      if (v.value0 instanceof ElasticOut) {
        return easeOutElastic;
      }
      ;
      if (v.value0 instanceof ElasticInOut) {
        return easeInOutElastic;
      }
      ;
      if (v.value0 instanceof Back) {
        return easeOutBack;
      }
      ;
      if (v.value0 instanceof BackIn) {
        return easeInBack;
      }
      ;
      if (v.value0 instanceof BackOut) {
        return easeOutBack;
      }
      ;
      if (v.value0 instanceof BackInOut) {
        return easeInOutBack;
      }
      ;
      if (v.value0 instanceof Bounce) {
        return easeOutBounce;
      }
      ;
      if (v.value0 instanceof BounceIn) {
        return easeInBounce;
      }
      ;
      if (v.value0 instanceof BounceOut) {
        return easeOutBounce;
      }
      ;
      if (v.value0 instanceof BounceInOut) {
        return easeInOutBounce;
      }
      ;
      throw new Error("Failed pattern match at Hylograph.HATS.Transitions (line 187, column 13 - line 220, column 45): " + [v.value0.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Hylograph.HATS.Transitions (line 185, column 16 - line 220, column 45): " + [v.constructor.name]);
  };

  // output/Hylograph.Interaction.Coordinated/index.js
  var Primary2 = /* @__PURE__ */ (function() {
    function Primary3() {
    }
    ;
    Primary3.value = new Primary3();
    return Primary3;
  })();
  var Related2 = /* @__PURE__ */ (function() {
    function Related3() {
    }
    ;
    Related3.value = new Related3();
    return Related3;
  })();
  var Selected = /* @__PURE__ */ (function() {
    function Selected2() {
    }
    ;
    Selected2.value = new Selected2();
    return Selected2;
  })();
  var Dimmed2 = /* @__PURE__ */ (function() {
    function Dimmed3() {
    }
    ;
    Dimmed3.value = new Dimmed3();
    return Dimmed3;
  })();
  var Neutral2 = /* @__PURE__ */ (function() {
    function Neutral3() {
    }
    ;
    Neutral3.value = new Neutral3();
    return Neutral3;
  })();
  var HoverTrigger = /* @__PURE__ */ (function() {
    function HoverTrigger2(value0) {
      this.value0 = value0;
    }
    ;
    HoverTrigger2.create = function(value0) {
      return new HoverTrigger2(value0);
    };
    return HoverTrigger2;
  })();
  var BrushTrigger = /* @__PURE__ */ (function() {
    function BrushTrigger2(value0) {
      this.value0 = value0;
    }
    ;
    BrushTrigger2.create = function(value0) {
      return new BrushTrigger2(value0);
    };
    return BrushTrigger2;
  })();
  var ClearTrigger = /* @__PURE__ */ (function() {
    function ClearTrigger2() {
    }
    ;
    ClearTrigger2.value = new ClearTrigger2();
    return ClearTrigger2;
  })();

  // output/Web.DOM.Document/foreign.js
  var getEffProp = function(name15) {
    return function(doc) {
      return function() {
        return doc[name15];
      };
    };
  };
  var url = getEffProp("URL");
  var documentURI = getEffProp("documentURI");
  var origin = getEffProp("origin");
  var compatMode = getEffProp("compatMode");
  var characterSet = getEffProp("characterSet");
  var contentType = getEffProp("contentType");
  var _documentElement = getEffProp("documentElement");
  function createElement(localName2) {
    return function(doc) {
      return function() {
        return doc.createElement(localName2);
      };
    };
  }
  function _createElementNS(ns) {
    return function(qualifiedName) {
      return function(doc) {
        return function() {
          return doc.createElementNS(ns, qualifiedName);
        };
      };
    };
  }

  // output/Web.Internal.FFI/foreign.js
  function _unsafeReadProtoTagged(nothing, just, name15, value12) {
    if (typeof window !== "undefined") {
      var ty = window[name15];
      if (ty != null && value12 instanceof ty) {
        return just(value12);
      }
    }
    var obj = value12;
    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;
      if (constructorName === name15) {
        return just(value12);
      } else if (constructorName === "Object") {
        return nothing;
      }
      obj = proto;
    }
    return nothing;
  }

  // output/Web.Internal.FFI/index.js
  var unsafeReadProtoTagged = function(name15) {
    return function(value12) {
      return _unsafeReadProtoTagged(Nothing.value, Just.create, name15, value12);
    };
  };

  // output/Web.DOM.Document/index.js
  var createElementNS = function($6) {
    return _createElementNS(toNullable($6));
  };

  // output/Hylograph.Internal.Element.Operations/index.js
  var elementTypeToString = function(v) {
    if (v instanceof Circle) {
      return "circle";
    }
    ;
    if (v instanceof Rect) {
      return "rect";
    }
    ;
    if (v instanceof Path) {
      return "path";
    }
    ;
    if (v instanceof Line) {
      return "line";
    }
    ;
    if (v instanceof Polygon) {
      return "polygon";
    }
    ;
    if (v instanceof Text) {
      return "text";
    }
    ;
    if (v instanceof Group) {
      return "g";
    }
    ;
    if (v instanceof SVG) {
      return "svg";
    }
    ;
    if (v instanceof Defs) {
      return "defs";
    }
    ;
    if (v instanceof LinearGradient) {
      return "linearGradient";
    }
    ;
    if (v instanceof RadialGradient) {
      return "radialGradient";
    }
    ;
    if (v instanceof Stop) {
      return "stop";
    }
    ;
    if (v instanceof PatternFill) {
      return "pattern";
    }
    ;
    if (v instanceof Title) {
      return "title";
    }
    ;
    if (v instanceof Div) {
      return "div";
    }
    ;
    if (v instanceof Span) {
      return "span";
    }
    ;
    if (v instanceof Table) {
      return "table";
    }
    ;
    if (v instanceof Tr) {
      return "tr";
    }
    ;
    if (v instanceof Td) {
      return "td";
    }
    ;
    if (v instanceof Th) {
      return "th";
    }
    ;
    if (v instanceof Tbody) {
      return "tbody";
    }
    ;
    if (v instanceof Thead) {
      return "thead";
    }
    ;
    if (v instanceof Code) {
      return "code";
    }
    ;
    if (v instanceof Var) {
      return "var";
    }
    ;
    if (v instanceof Dfn) {
      return "dfn";
    }
    ;
    if (v instanceof Dl) {
      return "dl";
    }
    ;
    if (v instanceof Dt) {
      return "dt";
    }
    ;
    if (v instanceof Dd) {
      return "dd";
    }
    ;
    if (v instanceof Ol) {
      return "ol";
    }
    ;
    if (v instanceof Ul) {
      return "ul";
    }
    ;
    if (v instanceof Li) {
      return "li";
    }
    ;
    if (v instanceof Small) {
      return "small";
    }
    ;
    if (v instanceof Em) {
      return "em";
    }
    ;
    if (v instanceof Strong) {
      return "strong";
    }
    ;
    if (v instanceof Anchor) {
      return "a";
    }
    ;
    if (v instanceof P) {
      return "p";
    }
    ;
    if (v instanceof Pre) {
      return "pre";
    }
    ;
    if (v instanceof Section) {
      return "section";
    }
    ;
    if (v instanceof Mark) {
      return "mark";
    }
    ;
    if (v instanceof Abbr) {
      return "abbr";
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Internal.Element.Operations (line 31, column 1 - line 31, column 45): " + [v.constructor.name]);
  };
  var createElementWithNS = function(elemType) {
    return function(doc) {
      var v = elementContext(elemType);
      if (v instanceof SVGContext) {
        return createElementNS(new Just("http://www.w3.org/2000/svg"))(elementTypeToString(elemType))(doc);
      }
      ;
      if (v instanceof HTMLContext) {
        return createElement(elementTypeToString(elemType))(doc);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Internal.Element.Operations (line 24, column 3 - line 28, column 64): " + [v.constructor.name]);
    };
  };

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp2("children");
  var _firstElementChild = getEffProp2("firstElementChild");
  var _lastElementChild = getEffProp2("lastElementChild");
  var childElementCount = getEffProp2("childElementCount");

  // output/Web.DOM.Element/index.js
  var toNode = unsafeCoerce2;
  var toEventTarget = unsafeCoerce2;

  // output/Web.DOM.Node/foreign.js
  var getEffProp3 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp3("baseURI");
  var _ownerDocument = getEffProp3("ownerDocument");
  var _parentNode = getEffProp3("parentNode");
  var _parentElement = getEffProp3("parentElement");
  var childNodes = getEffProp3("childNodes");
  var _firstChild = getEffProp3("firstChild");
  var _lastChild = getEffProp3("lastChild");
  var _previousSibling = getEffProp3("previousSibling");
  var _nextSibling = getEffProp3("nextSibling");
  var _nodeValue = getEffProp3("nodeValue");
  var textContent = getEffProp3("textContent");
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }

  // output/Web.Event.Event/foreign.js
  function stopPropagation(e) {
    return function() {
      return e.stopPropagation();
    };
  }

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target5) {
          return function() {
            return target5.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var toDocument = unsafeCoerce2;

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Web.UIEvent.MouseEvent/foreign.js
  function ctrlKey(e) {
    return e.ctrlKey;
  }
  function shiftKey(e) {
    return e.shiftKey;
  }
  function metaKey(e) {
    return e.metaKey;
  }

  // output/Web.UIEvent.MouseEvent/index.js
  var fromEvent = /* @__PURE__ */ unsafeReadProtoTagged("MouseEvent");

  // output/Hylograph.HATS.InterpreterTick/index.js
  var append3 = /* @__PURE__ */ append(semigroupArray);
  var map26 = /* @__PURE__ */ map(functorArray);
  var bind3 = /* @__PURE__ */ bind(bindEffect);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var pure11 = /* @__PURE__ */ pure(applicativeEffect);
  var unwrap2 = /* @__PURE__ */ unwrap();
  var map112 = /* @__PURE__ */ map(functorMaybe);
  var map27 = /* @__PURE__ */ map(functorEffect);
  var traverse5 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var union2 = /* @__PURE__ */ union(ordString);
  var $$void6 = /* @__PURE__ */ $$void(functorEffect);
  var traverseWithIndex2 = /* @__PURE__ */ traverseWithIndex(traversableWithIndexArray)(applicativeEffect);
  var insert3 = /* @__PURE__ */ insert(ordString);
  var foldM2 = /* @__PURE__ */ foldM(monadEffect);
  var fromFoldable10 = /* @__PURE__ */ fromFoldable4(foldableArray)(ordString);
  var difference4 = /* @__PURE__ */ difference2(ordString);
  var intersection4 = /* @__PURE__ */ intersection2(ordString);
  var member4 = /* @__PURE__ */ member2(ordString);
  var tooltipTriggerToInt = function(v) {
    if (v instanceof OnHover) {
      return 0;
    }
    ;
    if (v instanceof WhenPrimary) {
      return 1;
    }
    ;
    if (v instanceof WhenRelated) {
      return 2;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 718, column 23 - line 721, column 19): " + [v.constructor.name]);
  };
  var interactionStateToInt = function(v) {
    if (v instanceof Primary2) {
      return 0;
    }
    ;
    if (v instanceof Related2) {
      return 1;
    }
    ;
    if (v instanceof Selected) {
      return 2;
    }
    ;
    if (v instanceof Dimmed2) {
      return 3;
    }
    ;
    if (v instanceof Neutral2) {
      return 4;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 725, column 25 - line 730, column 18): " + [v.constructor.name]);
  };
  var highlightClassToInt = function(v) {
    if (v instanceof Primary) {
      return 0;
    }
    ;
    if (v instanceof Related) {
      return 1;
    }
    ;
    if (v instanceof Dimmed) {
      return 2;
    }
    ;
    if (v instanceof Neutral) {
      return 3;
    }
    ;
    if (v instanceof Upstream) {
      return 4;
    }
    ;
    if (v instanceof Downstream) {
      return 5;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 707, column 23 - line 713, column 21): " + [v.constructor.name]);
  };
  var enumerateDFS = function(root) {
    return function(getChildren) {
      return function(includeInternal) {
        var go2 = function(node) {
          var children2 = getChildren(node);
          var isLeaf = $$null(children2);
          var childResults = concatMap(go2)(children2);
          var $77 = isLeaf || includeInternal;
          if ($77) {
            return append3([node])(childResults);
          }
          ;
          return childResults;
        };
        return go2(root);
      };
    };
  };
  var enumerateBFS = function(root) {
    return function(getChildren) {
      return function(includeInternal) {
        var go2 = function(queue) {
          var v = uncons(queue);
          if (v instanceof Nothing) {
            return [];
          }
          ;
          if (v instanceof Just) {
            var children2 = getChildren(v.value0.head);
            var isLeaf = $$null(children2);
            var include = isLeaf || includeInternal;
            var thisNode = (function() {
              if (include) {
                return [v.value0.head];
              }
              ;
              return [];
            })();
            return append3(thisNode)(go2(append3(v.value0.tail)(children2)));
          }
          ;
          throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 556, column 14 - line 563, column 43): " + [v.constructor.name]);
        };
        return go2([root]);
      };
    };
  };
  var runEnumeration = function(v) {
    if (v instanceof FromArray) {
      return v.value0;
    }
    ;
    if (v instanceof FromTree) {
      if (v.value0.order instanceof DepthFirst) {
        return enumerateDFS(v.value0.root)(v.value0.children)(v.value0.includeInternal);
      }
      ;
      if (v.value0.order instanceof BreadthFirst) {
        return enumerateBFS(v.value0.root)(v.value0.children)(v.value0.includeInternal);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 537, column 5 - line 539, column 80): " + [v.value0.order.constructor.name]);
    }
    ;
    if (v instanceof WithContext) {
      return map26(function(v1) {
        return v1.datum;
      })(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 534, column 18 - line 540, column 41): " + [v.constructor.name]);
  };
  var elementTypeToTagName = function(v) {
    if (v instanceof Circle) {
      return "circle";
    }
    ;
    if (v instanceof Rect) {
      return "rect";
    }
    ;
    if (v instanceof Path) {
      return "path";
    }
    ;
    if (v instanceof Line) {
      return "line";
    }
    ;
    if (v instanceof Polygon) {
      return "polygon";
    }
    ;
    if (v instanceof Text) {
      return "text";
    }
    ;
    if (v instanceof Group) {
      return "g";
    }
    ;
    if (v instanceof SVG) {
      return "svg";
    }
    ;
    if (v instanceof Defs) {
      return "defs";
    }
    ;
    if (v instanceof LinearGradient) {
      return "linearGradient";
    }
    ;
    if (v instanceof RadialGradient) {
      return "radialGradient";
    }
    ;
    if (v instanceof Stop) {
      return "stop";
    }
    ;
    if (v instanceof PatternFill) {
      return "pattern";
    }
    ;
    if (v instanceof Title) {
      return "title";
    }
    ;
    if (v instanceof Div) {
      return "div";
    }
    ;
    if (v instanceof Span) {
      return "span";
    }
    ;
    if (v instanceof Table) {
      return "table";
    }
    ;
    if (v instanceof Tr) {
      return "tr";
    }
    ;
    if (v instanceof Td) {
      return "td";
    }
    ;
    if (v instanceof Th) {
      return "th";
    }
    ;
    if (v instanceof Tbody) {
      return "tbody";
    }
    ;
    if (v instanceof Thead) {
      return "thead";
    }
    ;
    if (v instanceof Code) {
      return "code";
    }
    ;
    if (v instanceof Var) {
      return "var";
    }
    ;
    if (v instanceof Dfn) {
      return "dfn";
    }
    ;
    if (v instanceof Dl) {
      return "dl";
    }
    ;
    if (v instanceof Dt) {
      return "dt";
    }
    ;
    if (v instanceof Dd) {
      return "dd";
    }
    ;
    if (v instanceof Ol) {
      return "ol";
    }
    ;
    if (v instanceof Ul) {
      return "ul";
    }
    ;
    if (v instanceof Li) {
      return "li";
    }
    ;
    if (v instanceof Small) {
      return "small";
    }
    ;
    if (v instanceof Em) {
      return "em";
    }
    ;
    if (v instanceof Strong) {
      return "strong";
    }
    ;
    if (v instanceof Anchor) {
      return "a";
    }
    ;
    if (v instanceof P) {
      return "p";
    }
    ;
    if (v instanceof Pre) {
      return "pre";
    }
    ;
    if (v instanceof Section) {
      return "section";
    }
    ;
    if (v instanceof Mark) {
      return "mark";
    }
    ;
    if (v instanceof Abbr) {
      return "abbr";
    }
    ;
    throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 593, column 24 - line 633, column 17): " + [v.constructor.name]);
  };
  var clearChildren = function(el) {
    return function __do2() {
      var children2 = getChildElements(el)();
      return traverse_2(removeElement)(children2)();
    };
  };
  var attachClickWithModifierPurs = function(el) {
    return function(plainHandler) {
      return function(modifierHandler) {
        return function __do2() {
          var listener = eventListener(function(event) {
            return function __do3() {
              stopPropagation(event)();
              var v = fromEvent(event);
              if (v instanceof Just) {
                var hasModifier = metaKey(v.value0) || (ctrlKey(v.value0) || shiftKey(v.value0));
                if (hasModifier) {
                  return modifierHandler(unit)();
                }
                ;
                return plainHandler(unit)();
              }
              ;
              if (v instanceof Nothing) {
                return plainHandler(unit)();
              }
              ;
              throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 691, column 5 - line 700, column 26): " + [v.constructor.name]);
            };
          })();
          addEventListener("click")(listener)(false)(toEventTarget(el))();
          return setCursorPointer(el)();
        };
      };
    };
  };
  var appendTo = function(p) {
    return function(child) {
      return function __do2() {
        appendChild(toNode(child))(toNode(p))();
        return unit;
      };
    };
  };
  var rerenderTree = function(doc) {
    return function(parent2) {
      return function(tree) {
        var mkUpdateTransition = function(el) {
          return function(duration2) {
            return function(delay) {
              return function(easing) {
                return function(v) {
                  if (v instanceof StaticAttr) {
                    return function __do2() {
                      var fromStr = getAttribute(el)(v.value0)();
                      var $93 = {
                        from: fromString(fromStr),
                        to: fromString(v.value1)
                      };
                      if ($93.from instanceof Just && ($93.to instanceof Just && $93.from.value0 !== $93.to.value0)) {
                        return new Just({
                          attrName: v.value0,
                          from: $93.from.value0,
                          to: $93.to.value0,
                          elapsed: 0,
                          duration: duration2,
                          delay,
                          easing
                        });
                      }
                      ;
                      return Nothing.value;
                    };
                  }
                  ;
                  if (v instanceof ThunkedAttr) {
                    return function __do2() {
                      var fromStr = getAttribute(el)(v.value0)();
                      var toNum = fromString(v.value1(unit));
                      var $100 = {
                        from: fromString(fromStr),
                        to: toNum
                      };
                      if ($100.from instanceof Just && ($100.to instanceof Just && $100.from.value0 !== $100.to.value0)) {
                        return new Just({
                          attrName: v.value0,
                          from: $100.from.value0,
                          to: $100.to.value0,
                          elapsed: 0,
                          duration: duration2,
                          delay,
                          easing
                        });
                      }
                      ;
                      return Nothing.value;
                    };
                  }
                  ;
                  throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 464, column 49 - line 477, column 26): " + [v.constructor.name]);
                };
              };
            };
          };
        };
        var mkExitTransition = function(el) {
          return function(duration2) {
            return function(delay) {
              return function(easing) {
                return function(v) {
                  if (v instanceof StaticAttr) {
                    return function __do2() {
                      var fromStr = getAttribute(el)(v.value0)();
                      var $108 = {
                        from: fromString(fromStr),
                        to: fromString(v.value1)
                      };
                      if ($108.from instanceof Just && $108.to instanceof Just) {
                        return new Just({
                          attrName: v.value0,
                          from: $108.from.value0,
                          to: $108.to.value0,
                          elapsed: 0,
                          duration: duration2,
                          delay,
                          easing
                        });
                      }
                      ;
                      return Nothing.value;
                    };
                  }
                  ;
                  if (v instanceof ThunkedAttr) {
                    return function __do2() {
                      var fromStr = getAttribute(el)(v.value0)();
                      var toVal = v.value1(unit);
                      var $115 = {
                        from: fromString(fromStr),
                        to: fromString(toVal)
                      };
                      if ($115.from instanceof Just && $115.to instanceof Just) {
                        return new Just({
                          attrName: v.value0,
                          from: $115.from.value0,
                          to: $115.to.value0,
                          elapsed: 0,
                          duration: duration2,
                          delay,
                          easing
                        });
                      }
                      ;
                      return Nothing.value;
                    };
                  }
                  ;
                  throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 497, column 47 - line 510, column 26): " + [v.constructor.name]);
                };
              };
            };
          };
        };
        var getAttrNum = function(targetName) {
          return function(v) {
            if (v instanceof StaticAttr && v.value0 === targetName) {
              return fromString(v.value1);
            }
            ;
            if (v instanceof ThunkedAttr && v.value0 === targetName) {
              return fromString(v.value1(unit));
            }
            ;
            return Nothing.value;
          };
        };
        var findAttrValue = function(name15) {
          return function(attrs) {
            return findMap(getAttrNum(name15))(attrs);
          };
        };
        var mkEnterTransition = function(duration2) {
          return function(delay) {
            return function(easing) {
              return function(templateAttrs) {
                return function(v) {
                  if (v instanceof StaticAttr) {
                    var toVal = findAttrValue(v.value0)(templateAttrs);
                    var $128 = {
                      from: fromString(v.value1),
                      to: toVal
                    };
                    if ($128.from instanceof Just && $128.to instanceof Just) {
                      return pure11(new Just({
                        attrName: v.value0,
                        from: $128.from.value0,
                        to: $128.to.value0,
                        elapsed: 0,
                        duration: duration2,
                        delay,
                        easing
                      }));
                    }
                    ;
                    return pure11(Nothing.value);
                  }
                  ;
                  if (v instanceof ThunkedAttr) {
                    var fromNum = fromString(v.value1(unit));
                    var toVal = findAttrValue(v.value0)(templateAttrs);
                    var $135 = {
                      from: fromNum,
                      to: toVal
                    };
                    if ($135.from instanceof Just && $135.to instanceof Just) {
                      return pure11(new Just({
                        attrName: v.value0,
                        from: $135.from.value0,
                        to: $135.to.value0,
                        elapsed: 0,
                        duration: duration2,
                        delay,
                        easing
                      }));
                    }
                    ;
                    return pure11(Nothing.value);
                  }
                  ;
                  throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 430, column 59 - line 443, column 26): " + [v.constructor.name]);
                };
              };
            };
          };
        };
        var emptyResult = {
          selections: empty2,
          entering: [],
          updating: [],
          exiting: []
        };
        var createUpdateTransitions = function(el) {
          return function(templateAttrs) {
            return function(phaseSpec) {
              return function(idx) {
                if (phaseSpec.transition instanceof Nothing) {
                  return pure11([]);
                }
                ;
                if (phaseSpec.transition instanceof Just) {
                  var duration2 = unwrap2(phaseSpec.transition.value0.duration);
                  var baseDelay = fromMaybe(0)(map112(unwrap2)(phaseSpec.transition.value0.delay));
                  var staggerDelay = fromMaybe(0)(phaseSpec.transition.value0.staggerDelay);
                  var delay = baseDelay + staggerDelay * toNumber(idx);
                  var easing = toTickEasing(phaseSpec.transition.value0.easing);
                  return map27(catMaybes)(traverse5(mkUpdateTransition(el)(duration2)(delay)(easing))(templateAttrs));
                }
                ;
                throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 452, column 5 - line 460, column 97): " + [phaseSpec.transition.constructor.name]);
              };
            };
          };
        };
        var createExitTransitions = function(el) {
          return function(phaseSpec) {
            return function(idx) {
              if (phaseSpec.transition instanceof Nothing) {
                return pure11([]);
              }
              ;
              if (phaseSpec.transition instanceof Just) {
                var duration2 = unwrap2(phaseSpec.transition.value0.duration);
                var baseDelay = fromMaybe(0)(map112(unwrap2)(phaseSpec.transition.value0.delay));
                var staggerDelay = fromMaybe(0)(phaseSpec.transition.value0.staggerDelay);
                var delay = baseDelay + staggerDelay * toNumber(idx);
                var easing = toTickEasing(phaseSpec.transition.value0.easing);
                return map27(catMaybes)(traverse5(mkExitTransition(el)(duration2)(delay)(easing))(phaseSpec.attrs));
              }
              ;
              throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 485, column 5 - line 493, column 97): " + [phaseSpec.transition.constructor.name]);
            };
          };
        };
        var createEnterTransitions = function(el) {
          return function(templateAttrs) {
            return function(phaseSpec) {
              return function(idx) {
                if (phaseSpec.transition instanceof Nothing) {
                  return pure11([]);
                }
                ;
                if (phaseSpec.transition instanceof Just) {
                  var duration2 = unwrap2(phaseSpec.transition.value0.duration);
                  var baseDelay = fromMaybe(0)(map112(unwrap2)(phaseSpec.transition.value0.delay));
                  var staggerDelay = fromMaybe(0)(phaseSpec.transition.value0.staggerDelay);
                  var delay = baseDelay + staggerDelay * toNumber(idx);
                  var easing = toTickEasing(phaseSpec.transition.value0.easing);
                  return map27(catMaybes)(traverse5(mkEnterTransition(duration2)(delay)(easing)(templateAttrs))(phaseSpec.attrs));
                }
                ;
                throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 418, column 5 - line 426, column 109): " + [phaseSpec.transition.constructor.name]);
              };
            };
          };
        };
        var combineResults = function(a) {
          return function(b) {
            return {
              selections: union2(a.selections)(b.selections),
              entering: append3(a.entering)(b.entering),
              updating: append3(a.updating)(b.updating),
              exiting: append3(a.exiting)(b.exiting)
            };
          };
        };
        var applyIfNonNumeric = function(el) {
          return function(v) {
            if (v instanceof StaticAttr) {
              var v1 = fromString(v.value1);
              if (v1 instanceof Just) {
                return pure11(unit);
              }
              ;
              if (v1 instanceof Nothing) {
                return setAttribute(el)(v.value0)(v.value1);
              }
              ;
              throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 350, column 7 - line 352, column 46): " + [v1.constructor.name]);
            }
            ;
            if (v instanceof ThunkedAttr) {
              var value12 = v.value1(unit);
              var v1 = fromString(value12);
              if (v1 instanceof Just) {
                return pure11(unit);
              }
              ;
              if (v1 instanceof Nothing) {
                return setAttribute(el)(v.value0)(value12);
              }
              ;
              throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 355, column 10 - line 357, column 46): " + [v1.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 348, column 26 - line 357, column 46): " + [v.constructor.name]);
          };
        };
        var applyNonNumericAttrs = function(el) {
          return function(attrs) {
            return function __do2() {
              traverse5(applyIfNonNumeric(el))(attrs)();
              return unit;
            };
          };
        };
        var applyBehavior = function(el) {
          return function(v) {
            if (v instanceof ThunkedMouseEnter) {
              return attachMouseEnterThunked(el)(v.value0);
            }
            ;
            if (v instanceof ThunkedMouseLeave) {
              return attachMouseLeaveThunked(el)(v.value0);
            }
            ;
            if (v instanceof ThunkedClick) {
              return attachClickThunked(el)(v.value0);
            }
            ;
            if (v instanceof ThunkedClickWithModifier) {
              return attachClickWithModifierPurs(el)(v.value0)(v.value1);
            }
            ;
            if (v instanceof ThunkedDrag && v.value0 instanceof SimpleDrag) {
              return attachSimpleDrag(el);
            }
            ;
            if (v instanceof ThunkedDrag && v.value0 instanceof SimulationDrag) {
              return attachSimulationDragById(el)(v.value0.value0);
            }
            ;
            if (v instanceof ThunkedDrag && v.value0 instanceof SimulationDragNested) {
              return attachSimulationDragNestedById(el)(v.value0.value0);
            }
            ;
            if (v instanceof ThunkedZoom) {
              return attachZoom(el)(v.value0.scaleExtent.value0)(v.value0.scaleExtent.value1)(v.value0.targetSelector);
            }
            ;
            if (v instanceof ThunkedCoordinatedHighlight) {
              var triggerAsInt = tooltipTriggerToInt(v.value0.tooltipTrigger);
              var classifyAsInt = function(hoveredId) {
                return highlightClassToInt(v.value0.classify(hoveredId));
              };
              return attachCoordinatedHighlightThunked(el)(v.value0.identify)(classifyAsInt)(toNullable(v.value0.group))(toNullable(v.value0.tooltipContent))(triggerAsInt)(toNullable(v.value0.tooltipBorderColor));
            }
            ;
            if (v instanceof ThunkedCoordinatedInteraction) {
              var respondToHover = function(hoveredId) {
                return interactionStateToInt(v.value0.respond(new HoverTrigger(hoveredId)));
              };
              var respondToClear = function(v1) {
                return interactionStateToInt(v.value0.respond(ClearTrigger.value));
              };
              var respondToBrush = function(box) {
                return interactionStateToInt(v.value0.respond(new BrushTrigger(box)));
              };
              return attachCoordinatedInteractionThunked(el)(v.value0.identify)(respondToHover)(respondToBrush)(respondToClear)(v.value0.position)(toNullable(v.value0.group));
            }
            ;
            if (v instanceof ThunkedBrush) {
              return $$void6(attachCoordinatedBrushThunked(el)(v.value0.extent)(toNullable(v.value0.group)));
            }
            ;
            if (v instanceof ThunkedPointerDown) {
              return attachPointerDownThunked(el)(v.value0);
            }
            ;
            throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 369, column 22 - line 405, column 70): " + [v.constructor.name]);
          };
        };
        var applyBehaviors = function(el) {
          return function(behaviors) {
            return function __do2() {
              traverse5(applyBehavior(el))(behaviors)();
              return unit;
            };
          };
        };
        var applyAttr = function(el) {
          return function(v) {
            if (v instanceof StaticAttr) {
              return setAttribute(el)(v.value0)(v.value1);
            }
            ;
            if (v instanceof ThunkedAttr) {
              return setAttribute(el)(v.value0)(v.value1(unit));
            }
            ;
            throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 338, column 18 - line 340, column 64): " + [v.constructor.name]);
          };
        };
        var applyAttrs = function(el) {
          return function(attrs) {
            return function __do2() {
              traverse5(applyAttr(el))(attrs)();
              return unit;
            };
          };
        };
        var renderEnteringItem = function(p) {
          return function(foldName) {
            return function(keyFn) {
              return function(template) {
                return function(gupSpec) {
                  return function(idx) {
                    return function(datum) {
                      var itemTree = template(datum);
                      var key = keyFn(datum);
                      if (itemTree instanceof Elem) {
                        return function __do2() {
                          var el = createElementWithNS(itemTree.value0.elemType)(doc)();
                          appendTo(p)(el)();
                          setKey(el)(key)();
                          setFoldName(el)(foldName)();
                          bindDatum(el)(datum)();
                          applyAttrs(el)(itemTree.value0.attrs)();
                          var transitions = (function() {
                            if (gupSpec instanceof Just) {
                              if (gupSpec.value0.enter instanceof Just) {
                                var ts = createEnterTransitions(el)(itemTree.value0.attrs)(gupSpec.value0.enter.value0)(idx)();
                                applyAttrs(el)(gupSpec.value0.enter.value0.attrs)();
                                return new Just({
                                  element: el,
                                  attrs: ts
                                });
                              }
                              ;
                              if (gupSpec.value0.enter instanceof Nothing) {
                                return Nothing.value;
                              }
                              ;
                              throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 285, column 22 - line 290, column 36): " + [gupSpec.value0.enter.constructor.name]);
                            }
                            ;
                            if (gupSpec instanceof Nothing) {
                              return Nothing.value;
                            }
                            ;
                            throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 284, column 24 - line 291, column 34): " + [gupSpec.constructor.name]);
                          })();
                          applyBehaviors(el)(itemTree.value0.behaviors)();
                          var childMaps = traverse5(goWithDatum(el))(itemTree.value0.children)();
                          return {
                            element: el,
                            childSelections: foldl2(union2)(empty2)(map26(function(v) {
                              return v.selections;
                            })(childMaps)),
                            transitions
                          };
                        };
                      }
                      ;
                      return function __do2() {
                        var el = createElementWithNS(Group.value)(doc)();
                        return {
                          element: el,
                          childSelections: empty2,
                          transitions: Nothing.value
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
        var goWithDatum = function(p) {
          return function(v) {
            if (v instanceof Empty2) {
              return pure11(emptyResult);
            }
            ;
            if (v instanceof Elem) {
              return function __do2() {
                var el = createElementWithNS(v.value0.elemType)(doc)();
                appendTo(p)(el)();
                applyAttrs(el)(v.value0.attrs)();
                applyBehaviors(el)(v.value0.behaviors)();
                var childResults = traverse5(goWithDatum(el))(v.value0.children)();
                return foldl2(combineResults)(emptyResult)(childResults);
              };
            }
            ;
            if (v instanceof MkFold) {
              return runSomeFold(v.value0)(function(foldSpec) {
                var items = runEnumeration(foldSpec.enumerate);
                return function __do2() {
                  var results = traverseWithIndex2(renderEnteringItem(p)(foldSpec.name)(foldSpec.keyFn)(foldSpec.template)(foldSpec.gup))(items)();
                  var elements = map26(function(v1) {
                    return v1.element;
                  })(results);
                  var childMaps = foldl2(union2)(empty2)(map26(function(v1) {
                    return v1.childSelections;
                  })(results));
                  return {
                    selections: insert3(foldSpec.name)(elements)(childMaps),
                    entering: [catMaybes(map26(function(v1) {
                      return v1.transitions;
                    })(results))],
                    updating: [],
                    exiting: []
                  };
                };
              });
            }
            ;
            throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 307, column 19 - line 326, column 10): " + [v.constructor.name]);
          };
        };
        var processChildren = function(p) {
          return function(children2) {
            var processChild = function(v) {
              return function(child) {
                return function __do2() {
                  var v1 = goWithDomIdx(p)(v.domIdx)(child)();
                  return {
                    result: combineResults(v.result)(v1.result),
                    domIdx: v.domIdx + v1.elementsCreated | 0
                  };
                };
              };
            };
            return function __do2() {
              var v = foldM2(processChild)({
                result: emptyResult,
                domIdx: 0
              })(children2)();
              return v.result;
            };
          };
        };
        var goWithDomIdx = function(p) {
          return function(domIdx) {
            return function(v) {
              if (v instanceof Empty2) {
                return pure11({
                  result: emptyResult,
                  elementsCreated: 0
                });
              }
              ;
              if (v instanceof Elem) {
                return function __do2() {
                  var existingChildren = getChildElements(p)();
                  var el = (function() {
                    var v1 = index(existingChildren)(domIdx);
                    if (v1 instanceof Just) {
                      applyAttrs(v1.value0)(v.value0.attrs)();
                      applyBehaviors(v1.value0)(v.value0.behaviors)();
                      return v1.value0;
                    }
                    ;
                    if (v1 instanceof Nothing) {
                      var el2 = createElementWithNS(v.value0.elemType)(doc)();
                      appendTo(p)(el2)();
                      applyAttrs(el2)(v.value0.attrs)();
                      applyBehaviors(el2)(v.value0.behaviors)();
                      return el2;
                    }
                    ;
                    throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 140, column 13 - line 150, column 18): " + [v1.constructor.name]);
                  })();
                  var childResult = processChildren(el)(v.value0.children)();
                  return {
                    result: childResult,
                    elementsCreated: 1
                  };
                };
              }
              ;
              if (v instanceof MkFold) {
                return runSomeFold(v.value0)(function(spec) {
                  var items = runEnumeration(spec.enumerate);
                  var newKeys = fromFoldable10(map26(spec.keyFn)(items));
                  var tagName2 = elementTypeToTagName(spec.elementType);
                  return function __do2() {
                    var existingEls = getChildElementsForFold(p)(tagName2)(spec.name)();
                    var existingKeysAndEls = traverse5(function(el) {
                      return function __do3() {
                        var k = getKey(el)();
                        return {
                          key: k,
                          element: el
                        };
                      };
                    })(existingEls)();
                    var existingKeys = fromFoldable10(map26(function(v1) {
                      return v1.key;
                    })(existingKeysAndEls));
                    var enterKeys = difference4(newKeys)(existingKeys);
                    var updateKeys = intersection4(newKeys)(existingKeys);
                    var exitKeys = difference4(existingKeys)(newKeys);
                    var exitTransitions = traverseWithIndex2(function(exitIdx) {
                      return function(v1) {
                        var $204 = member4(v1.key)(exitKeys);
                        if ($204) {
                          if (spec.gup instanceof Just) {
                            if (spec.gup.value0.exit instanceof Just) {
                              return function __do3() {
                                var transitions = createExitTransitions(v1.element)(spec.gup.value0.exit.value0)(exitIdx)();
                                applyNonNumericAttrs(v1.element)(spec.gup.value0.exit.value0.attrs)();
                                return new Just({
                                  element: v1.element,
                                  attrs: transitions
                                });
                              };
                            }
                            ;
                            if (spec.gup.value0.exit instanceof Nothing) {
                              return function __do3() {
                                removeElement(v1.element)();
                                return Nothing.value;
                              };
                            }
                            ;
                            throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 175, column 29 - line 184, column 29): " + [spec.gup.value0.exit.constructor.name]);
                          }
                          ;
                          if (spec.gup instanceof Nothing) {
                            return function __do3() {
                              removeElement(v1.element)();
                              return Nothing.value;
                            };
                          }
                          ;
                          throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 174, column 16 - line 187, column 27): " + [spec.gup.constructor.name]);
                        }
                        ;
                        return pure11(Nothing.value);
                      };
                    })(existingKeysAndEls)();
                    var updateTransitions = traverseWithIndex2(function(updateIdx) {
                      return function(v1) {
                        var $212 = member4(v1.key)(updateKeys);
                        if ($212) {
                          var v2 = find2(function(d) {
                            return spec.keyFn(d) === v1.key;
                          })(items);
                          if (v2 instanceof Just) {
                            return function __do3() {
                              bindDatum(v1.element)(v2.value0)();
                              var templateTree = spec.template(v2.value0);
                              var templateAttrs = (function() {
                                if (templateTree instanceof Elem) {
                                  return templateTree.value0.attrs;
                                }
                                ;
                                return [];
                              })();
                              var templateChildren = (function() {
                                if (templateTree instanceof Elem) {
                                  return templateTree.value0.children;
                                }
                                ;
                                return [];
                              })();
                              if (spec.gup instanceof Just) {
                                if (spec.gup.value0.update instanceof Just) {
                                  applyNonNumericAttrs(v1.element)(templateAttrs)();
                                  var transitions = createUpdateTransitions(v1.element)(templateAttrs)(spec.gup.value0.update.value0)(updateIdx)();
                                  clearChildren(v1.element)();
                                  traverse5(goWithDatum(v1.element))(templateChildren)();
                                  return new Just({
                                    element: v1.element,
                                    attrs: transitions
                                  });
                                }
                                ;
                                if (spec.gup.value0.update instanceof Nothing) {
                                  applyAttrs(v1.element)(templateAttrs)();
                                  clearChildren(v1.element)();
                                  traverse5(goWithDatum(v1.element))(templateChildren)();
                                  return Nothing.value;
                                }
                                ;
                                throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 207, column 33 - line 220, column 33): " + [spec.gup.value0.update.constructor.name]);
                              }
                              ;
                              if (spec.gup instanceof Nothing) {
                                applyAttrs(v1.element)(templateAttrs)();
                                clearChildren(v1.element)();
                                traverse5(goWithDatum(v1.element))(templateChildren)();
                                return Nothing.value;
                              }
                              ;
                              throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 206, column 15 - line 226, column 31): " + [spec.gup.constructor.name]);
                            };
                          }
                          ;
                          if (v2 instanceof Nothing) {
                            return pure11(Nothing.value);
                          }
                          ;
                          throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 194, column 16 - line 227, column 36): " + [v2.constructor.name]);
                        }
                        ;
                        return pure11(Nothing.value);
                      };
                    })(existingKeysAndEls)();
                    var enterItems = filter(function(d) {
                      return member4(spec.keyFn(d))(enterKeys);
                    })(items);
                    var enterResults = traverseWithIndex2(renderEnteringItem(p)(spec.name)(spec.keyFn)(spec.template)(spec.gup))(enterItems)();
                    var updateElements = map26(function(v1) {
                      return v1.element;
                    })(filter(function(v1) {
                      return member4(v1.key)(updateKeys);
                    })(existingKeysAndEls));
                    var enterElements = map26(function(v1) {
                      return v1.element;
                    })(enterResults);
                    var childMaps = foldl2(union2)(empty2)(map26(function(v1) {
                      return v1.childSelections;
                    })(enterResults));
                    var selections = insert3(spec.name)(append3(updateElements)(enterElements))(childMaps);
                    var totalElements = length(updateElements) + length(enterElements) | 0;
                    return {
                      result: {
                        selections,
                        entering: [catMaybes(map26(function(v1) {
                          return v1.transitions;
                        })(enterResults))],
                        updating: [catMaybes(updateTransitions)],
                        exiting: [catMaybes(exitTransitions)]
                      },
                      elementsCreated: totalElements
                    };
                  };
                });
              }
              ;
              throw new Error("Failed pattern match at Hylograph.HATS.InterpreterTick (line 135, column 27 - line 251, column 10): " + [v.constructor.name]);
            };
          };
        };
        return function __do2() {
          var v = goWithDomIdx(parent2)(0)(tree)();
          var allEntering = concat(v.result.entering);
          var allUpdating = concat(v.result.updating);
          var allExiting = concat(v.result.exiting);
          var hasTransitions = !($$null(allEntering) && ($$null(allUpdating) && $$null(allExiting)));
          return {
            selections: v.result.selections,
            transitions: (function() {
              if (hasTransitions) {
                return new Just({
                  entering: allEntering,
                  updating: allUpdating,
                  exiting: allExiting
                });
              }
              ;
              return Nothing.value;
            })()
          };
        };
      };
    };
  };
  var rerender = function(selector) {
    return function(tree) {
      return function __do2() {
        var doc = (function __do3() {
          var $235 = bind3(windowImpl)(document2)();
          return toDocument($235);
        })();
        var container = selectElement(selector)(doc)();
        return rerenderTree(doc)(container)(tree)();
      };
    };
  };

  // output/Hylograph.Transition.RAF/foreign.js
  var requestAnimationFrame_2 = function(callback) {
    return function() {
      return window.requestAnimationFrame(function(timestamp) {
        callback(timestamp)();
      });
    };
  };
  var performanceNow_ = function() {
    return performance.now();
  };

  // output/Hylograph.Transition.RAF/index.js
  var requestAnimationFrame3 = requestAnimationFrame_2;
  var performanceNow = performanceNow_;

  // output/Hylograph.Transition.Coordinator/index.js
  var when13 = /* @__PURE__ */ when(applicativeEffect);
  var for_5 = /* @__PURE__ */ for_(applicativeEffect);
  var append4 = /* @__PURE__ */ append(semigroupArray);
  var $$for2 = /* @__PURE__ */ $$for(applicativeEffect)(traversableArray);
  var for_23 = /* @__PURE__ */ for_5(foldableArray);
  var unless2 = /* @__PURE__ */ unless(applicativeEffect);
  var Completed = /* @__PURE__ */ (function() {
    function Completed2() {
    }
    ;
    Completed2.value = new Completed2();
    return Completed2;
  })();
  var Converged = /* @__PURE__ */ (function() {
    function Converged2(value0) {
      this.value0 = value0;
    }
    ;
    Converged2.create = function(value0) {
      return new Converged2(value0);
    };
    return Converged2;
  })();
  var eqConsumerId = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var notEq4 = /* @__PURE__ */ notEq(eqConsumerId);
  var unregister = function(v) {
    return function(consumerId) {
      return modify_(function(s) {
        return {
          frameId: s.frameId,
          lastTimestamp: s.lastTimestamp,
          nextId: s.nextId,
          running: s.running,
          consumers: filter(function(c) {
            return notEq4(c.id)(consumerId);
          })(s.consumers)
        };
      })(v);
    };
  };
  var register = function(v) {
    return function(spec) {
      return function __do2() {
        var state3 = read(v)();
        var consumer = {
          id: state3.nextId,
          tick: spec.tick,
          onComplete: spec.onComplete
        };
        write({
          frameId: state3.frameId,
          lastTimestamp: state3.lastTimestamp,
          running: state3.running,
          consumers: append4(state3.consumers)([consumer]),
          nextId: state3.nextId + 1 | 0
        })(v)();
        return state3.nextId;
      };
    };
  };
  var isCompleted = function(v) {
    if (v instanceof Completed) {
      return true;
    }
    ;
    return false;
  };
  var scheduleFrame = function(v) {
    return function __do2() {
      var frameId = requestAnimationFrame3(runFrame(v))();
      return modify_(function(v1) {
        return {
          consumers: v1.consumers,
          lastTimestamp: v1.lastTimestamp,
          nextId: v1.nextId,
          running: v1.running,
          frameId: new Just(frameId)
        };
      })(v)();
    };
  };
  var runFrame = function(v) {
    return function(timestamp) {
      return function __do2() {
        var state3 = read(v)();
        return when13(state3.running)((function() {
          var deltaMs = timestamp - state3.lastTimestamp;
          return function __do3() {
            modify_(function(v1) {
              return {
                consumers: v1.consumers,
                frameId: v1.frameId,
                nextId: v1.nextId,
                running: v1.running,
                lastTimestamp: timestamp
              };
            })(v)();
            var results = $$for2(state3.consumers)(function(consumer) {
              return function __do4() {
                var result = consumer.tick(deltaMs)();
                return {
                  consumer,
                  result
                };
              };
            })();
            var completed = filter(function(r) {
              return isCompleted(r.result);
            })(results);
            for_23(completed)(function(r) {
              return function __do4() {
                r.consumer.onComplete();
                return unregister(v)(r.consumer.id)();
              };
            })();
            var newState = read(v)();
            var $47 = $$null(newState.consumers);
            if ($47) {
              return scheduleFrame(v)();
            }
            ;
            return scheduleFrame(v)();
          };
        })())();
      };
    };
  };
  var start2 = function(v) {
    return function __do2() {
      var state3 = read(v)();
      return unless2(state3.running)(function __do3() {
        var now = performanceNow();
        modify_(function(v1) {
          return {
            consumers: v1.consumers,
            frameId: v1.frameId,
            nextId: v1.nextId,
            running: true,
            lastTimestamp: now
          };
        })(v)();
        return scheduleFrame(v)();
      })();
    };
  };
  var create6 = function __do() {
    var ref = $$new({
      consumers: [],
      running: false,
      lastTimestamp: 0,
      nextId: 0,
      frameId: Nothing.value
    })();
    return ref;
  };

  // output/Hylograph.Transition.Consumers/index.js
  var simulationConsumer = function(sim) {
    return function(alphaMin) {
      return function(onTick) {
        return function(_deltaMs) {
          return function __do2() {
            var alpha = tick(sim)();
            onTick();
            var $7 = alpha < alphaMin;
            if ($7) {
              return Completed.value;
            }
            ;
            return new Converged(alpha);
          };
        };
      };
    };
  };

  // output/Test.PSD3VisualTest/index.js
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var show5 = /* @__PURE__ */ show(showInt);
  var show1 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showString));
  var map28 = /* @__PURE__ */ map(functorArray);
  var $$void7 = /* @__PURE__ */ $$void(functorEffect);
  var discard22 = /* @__PURE__ */ discard2(bindAff);
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var wasmForceSetup = /* @__PURE__ */ (function() {
    return setup("physics")([withStrength($$static(-80))(manyBody2("charge")), withStrength($$static(1))(withY($$static(75))(withX($$static(350))(center2("center"))))]);
  })();
  var wasmConsumer = function(sim) {
    return function(alphaMin) {
      return function(onTick) {
        return function(_deltaMs) {
          return function __do2() {
            var alpha = tick3(sim)();
            onTick();
            var $16 = alpha < alphaMin;
            if ($16) {
              return Completed.value;
            }
            ;
            return new Converged(alpha);
          };
        };
      };
    };
  };
  var simulationTree = function(color) {
    return function(nodes) {
      return forEach("sim-nodes")(Circle.value)(nodes)(function(n) {
        return show5(n.id);
      })(function(node) {
        return elem2(Circle.value)([thunkedNum("cx")(node.x), thunkedNum("cy")(node.y), staticStr("r")("12"), staticStr("fill")(color)])([]);
      });
    };
  };
  var mkNode = function(id2) {
    return function(x) {
      return function(y) {
        return function(label4) {
          return {
            id: id2,
            x,
            y,
            vx: 0,
            vy: 0,
            fx: nullImpl,
            fy: nullImpl,
            label: label4
          };
        };
      };
    };
  };
  var initialWASMNodes = [/* @__PURE__ */ mkNode(0)(350)(75)("1"), /* @__PURE__ */ mkNode(1)(360)(80)("2"), /* @__PURE__ */ mkNode(2)(340)(70)("3"), /* @__PURE__ */ mkNode(3)(355)(85)("4"), /* @__PURE__ */ mkNode(4)(345)(65)("5"), /* @__PURE__ */ mkNode(5)(365)(75)("6")];
  var setupWASMSimulationPSD3 = function(coord) {
    return function __do2() {
      log("Setting up WASM force simulation with SAME declarative API")();
      var sim = create3(initialWASMNodes)([])();
      var result = applySetupWithData2(wasmForceSetup)(initialWASMNodes)([])(sim)();
      log("  WASM Entered: " + show1(map28(function(v) {
        return v.label;
      })(result.nodes.entered)))();
      $$void7(rerender("#wasm-sim-psd3")(simulationTree("#f97316")(initialWASMNodes)))();
      register(coord)({
        tick: wasmConsumer(sim)(1e-3)(function __do3() {
          var currentNodes = getNodes2(sim)();
          return $$void7(rerender("#wasm-sim-psd3")(simulationTree("#f97316")(currentNodes)))();
        }),
        onComplete: log("\u2713 WASM simulation (HATS) converged!")
      })();
      return unit;
    };
  };
  var initialD3Nodes = [/* @__PURE__ */ mkNode(0)(150)(75)("A"), /* @__PURE__ */ mkNode(1)(160)(80)("B"), /* @__PURE__ */ mkNode(2)(140)(70)("C"), /* @__PURE__ */ mkNode(3)(155)(85)("D"), /* @__PURE__ */ mkNode(4)(145)(65)("E"), /* @__PURE__ */ mkNode(5)(165)(75)("F")];
  var d3ForceSetup = /* @__PURE__ */ (function() {
    return setup("physics")([withStrength($$static(-80))(manyBody2("charge")), withStrength($$static(1))(withY($$static(75))(withX($$static(150))(center2("center"))))]);
  })();
  var setupD3SimulationPSD3 = function(coord) {
    return function __do2() {
      log("Setting up D3 force simulation with declarative applySetupWithData")();
      var sim = create(defaultConfig)();
      var result = applySetupWithData(d3ForceSetup)(initialD3Nodes)([])(sim)();
      log("  D3 Entered: " + show1(map28(function(v) {
        return v.label;
      })(result.nodes.entered)))();
      $$void7(rerender("#d3-sim-psd3")(simulationTree("#06b6d4")(initialD3Nodes)))();
      register(coord)({
        tick: simulationConsumer(sim)(1e-3)(function __do3() {
          var currentNodes = getNodes(sim)();
          return $$void7(rerender("#d3-sim-psd3")(simulationTree("#06b6d4")(currentNodes)))();
        }),
        onComplete: log("\u2713 D3 simulation (HATS) converged!")
      })();
      return unit;
    };
  };
  var main = /* @__PURE__ */ launchAff_(/* @__PURE__ */ discard22(/* @__PURE__ */ liftEffect2(/* @__PURE__ */ log("PSD3 Visualization Test Starting...")))(function() {
    return discard22(liftEffect2(log("Demonstrating UNIFIED Setup API for D3 and WASM")))(function() {
      return discard22(liftEffect2(log("Initializing WASM force kernel...")))(function() {
        return discard22(initWasm2("./wasm-pkg/force_kernel.js"))(function() {
          return discard22(liftEffect2(log("WASM initialized!")))(function() {
            return liftEffect2(function __do2() {
              var coord = create6();
              setupD3SimulationPSD3(coord)();
              setupWASMSimulationPSD3(coord)();
              log("Starting coordinator...")();
              start2(coord)();
              return log("Both simulations running with unified Setup API!")();
            });
          });
        });
      });
    });
  }));

  // <stdin>
  main();
})();

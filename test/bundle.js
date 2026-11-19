(() => {
  // output/Test.CoordinatorVisualTest/foreign.js
  var getElementById = function(id) {
    return function() {
      const el = document.getElementById(id);
      if (!el) {
        console.error("Element not found:", id);
      }
      return el;
    };
  };
  var setStyle = function(element) {
    return function(property) {
      return function(value) {
        return function() {
          if (element) {
            element.style[property] = value;
          }
        };
      };
    };
  };
  var setTransform = function(element) {
    return function(value) {
      return function() {
        if (element) {
          element.style.transform = value;
        }
      };
    };
  };
  var setPosition = function(element) {
    return function(x) {
      return function(y) {
        return function() {
          if (element) {
            element.style.left = x + "px";
            element.style.top = y + "px";
          }
        };
      };
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
    var map11 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map11($$const(identity2))(a))(b);
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
    var apply12 = apply(dictApplicative.Apply0());
    var pure1 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply12(pure1(f))(a);
      };
    };
  };

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

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
  var replicatePolyfill = function(count, value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = /* @__PURE__ */ (function() {
    function Cons2(head, tail) {
      this.head = head;
      this.tail = tail;
    }
    var emptyList = {};
    function curryCons(head) {
      return function(tail) {
        return new Cons2(head, tail);
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
  var unconsImpl = function(empty3, next, xs) {
    return xs.length === 0 ? empty3({}) : next(xs[0])(xs.slice(1));
  };
  var indexImpl = function(just, nothing, xs, i) {
    return i < 0 || i >= xs.length ? nothing : just(xs[i]);
  };
  var filterImpl = function(f, xs) {
    return xs.filter(f);
  };
  var anyImpl = function(p, xs) {
    var len = xs.length;
    for (var i = 0; i < len; i++) {
      if (p(xs[i])) return true;
    }
    return false;
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

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind2 = bind(dictMonad.Bind1());
    var pure11 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind2(f)(function(f$prime) {
          return bind2(a)(function(a$prime) {
            return pure11(f$prime(a$prime));
          });
        });
      };
    };
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
  var max = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return y;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return x;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
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

  // output/Data.Show/index.js
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
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
  var fromMaybe = function(a) {
    return maybe(a)(identity3);
  };

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
  var map2 = /* @__PURE__ */ map(functorEither);
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map2(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
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

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
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

  // output/Effect/index.js
  var $runtime_lazy = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
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
  var $runtime_lazy2 = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
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

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a) {
      return function(b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
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

  // output/Data.Monoid.Conj/index.js
  var Conj = function(x) {
    return x;
  };
  var semigroupConj = function(dictHeytingAlgebra) {
    var conj2 = conj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return conj2(v)(v1);
        };
      }
    };
  };
  var monoidConj = function(dictHeytingAlgebra) {
    var semigroupConj1 = semigroupConj(dictHeytingAlgebra);
    return {
      mempty: tt(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupConj1;
      }
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var alaF = function() {
    return function() {
      return function() {
        return function() {
          return function(v) {
            return coerce2;
          };
        };
      };
    };
  };

  // output/Data.Foldable/index.js
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure11 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr2 = foldr(dictFoldable);
      return function(f) {
        return foldr2(function($454) {
          return applySecond2(f($454));
        })(pure11(unit));
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
  var foldMapDefaultR = function(dictFoldable) {
    var foldr2 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append3 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr2(function(x) {
          return function(acc) {
            return append3(f(x))(acc);
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
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var all = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Conj)(foldMap2(monoidConj(dictHeytingAlgebra)));
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
    return function(apply12) {
      return function(map11) {
        return function(pure11) {
          return function(f) {
            return function(array) {
              function go(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure11([]);
                  case 1:
                    return map11(array1)(f(array[bot]));
                  case 2:
                    return apply12(map11(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply12(apply12(map11(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply12(map11(concat2)(go(bot, pivot)))(go(pivot, top2));
                }
              }
              return go(0, array.length);
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

  // output/Data.Array/index.js
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
  var snoc = function(xs) {
    return function(x) {
      return withArray(push(x))(xs)();
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
  var fromFoldable = function(dictFoldable) {
    return runFn2(fromFoldableImpl)(foldr(dictFoldable));
  };
  var filter = /* @__PURE__ */ runFn2(filterImpl);
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

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }

  // output/Data.Nullable/index.js
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

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
      var size2 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size2 !== 0) {
          size2--;
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
          if (size2 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size2) % limit] = cb;
          size2++;
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
      var step = aff;
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
                step = bhead(step);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail = util.left(e);
                step = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step)) {
                status = RETURN;
                fail = step;
                step = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step = util.fromRight(step);
              }
              break;
            case CONTINUE:
              switch (step.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step._2;
                  status = CONTINUE;
                  step = step._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step = util.right(step._1);
                  } else {
                    status = STEP_BIND;
                    step = step._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step = runSync(util.left, util.right, step._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step = runAsync(util.left, step._1, function(result2) {
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
                        step = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail = util.left(step._1);
                  step = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step._1) {
                    tmp.run();
                  }
                  step = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step = sequential2(util, supervisor, step._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step = interrupt || fail || step;
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
                      step = attempt._2(util.fromLeft(fail));
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
                      step = util.fromRight(step);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
                  case BRACKET:
                    bracketCount--;
                    if (fail === null) {
                      result = util.fromRight(step);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step = attempt._3(result);
                      }
                    }
                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail) {
                      step = attempt._1.failed(util.fromLeft(fail))(attempt._2);
                    } else {
                      step = attempt._1.completed(util.fromRight(step))(attempt._2);
                    }
                    fail = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail), attempts, interrupt);
                    status = CONTINUE;
                    step = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step = attempt._1;
                    fail = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step));
                }
              }
              joins = null;
              if (interrupt && fail) {
                setTimeout(function() {
                  throw util.fromLeft(fail);
                }, 0);
              } else if (util.isLeft(step) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step);
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
      function onComplete(join3) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join3.rethrow;
            join3.handler(step)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join3;
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
              step = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error3);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step(error3)), attempts, interrupt);
                }
                status = RETURN;
                step = null;
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
                step = null;
                fail = null;
              }
          }
          return canceler;
        };
      }
      function join2(cb) {
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
        join: join2,
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
        var step = par2;
        var head = null;
        var tail = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop: while (true) {
          tmp = null;
          switch (step.tag) {
            case FORKED:
              if (step._3 === EMPTY) {
                tmp = fibers[step._1];
                kills2[count++] = tmp.kill(error3, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head === null) {
                break loop;
              }
              step = head._2;
              if (tail === null) {
                head = null;
              } else {
                head = tail._1;
                tail = tail._2;
              }
              break;
            case MAP:
              step = step._2;
              break;
            case APPLY:
            case ALT:
              if (head) {
                tail = new Aff2(CONS, head, tail);
              }
              head = step;
              step = step._1;
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
      function join2(result, head, tail) {
        var fail, step, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail = result;
          step = null;
        } else {
          step = result;
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
          if (head === null) {
            cb(fail || step)();
            return;
          }
          if (head._3 !== EMPTY) {
            return;
          }
          switch (head.tag) {
            case MAP:
              if (fail === null) {
                head._3 = util.right(head._1(util.fromRight(step)));
                step = head._3;
              } else {
                head._3 = fail;
              }
              break;
            case APPLY:
              lhs = head._1._3;
              rhs = head._2._3;
              if (fail) {
                head._3 = fail;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail === lhs ? head._2 : head._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join2(fail, null, null);
                    } else {
                      join2(fail, tail._1, tail._2);
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
                step = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head._3 = step;
              }
              break;
            case ALT:
              lhs = head._1._3;
              rhs = head._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail = step === lhs ? rhs : lhs;
                step = null;
                head._3 = fail;
              } else {
                head._3 = step;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, step === lhs ? head._2 : head._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join2(step, null, null);
                    } else {
                      join2(step, tail._1, tail._2);
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
            head = null;
          } else {
            head = tail._1;
            tail = tail._2;
          }
        }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join2(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step = par;
        var head = null;
        var tail = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step.tag) {
                case MAP:
                  if (head) {
                    tail = new Aff2(CONS, head, tail);
                  }
                  head = new Aff2(MAP, step._1, EMPTY, EMPTY);
                  step = step._2;
                  break;
                case APPLY:
                  if (head) {
                    tail = new Aff2(CONS, head, tail);
                  }
                  head = new Aff2(APPLY, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;
                case ALT:
                  if (head) {
                    tail = new Aff2(CONS, head, tail);
                  }
                  head = new Aff2(ALT, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step;
                  step = new Aff2(FORKED, fid, new Aff2(CONS, head, tail), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head === null) {
                break loop;
              }
              if (head._1 === EMPTY) {
                head._1 = step;
                status = CONTINUE;
                step = head._2;
                head._2 = EMPTY;
              } else {
                head._2 = step;
                step = head;
                if (tail === null) {
                  head = null;
                } else {
                  head = tail._1;
                  tail = tail._2;
                }
              }
          }
        }
        root = step;
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
        return Aff.Bind(aff, function(value) {
          return Aff.Pure(f(value));
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

  // output/Effect.Class/index.js
  var liftEffect = function(dict) {
    return dict.liftEffect;
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
  var $runtime_lazy3 = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
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
        var rev = (function() {
          var go = function($copy_v) {
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
          return go(Nil.value);
        })();
        var $284 = foldl(foldableList)(flip(f))(b);
        return function($285) {
          return $284(rev($285));
        };
      };
    },
    foldl: function(f) {
      var go = function($copy_b) {
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
      return go;
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
  var $runtime_lazy4 = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
      return val;
    };
  };
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var Leaf = /* @__PURE__ */ (function() {
    function Leaf3() {
    }
    ;
    Leaf3.value = new Leaf3();
    return Leaf3;
  })();
  var Node = /* @__PURE__ */ (function() {
    function Node2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new Node2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return Node2;
  })();
  var Split = /* @__PURE__ */ (function() {
    function Split2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Split2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Split2(value0, value1, value2);
        };
      };
    };
    return Split2;
  })();
  var SplitLast = /* @__PURE__ */ (function() {
    function SplitLast2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    SplitLast2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new SplitLast2(value0, value1, value2);
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
    var height = function(v) {
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
      if (rl instanceof Node && rl.value0 > height(rr)) {
        return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
      }
      ;
      return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
    };
    var rotateRight = function(k, v, lk, lv, ll, lr, r) {
      if (lr instanceof Node && height(ll) <= lr.value0) {
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
  var lookup = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      var go = function($copy_v) {
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
      return go;
    };
  };
  var insert = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton4(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare2(k)(v1.value2);
            if (v2 instanceof LT) {
              return unsafeBalancedNode(v1.value2, v1.value3, go(v1.value4), v1.value5);
            }
            ;
            if (v2 instanceof GT) {
              return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go(v1.value5));
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
        return go;
      };
    };
  };
  var functorMap = {
    map: function(f) {
      var go = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          return new Node(v.value0, v.value1, v.value2, f(v.value3), go(v.value4), go(v.value5));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 10 - line 150, column 39): " + [v.constructor.name]);
      };
      return go;
    }
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
        var go = $lazy_go(169);
        return function(m) {
          return go(m, z);
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
        var go = $lazy_go(175);
        return function(m) {
          return go(z, m);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append1 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append1(go(v.value4))(append1(f(v.value3))(go(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 181, column 10 - line 184, column 28): " + [v.constructor.name]);
        };
        return go;
      };
    }
  };
  var traversableMap = {
    traverse: function(dictApplicative) {
      var pure11 = pure(dictApplicative);
      var Apply0 = dictApplicative.Apply0();
      var apply12 = apply(Apply0);
      var map14 = map(Apply0.Functor0());
      return function(f) {
        var go = function(v) {
          if (v instanceof Leaf) {
            return pure11(Leaf.value);
          }
          ;
          if (v instanceof Node) {
            return apply12(apply12(map14(function(l$prime) {
              return function(v$prime) {
                return function(r$prime) {
                  return new Node(v.value0, v.value1, v.value2, v$prime, l$prime, r$prime);
                };
              };
            })(go(v.value4)))(f(v.value3)))(go(v.value5));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 209, column 10 - line 215, column 19): " + [v.constructor.name]);
        };
        return go;
      };
    },
    sequence: function(dictApplicative) {
      return traverse(traversableMap)(dictApplicative)(identity5);
    },
    Functor0: function() {
      return functorMap;
    },
    Foldable1: function() {
      return foldableMap;
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

  // output/Data.Int/foreign.js
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isNaNImpl = isNaN;
  var infinity = Infinity;
  var isFiniteImpl = isFinite;
  var abs2 = Math.abs;
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

  // output/Data.Int/index.js
  var odd = function(x) {
    return (x & 1) !== 0;
  };

  // output/Hylograph.Forces.Node/foreign.js
  var writeVelocity = (node, vx, vy) => {
    node.vx = vx;
    node.vy = vy;
  };
  var writePosition = (node, x, y) => {
    node.x = x;
    node.y = y;
  };
  var writeIndex = (node, index2) => {
    node.index = index2;
  };

  // output/Hylograph.Forces.Center/index.js
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

  // output/Hylograph.Forces.Group/index.js
  var alter2 = /* @__PURE__ */ alter(ordInt);
  var fromFoldable3 = /* @__PURE__ */ fromFoldable(foldableList);
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
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
  var grouped = function(membership) {
    return function(items) {
      var add2 = function(item) {
        return function(members) {
          if (members instanceof Nothing) {
            return new Just([item]);
          }
          ;
          if (members instanceof Just) {
            return new Just(snoc(members.value0)(item));
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Group (line 37, column 22 - line 39, column 45): " + [members.constructor.name]);
        };
      };
      var place = function(groups) {
        return function(item) {
          var v = membership(item);
          if (v instanceof Ungrouped) {
            return groups;
          }
          ;
          if (v instanceof InGroup) {
            return alter2(add2(item))(v.value0)(groups);
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.Group (line 33, column 23 - line 35, column 55): " + [v.constructor.name]);
        };
      };
      return fromFoldable3(values(foldl2(place)(empty2)(items)));
    };
  };

  // output/Hylograph.Forces.Cohesion/index.js
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var apply3 = function(v) {
    return function(alpha) {
      return foreachE(v)(function(members) {
        return function __do2() {
          var sum2 = $$new({
            x: 0,
            y: 0,
            count: 0
          })();
          foreachE(members)(function(member2) {
            return when2(isFiniteImpl(member2.node.x) && isFiniteImpl(member2.node.y))(function __do3() {
              var total2 = read(sum2)();
              return write({
                x: total2.x + member2.node.x,
                y: total2.y + member2.node.y,
                count: total2.count + 1
              })(sum2)();
            });
          })();
          var total = read(sum2)();
          var centroidY = total.y / total.count;
          var centroidX = total.x / total.count;
          return foreachE(members)(function(member2) {
            return when2(isFiniteImpl(member2.node.x) && isFiniteImpl(member2.node.y))(function() {
              return writeVelocity(member2.node, member2.node.vx + (centroidX - member2.node.x) * member2.strength * alpha, member2.node.vy + (centroidY - member2.node.y) * member2.strength * alpha);
            });
          })();
        };
      });
    };
  };

  // output/Data.List/index.js
  var eq3 = /* @__PURE__ */ eq(eqOrdering);
  var notEq2 = /* @__PURE__ */ notEq(eqOrdering);
  var singleton5 = function(a) {
    return new Cons(a, Nil.value);
  };
  var sortBy = function(cmp) {
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
            if (v2 instanceof Cons && notEq2(cmp(v)(v2.value0))(GT.value)) {
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
    var go = function($copy_v) {
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
    return go(Nil.value);
  })();

  // output/Hylograph.Forces.Quadtree/index.js
  var $runtime_lazy5 = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
      return val;
    };
  };
  var Empty = /* @__PURE__ */ (function() {
    function Empty2() {
    }
    ;
    Empty2.value = new Empty2();
    return Empty2;
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
    function Branch2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Branch2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Branch2(value0, value1, value2, value3);
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
        throw new Error("Failed pattern match at Hylograph.Forces.Quadtree (line 168, column 10 - line 170, column 19): " + [v.constructor.name]);
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
        throw new Error("Failed pattern match at Hylograph.Forces.Quadtree (line 156, column 26 - line 160, column 16): " + [others.constructor.name]);
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
        var width = extent.x1 - extent.x0;
        var side = (function() {
          var $23 = width < 0 || width > 0;
          if ($23) {
            return width;
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
                  function $tco_loop(x0, y0, x1, y1, size2) {
                    var $24 = x0 > x || (x >= x1 || (y0 > y || y >= y1));
                    if ($24) {
                      var doubled = size2 * 2;
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
  var $lazy_cell = /* @__PURE__ */ $runtime_lazy5("cell", "Hylograph.Forces.Quadtree", function() {
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
                  throw new Error("Failed pattern match at Hylograph.Forces.Quadtree (line 122, column 62 - line 135, column 78): " + [remaining.constructor.name]);
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
      throw new Error("Failed pattern match at Hylograph.Forces.Quadtree (line 137, column 5 - line 141, column 41): " + [points.constructor.name]);
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
                  throw new Error("Failed pattern match at Hylograph.Forces.Quadtree (line 62, column 39 - line 76, column 47): " + [remaining.constructor.name]);
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

  // output/Hylograph.Forces.Random/index.js
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

  // output/Hylograph.Forces.Collide/index.js
  var $runtime_lazy6 = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
      return val;
    };
  };
  var pure3 = /* @__PURE__ */ pure(applicativeEffect);
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var NoPushes = /* @__PURE__ */ (function() {
    function NoPushes2() {
    }
    ;
    NoPushes2.value = new NoPushes2();
    return NoPushes2;
  })();
  var Push = /* @__PURE__ */ (function() {
    function Push2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Push2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Push2(value0, value1, value2, value3);
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
    function Region2(value0, value1, value2, value3, value4, value5, value6, value7) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
      this.value6 = value6;
      this.value7 = value7;
    }
    ;
    Region2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return function(value6) {
                  return function(value7) {
                    return new Region2(value0, value1, value2, value3, value4, value5, value6, value7);
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
  var seek = function(index2, radius, nodeX, nodeY, start3) {
    var go = function($copy_region) {
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
            var $23 = region.value7.value0 > index2;
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
          throw new Error("Failed pattern match at Hylograph.Forces.Collide (line 254, column 11 - line 266, column 28): " + [region.value7.constructor.name]);
        }
        ;
        if (region instanceof Finished) {
          $tco_done = true;
          return Finished.value;
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Collide (line 249, column 17 - line 267, column 27): " + [region.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_region);
      }
      ;
      return $tco_result;
    };
    return go(start3);
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
      throw new Error("Failed pattern match at Hylograph.Forces.Collide (line 187, column 10 - line 189, column 25): " + [region.constructor.name]);
    };
    var $lazy_thread = $runtime_lazy6("thread", "Hylograph.Forces.Collide", function() {
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
        throw new Error("Failed pattern match at Hylograph.Forces.Collide (line 160, column 45 - line 176, column 57): " + [tree.constructor.name]);
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
  var walk = function(strength, index2, radius, nodeX, nodeY, chain, startVx, startVy, startSeed, startPushes) {
    var go = function($copy_start) {
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
              function $tco_loop(start3, vx, vy, seed, pushes) {
                var v = seek(index2, radius, nodeX, nodeY, start3);
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
    return go(chain)(startVx)(startVy)(startSeed)(startPushes);
  };
  var initialize2 = function(config) {
    return function(random) {
      return function(nodes) {
        return {
          bodies: mapWithIndex2(function(index2) {
            return function(node) {
              return {
                index: index2,
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
        throw new Error("Failed pattern match at Hylograph.Forces.Collide (line 313, column 7 - line 318, column 21): " + [remaining.constructor.name]);
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
            return when3(body.included)(function __do3() {
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

  // output/Hylograph.Forces.Contain/index.js
  var when4 = /* @__PURE__ */ when(applicativeEffect);
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
  var Circle = /* @__PURE__ */ (function() {
    function Circle2(value0) {
      this.value0 = value0;
    }
    ;
    Circle2.create = function(value0) {
      return new Circle2(value0);
    };
    return Circle2;
  })();
  var Polygon = /* @__PURE__ */ (function() {
    function Polygon2(value0) {
      this.value0 = value0;
    }
    ;
    Polygon2.create = function(value0) {
      return new Polygon2(value0);
    };
    return Polygon2;
  })();
  var pass2 = function($copy_edges) {
    return function($copy_radius) {
      return function($copy_halfWidth) {
        return function($copy_halfHeight) {
          return function($copy_x) {
            return function($copy_y) {
              return function($copy_moved) {
                var $tco_var_edges = $copy_edges;
                var $tco_var_radius = $copy_radius;
                var $tco_var_halfWidth = $copy_halfWidth;
                var $tco_var_halfHeight = $copy_halfHeight;
                var $tco_var_x = $copy_x;
                var $tco_var_y = $copy_y;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(edges, radius, halfWidth, halfHeight, x, y, moved) {
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
                    var reach = radius + abs2(edges.value0.normalX) * halfWidth + abs2(edges.value0.normalY) * halfHeight;
                    var inside = (x - edges.value0.x) * edges.value0.normalX + (y - edges.value0.y) * edges.value0.normalY;
                    var $31 = inside < reach;
                    if ($31) {
                      $tco_var_edges = edges.value1;
                      $tco_var_radius = radius;
                      $tco_var_halfWidth = halfWidth;
                      $tco_var_halfHeight = halfHeight;
                      $tco_var_x = x + edges.value0.normalX * (reach - inside);
                      $tco_var_y = y + edges.value0.normalY * (reach - inside);
                      $copy_moved = true;
                      return;
                    }
                    ;
                    $tco_var_edges = edges.value1;
                    $tco_var_radius = radius;
                    $tco_var_halfWidth = halfWidth;
                    $tco_var_halfHeight = halfHeight;
                    $tco_var_x = x;
                    $tco_var_y = y;
                    $copy_moved = moved;
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Hylograph.Forces.Contain (line 171, column 52 - line 180, column 59): " + [edges.constructor.name]);
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_edges, $tco_var_radius, $tco_var_halfWidth, $tco_var_halfHeight, $tco_var_x, $tco_var_y, $copy_moved);
                }
                ;
                return $tco_result;
              };
            };
          };
        };
      };
    };
  };
  var passes = function($copy_edges) {
    return function($copy_radius) {
      return function($copy_halfWidth) {
        return function($copy_halfHeight) {
          return function($copy_remaining) {
            return function($copy_x) {
              return function($copy_y) {
                var $tco_var_edges = $copy_edges;
                var $tco_var_radius = $copy_radius;
                var $tco_var_halfWidth = $copy_halfWidth;
                var $tco_var_halfHeight = $copy_halfHeight;
                var $tco_var_remaining = $copy_remaining;
                var $tco_var_x = $copy_x;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(edges, radius, halfWidth, halfHeight, remaining, x, y) {
                  var $34 = remaining <= 0;
                  if ($34) {
                    $tco_done = true;
                    return {
                      x,
                      y
                    };
                  }
                  ;
                  var after = pass2(edges)(radius)(halfWidth)(halfHeight)(x)(y)(false);
                  if (after.moved) {
                    $tco_var_edges = edges;
                    $tco_var_radius = radius;
                    $tco_var_halfWidth = halfWidth;
                    $tco_var_halfHeight = halfHeight;
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
                  $tco_result = $tco_loop($tco_var_edges, $tco_var_radius, $tco_var_halfWidth, $tco_var_halfHeight, $tco_var_remaining, $tco_var_x, $copy_y);
                }
                ;
                return $tco_result;
              };
            };
          };
        };
      };
    };
  };
  var isHard = function(v) {
    return v.hard;
  };
  var confine = function(inset) {
    return function(low) {
      return function(high) {
        return function(value) {
          var most = high - inset;
          var least = low + inset;
          var $40 = least > most;
          if ($40) {
            return (low + high) / 2;
          }
          ;
          var $41 = value < least;
          if ($41) {
            return least;
          }
          ;
          var $42 = value > most;
          if ($42) {
            return most;
          }
          ;
          return value;
        };
      };
    };
  };
  var project = function(region) {
    return function(radius) {
      return function(halfWidth) {
        return function(halfHeight) {
          return function(x) {
            return function(y) {
              if (region instanceof Rectangle) {
                return {
                  x: confine(radius + halfWidth)(region.value0.x0)(region.value0.x1)(x),
                  y: confine(radius + halfHeight)(region.value0.y0)(region.value0.y1)(y)
                };
              }
              ;
              if (region instanceof Circle) {
                var reach = region.value0.radius - radius - sqrt(halfWidth * halfWidth + halfHeight * halfHeight);
                var offsetY = y - region.value0.y;
                var offsetX = x - region.value0.x;
                var inner = (function() {
                  var $45 = reach > 0;
                  if ($45) {
                    return reach;
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
              if (region instanceof Polygon) {
                return passes(region.value0)(radius)(halfWidth)(halfHeight)(8)(x)(y);
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Contain (line 124, column 50 - line 141, column 66): " + [region.constructor.name]);
            };
          };
        };
      };
    };
  };
  var constrain = function(v) {
    return when4(v.hard)(foreachE(v.kept)(function(kept) {
      return when4(isFiniteImpl(kept.node.x) && isFiniteImpl(kept.node.y))((function() {
        var allowed = project(kept.region)(kept.radius)(kept.halfWidth)(kept.halfHeight)(kept.node.x)(kept.node.y);
        return function __do2() {
          writeVelocity(kept.node, kept.node.vx + (allowed.x - kept.node.x), kept.node.vy + (allowed.y - kept.node.y));
          return writePosition(kept.node, allowed.x, allowed.y);
        };
      })());
    }));
  };
  var apply5 = function(v) {
    return foreachE(v.kept)(function(kept) {
      var aheadY = kept.node.y + kept.node.vy;
      var aheadX = kept.node.x + kept.node.vx;
      return when4(isFiniteImpl(aheadX) && isFiniteImpl(aheadY))((function() {
        var allowed = project(kept.region)(kept.radius)(kept.halfWidth)(kept.halfHeight)(aheadX)(aheadY);
        return function() {
          return writeVelocity(kept.node, kept.node.vx + (allowed.x - aheadX) * v.strength, kept.node.vy + (allowed.y - aheadY) * v.strength);
        };
      })());
    });
  };

  // output/Hylograph.Forces.Linkage/index.js
  var when5 = /* @__PURE__ */ when(applicativeST);
  var map3 = /* @__PURE__ */ map(functorArray);
  var map1 = /* @__PURE__ */ map(functorEither);
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
          return when5(connected(link))(function __do3() {
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
      return catMaybes(map3(joined)(links));
    };
  };
  var joinById = function(dictOrd) {
    var fromFoldable6 = fromFoldable2(dictOrd)(foldableArray);
    var lookup2 = lookup(dictOrd);
    return function(dictShow) {
      var show3 = show(dictShow);
      return function(identify) {
        return function(nodes) {
          return function(links) {
            var places = fromFoldable6(mapWithIndex2(function(place) {
              return function(node) {
                return new Tuple(identify(node), place);
              };
            })(nodes));
            var find2 = function(id) {
              var v = lookup2(id)(places);
              if (v instanceof Just) {
                return new Right(v.value0);
              }
              ;
              if (v instanceof Nothing) {
                return new Left("node not found: " + show3(id));
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Linkage (line 72, column 13 - line 74, column 52): " + [v.constructor.name]);
            };
            var placed = function(link) {
              var v = find2(link.source);
              if (v instanceof Left) {
                return new Left(v.value0);
              }
              ;
              if (v instanceof Right) {
                var v1 = find2(link.target);
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
                throw new Error("Failed pattern match at Hylograph.Forces.Linkage (line 78, column 21 - line 80, column 53): " + [v1.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Linkage (line 76, column 17 - line 80, column 53): " + [v.constructor.name]);
            };
            return map1(joinPlaces(nodes))(traverse2(placed)(links));
          };
        };
      };
    };
  };

  // output/Hylograph.Forces.Directional/index.js
  var when6 = /* @__PURE__ */ when(applicativeEffect);
  var pure4 = /* @__PURE__ */ pure(applicativeEffect);
  var passLimit = 32;
  var isHard2 = function(v) {
    return v.hard;
  };
  var constrain2 = function(v) {
    var held = function(node) {
      return v.axisX !== 0 && v.heldX(node) || v.axisY !== 0 && v.heldY(node);
    };
    return when6(v.hard)(function __do2() {
      var moving = $$new(true)();
      return forE(0)(passLimit)(function(v1) {
        return function __do3() {
          var wanted = read(moving)();
          return when6(wanted)(function __do4() {
            write(false)(moving)();
            return foreachE(v.links)(function(link) {
              var y = link.target.y - link.source.y;
              var x = link.target.x - link.source.x;
              var along = x * v.axisX + y * v.axisY;
              return when6(isFiniteImpl(link.target.x) && (isFiniteImpl(link.target.y) && (isFiniteImpl(link.source.x) && (isFiniteImpl(link.source.y) && along < link.gap))))((function() {
                var $$short = link.gap - along;
                var stepX = v.axisX * $$short;
                var stepY = v.axisY * $$short;
                var $30 = !held(link.target);
                if ($30) {
                  return function __do5() {
                    writeVelocity(link.target, link.target.vx + stepX, link.target.vy + stepY);
                    writePosition(link.target, link.target.x + stepX, link.target.y + stepY);
                    return write(true)(moving)();
                  };
                }
                ;
                var $31 = !held(link.source);
                if ($31) {
                  return function __do5() {
                    writeVelocity(link.source, link.source.vx - stepX, link.source.vy - stepY);
                    writePosition(link.source, link.source.x - stepX, link.source.y - stepY);
                    return write(true)(moving)();
                  };
                }
                ;
                return pure4(unit);
              })());
            })();
          })();
        };
      })();
    });
  };
  var apply6 = function(v) {
    return function(alpha) {
      return forE(0)(v.iterations)(function(v1) {
        return foreachE(v.links)(function(link) {
          var y = link.target.y + link.target.vy - link.source.y - link.source.vy;
          var x = link.target.x + link.target.vx - link.source.x - link.source.vx;
          var along = x * v.axisX + y * v.axisY;
          return when6(along < link.gap)((function() {
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

  // output/Hylograph.Forces.Link/index.js
  var min3 = /* @__PURE__ */ min(ordNumber);
  var map4 = /* @__PURE__ */ map(functorArray);
  var pure5 = /* @__PURE__ */ pure(applicativeEffect);
  var map12 = /* @__PURE__ */ map(functorEither);
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
              throw new Error("Failed pattern match at Hylograph.Forces.Link (line 119, column 17 - line 121, column 46): " + [config.strength.constructor.name]);
            })(),
            bias: end.bias
          };
        };
        return {
          springs: map4(spring)(joined),
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
    return function(value) {
      var $24 = value < 0 || value > 0;
      if ($24) {
        return pure5(value);
      }
      ;
      return draw(random);
    };
  };
  var initializeById = function(dictOrd) {
    var joinById2 = joinById(dictOrd);
    return function(dictShow) {
      var joinById1 = joinById2(dictShow);
      return function(identify) {
        return function(config) {
          return function(random) {
            return function(nodes) {
              return function(links) {
                return map12(springs(config)(random))(joinById1(identify)(nodes)(links));
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

  // output/Hylograph.Forces.ManyBody/index.js
  var $runtime_lazy7 = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
      return val;
    };
  };
  var map5 = /* @__PURE__ */ map(functorArray);
  var EveryPair = /* @__PURE__ */ (function() {
    function EveryPair3() {
    }
    ;
    EveryPair3.value = new EveryPair3();
    return EveryPair3;
  })();
  var WithinGroups = /* @__PURE__ */ (function() {
    function WithinGroups3() {
    }
    ;
    WithinGroups3.value = new WithinGroups3();
    return WithinGroups3;
  })();
  var BetweenGroups = /* @__PURE__ */ (function() {
    function BetweenGroups3() {
    }
    ;
    BetweenGroups3.value = new BetweenGroups3();
    return BetweenGroups3;
  })();
  var NoMembers = /* @__PURE__ */ (function() {
    function NoMembers2() {
    }
    ;
    NoMembers2.value = new NoMembers2();
    return NoMembers2;
  })();
  var Member = /* @__PURE__ */ (function() {
    function Member2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Member2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Member2(value0, value1, value2);
        };
      };
    };
    return Member2;
  })();
  var Cluster = /* @__PURE__ */ (function() {
    function Cluster2(value0, value1, value2, value3, value4, value5, value6) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
      this.value6 = value6;
    }
    ;
    Cluster2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return function(value6) {
                  return new Cluster2(value0, value1, value2, value3, value4, value5, value6);
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
  var weigh = function(cluster) {
    if (cluster instanceof Cluster) {
      var weight = abs2(cluster.value0);
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
    throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 275, column 17 - line 281, column 18): " + [cluster.constructor.name]);
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
    throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 289, column 22 - line 291, column 18): " + [cluster.constructor.name]);
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
    return function(index2) {
      return function(alpha) {
        return function(x) {
          return function(y) {
            return function(distance2) {
              var go = function($copy_remaining) {
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
                        var $47 = remaining.value0 === index2;
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
                      throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 443, column 24 - line 451, column 50): " + [remaining.constructor.name]);
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
              return go(nodes);
            };
          };
        };
      };
    };
  };
  var near = function(limits, alpha, index2, members, x, y, distance2, vx, vy, seed) {
    if (members instanceof Member && (members.value2 instanceof NoMembers && members.value0 === index2)) {
      return {
        vx,
        vy,
        seed
      };
    }
    ;
    var apart = separate(limits)(x)(y)(distance2)(seed);
    var pushed = pushFrom(members)(index2)(alpha)(apart.x)(apart.y)(apart.distance2)(vx)(vy);
    return {
      vx: pushed.vx,
      vy: pushed.vy,
      seed: apart.seed
    };
  };
  var initialize3 = function(config) {
    return function(random) {
      return function(nodes) {
        var negated = function(body) {
          return {
            index: body.index,
            node: body.node,
            strength: -1 * body.strength
          };
        };
        var bodies = mapWithIndex2(function(index2) {
          return function(node) {
            return {
              index: index2,
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
              })(map5(negated)(bodies));
            }
            ;
            throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 127, column 13 - line 130, column 86): " + [config.scope.constructor.name]);
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
  var walk2 = function(limits, alpha, index2, nodeX, nodeY, chain, startVx, startVy, startSeed) {
    var go = function($copy_cluster) {
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
                var moved = near(limits, alpha, index2, cluster.value6, x, y, distance2, vx, vy, seed);
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
              throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 326, column 29 - line 357, column 35): " + [cluster.constructor.name]);
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
    return go(chain)(startVx)(startVy)(startSeed);
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
    throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 299, column 21 - line 301, column 18): " + [cluster.constructor.name]);
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
    throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 294, column 21 - line 296, column 18): " + [cluster.constructor.name]);
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
            throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 268, column 21 - line 270, column 58): " + [nodes.constructor.name]);
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
            throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 262, column 33 - line 264, column 90): " + [oldestFirst.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_oldestFirst, $copy_newer);
          }
          ;
          return $tco_result;
        };
      };
      var $lazy_thread = $runtime_lazy7("thread", "Hylograph.Forces.ManyBody", function() {
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
          throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 205, column 39 - line 249, column 106): " + [tree.constructor.name]);
        };
      });
      var thread = $lazy_thread(204);
      return thread(planted.root, planted.extent.x0, planted.extent.x1, Finished2.value);
    };
  };
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
      throw new Error("Failed pattern match at Hylograph.Forces.ManyBody (line 146, column 32 - line 151, column 32): " + [v.scope.constructor.name]);
    };
  };

  // output/Hylograph.Forces.Position/index.js
  var map6 = /* @__PURE__ */ map(functorArray);
  var initialize4 = function(config) {
    return function(nodes) {
      var pulled = function(node) {
        var target = config.target(node);
        return {
          node,
          target,
          strength: (function() {
            var $8 = isNaNImpl(target);
            if ($8) {
              return 0;
            }
            ;
            return config.strength(node);
          })()
        };
      };
      return map6(pulled)(nodes);
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

  // output/Hylograph.Forces.Radial/index.js
  var map7 = /* @__PURE__ */ map(functorArray);
  var orTiny = function(value) {
    var $11 = value < 0 || value > 0;
    if ($11) {
      return value;
    }
    ;
    return 1e-6;
  };
  var initialize5 = function(config) {
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
        nodes: map7(pulled)(nodes),
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

  // output/Hylograph.Forces.RectCollide/index.js
  var $runtime_lazy8 = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
      return val;
    };
  };
  var comparing2 = /* @__PURE__ */ comparing(ordInt);
  var pure6 = /* @__PURE__ */ pure(applicativeEffect);
  var when7 = /* @__PURE__ */ when(applicativeEffect);
  var StaysPut = /* @__PURE__ */ (function() {
    function StaysPut2() {
    }
    ;
    StaysPut2.value = new StaysPut2();
    return StaysPut2;
  })();
  var GoesThrough = /* @__PURE__ */ (function() {
    function GoesThrough2() {
    }
    ;
    GoesThrough2.value = new GoesThrough2();
    return GoesThrough2;
  })();
  var EveryPair2 = /* @__PURE__ */ (function() {
    function EveryPair3() {
    }
    ;
    EveryPair3.value = new EveryPair3();
    return EveryPair3;
  })();
  var WithinGroups2 = /* @__PURE__ */ (function() {
    function WithinGroups3() {
    }
    ;
    WithinGroups3.value = new WithinGroups3();
    return WithinGroups3;
  })();
  var BetweenGroups2 = /* @__PURE__ */ (function() {
    function BetweenGroups3() {
    }
    ;
    BetweenGroups3.value = new BetweenGroups3();
    return BetweenGroups3;
  })();
  var Cell = /* @__PURE__ */ (function() {
    function Cell2(value0, value1, value2, value3, value4, value5, value6, value7, value8) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
      this.value6 = value6;
      this.value7 = value7;
      this.value8 = value8;
    }
    ;
    Cell2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return function(value6) {
                  return function(value7) {
                    return function(value8) {
                      return new Cell2(value0, value1, value2, value3, value4, value5, value6, value7, value8);
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
  var may = function(pairs) {
    return function(box) {
      return function(other) {
        if (pairs instanceof EveryPair2) {
          return true;
        }
        ;
        if (pairs instanceof WithinGroups2) {
          return box.group >= 0 && box.group === other.group;
        }
        ;
        if (pairs instanceof BetweenGroups2) {
          return !(box.group >= 0 && box.group === other.group);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 358, column 23 - line 361, column 68): " + [pairs.constructor.name]);
      };
    };
  };
  var inIndexOrder = function(found) {
    if (found instanceof Cons && found.value1 instanceof Nil) {
      return found;
    }
    ;
    return sortBy(comparing2(function(v) {
      return v.item.index;
    }))(found);
  };
  var direction = function(offset, tie, overlap) {
    var $43 = offset > 0;
    if ($43) {
      return 1 * overlap;
    }
    ;
    var $44 = offset < 0;
    if ($44) {
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
              var $46 = occupants.value0.item.halfWidth > most;
              if ($46) {
                return occupants.value0.item.halfWidth;
              }
              ;
              return most;
            })();
            return;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 426, column 27 - line 428, column 104): " + [occupants.constructor.name]);
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
        var $51 = cell2.value0 > most;
        if ($51) {
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
      throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 438, column 10 - line 440, column 23): " + [cell2.constructor.name]);
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
              var $62 = occupants.value0.item.halfHeight > most;
              if ($62) {
                return occupants.value0.item.halfHeight;
              }
              ;
              return most;
            })();
            return;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 431, column 28 - line 433, column 107): " + [occupants.constructor.name]);
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
        var $67 = cell2.value1 > most;
        if ($67) {
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
      throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 445, column 10 - line 447, column 23): " + [cell2.constructor.name]);
    };
    var $lazy_thread = $runtime_lazy8("thread", "Hylograph.Forces.RectCollide", function() {
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
          var fourth = $lazy_thread(412)(tree.value3, middleX, middleY, x1, y1, after);
          var third = $lazy_thread(413)(tree.value2, x0, middleY, middleX, y1, fourth);
          var second = $lazy_thread(414)(tree.value1, middleX, y0, x1, middleY, third);
          var first = $lazy_thread(415)(tree.value0, x0, y0, middleX, middleY, second);
          var height = taller(tree.value3, fourth, taller(tree.value2, third, taller(tree.value1, second, taller(tree.value0, first, 0))));
          var width = wider(tree.value3, fourth, wider(tree.value2, third, wider(tree.value1, second, wider(tree.value0, first, 0))));
          return new Cell(width, height, x0, y0, x1, y1, first, after, Nil.value);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 401, column 45 - line 423, column 54): " + [tree.constructor.name]);
      };
    });
    var thread = $lazy_thread(400);
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
  var among = function(allowed, halfWidth, halfHeight, aheadX, aheadY, occupants, start3) {
    var go = function($copy_remaining) {
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
            var $85 = allowed(remaining.value0.item) && (halfWidth + remaining.value0.item.halfWidth - abs2(aheadX - remaining.value0.x) > 0 && halfHeight + remaining.value0.item.halfHeight - abs2(aheadY - remaining.value0.y) > 0);
            if ($85) {
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
          throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 477, column 26 - line 484, column 27): " + [remaining.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_remaining, $copy_found);
        }
        ;
        return $tco_result;
      };
    };
    return go(occupants)(start3);
  };
  var overlapping = function(allowed, halfWidth, halfHeight, aheadX, aheadY, chain, start3) {
    var go = function($copy_cell) {
      return function($copy_found) {
        var $tco_var_cell = $copy_cell;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(cell2, found) {
          if (cell2 instanceof Cell) {
            if (cell2.value8 instanceof Nil) {
              var $90 = cell2.value2 - aheadX > halfWidth + cell2.value0 || (aheadX - cell2.value4 > halfWidth + cell2.value0 || (cell2.value3 - aheadY > halfHeight + cell2.value1 || aheadY - cell2.value5 > halfHeight + cell2.value1));
              if ($90) {
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
            $copy_found = among(allowed, halfWidth, halfHeight, aheadX, aheadY, cell2.value8, found);
            return;
          }
          ;
          if (cell2 instanceof Finished3) {
            $tco_done = true;
            return found;
          }
          ;
          throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 458, column 21 - line 467, column 24): " + [cell2.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_cell, $copy_found);
        }
        ;
        return $tco_result;
      };
    };
    return go(chain)(start3);
  };
  var apply10 = function(v) {
    var unpushed = {
      startVx: 0,
      startVy: 0,
      upX: 0,
      downX: 0,
      upY: 0,
      downY: 0
    };
    var through = function(index2) {
      return function(startVelocity) {
        return function(up) {
          return function(down) {
            var $101 = up > 0 && down > 0;
            if ($101) {
              var $102 = abs2(startVelocity) > 1e-3 * (up + down);
              if ($102) {
                var $103 = startVelocity > 0;
                if ($103) {
                  return down;
                }
                ;
                return -up;
              }
              ;
              var $104 = up > down;
              if ($104) {
                return down;
              }
              ;
              var $105 = up < down;
              if ($105) {
                return -up;
              }
              ;
              var $106 = odd(index2);
              if ($106) {
                return down;
              }
              ;
              return -up;
            }
            ;
            return 0;
          };
        };
      };
    };
    var starting = function(boxes) {
      return function __do2() {
        var gathered = $$new(Nil.value)();
        foreachE(boxes)(function(box) {
          return modify_(Cons.create({
            startVx: box.node.vx,
            startVy: box.node.vy,
            upX: 0,
            downX: 0,
            upY: 0,
            downY: 0
          }))(gathered);
        })();
        var newestFirst = read(gathered)();
        return reverse2(newestFirst);
      };
    };
    var settle = function(strength, box, found, aheadX, aheadY, startVx, startVy, soFar) {
      var heldY = v.heldY(box.node);
      var heldX = v.heldX(box.node);
      var area = box.halfWidth * box.halfHeight;
      var go = function($copy_others) {
        return function($copy_vx) {
          return function($copy_vy) {
            return function($copy_upX) {
              return function($copy_downX) {
                return function($copy_upY) {
                  return function($copy_downY) {
                    var $tco_var_others = $copy_others;
                    var $tco_var_vx = $copy_vx;
                    var $tco_var_vy = $copy_vy;
                    var $tco_var_upX = $copy_upX;
                    var $tco_var_downX = $copy_downX;
                    var $tco_var_upY = $copy_upY;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(others, vx, vy, upX, downX, upY, downY) {
                      if (others instanceof Nil) {
                        $tco_done = true;
                        return {
                          vx,
                          vy,
                          pushed: {
                            startVx: soFar.startVx,
                            startVy: soFar.startVy,
                            upX,
                            downX,
                            upY,
                            downY
                          }
                        };
                      }
                      ;
                      if (others instanceof Cons) {
                        var tie = (function() {
                          var $108 = box.index < others.value0.item.index;
                          if ($108) {
                            return -1;
                          }
                          ;
                          return 1;
                        })();
                        var otherArea = others.value0.item.halfWidth * others.value0.item.halfHeight;
                        var offsetY = aheadY - others.value0.y;
                        var overlapY = box.halfHeight + others.value0.item.halfHeight - abs2(offsetY);
                        var offsetX = aheadX - others.value0.x;
                        var overlapX = box.halfWidth + others.value0.item.halfWidth - abs2(offsetX);
                        var byArea = (function() {
                          var $109 = area + otherArea > 0;
                          if ($109) {
                            return otherArea / (area + otherArea);
                          }
                          ;
                          return 0.5;
                        })();
                        var $110 = overlapX <= overlapY;
                        if ($110) {
                          if (heldX) {
                            $tco_var_others = others.value1;
                            $tco_var_vx = vx;
                            $tco_var_vy = vy;
                            $tco_var_upX = upX;
                            $tco_var_downX = downX;
                            $tco_var_upY = upY;
                            $copy_downY = downY;
                            return;
                          }
                          ;
                          var push2 = direction(offsetX, tie, overlapX) * strength * (function() {
                            var $112 = v.heldX(others.value0.item.node);
                            if ($112) {
                              return 1;
                            }
                            ;
                            return byArea;
                          })();
                          var $113 = push2 > 0;
                          if ($113) {
                            $tco_var_others = others.value1;
                            $tco_var_vx = vx + push2;
                            $tco_var_vy = vy;
                            $tco_var_upX = upX + push2;
                            $tco_var_downX = downX;
                            $tco_var_upY = upY;
                            $copy_downY = downY;
                            return;
                          }
                          ;
                          var $114 = push2 < 0;
                          if ($114) {
                            $tco_var_others = others.value1;
                            $tco_var_vx = vx + push2;
                            $tco_var_vy = vy;
                            $tco_var_upX = upX;
                            $tco_var_downX = downX - push2;
                            $tco_var_upY = upY;
                            $copy_downY = downY;
                            return;
                          }
                          ;
                          $tco_var_others = others.value1;
                          $tco_var_vx = vx + push2;
                          $tco_var_vy = vy;
                          $tco_var_upX = upX;
                          $tco_var_downX = downX;
                          $tco_var_upY = upY;
                          $copy_downY = downY;
                          return;
                        }
                        ;
                        if (heldY) {
                          $tco_var_others = others.value1;
                          $tco_var_vx = vx;
                          $tco_var_vy = vy;
                          $tco_var_upX = upX;
                          $tco_var_downX = downX;
                          $tco_var_upY = upY;
                          $copy_downY = downY;
                          return;
                        }
                        ;
                        var push2 = direction(offsetY, tie, overlapY) * strength * (function() {
                          var $116 = v.heldY(others.value0.item.node);
                          if ($116) {
                            return 1;
                          }
                          ;
                          return byArea;
                        })();
                        var $117 = push2 > 0;
                        if ($117) {
                          $tco_var_others = others.value1;
                          $tco_var_vx = vx;
                          $tco_var_vy = vy + push2;
                          $tco_var_upX = upX;
                          $tco_var_downX = downX;
                          $tco_var_upY = upY + push2;
                          $copy_downY = downY;
                          return;
                        }
                        ;
                        var $118 = push2 < 0;
                        if ($118) {
                          $tco_var_others = others.value1;
                          $tco_var_vx = vx;
                          $tco_var_vy = vy + push2;
                          $tco_var_upX = upX;
                          $tco_var_downX = downX;
                          $tco_var_upY = upY;
                          $copy_downY = downY - push2;
                          return;
                        }
                        ;
                        $tco_var_others = others.value1;
                        $tco_var_vx = vx;
                        $tco_var_vy = vy + push2;
                        $tco_var_upX = upX;
                        $tco_var_downX = downX;
                        $tco_var_upY = upY;
                        $copy_downY = downY;
                        return;
                      }
                      ;
                      throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 312, column 45 - line 340, column 64): " + [others.constructor.name]);
                    }
                    ;
                    while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_others, $tco_var_vx, $tco_var_vy, $tco_var_upX, $tco_var_downX, $tco_var_upY, $copy_downY);
                    }
                    ;
                    return $tco_result;
                  };
                };
              };
            };
          };
        };
      };
      return go(inIndexOrder(found))(startVx)(startVy)(soFar.upX)(soFar.downX)(soFar.upY)(soFar.downY);
    };
    var partners = function(box) {
      return function(other) {
        return other.index !== box.index && may(v.pairs)(box)(other);
      };
    };
    if (v.squeezed instanceof StaysPut) {
      return forE(0)(v.iterations)(function(v1) {
        return function __do2() {
          var points = anticipate2(v.boxes)();
          var chain = cells(build(points));
          return foreachE(v.boxes)(function(box) {
            var aheadY = box.node.y + box.node.vy;
            var aheadX = box.node.x + box.node.vx;
            return when7(isFiniteImpl(aheadX) && isFiniteImpl(aheadY))((function() {
              var v2 = overlapping(partners(box), box.halfWidth, box.halfHeight, aheadX, aheadY, chain, Nil.value);
              if (v2 instanceof Nil) {
                return pure6(unit);
              }
              ;
              var moved = settle(v.strength, box, v2, aheadX, aheadY, box.node.vx, box.node.vy, unpushed);
              return function() {
                return writeVelocity(box.node, moved.vx, moved.vy);
              };
            })());
          })();
        };
      });
    }
    ;
    if (v.squeezed instanceof GoesThrough) {
      return function __do2() {
        var start3 = starting(v.boxes)();
        var pushed = $$new(start3)();
        forE(0)(v.iterations)(function(v1) {
          return function __do3() {
            var points = anticipate2(v.boxes)();
            var chain = cells(build(points));
            var before = read(pushed)();
            var waiting = $$new(before)();
            var done = $$new(Nil.value)();
            foreachE(v.boxes)(function(box) {
              return function __do4() {
                var queue = read(waiting)();
                if (queue instanceof Nil) {
                  return unit;
                }
                ;
                if (queue instanceof Cons) {
                  write(queue.value1)(waiting)();
                  var aheadY = box.node.y + box.node.vy;
                  var aheadX = box.node.x + box.node.vx;
                  var $124 = isFiniteImpl(aheadX) && isFiniteImpl(aheadY);
                  if ($124) {
                    var v2 = overlapping(partners(box), box.halfWidth, box.halfHeight, aheadX, aheadY, chain, Nil.value);
                    if (v2 instanceof Nil) {
                      return modify_(Cons.create(queue.value0))(done)();
                    }
                    ;
                    var moved = settle(v.strength, box, v2, aheadX, aheadY, box.node.vx, box.node.vy, queue.value0);
                    writeVelocity(box.node, moved.vx, moved.vy);
                    return modify_(Cons.create(moved.pushed))(done)();
                  }
                  ;
                  return modify_(Cons.create(queue.value0))(done)();
                }
                ;
                throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 258, column 9 - line 272, column 47): " + [queue.constructor.name]);
              };
            })();
            var newestFirst = read(done)();
            return write(reverse2(newestFirst))(pushed)();
          };
        })();
        var sums = read(pushed)();
        var remaining = $$new(sums)();
        return foreachE(v.boxes)(function(box) {
          return function __do3() {
            var queue = read(remaining)();
            if (queue instanceof Nil) {
              return unit;
            }
            ;
            if (queue instanceof Cons) {
              write(queue.value1)(remaining)();
              var throughY = through(box.index)(queue.value0.startVy)(queue.value0.upY)(queue.value0.downY);
              var throughX = through(box.index)(queue.value0.startVx)(queue.value0.upX)(queue.value0.downX);
              return when7(throughX !== 0 || throughY !== 0)(function() {
                return writeVelocity(box.node, box.node.vx + throughX, box.node.vy + throughY);
              })();
            }
            ;
            throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 280, column 7 - line 288, column 98): " + [queue.constructor.name]);
          };
        })();
      };
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Forces.RectCollide (line 227, column 29 - line 288, column 98): " + [v.squeezed.constructor.name]);
  };

  // output/Hylograph.Forces.Tick/index.js
  var pure7 = /* @__PURE__ */ pure(applicativeEffect);
  var when8 = /* @__PURE__ */ when(applicativeEffect);
  var pin = function(nodes) {
    return foreachE(nodes)(function(node) {
      var v = toMaybe(node.fy);
      var v1 = toMaybe(node.fx);
      if (v1 instanceof Nothing && v instanceof Nothing) {
        return pure7(unit);
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
      throw new Error("Failed pattern match at Hylograph.Forces.Tick (line 125, column 27 - line 135, column 46): " + [v1.constructor.name, v.constructor.name]);
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
        throw new Error("Failed pattern match at Hylograph.Forces.Tick (line 52, column 27 - line 64, column 46): " + [v1.constructor.name, v.constructor.name]);
      });
    };
  };
  var initialAngle = /* @__PURE__ */ (function() {
    return pi * (3 - sqrt(5));
  })();
  var initializeNodes = function(nodes) {
    return foreachE(mapWithIndex2(function(index2) {
      return function(node) {
        return {
          index: index2,
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
          throw new Error("Failed pattern match at Hylograph.Forces.Tick (line 98, column 9 - line 100, column 24): " + [v.constructor.name]);
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
          throw new Error("Failed pattern match at Hylograph.Forces.Tick (line 102, column 9 - line 104, column 24): " + [v.constructor.name]);
        })());
        when8(isNaNImpl(placed.node.x) || isNaNImpl(placed.node.y))((function() {
          var radius = 10 * sqrt(0.5 + toNumber(placed.index));
          var angle = toNumber(placed.index) * initialAngle;
          return function() {
            return writePosition(placed.node, radius * cos(angle), radius * sin(angle));
          };
        })())();
        return when8(isNaNImpl(placed.node.vx) || isNaNImpl(placed.node.vy))(function() {
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

  // output/Hylograph.Forces.Towards/index.js
  var apply11 = function(v) {
    return function(alpha) {
      return foreachE(v)(function(pulled) {
        return function() {
          return writeVelocity(pulled.node, pulled.node.vx + (pulled.x - pulled.node.x) * pulled.strength * alpha, pulled.node.vy + (pulled.y - pulled.node.y) * pulled.strength * alpha);
        };
      });
    };
  };

  // output/Hylograph.Forces/index.js
  var map8 = /* @__PURE__ */ map(functorEither);
  var when9 = /* @__PURE__ */ when(applicativeEffect);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var ManyBodyForce = /* @__PURE__ */ (function() {
    function ManyBodyForce2(value0) {
      this.value0 = value0;
    }
    ;
    ManyBodyForce2.create = function(value0) {
      return new ManyBodyForce2(value0);
    };
    return ManyBodyForce2;
  })();
  var LinkForce = /* @__PURE__ */ (function() {
    function LinkForce2(value0) {
      this.value0 = value0;
    }
    ;
    LinkForce2.create = function(value0) {
      return new LinkForce2(value0);
    };
    return LinkForce2;
  })();
  var CollideForce = /* @__PURE__ */ (function() {
    function CollideForce2(value0) {
      this.value0 = value0;
    }
    ;
    CollideForce2.create = function(value0) {
      return new CollideForce2(value0);
    };
    return CollideForce2;
  })();
  var CenterForce = /* @__PURE__ */ (function() {
    function CenterForce2(value0) {
      this.value0 = value0;
    }
    ;
    CenterForce2.create = function(value0) {
      return new CenterForce2(value0);
    };
    return CenterForce2;
  })();
  var PositionXForce = /* @__PURE__ */ (function() {
    function PositionXForce2(value0) {
      this.value0 = value0;
    }
    ;
    PositionXForce2.create = function(value0) {
      return new PositionXForce2(value0);
    };
    return PositionXForce2;
  })();
  var PositionYForce = /* @__PURE__ */ (function() {
    function PositionYForce2(value0) {
      this.value0 = value0;
    }
    ;
    PositionYForce2.create = function(value0) {
      return new PositionYForce2(value0);
    };
    return PositionYForce2;
  })();
  var RadialForce = /* @__PURE__ */ (function() {
    function RadialForce2(value0) {
      this.value0 = value0;
    }
    ;
    RadialForce2.create = function(value0) {
      return new RadialForce2(value0);
    };
    return RadialForce2;
  })();
  var TowardsForce = /* @__PURE__ */ (function() {
    function TowardsForce2(value0) {
      this.value0 = value0;
    }
    ;
    TowardsForce2.create = function(value0) {
      return new TowardsForce2(value0);
    };
    return TowardsForce2;
  })();
  var RectCollideForce = /* @__PURE__ */ (function() {
    function RectCollideForce2(value0) {
      this.value0 = value0;
    }
    ;
    RectCollideForce2.create = function(value0) {
      return new RectCollideForce2(value0);
    };
    return RectCollideForce2;
  })();
  var ContainForce = /* @__PURE__ */ (function() {
    function ContainForce2(value0) {
      this.value0 = value0;
    }
    ;
    ContainForce2.create = function(value0) {
      return new ContainForce2(value0);
    };
    return ContainForce2;
  })();
  var CohesionForce = /* @__PURE__ */ (function() {
    function CohesionForce2(value0) {
      this.value0 = value0;
    }
    ;
    CohesionForce2.create = function(value0) {
      return new CohesionForce2(value0);
    };
    return CohesionForce2;
  })();
  var DirectionalForce = /* @__PURE__ */ (function() {
    function DirectionalForce2(value0) {
      this.value0 = value0;
    }
    ;
    DirectionalForce2.create = function(value0) {
      return new DirectionalForce2(value0);
    };
    return DirectionalForce2;
  })();
  var radial = function(config) {
    return function(nodes) {
      return new RadialForce(initialize5(config)(nodes));
    };
  };
  var positionY = function(config) {
    return function(nodes) {
      return new PositionYForce(initialize4(config)(nodes));
    };
  };
  var positionX = function(config) {
    return function(nodes) {
      return new PositionXForce(initialize4(config)(nodes));
    };
  };
  var manyBody = function(config) {
    return function(random) {
      return function(nodes) {
        return new ManyBodyForce(initialize3(config)(random)(nodes));
      };
    };
  };
  var linkById = function(dictOrd) {
    var initializeById3 = initializeById(dictOrd);
    return function(dictShow) {
      var initializeById1 = initializeById3(dictShow);
      return function(identify) {
        return function(config) {
          return function(random) {
            return function(nodes) {
              return function(links) {
                return map8(LinkForce.create)(initializeById1(identify)(config)(random)(nodes)(links));
              };
            };
          };
        };
      };
    };
  };
  var constrain3 = function(forces) {
    return function(nodes) {
      var hard = function(force) {
        if (force instanceof ContainForce) {
          return isHard(force.value0);
        }
        ;
        if (force instanceof DirectionalForce) {
          return isHard2(force.value0);
        }
        ;
        return false;
      };
      return when9(any2(hard)(forces))(function __do2() {
        foreachE(forces)(function(force) {
          if (force instanceof ContainForce) {
            return constrain(force.value0);
          }
          ;
          if (force instanceof DirectionalForce) {
            return constrain2(force.value0);
          }
          ;
          return pure8(unit);
        })();
        return pin(nodes)();
      });
    };
  };
  var collide = function(config) {
    return function(random) {
      return function(nodes) {
        return new CollideForce(initialize2(config)(random)(nodes));
      };
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
      throw new Error("Failed pattern match at Hylograph.Forces (line 159, column 26 - line 171, column 58): " + [force.constructor.name]);
    };
  };

  // output/Hylograph.ForceEngine.Core/index.js
  var map9 = /* @__PURE__ */ map(functorEither);
  var linkById2 = /* @__PURE__ */ linkById(ordInt)(showInt);
  var map13 = /* @__PURE__ */ map(functorArray);
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
        return function(recipe) {
          return map9(function(force) {
            return {
              recipe,
              force
            };
          })(recipe(random)(nodes)(links));
        };
      };
    };
  };
  var reinitializeForce = function(random) {
    return function(nodes) {
      return function(links) {
        return function(handle) {
          return initializeForce(random)(nodes)(links)(handle.recipe);
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
  var createRadial = function(config) {
    return function(v) {
      return function(nodes) {
        return function(v1) {
          return new Right(radial({
            radius: function(v2) {
              return config.radius;
            },
            strength: function(v2) {
              return config.strength;
            },
            x: config.x,
            y: config.y
          })(nodes));
        };
      };
    };
  };
  var createManyBody = function(config) {
    return function(random) {
      return function(nodes) {
        return function(v) {
          return new Right(manyBody({
            strength: function(v1) {
              return config.strength;
            },
            theta: config.theta,
            distanceMin: config.distanceMin,
            distanceMax: config.distanceMax,
            scope: everyPair,
            group: function(v1) {
              return ungrouped;
            }
          })(random)(nodes));
        };
      };
    };
  };
  var createLink = function(config) {
    return function(random) {
      return function(nodes) {
        return function(links) {
          return linkById2(function(v) {
            return v.id;
          })({
            distance: function(v) {
              return config.distance;
            },
            strength: strengthPerLink(function(v) {
              return config.strength;
            }),
            iterations: config.iterations
          })(random)(nodes)(links);
        };
      };
    };
  };
  var createForceY = function(config) {
    return function(v) {
      return function(nodes) {
        return function(v1) {
          return new Right(positionY({
            target: function(v2) {
              return config.y;
            },
            strength: function(v2) {
              return config.strength;
            }
          })(nodes));
        };
      };
    };
  };
  var createForceX = function(config) {
    return function(v) {
      return function(nodes) {
        return function(v1) {
          return new Right(positionX({
            target: function(v2) {
              return config.x;
            },
            strength: function(v2) {
              return config.strength;
            }
          })(nodes));
        };
      };
    };
  };
  var createCollide = function(config) {
    return function(random) {
      return function(nodes) {
        return function(v) {
          return new Right(collide({
            radius: function(v1) {
              return config.radius;
            },
            strength: config.strength,
            iterations: config.iterations,
            include: function(v1) {
              return true;
            },
            alphaScaled: false
          })(random)(nodes));
        };
      };
    };
  };
  var createCenter = function(config) {
    return function(v) {
      return function(nodes) {
        return function(v1) {
          return new Right(center(config)(nodes));
        };
      };
    };
  };
  var constrain4 = function(forces) {
    return function(nodes) {
      return constrain3(map13(function(v) {
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

  // output/Hylograph.ForceEngine.Types/index.js
  var ManyBody = /* @__PURE__ */ (function() {
    function ManyBody2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ManyBody2.create = function(value0) {
      return function(value1) {
        return new ManyBody2(value0, value1);
      };
    };
    return ManyBody2;
  })();
  var Collide = /* @__PURE__ */ (function() {
    function Collide2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Collide2.create = function(value0) {
      return function(value1) {
        return new Collide2(value0, value1);
      };
    };
    return Collide2;
  })();
  var Link = /* @__PURE__ */ (function() {
    function Link2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Link2.create = function(value0) {
      return function(value1) {
        return new Link2(value0, value1);
      };
    };
    return Link2;
  })();
  var Center = /* @__PURE__ */ (function() {
    function Center2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Center2.create = function(value0) {
      return function(value1) {
        return new Center2(value0, value1);
      };
    };
    return Center2;
  })();
  var PositionX = /* @__PURE__ */ (function() {
    function PositionX2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    PositionX2.create = function(value0) {
      return function(value1) {
        return new PositionX2(value0, value1);
      };
    };
    return PositionX2;
  })();
  var PositionY = /* @__PURE__ */ (function() {
    function PositionY2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    PositionY2.create = function(value0) {
      return function(value1) {
        return new PositionY2(value0, value1);
      };
    };
    return PositionY2;
  })();
  var Radial = /* @__PURE__ */ (function() {
    function Radial2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Radial2.create = function(value0) {
      return function(value1) {
        return new Radial2(value0, value1);
      };
    };
    return Radial2;
  })();
  var defaultSimParams = {
    alpha: 1,
    alphaMin: 1e-3,
    alphaDecay: 0.01,
    alphaTarget: 0,
    velocityRetention: 0.4
  };
  var defaultManyBody = /* @__PURE__ */ (function() {
    return {
      strength: -30,
      theta: 0.9,
      distanceMin: 1,
      distanceMax: 1e10
    };
  })();
  var defaultCenter = {
    x: 0,
    y: 0,
    strength: 1
  };

  // output/Hylograph.ForceEngine.Simulation/index.js
  var traverse3 = /* @__PURE__ */ traverse(traversableMap)(applicativeEither);
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var for_3 = /* @__PURE__ */ for_(applicativeEffect);
  var for_1 = /* @__PURE__ */ for_3(foldableArray);
  var when10 = /* @__PURE__ */ when(applicativeEffect);
  var fromFoldable5 = /* @__PURE__ */ fromFoldable(foldableList);
  var insert2 = /* @__PURE__ */ insert(ordString);
  var linkIds = /* @__PURE__ */ map(functorArray)(function(link) {
    return {
      source: link.source,
      target: link.target
    };
  });
  var reinitializeForces = function(sim) {
    return function __do2() {
      var nodes = read(sim.nodes)();
      var links = read(sim.links)();
      var forces = read(sim.forces)();
      var v = traverse3(reinitializeForce(sim.random)(nodes)(linkIds(links)))(forces);
      if (v instanceof Left) {
        return $$throw("Hylograph.ForceEngine.Simulation: " + v.value0)();
      }
      ;
      if (v instanceof Right) {
        return write(v.value0)(sim.forces)();
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 297, column 3 - line 299, column 44): " + [v.constructor.name]);
    };
  };
  var setNodes = function(nodes) {
    return function(sim) {
      return function __do2() {
        initializeNodes2(nodes)();
        write(nodes)(sim.nodes)();
        return reinitializeForces(sim)();
      };
    };
  };
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
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 520, column 26 - line 524, column 13): " + [sim.callbacks.constructor.name]);
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
      var settings = $$new(config)();
      return {
        nodes: nodesRef,
        links: linksRef,
        forces: forcesRef,
        random,
        alpha: alphaRef,
        prevAlpha: prevAlphaRef,
        config,
        settings,
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
            return when10(crossedThreshold(prevAlpha)(newAlpha)(threshold))(function __do2() {
              var callback = read(sim.callbacks.value0.onAlphaThreshold)();
              return callback(newAlpha)();
            });
          });
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 533, column 47 - line 540, column 26): " + [sim.callbacks.constructor.name]);
      };
    };
  };
  var tick = function(sim) {
    return function __do2() {
      var nodes = read(sim.nodes)();
      var forces = read(sim.forces)();
      var alpha = read(sim.alpha)();
      var prevAlpha = read(sim.prevAlpha)();
      var settings = read(sim.settings)();
      var forceHandles = fromFoldable5(values(forces));
      applyForces(forceHandles)(alpha)();
      integratePositions(nodes)(settings.velocityRetention)();
      constrain4(forceHandles)(nodes)();
      var newAlpha = decayAlpha(alpha)(settings.alphaMin)(settings.alphaDecay)(settings.alphaTarget);
      write(newAlpha)(sim.alpha)();
      write(alpha)(sim.prevAlpha)();
      var legacyCallback = read(sim.tickCallback)();
      legacyCallback();
      invokeTickCallback(sim)();
      checkAlphaThresholds(prevAlpha)(newAlpha)(sim)();
      return newAlpha;
    };
  };
  var addForceRecipe = function(name2) {
    return function(recipe) {
      return function(sim) {
        return function __do2() {
          var nodes = read(sim.nodes)();
          var links = read(sim.links)();
          var v = initializeForce(sim.random)(nodes)(linkIds(links))(recipe);
          if (v instanceof Left) {
            return $$throw("Hylograph.ForceEngine.Simulation: " + v.value0)();
          }
          ;
          if (v instanceof Right) {
            return modify_(insert2(name2)(v.value0))(sim.forces)();
          }
          ;
          throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 342, column 3 - line 344, column 68): " + [v.constructor.name]);
        };
      };
    };
  };
  var addForce = function(spec) {
    return function(sim) {
      var recipe = (function() {
        if (spec instanceof ManyBody) {
          return createManyBody(spec.value1);
        }
        ;
        if (spec instanceof Collide) {
          return createCollide(spec.value1);
        }
        ;
        if (spec instanceof Link) {
          return createLink(spec.value1);
        }
        ;
        if (spec instanceof Center) {
          return createCenter(spec.value1);
        }
        ;
        if (spec instanceof PositionX) {
          return createForceX(spec.value1);
        }
        ;
        if (spec instanceof PositionY) {
          return createForceY(spec.value1);
        }
        ;
        if (spec instanceof Radial) {
          return createRadial(spec.value1);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 320, column 12 - line 327, column 48): " + [spec.constructor.name]);
      })();
      var name2 = (function() {
        if (spec instanceof ManyBody) {
          return spec.value0;
        }
        ;
        if (spec instanceof Collide) {
          return spec.value0;
        }
        ;
        if (spec instanceof Link) {
          return spec.value0;
        }
        ;
        if (spec instanceof Center) {
          return spec.value0;
        }
        ;
        if (spec instanceof PositionX) {
          return spec.value0;
        }
        ;
        if (spec instanceof PositionY) {
          return spec.value0;
        }
        ;
        if (spec instanceof Radial) {
          return spec.value0;
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 312, column 10 - line 319, column 20): " + [spec.constructor.name]);
      })();
      return addForceRecipe(name2)(recipe)(sim);
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
  var requestAnimationFrame2 = requestAnimationFrame_2;
  var performanceNow = performanceNow_;

  // output/Hylograph.Transition.Coordinator/index.js
  var when11 = /* @__PURE__ */ when(applicativeEffect);
  var for_4 = /* @__PURE__ */ for_(applicativeEffect);
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var $$for2 = /* @__PURE__ */ $$for(applicativeEffect)(traversableArray);
  var for_22 = /* @__PURE__ */ for_4(foldableArray);
  var unless2 = /* @__PURE__ */ unless(applicativeEffect);
  var StillRunning = /* @__PURE__ */ (function() {
    function StillRunning2() {
    }
    ;
    StillRunning2.value = new StillRunning2();
    return StillRunning2;
  })();
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
  var notEq3 = /* @__PURE__ */ notEq(eqConsumerId);
  var unregister = function(v) {
    return function(consumerId) {
      return modify_(function(s) {
        return {
          frameId: s.frameId,
          lastTimestamp: s.lastTimestamp,
          nextId: s.nextId,
          running: s.running,
          consumers: filter(function(c) {
            return notEq3(c.id)(consumerId);
          })(s.consumers)
        };
      })(v);
    };
  };
  var register = function(v) {
    return function(spec) {
      return function __do2() {
        var state2 = read(v)();
        var consumer = {
          id: state2.nextId,
          tick: spec.tick,
          onComplete: spec.onComplete
        };
        write({
          frameId: state2.frameId,
          lastTimestamp: state2.lastTimestamp,
          running: state2.running,
          consumers: append2(state2.consumers)([consumer]),
          nextId: state2.nextId + 1 | 0
        })(v)();
        return state2.nextId;
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
      var frameId = requestAnimationFrame2(runFrame(v))();
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
        var state2 = read(v)();
        return when11(state2.running)((function() {
          var deltaMs = timestamp - state2.lastTimestamp;
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
            var results = $$for2(state2.consumers)(function(consumer) {
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
            for_22(completed)(function(r) {
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
  var start = function(v) {
    return function __do2() {
      var state2 = read(v)();
      return unless2(state2.running)(function __do3() {
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
  var create2 = function __do() {
    var ref = $$new({
      consumers: [],
      running: false,
      lastTimestamp: 0,
      nextId: 0,
      frameId: Nothing.value
    })();
    return ref;
  };

  // output/Hylograph.Transition.Tick/index.js
  var linear = function(t) {
    return t;
  };
  var lerp = function(start3) {
    return function(end) {
      return function(t) {
        return start3 + (end - start3) * t;
      };
    };
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

  // output/Hylograph.Transition.Easing/index.js
  var Linear = /* @__PURE__ */ (function() {
    function Linear2() {
    }
    ;
    Linear2.value = new Linear2();
    return Linear2;
  })();
  var QuadIn = /* @__PURE__ */ (function() {
    function QuadIn2() {
    }
    ;
    QuadIn2.value = new QuadIn2();
    return QuadIn2;
  })();
  var QuadOut = /* @__PURE__ */ (function() {
    function QuadOut2() {
    }
    ;
    QuadOut2.value = new QuadOut2();
    return QuadOut2;
  })();
  var QuadInOut = /* @__PURE__ */ (function() {
    function QuadInOut2() {
    }
    ;
    QuadInOut2.value = new QuadInOut2();
    return QuadInOut2;
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
  var toFunction = function(v) {
    if (v instanceof Linear) {
      return linear;
    }
    ;
    if (v instanceof QuadIn) {
      return easeInQuad;
    }
    ;
    if (v instanceof QuadOut) {
      return easeOutQuad;
    }
    ;
    if (v instanceof QuadInOut) {
      return easeInOutQuad;
    }
    ;
    if (v instanceof CubicIn) {
      return easeInCubic;
    }
    ;
    if (v instanceof CubicOut) {
      return easeOutCubic;
    }
    ;
    if (v instanceof CubicInOut) {
      return easeInOutCubic;
    }
    ;
    if (v instanceof SinIn) {
      return easeInSin;
    }
    ;
    if (v instanceof SinOut) {
      return easeOutSin;
    }
    ;
    if (v instanceof SinInOut) {
      return easeInOutSin;
    }
    ;
    if (v instanceof ExpIn) {
      return easeInExp;
    }
    ;
    if (v instanceof ExpOut) {
      return easeOutExp;
    }
    ;
    if (v instanceof ExpInOut) {
      return easeInOutExp;
    }
    ;
    if (v instanceof CircleIn) {
      return easeInCircle;
    }
    ;
    if (v instanceof CircleOut) {
      return easeOutCircle;
    }
    ;
    if (v instanceof CircleInOut) {
      return easeInOutCircle;
    }
    ;
    if (v instanceof BackIn) {
      return easeInBack;
    }
    ;
    if (v instanceof BackOut) {
      return easeOutBack;
    }
    ;
    if (v instanceof BackInOut) {
      return easeInOutBack;
    }
    ;
    if (v instanceof ElasticIn) {
      return easeInElastic;
    }
    ;
    if (v instanceof ElasticOut) {
      return easeOutElastic;
    }
    ;
    if (v instanceof ElasticInOut) {
      return easeInOutElastic;
    }
    ;
    if (v instanceof BounceIn) {
      return easeInBounce;
    }
    ;
    if (v instanceof BounceOut) {
      return easeOutBounce;
    }
    ;
    if (v instanceof BounceInOut) {
      return easeInOutBounce;
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Transition.Easing (line 77, column 14 - line 102, column 35): " + [v.constructor.name]);
  };

  // output/Hylograph.Transition.Engine/index.js
  var max3 = /* @__PURE__ */ max(ordNumber);
  var min4 = /* @__PURE__ */ min(ordNumber);
  var map10 = /* @__PURE__ */ map(functorArray);
  var all2 = /* @__PURE__ */ all(foldableArray)(heytingAlgebraBoolean);
  var transitionWith = function(interpolate) {
    return function(values2) {
      return function(config) {
        return {
          from: values2.from,
          to: values2.to,
          interpolate,
          config
        };
      };
    };
  };
  var transition = function(values2) {
    return function(opts) {
      return {
        from: values2.from,
        to: values2.to,
        interpolate: lerp,
        config: {
          duration: opts.duration,
          easing: opts.easing,
          delay: 0
        }
      };
    };
  };
  var tickBy = function(delta) {
    return function(state2) {
      var newElapsed = state2.elapsed + delta;
      var effectiveElapsed = max3(0)(newElapsed - state2.spec.config.delay);
      var rawProgress = effectiveElapsed / state2.spec.config.duration;
      var easingFn = toFunction(state2.spec.config.easing);
      var clampedProgress = min4(1)(max3(0)(rawProgress));
      var easedProgress = easingFn(clampedProgress);
      var newValue = state2.spec.interpolate(state2.spec.from)(state2.spec.to)(easedProgress);
      return {
        spec: state2.spec,
        elapsed: newElapsed,
        current: newValue
      };
    };
  };
  var tick2 = function(delta) {
    return function(state2) {
      return tickBy(delta)(state2);
    };
  };
  var start2 = function(spec) {
    return {
      spec,
      elapsed: 0,
      current: spec.from
    };
  };
  var isComplete = function(state2) {
    return state2.elapsed >= state2.spec.config.duration + state2.spec.config.delay;
  };
  var groupTick = function(delta) {
    return function(v) {
      return {
        transitions: map10(tick2(delta))(v.transitions)
      };
    };
  };
  var groupComplete = function(v) {
    return all2(isComplete)(v.transitions);
  };
  var group2 = function(specs) {
    return {
      transitions: map10(start2)(specs)
    };
  };
  var currentValue = function(v) {
    return v.current;
  };
  var groupValues = function(v) {
    return map10(currentValue)(v.transitions);
  };

  // output/Hylograph.Transition.Consumers/index.js
  var transitionGroupConsumer = function(groupRef) {
    return function(onValues) {
      return function(deltaMs) {
        return function __do2() {
          var grp = read(groupRef)();
          var newGroup = groupTick(deltaMs)(grp);
          write(newGroup)(groupRef)();
          onValues(groupValues(newGroup))();
          var $5 = groupComplete(newGroup);
          if ($5) {
            return Completed.value;
          }
          ;
          return StillRunning.value;
        };
      };
    };
  };
  var transitionConsumer = function(stateRef) {
    return function(onValue) {
      return function(deltaMs) {
        return function __do2() {
          var state2 = read(stateRef)();
          var newState = tick2(deltaMs)(state2);
          write(newState)(stateRef)();
          onValue(currentValue(newState))();
          var $6 = isComplete(newState);
          if ($6) {
            return Completed.value;
          }
          ;
          return StillRunning.value;
        };
      };
    };
  };
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

  // output/Test.WASMSimulation/foreign.js
  var wasmModule = null;
  var SimulationClass = null;
  var initWasm_ = function(wasmPath) {
    return function(onError, onSuccess) {
      (async () => {
        if (wasmModule) {
          onSuccess();
          return;
        }
        try {
          const module = await import(wasmPath);
          await module.default();
          wasmModule = module;
          SimulationClass = module.Simulation;
          console.log("WASM force kernel initialized");
          onSuccess();
        } catch (e) {
          onError(e);
        }
      })();
      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };
  };
  var createSimulation = function(nodeCount) {
    return function() {
      if (!SimulationClass) {
        throw new Error("WASM not initialized. Call initWasm first.");
      }
      return new SimulationClass(nodeCount);
    };
  };
  var setPositions = function(sim) {
    return function(positions) {
      return function() {
        for (let i = 0; i < positions.length / 2; i++) {
          sim.set_position(i, positions[i * 2], positions[i * 2 + 1]);
        }
      };
    };
  };
  var configureManyBody = function(sim) {
    return function(strength) {
      return function(theta) {
        return function(distMin) {
          return function(distMax) {
            return function() {
              sim.configure_many_body(strength, theta, distMin, distMax);
            };
          };
        };
      };
    };
  };
  var configureCenter = function(sim) {
    return function(x) {
      return function(y) {
        return function(strength) {
          return function() {
            sim.configure_center(x, y, strength);
          };
        };
      };
    };
  };
  var enableForces = function(sim) {
    return function(manyBody2) {
      return function(links) {
        return function(center2) {
          return function() {
            sim.enable_force(manyBody2, links, center2);
          };
        };
      };
    };
  };
  var tick3 = function(sim) {
    return function() {
      return sim.tick();
    };
  };
  var getPositions = function(sim) {
    return function() {
      return sim.get_positions();
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

  // output/Test.WASMSimulation/index.js
  var initWasm = function(path) {
    return fromEffectFnAff(initWasm_(path));
  };

  // output/Test.CoordinatorVisualTest/index.js
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var pure10 = /* @__PURE__ */ pure(applicativeEffect);
  var show2 = /* @__PURE__ */ show(showNumber);
  var discard22 = /* @__PURE__ */ discard2(bindAff);
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var wasmConsumer = function(sim) {
    return function(alphaMin) {
      return function(onTick) {
        return function(_deltaMs) {
          return function __do2() {
            var alpha = tick3(sim)();
            onTick();
            var $19 = alpha < alphaMin;
            if ($19) {
              return Completed.value;
            }
            ;
            return new Converged(alpha);
          };
        };
      };
    };
  };
  var setupWidthTest = function(coord) {
    return function(element) {
      return function __do2() {
        log("Setting up width transition (50 \u2192 300px, 1000ms)")();
        var spec = transition({
          from: 50,
          to: 300
        })({
          duration: 1e3,
          easing: CubicInOut.value
        });
        var stateRef = $$new(start2(spec))();
        register(coord)({
          tick: transitionConsumer(stateRef)(function(value) {
            return setStyle(element)("width")(show2(value) + "px");
          }),
          onComplete: log("\u2713 Width transition complete!")
        })();
        return unit;
      };
    };
  };
  var setupStaggeredCircle = function(coord) {
    return function(element) {
      return function(delayMs) {
        var config = {
          duration: 300,
          easing: QuadOut.value,
          delay: delayMs
        };
        var spec = transitionWith(lerp)({
          from: 0,
          to: 1
        })(config);
        return function __do2() {
          var stateRef = $$new(start2(spec))();
          register(coord)({
            tick: transitionConsumer(stateRef)(function(value) {
              return function __do3() {
                setStyle(element)("opacity")(show2(value))();
                return setTransform(element)("scale(" + (show2(value) + ")"))();
              };
            }),
            onComplete: pure10(unit)
          })();
          return unit;
        };
      };
    };
  };
  var setupStaggeredTest = function(coord) {
    return function __do2() {
      log("Setting up staggered transitions (5 circles, 100ms apart)")();
      var c0 = getElementById("circle0")();
      setupStaggeredCircle(coord)(c0)(0)();
      var c1 = getElementById("circle1")();
      setupStaggeredCircle(coord)(c1)(100)();
      var c2 = getElementById("circle2")();
      setupStaggeredCircle(coord)(c2)(200)();
      var c3 = getElementById("circle3")();
      setupStaggeredCircle(coord)(c3)(300)();
      var c4 = getElementById("circle4")();
      return setupStaggeredCircle(coord)(c4)(400)();
    };
  };
  var setupOpacityTest = function(coord) {
    return function(element) {
      return function __do2() {
        log("Setting up opacity transition (0 \u2192 1, 500ms)")();
        var spec = transition({
          from: 0,
          to: 1
        })({
          duration: 500,
          easing: QuadOut.value
        });
        var stateRef = $$new(start2(spec))();
        register(coord)({
          tick: transitionConsumer(stateRef)(function(value) {
            return setStyle(element)("opacity")(show2(value));
          }),
          onComplete: log("\u2713 Opacity transition complete!")
        })();
        return unit;
      };
    };
  };
  var setupGroupedTest = function(coord) {
    return function(element) {
      return function __do2() {
        log("Setting up grouped transition (X + opacity, 800ms)")();
        var specs = [transition({
          from: 0,
          to: 200
        })({
          duration: 800,
          easing: QuadOut.value
        }), transition({
          from: 0,
          to: 1
        })({
          duration: 400,
          easing: Linear.value
        })];
        var groupRef = $$new(group2(specs))();
        register(coord)({
          tick: transitionGroupConsumer(groupRef)(function(values2) {
            if (values2.length === 2) {
              return function __do3() {
                setStyle(element)("opacity")(show2(values2[1]))();
                return setTransform(element)("translateX(" + (show2(values2[0]) + "px)"))();
              };
            }
            ;
            return pure10(unit);
          }),
          onComplete: log("\u2713 Grouped transition complete!")
        })();
        return unit;
      };
    };
  };
  var renderWASMNodes = function(elements) {
    return function(positions) {
      var go = function(i) {
        return function(es) {
          var v = uncons(es);
          if (v instanceof Nothing) {
            return pure10(unit);
          }
          ;
          if (v instanceof Just) {
            return function __do2() {
              (function() {
                var v1 = index(positions)((i * 2 | 0) + 1 | 0);
                var v2 = index(positions)(i * 2 | 0);
                if (v2 instanceof Just && v1 instanceof Just) {
                  return setPosition(v.value0.head)(v2.value0)(v1.value0)();
                }
                ;
                return unit;
              })();
              return go(i + 1 | 0)(v.value0.tail)();
            };
          }
          ;
          throw new Error("Failed pattern match at Test.CoordinatorVisualTest (line 310, column 13 - line 316, column 22): " + [v.constructor.name]);
        };
      };
      return go(0)(elements);
    };
  };
  var setupWASMSimulationTest = function(coord) {
    return function __do2() {
      log("Setting up WASM force simulation (6 nodes repelling each other)")();
      var sim = createSimulation(6)();
      var initialPositions = [350, 75, 360, 80, 340, 70, 355, 85, 345, 65, 365, 75];
      setPositions(sim)(initialPositions)();
      configureManyBody(sim)(-50)(0.9)(1)(1e3)();
      configureCenter(sim)(350)(75)(0.1)();
      enableForces(sim)(true)(false)(true)();
      var w0 = getElementById("wasm0")();
      var w1 = getElementById("wasm1")();
      var w2 = getElementById("wasm2")();
      var w3 = getElementById("wasm3")();
      var w4 = getElementById("wasm4")();
      var w5 = getElementById("wasm5")();
      var wasmElements = [w0, w1, w2, w3, w4, w5];
      register(coord)({
        tick: wasmConsumer(sim)(1e-3)(function __do3() {
          var positions = getPositions(sim)();
          return renderWASMNodes(wasmElements)(positions)();
        }),
        onComplete: log("\u2713 WASM simulation converged!")
      })();
      return unit;
    };
  };
  var renderNodes = function(elements) {
    return function(nodes) {
      var go = function(es) {
        return function(ns) {
          var v = uncons(ns);
          var v1 = uncons(es);
          if (v1 instanceof Just && v instanceof Just) {
            return function __do2() {
              setPosition(v1.value0.head)(v.value0.head.x)(v.value0.head.y)();
              return go(v1.value0.tail)(v.value0.tail)();
            };
          }
          ;
          return pure10(unit);
        };
      };
      return go(elements)(nodes);
    };
  };
  var setupSimulationTest = function(coord) {
    return function __do2() {
      log("Setting up D3 force simulation (8 nodes repelling each other)")();
      var sim = create(defaultConfig)();
      var nodes = [{
        id: 0,
        x: 200,
        y: 75,
        vx: 0,
        vy: 0,
        fx: nullImpl,
        fy: nullImpl,
        label: "A"
      }, {
        id: 1,
        x: 210,
        y: 80,
        vx: 0,
        vy: 0,
        fx: nullImpl,
        fy: nullImpl,
        label: "B"
      }, {
        id: 2,
        x: 190,
        y: 70,
        vx: 0,
        vy: 0,
        fx: nullImpl,
        fy: nullImpl,
        label: "C"
      }, {
        id: 3,
        x: 205,
        y: 85,
        vx: 0,
        vy: 0,
        fx: nullImpl,
        fy: nullImpl,
        label: "D"
      }, {
        id: 4,
        x: 195,
        y: 65,
        vx: 0,
        vy: 0,
        fx: nullImpl,
        fy: nullImpl,
        label: "E"
      }, {
        id: 5,
        x: 215,
        y: 75,
        vx: 0,
        vy: 0,
        fx: nullImpl,
        fy: nullImpl,
        label: "F"
      }, {
        id: 6,
        x: 185,
        y: 80,
        vx: 0,
        vy: 0,
        fx: nullImpl,
        fy: nullImpl,
        label: "G"
      }, {
        id: 7,
        x: 200,
        y: 90,
        vx: 0,
        vy: 0,
        fx: nullImpl,
        fy: nullImpl,
        label: "H"
      }];
      setNodes(nodes)(sim)();
      addForce(new ManyBody("charge", {
        theta: defaultManyBody.theta,
        distanceMin: defaultManyBody.distanceMin,
        distanceMax: defaultManyBody.distanceMax,
        strength: -50
      }))(sim)();
      addForce(new Center("center", {
        strength: defaultCenter.strength,
        x: 200,
        y: 75
      }))(sim)();
      var n0 = getElementById("node0")();
      var n1 = getElementById("node1")();
      var n2 = getElementById("node2")();
      var n3 = getElementById("node3")();
      var n4 = getElementById("node4")();
      var n5 = getElementById("node5")();
      var n6 = getElementById("node6")();
      var n7 = getElementById("node7")();
      var nodeElements = [n0, n1, n2, n3, n4, n5, n6, n7];
      register(coord)({
        tick: simulationConsumer(sim)(1e-3)(function __do3() {
          var currentNodes = getNodes(sim)();
          return renderNodes(nodeElements)(currentNodes)();
        }),
        onComplete: log("\u2713 Force simulation converged!")
      })();
      return unit;
    };
  };
  var main = /* @__PURE__ */ launchAff_(/* @__PURE__ */ discard22(/* @__PURE__ */ liftEffect2(/* @__PURE__ */ log("TickCoordinator Test Starting...")))(function() {
    return discard22(liftEffect2(log("Initializing WASM force kernel...")))(function() {
      return discard22(initWasm("./wasm-pkg/force_kernel.js"))(function() {
        return discard22(liftEffect2(log("WASM initialized!")))(function() {
          return liftEffect2(function __do2() {
            var coord = create2();
            var box1 = getElementById("box1")();
            setupOpacityTest(coord)(box1)();
            var box2 = getElementById("box2")();
            setupWidthTest(coord)(box2)();
            var box3 = getElementById("box3")();
            setupGroupedTest(coord)(box3)();
            setupStaggeredTest(coord)();
            setupSimulationTest(coord)();
            setupWASMSimulationTest(coord)();
            log("Starting coordinator with all consumers...")();
            start(coord)();
            return log("All transitions and simulations registered and running!")();
          });
        });
      });
    });
  }));

  // <stdin>
  main();
})();

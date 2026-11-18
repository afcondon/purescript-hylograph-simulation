(() => {
  // output/Affjax/foreign.js
  function _ajax(platformSpecificDriver, timeoutErrorMessageIdent, requestFailedMessageIdent, mkHeader, options) {
    return function(errback, callback) {
      var xhr = platformSpecificDriver.newXHR();
      var fixedUrl = platformSpecificDriver.fixupUrl(options.url, xhr);
      xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
      if (options.headers) {
        try {
          for (var i = 0, header; (header = options.headers[i]) != null; i++) {
            xhr.setRequestHeader(header.field, header.value);
          }
        } catch (e) {
          errback(e);
        }
      }
      var onerror = function(msgIdent) {
        return function() {
          errback(new Error(msgIdent));
        };
      };
      xhr.onerror = onerror(requestFailedMessageIdent);
      xhr.ontimeout = onerror(timeoutErrorMessageIdent);
      xhr.onload = function() {
        callback({
          status: xhr.status,
          statusText: xhr.statusText,
          headers: xhr.getAllResponseHeaders().split("\r\n").filter(function(header2) {
            return header2.length > 0;
          }).map(function(header2) {
            var i2 = header2.indexOf(":");
            return mkHeader(header2.substring(0, i2))(header2.substring(i2 + 2));
          }),
          body: xhr.response
        });
      };
      xhr.responseType = options.responseType;
      xhr.withCredentials = options.withCredentials;
      xhr.timeout = options.timeout;
      xhr.send(options.content);
      return function(error3, cancelErrback, cancelCallback) {
        try {
          xhr.abort();
        } catch (e) {
          return cancelErrback(e);
        }
        return cancelCallback();
      };
    };
  }

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
  var on = function(f) {
    return function(g) {
      return function(x) {
        return function(y) {
          return f(g(x))(g(y));
        };
      };
    };
  };
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
  var applyFlipped = function(x) {
    return function(f) {
      return f(x);
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map113 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map113(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var functorArray = {
    map: arrayMap
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

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
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
    var map31 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map31($$const(identity2))(a))(b);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply16 = apply(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply16(pure16(f))(a);
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
    return function(eq5) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq5 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
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
  var eqChar = {
    eq: eqCharImpl
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

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
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

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
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
  var ordChar = /* @__PURE__ */ (function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
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

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
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
        var empty5 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty5;
      }
    ) + '"';
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
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
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
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
  var applicativeMaybe = /* @__PURE__ */ (function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  })();

  // output/Data.MediaType.Common/index.js
  var applicationJSON = "application/json";
  var applicationFormURLEncoded = "application/x-www-form-urlencoded";

  // output/Affjax.RequestBody/index.js
  var ArrayView = /* @__PURE__ */ (function() {
    function ArrayView2(value0) {
      this.value0 = value0;
    }
    ;
    ArrayView2.create = function(value0) {
      return new ArrayView2(value0);
    };
    return ArrayView2;
  })();
  var Blob = /* @__PURE__ */ (function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  })();
  var Document = /* @__PURE__ */ (function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  })();
  var $$String = /* @__PURE__ */ (function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  })();
  var FormData = /* @__PURE__ */ (function() {
    function FormData2(value0) {
      this.value0 = value0;
    }
    ;
    FormData2.create = function(value0) {
      return new FormData2(value0);
    };
    return FormData2;
  })();
  var FormURLEncoded = /* @__PURE__ */ (function() {
    function FormURLEncoded2(value0) {
      this.value0 = value0;
    }
    ;
    FormURLEncoded2.create = function(value0) {
      return new FormURLEncoded2(value0);
    };
    return FormURLEncoded2;
  })();
  var Json = /* @__PURE__ */ (function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  })();
  var toMediaType = function(v) {
    if (v instanceof FormURLEncoded) {
      return new Just(applicationFormURLEncoded);
    }
    ;
    if (v instanceof Json) {
      return new Just(applicationJSON);
    }
    ;
    return Nothing.value;
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
  var unwrap = function() {
    return coerce2;
  };
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

  // output/Affjax.RequestHeader/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var Accept = /* @__PURE__ */ (function() {
    function Accept2(value0) {
      this.value0 = value0;
    }
    ;
    Accept2.create = function(value0) {
      return new Accept2(value0);
    };
    return Accept2;
  })();
  var ContentType = /* @__PURE__ */ (function() {
    function ContentType2(value0) {
      this.value0 = value0;
    }
    ;
    ContentType2.create = function(value0) {
      return new ContentType2(value0);
    };
    return ContentType2;
  })();
  var RequestHeader = /* @__PURE__ */ (function() {
    function RequestHeader2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RequestHeader2.create = function(value0) {
      return function(value1) {
        return new RequestHeader2(value0, value1);
      };
    };
    return RequestHeader2;
  })();
  var value = function(v) {
    if (v instanceof Accept) {
      return unwrap2(v.value0);
    }
    ;
    if (v instanceof ContentType) {
      return unwrap2(v.value0);
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value1;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [v.constructor.name]);
  };
  var name = function(v) {
    if (v instanceof Accept) {
      return "Accept";
    }
    ;
    if (v instanceof ContentType) {
      return "Content-Type";
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 21, column 1 - line 21, column 32): " + [v.constructor.name]);
  };

  // output/Affjax.ResponseFormat/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var $$ArrayBuffer = /* @__PURE__ */ (function() {
    function $$ArrayBuffer2(value0) {
      this.value0 = value0;
    }
    ;
    $$ArrayBuffer2.create = function(value0) {
      return new $$ArrayBuffer2(value0);
    };
    return $$ArrayBuffer2;
  })();
  var Blob2 = /* @__PURE__ */ (function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  })();
  var Document2 = /* @__PURE__ */ (function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  })();
  var Json2 = /* @__PURE__ */ (function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  })();
  var $$String2 = /* @__PURE__ */ (function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  })();
  var Ignore = /* @__PURE__ */ (function() {
    function Ignore2(value0) {
      this.value0 = value0;
    }
    ;
    Ignore2.create = function(value0) {
      return new Ignore2(value0);
    };
    return Ignore2;
  })();
  var toResponseType = function(v) {
    if (v instanceof $$ArrayBuffer) {
      return "arraybuffer";
    }
    ;
    if (v instanceof Blob2) {
      return "blob";
    }
    ;
    if (v instanceof Document2) {
      return "document";
    }
    ;
    if (v instanceof Json2) {
      return "text";
    }
    ;
    if (v instanceof $$String2) {
      return "text";
    }
    ;
    if (v instanceof Ignore) {
      return "";
    }
    ;
    throw new Error("Failed pattern match at Affjax.ResponseFormat (line 44, column 3 - line 50, column 19): " + [v.constructor.name]);
  };
  var toMediaType2 = function(v) {
    if (v instanceof Json2) {
      return new Just(applicationJSON);
    }
    ;
    return Nothing.value;
  };
  var json = /* @__PURE__ */ (function() {
    return new Json2(identity4);
  })();
  var ignore = /* @__PURE__ */ (function() {
    return new Ignore(identity4);
  })();

  // output/Affjax.ResponseHeader/index.js
  var ResponseHeader = /* @__PURE__ */ (function() {
    function ResponseHeader2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ResponseHeader2.create = function(value0) {
      return function(value1) {
        return new ResponseHeader2(value0, value1);
      };
    };
    return ResponseHeader2;
  })();

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
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped1 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a) {
          return bindFlipped1(f)(g(a));
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
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
  var note = function(a) {
    return maybe(new Left(a))(Right.create);
  };
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
    var bind5 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind5(f)(function(f$prime) {
          return bind5(a)(function(a$prime) {
            return pure16(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0) return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0) return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name3, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
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

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function message(e) {
    return e.message;
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

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map31 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    return function(a) {
      return catchError1(map31(Right.create)(a))(function($52) {
        return pure16(Left.create($52));
      });
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
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
  var $runtime_lazy2 = function(name3, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
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
  var not = function(dict) {
    return dict.not;
  };
  var ff = function(dict) {
    return dict.ff;
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
  var uncurry = function(f) {
    return function(v) {
      return f(v.value0)(v.value1);
    };
  };
  var snd = function(v) {
    return v.value1;
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    var eq5 = eq(dictEq);
    return function(dictEq1) {
      var eq13 = eq(dictEq1);
      return {
        eq: function(x) {
          return function(y) {
            return eq5(x.value0)(y.value0) && eq13(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare3 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x) {
          return function(y) {
            var v = compare3(x.value0)(y.value0);
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
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map4 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map113 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map113(map4(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind5(v)(either(function($193) {
            return pure16(Left.create($193));
          })(function(a) {
            var v1 = k(a);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: (function() {
        var $194 = pure(dictMonad.Applicative0());
        return function($195) {
          return ExceptT($194(Right.create($195)));
        };
      })(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: (function() {
        var $204 = pure(dictMonad.Applicative0());
        return function($205) {
          return ExceptT($204(Left.create($205)));
        };
      })(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var altExceptT = function(dictSemigroup) {
    var append5 = append(dictSemigroup);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind5 = bind(Bind1);
      var pure16 = pure(dictMonad.Applicative0());
      var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
      return {
        alt: function(v) {
          return function(v1) {
            return bind5(v)(function(rm) {
              if (rm instanceof Right) {
                return pure16(new Right(rm.value0));
              }
              ;
              if (rm instanceof Left) {
                return bind5(v1)(function(rn) {
                  if (rn instanceof Right) {
                    return pure16(new Right(rn.value0));
                  }
                  ;
                  if (rn instanceof Left) {
                    return pure16(new Left(append5(rm.value0)(rn.value0)));
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 87, column 9 - line 89, column 49): " + [rn.constructor.name]);
                });
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 83, column 5 - line 89, column 49): " + [rm.constructor.name]);
            });
          };
        },
        Functor0: function() {
          return functorExceptT1;
        }
      };
    };
  };

  // output/Control.Monad.Except/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap3(runExceptT($3));
  };

  // output/Data.Argonaut.Core/foreign.js
  function id(x) {
    return x;
  }
  function stringify(j) {
    return JSON.stringify(j);
  }
  function _caseJson(isNull3, isBool, isNum, isStr, isArr, isObj, j) {
    if (j == null) return isNull3();
    else if (typeof j === "boolean") return isBool(j);
    else if (typeof j === "number") return isNum(j);
    else if (typeof j === "string") return isStr(j);
    else if (Object.prototype.toString.call(j) === "[object Array]")
      return isArr(j);
    else return isObj(j);
  }

  // output/Foreign.Object/foreign.js
  function _copyST(m) {
    return function() {
      var r = {};
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  }
  var empty = {};
  function runST(f) {
    return f();
  }
  function _fmapObject(m0, f) {
    var m = {};
    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(m0[k]);
      }
    }
    return m;
  }
  function _mapWithKey(m0, f) {
    var m = {};
    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(k)(m0[k]);
      }
    }
    return m;
  }
  function _foldM(bind5) {
    return function(f) {
      return function(mz) {
        return function(m) {
          var acc = mz;
          function g(k2) {
            return function(z) {
              return f(z)(k2)(m[k2]);
            };
          }
          for (var k in m) {
            if (hasOwnProperty.call(m, k)) {
              acc = bind5(acc)(g(k));
            }
          }
          return acc;
        };
      };
    };
  }
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Data.Array/foreign.js
  var rangeImpl = function(start3, end) {
    var step = start3 > end ? -1 : 1;
    var result = new Array(step * (end - start3) + 1);
    var i = start3, n = 0;
    while (i !== end) {
      result[n++] = i;
      i += step;
    }
    result[n] = i;
    return result;
  };
  var replicateFill = function(count, value2) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value2);
  };
  var replicatePolyfill = function(count, value2) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value2;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = /* @__PURE__ */ (function() {
    function Cons2(head3, tail2) {
      this.head = head3;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head3) {
      return function(tail2) {
        return new Cons2(head3, tail2);
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
    return function(foldr4, xs) {
      return listToArray(foldr4(curryCons)(emptyList)(xs));
    };
  })();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty5, next, xs) {
    return xs.length === 0 ? empty5({}) : next(xs[0])(xs.slice(1));
  };
  var indexImpl = function(just, nothing, xs, i) {
    return i < 0 || i >= xs.length ? nothing : just(xs[i]);
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i = 0, l = xs.length; i < l; i++) {
      if (f(xs[i])) return just(i);
    }
    return nothing;
  };
  var _insertAt = function(just, nothing, i, a, l) {
    if (i < 0 || i > l.length) return nothing;
    var l1 = l.slice();
    l1.splice(i, 0, a);
    return just(l1);
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
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from2, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to - from2 >> 1);
      if (mid - from2 > 1) mergeFromTo(compare3, fromOrdering, xs2, xs1, from2, mid);
      if (to - mid > 1) mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to);
      i = from2;
      j = mid;
      k = from2;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
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
    return function(compare3, fromOrdering, xs) {
      var out;
      if (xs.length < 2) return xs;
      out = xs.slice(0);
      mergeFromTo(compare3, fromOrdering, out, xs.slice(0), 0, xs.length);
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
  var allImpl = function(p, xs) {
    var len = xs.length;
    for (var i = 0; i < len; i++) {
      if (!p(xs[i])) return false;
    }
    return true;
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
      return function __do3() {
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
        return function __do3() {
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

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty2 = function(dict) {
    return dict.empty;
  };

  // output/Data.Bifunctor/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var bimap = function(dict) {
    return dict.bimap;
  };
  var lmap = function(dictBifunctor) {
    var bimap1 = bimap(dictBifunctor);
    return function(f) {
      return bimap1(f)(identity5);
    };
  };
  var bifunctorEither = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return new Left(v(v2.value0));
          }
          ;
          if (v2 instanceof Right) {
            return new Right(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Bifunctor (line 38, column 1 - line 40, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x) {
    return x;
  };
  var semigroupDisj = function(dictHeytingAlgebra) {
    var disj2 = disj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return disj2(v)(v1);
        };
      }
    };
  };
  var monoidDisj = function(dictHeytingAlgebra) {
    var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
    return {
      mempty: ff(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupDisj1;
      }
    };
  };

  // output/Data.Foldable/index.js
  var eq12 = /* @__PURE__ */ eq(eqOrdering);
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure16(unit));
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
  var minimumBy = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(cmp) {
      var min$prime = function(v) {
        return function(v1) {
          if (v instanceof Nothing) {
            return new Just(v1);
          }
          ;
          if (v instanceof Just) {
            return new Just((function() {
              var $307 = eq12(cmp(v.value0)(v1))(LT.value);
              if ($307) {
                return v.value0;
              }
              ;
              return v1;
            })());
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 454, column 3 - line 454, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return foldl22(min$prime)(Nothing.value);
    };
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
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append5 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
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
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var any = function(dictFoldable) {
    var foldMap22 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap22(monoidDisj(dictHeytingAlgebra)));
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
  var runFn5 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function(e) {
              return fn(a, b, c, d, e);
            };
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
    return function(apply16) {
      return function(map31) {
        return function(pure16) {
          return function(f) {
            return function(array) {
              function go(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure16([]);
                  case 1:
                    return map31(array1)(f(array[bot]));
                  case 2:
                    return apply16(map31(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply16(apply16(map31(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply16(map31(concat2)(go(bot, pivot)))(go(pivot, top2));
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
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity6);
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

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value2 = b;
              while (true) {
                var maybe2 = f(value2);
                if (isNothing2(maybe2)) return result;
                var tuple = fromJust5(maybe2);
                result.push(fst2(tuple));
                value2 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value2 = b;
              while (true) {
                var tuple = f(value2);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2)) return result;
                value2 = fromJust5(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Array/index.js
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var map1 = /* @__PURE__ */ map(functorArray);
  var map22 = /* @__PURE__ */ map(functorST);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var when2 = /* @__PURE__ */ when(applicativeST);
  var notEq2 = /* @__PURE__ */ notEq(eqOrdering);
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zipWith = /* @__PURE__ */ runFn3(zipWithImpl);
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
    var comparing4 = comparing(dictOrd);
    return function(f) {
      return sortBy(comparing4(f));
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
  var range2 = /* @__PURE__ */ runFn2(rangeImpl);
  var $$null = function(xs) {
    return length(xs) === 0;
  };
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var insertAt = /* @__PURE__ */ (function() {
    return runFn5(_insertAt)(Just.create)(Nothing.value);
  })();
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
        return map1(snd)(sortWith1(fst)((function __do3() {
          var result = unsafeThaw(singleton2(v.value0))();
          foreach(indexedAndSorted)(function(v1) {
            return function __do4() {
              var lst = map22(/* @__PURE__ */ (function() {
                var $183 = function($185) {
                  return fromJust4(last($185));
                };
                return function($184) {
                  return snd($183($184));
                };
              })())(unsafeFreeze(result))();
              return when2(notEq2(comp(lst)(v1.value1))(EQ.value))($$void3(push(v1)(result)))();
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
  var foldM = function(dictMonad) {
    var pure16 = pure(dictMonad.Applicative0());
    var bind14 = bind(dictMonad.Bind1());
    return function(f) {
      return function(b) {
        return runFn3(unconsImpl)(function(v) {
          return pure16(b);
        })(function(a) {
          return function(as) {
            return bind14(f(b)(a))(function(b$prime) {
              return foldM(dictMonad)(f)(b$prime)(as);
            });
          };
        });
      };
    };
  };
  var findIndex = /* @__PURE__ */ (function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  })();
  var filter = /* @__PURE__ */ runFn2(filterImpl);
  var elemIndex = function(dictEq) {
    var eq22 = eq(dictEq);
    return function(x) {
      return findIndex(function(v) {
        return eq22(v)(x);
      });
    };
  };
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
  var cons = function(x) {
    return function(xs) {
      return append2([x])(xs);
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
  var all2 = /* @__PURE__ */ runFn2(allImpl);

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

  // output/Foreign.Object.ST/foreign.js
  function poke2(k) {
    return function(v) {
      return function(m) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }

  // output/Foreign.Object/index.js
  var foldr2 = /* @__PURE__ */ foldr(foldableArray);
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var values = /* @__PURE__ */ toArrayWithKey(function(v) {
    return function(v1) {
      return v1;
    };
  });
  var thawST = _copyST;
  var mutate = function(f) {
    return function(m) {
      return runST(function __do3() {
        var s = thawST(m)();
        f(s)();
        return s;
      });
    };
  };
  var mapWithKey = function(f) {
    return function(m) {
      return _mapWithKey(m, f);
    };
  };
  var lookup = /* @__PURE__ */ (function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  })();
  var insert = function(k) {
    return function(v) {
      return mutate(poke2(k)(v));
    };
  };
  var functorObject = {
    map: function(f) {
      return function(m) {
        return _fmapObject(m, f);
      };
    }
  };
  var functorWithIndexObject = {
    mapWithIndex: mapWithKey,
    Functor0: function() {
      return functorObject;
    }
  };
  var fold2 = /* @__PURE__ */ _foldM(applyFlipped);
  var foldMap2 = function(dictMonoid) {
    var append13 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return fold2(function(acc) {
        return function(k) {
          return function(v) {
            return append13(acc)(f(k)(v));
          };
        };
      })(mempty2);
    };
  };
  var foldableObject = {
    foldl: function(f) {
      return fold2(function(z) {
        return function(v) {
          return f(z);
        };
      });
    },
    foldr: function(f) {
      return function(z) {
        return function(m) {
          return foldr2(f)(z)(values(m));
        };
      };
    },
    foldMap: function(dictMonoid) {
      var foldMap12 = foldMap2(dictMonoid);
      return function(f) {
        return foldMap12($$const(f));
      };
    }
  };
  var foldableWithIndexObject = {
    foldlWithIndex: function(f) {
      return fold2(flip(f));
    },
    foldrWithIndex: function(f) {
      return function(z) {
        return function(m) {
          return foldr2(uncurry(f))(z)(toArrayWithKey(Tuple.create)(m));
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      return foldMap2(dictMonoid);
    },
    Foldable0: function() {
      return foldableObject;
    }
  };
  var traversableWithIndexObject = {
    traverseWithIndex: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      var apply16 = apply(Apply0);
      var map31 = map(Apply0.Functor0());
      var pure16 = pure(dictApplicative);
      return function(f) {
        return function(ms) {
          return fold2(function(acc) {
            return function(k) {
              return function(v) {
                return apply16(map31(flip(insert(k)))(acc))(f(k)(v));
              };
            };
          })(pure16(empty))(ms);
        };
      };
    },
    FunctorWithIndex0: function() {
      return functorWithIndexObject;
    },
    FoldableWithIndex1: function() {
      return foldableWithIndexObject;
    },
    Traversable2: function() {
      return traversableObject;
    }
  };
  var traversableObject = {
    traverse: function(dictApplicative) {
      var $96 = traverseWithIndex(traversableWithIndexObject)(dictApplicative);
      return function($97) {
        return $96($$const($97));
      };
    },
    sequence: function(dictApplicative) {
      return traverse(traversableObject)(dictApplicative)(identity7);
    },
    Functor0: function() {
      return functorObject;
    },
    Foldable1: function() {
      return foldableObject;
    }
  };

  // output/Data.Argonaut.Core/index.js
  var verbJsonType = function(def) {
    return function(f) {
      return function(g) {
        return g(def)(f);
      };
    };
  };
  var toJsonType = /* @__PURE__ */ (function() {
    return verbJsonType(Nothing.value)(Just.create);
  })();
  var jsonEmptyObject = /* @__PURE__ */ id(empty);
  var caseJsonString = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), f, $$const(d), $$const(d), j);
      };
    };
  };
  var caseJsonObject = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), $$const(d), f, j);
      };
    };
  };
  var toObject = /* @__PURE__ */ toJsonType(caseJsonObject);
  var caseJsonNumber = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), f, $$const(d), $$const(d), $$const(d), j);
      };
    };
  };
  var caseJsonArray = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), f, $$const(d), j);
      };
    };
  };
  var toArray = /* @__PURE__ */ toJsonType(caseJsonArray);

  // output/Data.Argonaut.Parser/foreign.js
  function _jsonParser(fail3, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail3(e.message);
    }
  }

  // output/Data.Argonaut.Parser/index.js
  var jsonParser = function(j) {
    return _jsonParser(Left.create, Right.create, j);
  };

  // output/Data.String.Common/foreign.js
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/JSURI/foreign.js
  function encodeURIComponent_to_RFC3986(input) {
    return input.replace(/[!'()*]/g, function(c) {
      return "%" + c.charCodeAt(0).toString(16);
    });
  }
  function _encodeFormURLComponent(fail3, succeed, input) {
    try {
      return succeed(encodeURIComponent_to_RFC3986(encodeURIComponent(input)).replace(/%20/g, "+"));
    } catch (err) {
      return fail3(err);
    }
  }

  // output/JSURI/index.js
  var encodeFormURLComponent = /* @__PURE__ */ (function() {
    return runFn3(_encodeFormURLComponent)($$const(Nothing.value))(Just.create);
  })();

  // output/Data.FormURLEncoded/index.js
  var apply2 = /* @__PURE__ */ apply(applyMaybe);
  var map5 = /* @__PURE__ */ map(functorMaybe);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeMaybe);
  var toArray2 = function(v) {
    return v;
  };
  var encode = /* @__PURE__ */ (function() {
    var encodePart = function(v) {
      if (v.value1 instanceof Nothing) {
        return encodeFormURLComponent(v.value0);
      }
      ;
      if (v.value1 instanceof Just) {
        return apply2(map5(function(key) {
          return function(val) {
            return key + ("=" + val);
          };
        })(encodeFormURLComponent(v.value0)))(encodeFormURLComponent(v.value1.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.FormURLEncoded (line 37, column 16 - line 39, column 114): " + [v.constructor.name]);
    };
    var $37 = map5(joinWith("&"));
    var $38 = traverse2(encodePart);
    return function($39) {
      return $37($38(toArray2($39)));
    };
  })();

  // output/Data.HTTP.Method/index.js
  var OPTIONS = /* @__PURE__ */ (function() {
    function OPTIONS2() {
    }
    ;
    OPTIONS2.value = new OPTIONS2();
    return OPTIONS2;
  })();
  var GET = /* @__PURE__ */ (function() {
    function GET2() {
    }
    ;
    GET2.value = new GET2();
    return GET2;
  })();
  var HEAD = /* @__PURE__ */ (function() {
    function HEAD2() {
    }
    ;
    HEAD2.value = new HEAD2();
    return HEAD2;
  })();
  var POST = /* @__PURE__ */ (function() {
    function POST2() {
    }
    ;
    POST2.value = new POST2();
    return POST2;
  })();
  var PUT = /* @__PURE__ */ (function() {
    function PUT2() {
    }
    ;
    PUT2.value = new PUT2();
    return PUT2;
  })();
  var DELETE = /* @__PURE__ */ (function() {
    function DELETE2() {
    }
    ;
    DELETE2.value = new DELETE2();
    return DELETE2;
  })();
  var TRACE = /* @__PURE__ */ (function() {
    function TRACE2() {
    }
    ;
    TRACE2.value = new TRACE2();
    return TRACE2;
  })();
  var CONNECT = /* @__PURE__ */ (function() {
    function CONNECT2() {
    }
    ;
    CONNECT2.value = new CONNECT2();
    return CONNECT2;
  })();
  var PROPFIND = /* @__PURE__ */ (function() {
    function PROPFIND2() {
    }
    ;
    PROPFIND2.value = new PROPFIND2();
    return PROPFIND2;
  })();
  var PROPPATCH = /* @__PURE__ */ (function() {
    function PROPPATCH2() {
    }
    ;
    PROPPATCH2.value = new PROPPATCH2();
    return PROPPATCH2;
  })();
  var MKCOL = /* @__PURE__ */ (function() {
    function MKCOL2() {
    }
    ;
    MKCOL2.value = new MKCOL2();
    return MKCOL2;
  })();
  var COPY = /* @__PURE__ */ (function() {
    function COPY2() {
    }
    ;
    COPY2.value = new COPY2();
    return COPY2;
  })();
  var MOVE = /* @__PURE__ */ (function() {
    function MOVE2() {
    }
    ;
    MOVE2.value = new MOVE2();
    return MOVE2;
  })();
  var LOCK = /* @__PURE__ */ (function() {
    function LOCK2() {
    }
    ;
    LOCK2.value = new LOCK2();
    return LOCK2;
  })();
  var UNLOCK = /* @__PURE__ */ (function() {
    function UNLOCK2() {
    }
    ;
    UNLOCK2.value = new UNLOCK2();
    return UNLOCK2;
  })();
  var PATCH = /* @__PURE__ */ (function() {
    function PATCH2() {
    }
    ;
    PATCH2.value = new PATCH2();
    return PATCH2;
  })();
  var QUERY = /* @__PURE__ */ (function() {
    function QUERY2() {
    }
    ;
    QUERY2.value = new QUERY2();
    return QUERY2;
  })();
  var unCustomMethod = function(v) {
    return v;
  };
  var showMethod = {
    show: function(v) {
      if (v instanceof OPTIONS) {
        return "OPTIONS";
      }
      ;
      if (v instanceof GET) {
        return "GET";
      }
      ;
      if (v instanceof HEAD) {
        return "HEAD";
      }
      ;
      if (v instanceof POST) {
        return "POST";
      }
      ;
      if (v instanceof PUT) {
        return "PUT";
      }
      ;
      if (v instanceof DELETE) {
        return "DELETE";
      }
      ;
      if (v instanceof TRACE) {
        return "TRACE";
      }
      ;
      if (v instanceof CONNECT) {
        return "CONNECT";
      }
      ;
      if (v instanceof PROPFIND) {
        return "PROPFIND";
      }
      ;
      if (v instanceof PROPPATCH) {
        return "PROPPATCH";
      }
      ;
      if (v instanceof MKCOL) {
        return "MKCOL";
      }
      ;
      if (v instanceof COPY) {
        return "COPY";
      }
      ;
      if (v instanceof MOVE) {
        return "MOVE";
      }
      ;
      if (v instanceof LOCK) {
        return "LOCK";
      }
      ;
      if (v instanceof UNLOCK) {
        return "UNLOCK";
      }
      ;
      if (v instanceof PATCH) {
        return "PATCH";
      }
      ;
      if (v instanceof QUERY) {
        return "QUERY";
      }
      ;
      throw new Error("Failed pattern match at Data.HTTP.Method (line 47, column 1 - line 64, column 23): " + [v.constructor.name]);
    }
  };
  var print = /* @__PURE__ */ either(/* @__PURE__ */ show(showMethod))(unCustomMethod);

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ (function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  })();
  var singleton4 = function(dictPlus) {
    var empty5 = empty2(dictPlus);
    return function(a) {
      return new NonEmpty(a, empty5);
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
  var NonEmptyList = function(x) {
    return x;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
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
  var foldr3 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr3(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var semigroupNonEmptyList = {
    append: function(v) {
      return function(as$prime) {
        return new NonEmpty(v.value0, append1(v.value1)(toList(as$prime)));
      };
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ (function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  })();

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
  var fromFoldable2 = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
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

  // output/Data.List.NonEmpty/index.js
  var singleton6 = /* @__PURE__ */ (function() {
    var $200 = singleton4(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  })();
  var head2 = function(v) {
    return v.value0;
  };

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
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
      var size3 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size3 !== 0) {
          size3--;
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
          if (size3 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size3) % limit] = cb;
          size3++;
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
      var fail3 = null;
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
                fail3 = util.left(e);
                step = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step)) {
                status = RETURN;
                fail3 = step;
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
                  fail3 = util.left(step._1);
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
                step = interrupt || fail3 || step;
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
                    } else if (fail3) {
                      status = CONTINUE;
                      step = attempt._2(util.fromLeft(fail3));
                      fail3 = null;
                    }
                    break;
                  // We cannot resume from an unmasked interrupt or exception.
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail3) {
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
                    if (fail3 === null) {
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
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail3), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail3) {
                      step = attempt._1.failed(util.fromLeft(fail3))(attempt._2);
                    } else {
                      step = attempt._1.completed(util.fromRight(step))(attempt._2);
                    }
                    fail3 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail3), attempts, interrupt);
                    status = CONTINUE;
                    step = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step = attempt._1;
                    fail3 = attempt._2;
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
              if (interrupt && fail3) {
                setTimeout(function() {
                  throw util.fromLeft(fail3);
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
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step)();
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
                fail3 = null;
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
                fail3 = null;
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
        var step = par2;
        var head3 = null;
        var tail2 = null;
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
              if (head3 === null) {
                break loop;
              }
              step = head3._2;
              if (tail2 === null) {
                head3 = null;
              } else {
                head3 = tail2._1;
                tail2 = tail2._2;
              }
              break;
            case MAP:
              step = step._2;
              break;
            case APPLY:
            case ALT:
              if (head3) {
                tail2 = new Aff2(CONS, head3, tail2);
              }
              head3 = step;
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
      function join3(result, head3, tail2) {
        var fail3, step, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail3 = result;
          step = null;
        } else {
          step = result;
          fail3 = null;
        }
        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head3 === null) {
            cb(fail3 || step)();
            return;
          }
          if (head3._3 !== EMPTY) {
            return;
          }
          switch (head3.tag) {
            case MAP:
              if (fail3 === null) {
                head3._3 = util.right(head3._1(util.fromRight(step)));
                step = head3._3;
              } else {
                head3._3 = fail3;
              }
              break;
            case APPLY:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (fail3) {
                head3._3 = fail3;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail3 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(fail3, null, null);
                    } else {
                      join3(fail3, tail2._1, tail2._2);
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
                head3._3 = step;
              }
              break;
            case ALT:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail3 = step === lhs ? rhs : lhs;
                step = null;
                head3._3 = fail3;
              } else {
                head3._3 = step;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, step === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(step, null, null);
                    } else {
                      join3(step, tail2._1, tail2._2);
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
          if (tail2 === null) {
            head3 = null;
          } else {
            head3 = tail2._1;
            tail2 = tail2._2;
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
        var step = par;
        var head3 = null;
        var tail2 = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step.tag) {
                case MAP:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(MAP, step._1, EMPTY, EMPTY);
                  step = step._2;
                  break;
                case APPLY:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(APPLY, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;
                case ALT:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(ALT, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step;
                  step = new Aff2(FORKED, fid, new Aff2(CONS, head3, tail2), EMPTY);
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
              if (head3 === null) {
                break loop;
              }
              if (head3._1 === EMPTY) {
                head3._1 = step;
                status = CONTINUE;
                step = head3._2;
                head3._2 = EMPTY;
              } else {
                head3._2 = step;
                step = head3;
                if (tail2 === null) {
                  head3 = null;
                } else {
                  head3 = tail2._1;
                  tail2 = tail2._2;
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
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value2) {
          return Aff.Pure(f(value2));
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

  // output/Effect.Aff/index.js
  var $runtime_lazy3 = function(name3, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
      return val;
    };
  };
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
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
    return function __do3() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var launchAff_ = function($75) {
    return $$void4(launchAff($75));
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
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure2(unit));

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
      return function __do3() {
        var v1 = v(function($9) {
          return k(Left.create($9))();
        }, function($10) {
          return k(Right.create($10))();
        });
        return function(e) {
          return makeAff(function(k2) {
            return function __do4() {
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

  // output/Foreign/foreign.js
  function tagOf(value2) {
    return Object.prototype.toString.call(value2).slice(8, -1);
  }

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var nan = NaN;
  var isNaNImpl = isNaN;
  var infinity = Infinity;
  var isFiniteImpl = isFinite;
  var abs = Math.abs;
  var cos = Math.cos;
  var floor = Math.floor;
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
  var fromNumber = /* @__PURE__ */ (function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  })();

  // output/Data.String.CodeUnits/foreign.js
  var length3 = function(s) {
    return s.length;
  };
  var drop3 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length) return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Foreign/index.js
  var show2 = /* @__PURE__ */ show(showString);
  var show1 = /* @__PURE__ */ show(showInt);
  var ForeignError = /* @__PURE__ */ (function() {
    function ForeignError2(value0) {
      this.value0 = value0;
    }
    ;
    ForeignError2.create = function(value0) {
      return new ForeignError2(value0);
    };
    return ForeignError2;
  })();
  var TypeMismatch = /* @__PURE__ */ (function() {
    function TypeMismatch3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return function(value1) {
        return new TypeMismatch3(value0, value1);
      };
    };
    return TypeMismatch3;
  })();
  var ErrorAtIndex = /* @__PURE__ */ (function() {
    function ErrorAtIndex2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtIndex2.create = function(value0) {
      return function(value1) {
        return new ErrorAtIndex2(value0, value1);
      };
    };
    return ErrorAtIndex2;
  })();
  var ErrorAtProperty = /* @__PURE__ */ (function() {
    function ErrorAtProperty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtProperty2.create = function(value0) {
      return function(value1) {
        return new ErrorAtProperty2(value0, value1);
      };
    };
    return ErrorAtProperty2;
  })();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var renderForeignError = function(v) {
    if (v instanceof ForeignError) {
      return v.value0;
    }
    ;
    if (v instanceof ErrorAtIndex) {
      return "Error at array index " + (show1(v.value0) + (": " + renderForeignError(v.value1)));
    }
    ;
    if (v instanceof ErrorAtProperty) {
      return "Error at property " + (show2(v.value0) + (": " + renderForeignError(v.value1)));
    }
    ;
    if (v instanceof TypeMismatch) {
      return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    }
    ;
    throw new Error("Failed pattern match at Foreign (line 78, column 1 - line 78, column 45): " + [v.constructor.name]);
  };
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton6($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure16 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value2) {
        if (tagOf(value2) === tag) {
          return pure16(unsafeFromForeign(value2));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value2)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value2.constructor.name]);
      };
    };
  };

  // output/Affjax/index.js
  var pure3 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var fail2 = /* @__PURE__ */ fail(monadIdentity);
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var alt2 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var map6 = /* @__PURE__ */ map(functorMaybe);
  var any3 = /* @__PURE__ */ any(foldableArray)(heytingAlgebraBoolean);
  var eq4 = /* @__PURE__ */ eq(eqString);
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var map12 = /* @__PURE__ */ map(functorArray);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorAff);
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var pure1 = /* @__PURE__ */ pure(applicativeAff);
  var RequestContentError = /* @__PURE__ */ (function() {
    function RequestContentError2(value0) {
      this.value0 = value0;
    }
    ;
    RequestContentError2.create = function(value0) {
      return new RequestContentError2(value0);
    };
    return RequestContentError2;
  })();
  var ResponseBodyError = /* @__PURE__ */ (function() {
    function ResponseBodyError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ResponseBodyError2.create = function(value0) {
      return function(value1) {
        return new ResponseBodyError2(value0, value1);
      };
    };
    return ResponseBodyError2;
  })();
  var TimeoutError = /* @__PURE__ */ (function() {
    function TimeoutError2() {
    }
    ;
    TimeoutError2.value = new TimeoutError2();
    return TimeoutError2;
  })();
  var RequestFailedError = /* @__PURE__ */ (function() {
    function RequestFailedError2() {
    }
    ;
    RequestFailedError2.value = new RequestFailedError2();
    return RequestFailedError2;
  })();
  var XHROtherError = /* @__PURE__ */ (function() {
    function XHROtherError2(value0) {
      this.value0 = value0;
    }
    ;
    XHROtherError2.create = function(value0) {
      return new XHROtherError2(value0);
    };
    return XHROtherError2;
  })();
  var request = function(driver2) {
    return function(req) {
      var parseJSON = function(v2) {
        if (v2 === "") {
          return pure3(jsonEmptyObject);
        }
        ;
        return either(function($74) {
          return fail2(ForeignError.create($74));
        })(pure3)(jsonParser(v2));
      };
      var fromResponse = (function() {
        if (req.responseFormat instanceof $$ArrayBuffer) {
          return unsafeReadTagged2("ArrayBuffer");
        }
        ;
        if (req.responseFormat instanceof Blob2) {
          return unsafeReadTagged2("Blob");
        }
        ;
        if (req.responseFormat instanceof Document2) {
          return function(x) {
            return alt2(unsafeReadTagged2("Document")(x))(alt2(unsafeReadTagged2("XMLDocument")(x))(unsafeReadTagged2("HTMLDocument")(x)));
          };
        }
        ;
        if (req.responseFormat instanceof Json2) {
          return composeKleisliFlipped2(function($75) {
            return req.responseFormat.value0(parseJSON($75));
          })(unsafeReadTagged2("String"));
        }
        ;
        if (req.responseFormat instanceof $$String2) {
          return unsafeReadTagged2("String");
        }
        ;
        if (req.responseFormat instanceof Ignore) {
          return $$const(req.responseFormat.value0(pure3(unit)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 274, column 18 - line 283, column 57): " + [req.responseFormat.constructor.name]);
      })();
      var extractContent = function(v2) {
        if (v2 instanceof ArrayView) {
          return new Right(v2.value0(unsafeToForeign));
        }
        ;
        if (v2 instanceof Blob) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof Document) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof $$String) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof FormData) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof FormURLEncoded) {
          return note("Body contains values that cannot be encoded as application/x-www-form-urlencoded")(map6(unsafeToForeign)(encode(v2.value0)));
        }
        ;
        if (v2 instanceof Json) {
          return new Right(unsafeToForeign(stringify(v2.value0)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 235, column 20 - line 250, column 69): " + [v2.constructor.name]);
      };
      var addHeader = function(mh) {
        return function(hs) {
          if (mh instanceof Just && !any3(on(eq4)(name)(mh.value0))(hs)) {
            return snoc(hs)(mh.value0);
          }
          ;
          return hs;
        };
      };
      var headers = function(reqContent) {
        return addHeader(map6(ContentType.create)(bindFlipped2(toMediaType)(reqContent)))(addHeader(map6(Accept.create)(toMediaType2(req.responseFormat)))(req.headers));
      };
      var ajaxRequest = function(v2) {
        return {
          method: print(req.method),
          url: req.url,
          headers: map12(function(h) {
            return {
              field: name(h),
              value: value(h)
            };
          })(headers(req.content)),
          content: v2,
          responseType: toResponseType(req.responseFormat),
          username: toNullable(req.username),
          password: toNullable(req.password),
          withCredentials: req.withCredentials,
          timeout: fromMaybe(0)(map6(function(v1) {
            return v1;
          })(req.timeout))
        };
      };
      var send = function(content) {
        return mapFlipped2($$try2(fromEffectFnAff(_ajax(driver2, "AffjaxTimeoutErrorMessageIdent", "AffjaxRequestFailedMessageIdent", ResponseHeader.create, ajaxRequest(content)))))(function(v2) {
          if (v2 instanceof Right) {
            var v1 = runExcept(fromResponse(v2.value0.body));
            if (v1 instanceof Left) {
              return new Left(new ResponseBodyError(head2(v1.value0), v2.value0));
            }
            ;
            if (v1 instanceof Right) {
              return new Right({
                headers: v2.value0.headers,
                status: v2.value0.status,
                statusText: v2.value0.statusText,
                body: v1.value0
              });
            }
            ;
            throw new Error("Failed pattern match at Affjax (line 209, column 9 - line 211, column 52): " + [v1.constructor.name]);
          }
          ;
          if (v2 instanceof Left) {
            return new Left((function() {
              var message2 = message(v2.value0);
              var $61 = message2 === "AffjaxTimeoutErrorMessageIdent";
              if ($61) {
                return TimeoutError.value;
              }
              ;
              var $62 = message2 === "AffjaxRequestFailedMessageIdent";
              if ($62) {
                return RequestFailedError.value;
              }
              ;
              return new XHROtherError(v2.value0);
            })());
          }
          ;
          throw new Error("Failed pattern match at Affjax (line 207, column 144 - line 219, column 28): " + [v2.constructor.name]);
        });
      };
      if (req.content instanceof Nothing) {
        return send(toNullable(Nothing.value));
      }
      ;
      if (req.content instanceof Just) {
        var v = extractContent(req.content.value0);
        if (v instanceof Right) {
          return send(toNullable(new Just(v.value0)));
        }
        ;
        if (v instanceof Left) {
          return pure1(new Left(new RequestContentError(v.value0)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 199, column 7 - line 203, column 48): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Affjax (line 195, column 3 - line 203, column 48): " + [req.content.constructor.name]);
    };
  };
  var printError = function(v) {
    if (v instanceof RequestContentError) {
      return "There was a problem with the request content: " + v.value0;
    }
    ;
    if (v instanceof ResponseBodyError) {
      return "There was a problem with the response body: " + renderForeignError(v.value0);
    }
    ;
    if (v instanceof TimeoutError) {
      return "There was a problem making the request: timeout";
    }
    ;
    if (v instanceof RequestFailedError) {
      return "There was a problem making the request: request failed";
    }
    ;
    if (v instanceof XHROtherError) {
      return "There was a problem making the request: " + message(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Affjax (line 113, column 14 - line 123, column 66): " + [v.constructor.name]);
  };
  var defaultRequest = /* @__PURE__ */ (function() {
    return {
      method: new Left(GET.value),
      url: "/",
      headers: [],
      content: Nothing.value,
      username: Nothing.value,
      password: Nothing.value,
      withCredentials: false,
      responseFormat: ignore,
      timeout: Nothing.value
    };
  })();
  var get = function(driver2) {
    return function(rf) {
      return function(u) {
        return request(driver2)({
          method: defaultRequest.method,
          headers: defaultRequest.headers,
          content: defaultRequest.content,
          username: defaultRequest.username,
          password: defaultRequest.password,
          withCredentials: defaultRequest.withCredentials,
          timeout: defaultRequest.timeout,
          url: u,
          responseFormat: rf
        });
      };
    };
  };

  // output/Affjax.Web/foreign.js
  var driver = {
    newXHR: function() {
      return new XMLHttpRequest();
    },
    fixupUrl: function(url) {
      return url || "/";
    }
  };

  // output/Affjax.Web/index.js
  var get2 = /* @__PURE__ */ get(driver);

  // output/Data.Argonaut.Decode.Error/index.js
  var show12 = /* @__PURE__ */ show(showInt);
  var TypeMismatch2 = /* @__PURE__ */ (function() {
    function TypeMismatch3(value0) {
      this.value0 = value0;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return new TypeMismatch3(value0);
    };
    return TypeMismatch3;
  })();
  var UnexpectedValue = /* @__PURE__ */ (function() {
    function UnexpectedValue2(value0) {
      this.value0 = value0;
    }
    ;
    UnexpectedValue2.create = function(value0) {
      return new UnexpectedValue2(value0);
    };
    return UnexpectedValue2;
  })();
  var AtIndex = /* @__PURE__ */ (function() {
    function AtIndex2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AtIndex2.create = function(value0) {
      return function(value1) {
        return new AtIndex2(value0, value1);
      };
    };
    return AtIndex2;
  })();
  var AtKey = /* @__PURE__ */ (function() {
    function AtKey2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AtKey2.create = function(value0) {
      return function(value1) {
        return new AtKey2(value0, value1);
      };
    };
    return AtKey2;
  })();
  var Named = /* @__PURE__ */ (function() {
    function Named2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Named2.create = function(value0) {
      return function(value1) {
        return new Named2(value0, value1);
      };
    };
    return Named2;
  })();
  var MissingValue = /* @__PURE__ */ (function() {
    function MissingValue2() {
    }
    ;
    MissingValue2.value = new MissingValue2();
    return MissingValue2;
  })();
  var printJsonDecodeError = function(err) {
    var go = function(v) {
      if (v instanceof TypeMismatch2) {
        return "  Expected value of type '" + (v.value0 + "'.");
      }
      ;
      if (v instanceof UnexpectedValue) {
        return "  Unexpected value " + (stringify(v.value0) + ".");
      }
      ;
      if (v instanceof AtIndex) {
        return "  At array index " + (show12(v.value0) + (":\n" + go(v.value1)));
      }
      ;
      if (v instanceof AtKey) {
        return "  At object key '" + (v.value0 + ("':\n" + go(v.value1)));
      }
      ;
      if (v instanceof Named) {
        return "  Under '" + (v.value0 + ("':\n" + go(v.value1)));
      }
      ;
      if (v instanceof MissingValue) {
        return "  No value was found.";
      }
      ;
      throw new Error("Failed pattern match at Data.Argonaut.Decode.Error (line 37, column 8 - line 43, column 44): " + [v.constructor.name]);
    };
    return "An error occurred while decoding a JSON value:\n" + go(err);
  };

  // output/Data.Map.Internal/index.js
  var $runtime_lazy4 = function(name3, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
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
  var singleton8 = function(k) {
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
          return singleton8(k)(v);
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
  var member = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(k) {
      var go = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return false;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare3(k)(v.value2);
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
      return go;
    };
  };
  var lookup2 = function(dictOrd) {
    var compare3 = compare(dictOrd);
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
            var v1 = compare3(k)(v.value2);
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
  var intersectionWith = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(app) {
      return function(m1) {
        return function(m2) {
          return unsafeIntersectionWith(compare3, app, m1, m2);
        };
      };
    };
  };
  var intersection = function(dictOrd) {
    return intersectionWith(dictOrd)($$const);
  };
  var insertWith = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(app) {
      return function(k) {
        return function(v) {
          var go = function(v1) {
            if (v1 instanceof Leaf) {
              return singleton8(k)(v);
            }
            ;
            if (v1 instanceof Node) {
              var v2 = compare3(k)(v1.value2);
              if (v2 instanceof LT) {
                return unsafeBalancedNode(v1.value2, v1.value3, go(v1.value4), v1.value5);
              }
              ;
              if (v2 instanceof GT) {
                return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go(v1.value5));
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
          return go;
        };
      };
    };
  };
  var insert3 = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton8(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare3(k)(v1.value2);
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
      var append13 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append13(go(v.value4))(append13(f(v.value3))(go(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 181, column 10 - line 184, column 28): " + [v.constructor.name]);
        };
        return go;
      };
    }
  };
  var values2 = /* @__PURE__ */ (function() {
    return foldr(foldableMap)(Cons.create)(Nil.value);
  })();
  var empty3 = /* @__PURE__ */ (function() {
    return Leaf.value;
  })();
  var fromFoldable3 = function(dictOrd) {
    var insert1 = insert3(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert1(v.value0)(v.value1)(m);
        };
      })(empty3);
    };
  };
  var fromFoldableWith = function(dictOrd) {
    var insertWith1 = insertWith(dictOrd);
    return function(dictFoldable) {
      var foldl6 = foldl(dictFoldable);
      return function(f) {
        var f$prime = insertWith1(flip(f));
        return foldl6(function(m) {
          return function(v) {
            return f$prime(v.value0)(v.value1)(m);
          };
        })(empty3);
      };
    };
  };
  var difference2 = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(m1) {
      return function(m2) {
        return unsafeDifference(compare3, m1, m2);
      };
    };
  };
  var alter = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare3, k, m);
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

  // output/Data.Set/index.js
  var coerce3 = /* @__PURE__ */ coerce();
  var member2 = function(dictOrd) {
    return coerce3(member(dictOrd));
  };
  var intersection2 = function(dictOrd) {
    return coerce3(intersection(dictOrd));
  };
  var insert4 = function(dictOrd) {
    var insert1 = insert3(dictOrd);
    return function(a) {
      return function(v) {
        return insert1(a)(unit)(v);
      };
    };
  };
  var empty4 = empty3;
  var fromFoldable4 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      var insert1 = insert4(dictOrd);
      return foldl22(function(m) {
        return function(a) {
          return insert1(a)(m);
        };
      })(empty4);
    };
  };
  var difference3 = function(dictOrd) {
    return coerce3(difference2(dictOrd));
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ (function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  })();

  // output/Data.String.CodePoints/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map7 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons3 = function(s) {
    var v = length3(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop3(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop3(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map7(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons3(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length3(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length4 = function($74) {
    return length(toCodePointArray($74));
  };

  // output/Data.Argonaut.Decode.Decoders/index.js
  var lmap2 = /* @__PURE__ */ lmap(bifunctorEither);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(bindEither);
  var traverse5 = /* @__PURE__ */ traverse(traversableObject)(applicativeEither);
  var traverseWithIndex2 = /* @__PURE__ */ traverseWithIndex(traversableWithIndexArray)(applicativeEither);
  var getField = function(decoder) {
    return function(obj) {
      return function(str) {
        return maybe(new Left(new AtKey(str, MissingValue.value)))((function() {
          var $48 = lmap2(AtKey.create(str));
          return function($49) {
            return $48(decoder($49));
          };
        })())(lookup(str)(obj));
      };
    };
  };
  var decodeString = /* @__PURE__ */ (function() {
    return caseJsonString(new Left(new TypeMismatch2("String")))(Right.create);
  })();
  var decodeNumber = /* @__PURE__ */ (function() {
    return caseJsonNumber(new Left(new TypeMismatch2("Number")))(Right.create);
  })();
  var decodeJObject = /* @__PURE__ */ (function() {
    var $50 = note(new TypeMismatch2("Object"));
    return function($51) {
      return $50(toObject($51));
    };
  })();
  var decodeJArray = /* @__PURE__ */ (function() {
    var $52 = note(new TypeMismatch2("Array"));
    return function($53) {
      return $52(toArray($53));
    };
  })();
  var decodeInt = /* @__PURE__ */ composeKleisliFlipped3(/* @__PURE__ */ (function() {
    var $84 = note(new TypeMismatch2("Integer"));
    return function($85) {
      return $84(fromNumber($85));
    };
  })())(decodeNumber);
  var decodeForeignObject = function(decoder) {
    return composeKleisliFlipped3((function() {
      var $86 = lmap2(Named.create("ForeignObject"));
      var $87 = traverse5(decoder);
      return function($88) {
        return $86($87($88));
      };
    })())(decodeJObject);
  };
  var decodeArray = function(decoder) {
    return composeKleisliFlipped3((function() {
      var $89 = lmap2(Named.create("Array"));
      var $90 = traverseWithIndex2(function(i) {
        var $92 = lmap2(AtIndex.create(i));
        return function($93) {
          return $92(decoder($93));
        };
      });
      return function($91) {
        return $89($90($91));
      };
    })())(decodeJArray);
  };

  // output/Data.Argonaut.Decode.Class/index.js
  var decodeJsonString = {
    decodeJson: decodeString
  };
  var decodeJsonJson = /* @__PURE__ */ (function() {
    return {
      decodeJson: Right.create
    };
  })();
  var decodeJsonInt = {
    decodeJson: decodeInt
  };
  var decodeJson = function(dict) {
    return dict.decodeJson;
  };
  var decodeForeignObject2 = function(dictDecodeJson) {
    return {
      decodeJson: decodeForeignObject(decodeJson(dictDecodeJson))
    };
  };
  var decodeArray2 = function(dictDecodeJson) {
    return {
      decodeJson: decodeArray(decodeJson(dictDecodeJson))
    };
  };

  // output/Data.Argonaut.Decode.Combinators/index.js
  var getField2 = function(dictDecodeJson) {
    return getField(decodeJson(dictDecodeJson));
  };

  // output/DataViz.Layout.Hierarchy.Pack/index.js
  var elemIndex2 = /* @__PURE__ */ elemIndex(eqInt);
  var append12 = /* @__PURE__ */ append(semigroupArray);
  var map8 = /* @__PURE__ */ map(functorArray);
  var max3 = /* @__PURE__ */ max(ordNumber);
  var insert6 = /* @__PURE__ */ insert3(ordInt);
  var fromFoldable5 = /* @__PURE__ */ fromFoldable3(ordInt)(foldableArray);
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var map13 = /* @__PURE__ */ map(functorMaybe);
  var apply3 = /* @__PURE__ */ apply(applyMaybe);
  var bind2 = /* @__PURE__ */ bind(bindMaybe);
  var lookup3 = /* @__PURE__ */ lookup2(ordInt);
  var pure4 = /* @__PURE__ */ pure(applicativeMaybe);
  var minimumBy2 = /* @__PURE__ */ minimumBy(foldableArray);
  var comparing2 = /* @__PURE__ */ comparing(ordNumber);
  var NoCollision = /* @__PURE__ */ (function() {
    function NoCollision2() {
    }
    ;
    NoCollision2.value = new NoCollision2();
    return NoCollision2;
  })();
  var CollisionFromJ = /* @__PURE__ */ (function() {
    function CollisionFromJ2(value0) {
      this.value0 = value0;
    }
    ;
    CollisionFromJ2.create = function(value0) {
      return new CollisionFromJ2(value0);
    };
    return CollisionFromJ2;
  })();
  var CollisionFromK = /* @__PURE__ */ (function() {
    function CollisionFromK2(value0) {
      this.value0 = value0;
    }
    ;
    CollisionFromK2.create = function(value0) {
      return new CollisionFromK2(value0);
    };
    return CollisionFromK2;
  })();
  var shortenChainOnly = function(aId) {
    return function(jId) {
      return function(chain) {
        var v = elemIndex2(jId)(chain);
        var v1 = elemIndex2(aId)(chain);
        if (v1 instanceof Just && v instanceof Just) {
          var $135 = v.value0 > v1.value0;
          if ($135) {
            var before = take(v1.value0 + 1 | 0)(chain);
            var after = drop(v.value0)(chain);
            return append12(before)(after);
          }
          ;
          var $136 = v.value0 < v1.value0;
          if ($136) {
            return slice(v.value0)(v1.value0 + 1 | 0)(chain);
          }
          ;
          return chain;
        }
        ;
        return chain;
      };
    };
  };
  var place = function(b) {
    return function(a) {
      return function(c) {
        var dy = b.y - a.y;
        var dx = b.x - a.x;
        var d2 = dx * dx + dy * dy;
        var $147 = d2 !== 0;
        if ($147) {
          var b2 = (b.r + c.r) * (b.r + c.r);
          var a2 = (a.r + c.r) * (a.r + c.r);
          var $148 = a2 > b2;
          if ($148) {
            var x = (d2 + b2 - a2) / (2 * d2);
            var y = sqrt(max3(0)(b2 / d2 - x * x));
            return {
              x: b.x - x * dx - y * dy,
              y: b.y - x * dy + y * dx,
              r: c.r
            };
          }
          ;
          var x = (d2 + a2 - b2) / (2 * d2);
          var y = sqrt(max3(0)(a2 / d2 - x * x));
          return {
            x: a.x + x * dx - y * dy,
            y: a.y + x * dy + y * dx,
            r: c.r
          };
        }
        ;
        return {
          x: a.x + c.r,
          y: a.y,
          r: c.r
        };
      };
    };
  };
  var insertInChain = function(aId) {
    return function(_bId) {
      return function(newId) {
        return function(chain) {
          var v = elemIndex2(aId)(chain);
          if (v instanceof Nothing) {
            return chain;
          }
          ;
          if (v instanceof Just) {
            return fromMaybe(chain)(insertAt(v.value0 + 1 | 0)(newId)(chain));
          }
          ;
          throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 188, column 3 - line 192, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var insertBetween = function(aId) {
    return function(bId) {
      return function(newCircle) {
        return function(state2) {
          var newCircles = insert6(state2.nextId)(newCircle)(state2.circles);
          var newChain = insertInChain(aId)(bId)(state2.nextId)(state2.frontChain);
          return {
            circles: newCircles,
            frontChain: newChain,
            nextId: state2.nextId + 1 | 0
          };
        };
      };
    };
  };
  var initState = function(c0) {
    return function(c1) {
      return function(c2) {
        var c1$prime = {
          r: c1.r,
          x: c0.r,
          y: 0
        };
        var c0$prime = {
          r: c0.r,
          x: -c1.r,
          y: 0
        };
        var c2$prime = place(c1$prime)(c0$prime)(c2);
        var circles = fromFoldable5([new Tuple(0, c0$prime), new Tuple(1, c1$prime), new Tuple(2, c2$prime)]);
        return {
          circles,
          frontChain: [0, 2, 1],
          nextId: 3
        };
      };
    };
  };
  var getPrevInChain = function(nodeId) {
    return function(chain) {
      var v = elemIndex2(nodeId)(chain);
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      if (v instanceof Just) {
        var n = length(chain);
        var prevIdx = mod2((v.value0 - 1 | 0) + n | 0)(n);
        return index(chain)(prevIdx);
      }
      ;
      throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 255, column 3 - line 262, column 34): " + [v.constructor.name]);
    };
  };
  var getNextInChain = function(nodeId) {
    return function(chain) {
      var v = elemIndex2(nodeId)(chain);
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      if (v instanceof Just) {
        var nextIdx = mod2(v.value0 + 1 | 0)(length(chain));
        return index(chain)(nextIdx);
      }
      ;
      throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 244, column 3 - line 250, column 34): " + [v.constructor.name]);
    };
  };
  var getNextId = function(nodeId) {
    return function(chain) {
      var v = elemIndex2(nodeId)(chain);
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      if (v instanceof Just) {
        var nextIdx = mod2(v.value0 + 1 | 0)(length(chain));
        return index(chain)(nextIdx);
      }
      ;
      throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 152, column 3 - line 158, column 34): " + [v.constructor.name]);
    };
  };
  var epsilon6 = 1e-6;
  var intersects = function(a) {
    return function(b) {
      var dy = b.y - a.y;
      var dx = b.x - a.x;
      var dr = a.r + b.r - epsilon6;
      return dr > 0 && dr * dr > dx * dx + dy * dy;
    };
  };
  var epsilon = 1e-9;
  var enclosesWeak = function(a) {
    return function(b) {
      var dy = b.y - a.y;
      var dx = b.x - a.x;
      var dr = a.r - b.r + max3(a.r)(max3(b.r)(1)) * epsilon;
      return dr > 0 && dr * dr > dx * dx + dy * dy;
    };
  };
  var enclosesWeakAll = function(a) {
    return function(bs) {
      return all2(enclosesWeak(a))(bs);
    };
  };
  var enclosesNot = function(a) {
    return function(b) {
      var dy = b.y - a.y;
      var dx = b.x - a.x;
      var dr = a.r - b.r;
      return dr < 0 || dr * dr < dx * dx + dy * dy;
    };
  };
  var encloseBasis3 = function(a) {
    return function(b) {
      return function(c) {
        var d1 = a.x * a.x + a.y * a.y - a.r * a.r;
        var d2 = d1 - b.x * b.x - b.y * b.y + b.r * b.r;
        var d3 = d1 - c.x * c.x - c.y * c.y + c.r * c.r;
        var c3 = c.r - a.r;
        var c2 = b.r - a.r;
        var b3 = a.y - c.y;
        var b2 = a.y - b.y;
        var a3 = a.x - c.x;
        var a2 = a.x - b.x;
        var ab = a3 * b2 - a2 * b3;
        var xa = (b2 * d3 - b3 * d2) / (ab * 2) - a.x;
        var xb = (b3 * c2 - b2 * c3) / ab;
        var ya = (a3 * d2 - a2 * d3) / (ab * 2) - a.y;
        var bigC = xa * xa + ya * ya - a.r * a.r;
        var yb = (a2 * c3 - a3 * c2) / ab;
        var bigA = xb * xb + yb * yb - 1;
        var bigB = 2 * (a.r + xa * xb + ya * yb);
        var r = (function() {
          var $182 = abs(bigA) > epsilon6;
          if ($182) {
            return -(bigB + sqrt(bigB * bigB - 4 * bigA * bigC)) / (2 * bigA);
          }
          ;
          return -bigC / bigB;
        })();
        return {
          x: a.x + xa + xb * r,
          y: a.y + ya + yb * r,
          r
        };
      };
    };
  };
  var encloseBasis2 = function(a) {
    return function(b) {
      var y21 = b.y - a.y;
      var x21 = b.x - a.x;
      var r21 = b.r - a.r;
      var l = sqrt(x21 * x21 + y21 * y21);
      return {
        x: (a.x + b.x + x21 / l * r21) / 2,
        y: (a.y + b.y + y21 / l * r21) / 2,
        r: (l + a.r + b.r) / 2
      };
    };
  };
  var extendBasis = function(basis) {
    return function(p) {
      if (enclosesWeakAll(p)(basis)) {
        return [p];
      }
      ;
      if (otherwise) {
        var n = length(basis);
        var findTwoCircleBasis = function($copy_i) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(i) {
            if (i >= n) {
              $tco_done = true;
              return Nothing.value;
            }
            ;
            if (otherwise) {
              var v2 = index(basis)(i);
              if (v2 instanceof Just) {
                var candidate = encloseBasis2(v2.value0)(p);
                var $187 = enclosesNot(p)(v2.value0) && enclosesWeakAll(candidate)(basis);
                if ($187) {
                  $tco_done = true;
                  return new Just([v2.value0, p]);
                }
                ;
                $copy_i = i + 1 | 0;
                return;
              }
              ;
              if (v2 instanceof Nothing) {
                $copy_i = i + 1 | 0;
                return;
              }
              ;
              throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 560, column 13 - line 567, column 52): " + [v2.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 556, column 7 - line 556, column 56): " + [i.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_i);
          }
          ;
          return $tco_result;
        };
        var findInnerPair = function($copy_i) {
          return function($copy_j) {
            var $tco_var_i = $copy_i;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(i, j) {
              if (j >= n) {
                $tco_done1 = true;
                return Nothing.value;
              }
              ;
              if (otherwise) {
                var v2 = index(basis)(j);
                var v12 = index(basis)(i);
                if (v12 instanceof Just && v2 instanceof Just) {
                  var cjp = encloseBasis2(v2.value0)(p);
                  var cip = encloseBasis2(v12.value0)(p);
                  var cij = encloseBasis2(v12.value0)(v2.value0);
                  var candidate = encloseBasis3(v12.value0)(v2.value0)(p);
                  var $193 = enclosesNot(cij)(p) && (enclosesNot(cip)(v2.value0) && (enclosesNot(cjp)(v12.value0) && enclosesWeakAll(candidate)(basis)));
                  if ($193) {
                    $tco_done1 = true;
                    return new Just([v12.value0, v2.value0, p]);
                  }
                  ;
                  $tco_var_i = i;
                  $copy_j = j + 1 | 0;
                  return;
                }
                ;
                $tco_var_i = i;
                $copy_j = j + 1 | 0;
                return;
              }
              ;
              throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 577, column 7 - line 577, column 58): " + [i.constructor.name, j.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_i, $copy_j);
            }
            ;
            return $tco_result;
          };
        };
        var findThreeCircleBasis = function($copy_i) {
          var $tco_done2 = false;
          var $tco_result;
          function $tco_loop(i) {
            if (i >= (n - 1 | 0)) {
              $tco_done2 = true;
              return Nothing.value;
            }
            ;
            if (otherwise) {
              var v2 = findInnerPair(i)(i + 1 | 0);
              if (v2 instanceof Just) {
                $tco_done2 = true;
                return new Just(v2.value0);
              }
              ;
              if (v2 instanceof Nothing) {
                $copy_i = i + 1 | 0;
                return;
              }
              ;
              throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 573, column 13 - line 575, column 54): " + [v2.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 569, column 7 - line 569, column 58): " + [i.constructor.name]);
          }
          ;
          while (!$tco_done2) {
            $tco_result = $tco_loop($copy_i);
          }
          ;
          return $tco_result;
        };
        var v = findTwoCircleBasis(0);
        if (v instanceof Just) {
          return v.value0;
        }
        ;
        if (v instanceof Nothing) {
          var v1 = findThreeCircleBasis(0);
          if (v1 instanceof Just) {
            return v1.value0;
          }
          ;
          if (v1 instanceof Nothing) {
            return basis;
          }
          ;
          throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 550, column 11 - line 552, column 29): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 547, column 7 - line 552, column 29): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 543, column 1 - line 543, column 54): " + [basis.constructor.name, p.constructor.name]);
    };
  };
  var encloseBasis1 = function(a) {
    return {
      x: a.x,
      y: a.y,
      r: a.r
    };
  };
  var encloseBasis = function(basis) {
    var v = length(basis);
    if (v === 1) {
      return fromMaybe({
        x: 0,
        y: 0,
        r: 0
      })(map13(encloseBasis1)(index(basis)(0)));
    }
    ;
    if (v === 2) {
      return fromMaybe({
        x: 0,
        y: 0,
        r: 0
      })(apply3(map13(encloseBasis2)(index(basis)(0)))(index(basis)(1)));
    }
    ;
    if (v === 3) {
      return fromMaybe({
        x: 0,
        y: 0,
        r: 0
      })(apply3(apply3(map13(encloseBasis3)(index(basis)(0)))(index(basis)(1)))(index(basis)(2)));
    }
    ;
    return {
      x: 0,
      y: 0,
      r: 0
    };
  };
  var packEnclose = function(circles) {
    var n = length(circles);
    var go = function($copy_i) {
      return function($copy_basis) {
        return function($copy_maybeE) {
          var $tco_var_i = $copy_i;
          var $tco_var_basis = $copy_basis;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(i, basis, maybeE) {
            if (i >= n) {
              if (maybeE instanceof Just) {
                $tco_done = true;
                return maybeE.value0;
              }
              ;
              if (maybeE instanceof Nothing) {
                $tco_done = true;
                return {
                  x: 0,
                  y: 0,
                  r: 0
                };
              }
              ;
              throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 601, column 9 - line 603, column 48): " + [maybeE.constructor.name]);
            }
            ;
            if (otherwise) {
              var v = index(circles)(i);
              if (v instanceof Nothing) {
                $tco_var_i = i + 1 | 0;
                $tco_var_basis = basis;
                $copy_maybeE = maybeE;
                return;
              }
              ;
              if (v instanceof Just) {
                if (maybeE instanceof Just && enclosesWeak(maybeE.value0)(v.value0)) {
                  $tco_var_i = i + 1 | 0;
                  $tco_var_basis = basis;
                  $copy_maybeE = maybeE;
                  return;
                }
                ;
                var newBasis = extendBasis(basis)(v.value0);
                var newE = encloseBasis(newBasis);
                $tco_var_i = 0;
                $tco_var_basis = newBasis;
                $copy_maybeE = new Just(newE);
                return;
              }
              ;
              throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 605, column 9 - line 616, column 44): " + [v.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 598, column 3 - line 598, column 54): " + [i.constructor.name, basis.constructor.name, maybeE.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_i, $tco_var_basis, $copy_maybeE);
          }
          ;
          return $tco_result;
        };
      };
    };
    return go(0)([])(Nothing.value);
  };
  var calculateScore = function(a) {
    return function(b) {
      var ab = a.r + b.r;
      var dx = (a.x * b.r + b.x * a.r) / ab;
      var dy = (a.y * b.r + b.y * a.r) / ab;
      return dx * dx + dy * dy;
    };
  };
  var scoreNode = function(nodeId) {
    return function(state2) {
      return bind2(lookup3(nodeId)(state2.circles))(function(node) {
        return bind2(getNextId(nodeId)(state2.frontChain))(function(nextId) {
          return bind2(lookup3(nextId)(state2.circles))(function(next) {
            return pure4({
              nodeId,
              score: calculateScore(node)(next)
            });
          });
        });
      });
    };
  };
  var findBestPair = function(state2) {
    var scored = mapMaybe(function(nodeId) {
      return scoreNode(nodeId)(state2);
    })(state2.frontChain);
    var bestNode = minimumBy2(comparing2(function(v) {
      return v.score;
    }))(scored);
    if (bestNode instanceof Nothing) {
      return Nothing.value;
    }
    ;
    if (bestNode instanceof Just) {
      return bind2(getNextId(bestNode.value0.nodeId)(state2.frontChain))(function(nextId) {
        return pure4(new Tuple(bestNode.value0.nodeId, nextId));
      });
    }
    ;
    throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 179, column 5 - line 183, column 40): " + [bestNode.constructor.name]);
  };
  var bidirectionalCollisionSearch = function(aId) {
    return function(bId) {
      return function(newCircle) {
        return function(state2) {
          var doWhileStep = function($copy_j) {
            return function($copy_k) {
              return function($copy_sj) {
                return function($copy_sk) {
                  var $tco_var_j = $copy_j;
                  var $tco_var_k = $copy_k;
                  var $tco_var_sj = $copy_sj;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(j, k, sj, sk) {
                    var $215 = sj <= sk;
                    if ($215) {
                      var v4 = lookup3(j)(state2.circles);
                      if (v4 instanceof Just) {
                        var $217 = intersects(v4.value0)(newCircle);
                        if ($217) {
                          $tco_done = true;
                          return new CollisionFromJ(j);
                        }
                        ;
                        var v12 = getNextInChain(j)(state2.frontChain);
                        if (v12 instanceof Just) {
                          var v22 = getNextInChain(k)(state2.frontChain);
                          if (v22 instanceof Just && v12.value0 === v22.value0) {
                            $tco_done = true;
                            return NoCollision.value;
                          }
                          ;
                          $tco_var_j = v12.value0;
                          $tco_var_k = k;
                          $tco_var_sj = sj + v4.value0.r;
                          $copy_sk = sk;
                          return;
                        }
                        ;
                        if (v12 instanceof Nothing) {
                          $tco_done = true;
                          return NoCollision.value;
                        }
                        ;
                        throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 294, column 13 - line 300, column 37): " + [v12.constructor.name]);
                      }
                      ;
                      if (v4 instanceof Nothing) {
                        $tco_done = true;
                        return NoCollision.value;
                      }
                      ;
                      throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 288, column 7 - line 301, column 31): " + [v4.constructor.name]);
                    }
                    ;
                    var v4 = lookup3(k)(state2.circles);
                    if (v4 instanceof Just) {
                      var $224 = intersects(v4.value0)(newCircle);
                      if ($224) {
                        $tco_done = true;
                        return new CollisionFromK(k);
                      }
                      ;
                      var v12 = getPrevInChain(k)(state2.frontChain);
                      if (v12 instanceof Just) {
                        var v22 = getNextInChain(v12.value0)(state2.frontChain);
                        if (v22 instanceof Just && j === v22.value0) {
                          $tco_done = true;
                          return NoCollision.value;
                        }
                        ;
                        $tco_var_j = j;
                        $tco_var_k = v12.value0;
                        $tco_var_sj = sj;
                        $copy_sk = sk + v4.value0.r;
                        return;
                      }
                      ;
                      if (v12 instanceof Nothing) {
                        $tco_done = true;
                        return NoCollision.value;
                      }
                      ;
                      throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 310, column 13 - line 316, column 37): " + [v12.constructor.name]);
                    }
                    ;
                    if (v4 instanceof Nothing) {
                      $tco_done = true;
                      return NoCollision.value;
                    }
                    ;
                    throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 304, column 7 - line 317, column 31): " + [v4.constructor.name]);
                  }
                  ;
                  while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_j, $tco_var_k, $tco_var_sj, $copy_sk);
                  }
                  ;
                  return $tco_result;
                };
              };
            };
          };
          var v = lookup3(bId)(state2.circles);
          var v1 = lookup3(aId)(state2.circles);
          if (v1 instanceof Just && v instanceof Just) {
            var v2 = getPrevInChain(aId)(state2.frontChain);
            var v3 = getNextInChain(bId)(state2.frontChain);
            if (v3 instanceof Just && v2 instanceof Just) {
              return doWhileStep(v3.value0)(v2.value0)(v.value0.r)(v1.value0.r);
            }
            ;
            return NoCollision.value;
          }
          ;
          return NoCollision.value;
        };
      };
    };
  };
  var tryPlaceCircleWithLimit = function($copy_maxRetries) {
    return function($copy_newCircle) {
      return function($copy_aId) {
        return function($copy_bId) {
          return function($copy_state) {
            var $tco_var_maxRetries = $copy_maxRetries;
            var $tco_var_newCircle = $copy_newCircle;
            var $tco_var_aId = $copy_aId;
            var $tco_var_bId = $copy_bId;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(maxRetries, newCircle, aId, bId, state2) {
              if (maxRetries <= 0) {
                $tco_done = true;
                return insertBetween(aId)(bId)(newCircle)(state2);
              }
              ;
              if (otherwise) {
                var v = lookup3(bId)(state2.circles);
                var v1 = lookup3(aId)(state2.circles);
                if (v1 instanceof Just && v instanceof Just) {
                  var positioned = place(v.value0)(v1.value0)(newCircle);
                  var collisionResult = bidirectionalCollisionSearch(aId)(bId)(positioned)(state2);
                  if (collisionResult instanceof NoCollision) {
                    $tco_done = true;
                    return insertBetween(aId)(bId)(positioned)(state2);
                  }
                  ;
                  if (collisionResult instanceof CollisionFromJ) {
                    var shortenedChain = shortenChainOnly(aId)(collisionResult.value0)(state2.frontChain);
                    var stateWithShortenedChain = {
                      circles: state2.circles,
                      nextId: state2.nextId,
                      frontChain: shortenedChain
                    };
                    $tco_var_maxRetries = maxRetries - 1 | 0;
                    $tco_var_newCircle = newCircle;
                    $tco_var_aId = aId;
                    $tco_var_bId = collisionResult.value0;
                    $copy_state = stateWithShortenedChain;
                    return;
                  }
                  ;
                  if (collisionResult instanceof CollisionFromK) {
                    var shortenedChain = shortenChainOnly(collisionResult.value0)(bId)(state2.frontChain);
                    var stateWithShortenedChain = {
                      circles: state2.circles,
                      nextId: state2.nextId,
                      frontChain: shortenedChain
                    };
                    $tco_var_maxRetries = maxRetries - 1 | 0;
                    $tco_var_newCircle = newCircle;
                    $tco_var_aId = collisionResult.value0;
                    $tco_var_bId = bId;
                    $copy_state = stateWithShortenedChain;
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 380, column 13 - line 396, column 101): " + [collisionResult.constructor.name]);
                }
                ;
                $tco_done = true;
                return state2;
              }
              ;
              throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 368, column 1 - line 368, column 91): " + [maxRetries.constructor.name, newCircle.constructor.name, aId.constructor.name, bId.constructor.name, state2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_maxRetries, $tco_var_newCircle, $tco_var_aId, $tco_var_bId, $copy_state);
            }
            ;
            return $tco_result;
          };
        };
      };
    };
  };
  var tryPlaceCircle = /* @__PURE__ */ tryPlaceCircleWithLimit(100);
  var addCircle = function(newCircle) {
    return function(state2) {
      var v = findBestPair(state2);
      if (v instanceof Nothing) {
        return state2;
      }
      ;
      if (v instanceof Just) {
        return tryPlaceCircle(newCircle)(v.value0.value0)(v.value0.value1)(state2);
      }
      ;
      throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 404, column 3 - line 406, column 67): " + [v.constructor.name]);
    };
  };
  var packSiblingsMap = function(inputCircles) {
    var n = length(inputCircles);
    var $260 = n === 0;
    if ($260) {
      return {
        circles: [],
        radius: 0
      };
    }
    ;
    var $261 = n === 1;
    if ($261) {
      var v = index(inputCircles)(0);
      if (v instanceof Just) {
        return {
          circles: [{
            r: v.value0.r,
            x: 0,
            y: 0
          }],
          radius: v.value0.r
        };
      }
      ;
      if (v instanceof Nothing) {
        return {
          circles: [],
          radius: 0
        };
      }
      ;
      throw new Error("Failed pattern match at DataViz.Layout.Hierarchy.Pack (line 416, column 7 - line 418, column 48): " + [v.constructor.name]);
    }
    ;
    var $264 = n === 2;
    if ($264) {
      var v = index(inputCircles)(1);
      var v1 = index(inputCircles)(0);
      if (v1 instanceof Just && v instanceof Just) {
        var c1$prime = {
          r: v.value0.r,
          x: v1.value0.r,
          y: 0
        };
        var c0$prime = {
          r: v1.value0.r,
          x: -v.value0.r,
          y: 0
        };
        return {
          circles: [c0$prime, c1$prime],
          radius: v1.value0.r + v.value0.r
        };
      }
      ;
      return {
        circles: [],
        radius: 0
      };
    }
    ;
    var v = index(inputCircles)(2);
    var v1 = index(inputCircles)(1);
    var v2 = index(inputCircles)(0);
    if (v2 instanceof Just && (v1 instanceof Just && v instanceof Just)) {
      var remaining = drop(3)(inputCircles);
      var initialState = initState(v2.value0)(v1.value0)(v.value0);
      var finalState = foldl2(flip(addCircle))(initialState)(remaining);
      var positioned = mapMaybe(function(i) {
        return lookup3(i)(finalState.circles);
      })(range2(0)(n - 1 | 0));
      var enclosing = packEnclose(positioned);
      var translated = map8(function(c) {
        return {
          r: c.r,
          x: c.x - enclosing.x,
          y: c.y - enclosing.y
        };
      })(positioned);
      return {
        circles: translated,
        radius: enclosing.r
      };
    }
    ;
    return {
      circles: [],
      radius: 0
    };
  };

  // output/Effect.Console/foreign.js
  var log2 = function(s) {
    return function() {
      console.log(s);
    };
  };
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Effect.Class.Console/index.js
  var log3 = function(dictMonadEffect) {
    var $67 = liftEffect(dictMonadEffect);
    return function($68) {
      return $67(log2($68));
    };
  };

  // output/Hylograph.ForceEngine.Core/foreign.js
  var USE_SETTIMEOUT = true;
  var FRAME_TIME = 8;
  function requestAnimationFrame_(callback) {
    return function() {
      let id2;
      const handler = function(timestamp) {
        callback(timestamp || performance.now())();
      };
      if (USE_SETTIMEOUT) {
        id2 = setTimeout(handler, FRAME_TIME);
        return function() {
          clearTimeout(id2);
        };
      } else {
        id2 = requestAnimationFrame(handler);
        return function() {
          cancelAnimationFrame(id2);
        };
      }
    };
  }
  function newCancelRef() {
    return { cancel: function() {
    } };
  }
  function setCancelRef(ref) {
    return function(cancel) {
      return function() {
        ref.cancel = cancel;
      };
    };
  }
  function getCancelRef(ref) {
    return function() {
      return ref.cancel;
    };
  }

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
  var apply4 = function(v) {
    return function __do3() {
      var sum2 = $$new({
        x: 0,
        y: 0
      })();
      foreachE(v.nodes)(function(node) {
        return function __do4() {
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
  var fromFoldable6 = /* @__PURE__ */ fromFoldable(foldableList);
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
      var place2 = function(groups) {
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
      return fromFoldable6(values2(foldl3(place2)(empty3)(items)));
    };
  };

  // output/Hylograph.Forces.Pure.Cohesion/index.js
  var map9 = /* @__PURE__ */ map(functorArray);
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var initialize2 = function(config) {
    return function(nodes) {
      var member4 = function(node) {
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
      return map9(map9(member4))(grouped(config.group)(nodes));
    };
  };
  var apply5 = function(v) {
    return function(alpha) {
      return foreachE(v)(function(members) {
        return function __do3() {
          var sum2 = $$new({
            x: 0,
            y: 0,
            count: 0
          })();
          foreachE(members)(function(member4) {
            return when3(isFiniteImpl(member4.node.x) && isFiniteImpl(member4.node.y))(function __do4() {
              var total2 = read(sum2)();
              return write({
                x: total2.x + member4.node.x,
                y: total2.y + member4.node.y,
                count: total2.count + 1
              })(sum2)();
            });
          })();
          var total = read(sum2)();
          var centroidY = total.y / total.count;
          var centroidX = total.x / total.count;
          return foreachE(members)(function(member4) {
            return when3(isFiniteImpl(member4.node.x) && isFiniteImpl(member4.node.y))(function() {
              return writeVelocity(member4.node, member4.node.vx + (centroidX - member4.node.x) * member4.strength * alpha, member4.node.vy + (centroidY - member4.node.y) * member4.strength * alpha);
            });
          })();
        };
      });
    };
  };

  // output/Hylograph.Forces.Pure.Quadtree/index.js
  var $runtime_lazy5 = function(name3, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
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
                  function $tco_loop(x0, y0, x1, y1, size3) {
                    var $24 = x0 > x || (x >= x1 || (y0 > y || y >= y1));
                    if ($24) {
                      var doubled = size3 * 2;
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
    return function __do3() {
      var seed = read(source)();
      var next = advance(seed);
      write(next)(source)();
      return jiggle(next);
    };
  };

  // output/Hylograph.Forces.Pure.Collide/index.js
  var $runtime_lazy6 = function(name3, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
      return val;
    };
  };
  var pure5 = /* @__PURE__ */ pure(applicativeEffect);
  var when4 = /* @__PURE__ */ when(applicativeEffect);
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
  var seek = function(index3, radius, nodeX, nodeY, start3) {
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
                var v = seek(index3, radius, nodeX, nodeY, start3);
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
      return pure5(unit);
    }
    ;
    return function __do3() {
      var waiting = $$new(pushes)();
      while (!(function __do4() {
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
    return function __do3() {
      var points = $$new(Nil.value)();
      foreachE(bodies)(function(body) {
        return function __do4() {
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
  var apply6 = function(v) {
    return function(alpha) {
      var strength = (function() {
        if (v.alphaScaled) {
          return v.strength * alpha;
        }
        ;
        return v.strength;
      })();
      return forE(0)(v.iterations)(function(v1) {
        return function __do3() {
          var points = anticipate(v.bodies)();
          var chain = regions(build(points));
          return foreachE(v.bodies)(function(body) {
            return when4(body.included)(function __do4() {
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
  var append3 = /* @__PURE__ */ append(semigroupArray);
  var foldl4 = /* @__PURE__ */ foldl(foldableArray);
  var map10 = /* @__PURE__ */ map(functorArray);
  var fromFoldable7 = /* @__PURE__ */ fromFoldable2(foldableArray);
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
    var sides = zipWith(function(from2) {
      return function(to) {
        return {
          from: from2,
          to
        };
      };
    })(vertices)(append3(drop(1)(vertices))(take(1)(vertices)));
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
        return catMaybes(map10(edge)(sides));
      }
      ;
      return [];
    })();
    return new Polygon(fromFoldable7(edges));
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
        kept: catMaybes(map10(kept)(nodes)),
        strength: config.strength,
        hard: config.hard
      };
    };
  };
  var free = /* @__PURE__ */ (function() {
    return Free.value;
  })();
  var confine = function(radius) {
    return function(low) {
      return function(high) {
        return function(value2) {
          var most = high - radius;
          var least = low + radius;
          var $40 = least > most;
          if ($40) {
            return (low + high) / 2;
          }
          ;
          var $41 = value2 < least;
          if ($41) {
            return least;
          }
          ;
          var $42 = value2 > most;
          if ($42) {
            return most;
          }
          ;
          return value2;
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
          if (region instanceof Circle) {
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
          if (region instanceof Polygon) {
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
        return function __do3() {
          writeVelocity(kept.node, kept.node.vx + (allowed.x - kept.node.x), kept.node.vy + (allowed.y - kept.node.y));
          return writePosition(kept.node, allowed.x, allowed.y);
        };
      })());
    }));
  };
  var circle = function(x) {
    return function(y) {
      return function(radius) {
        return new Circle({
          x,
          y,
          radius
        });
      };
    };
  };
  var apply7 = function(v) {
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
  var map11 = /* @__PURE__ */ map(functorArray);
  var map14 = /* @__PURE__ */ map(functorEither);
  var traverse3 = /* @__PURE__ */ traverse(traversableArray)(applicativeEither);
  var joinPlaces = function(nodes) {
    return function(links) {
      var count = length(nodes);
      var connected = function(link2) {
        return link2.source >= 0 && (link2.source < count && (link2.target >= 0 && link2.target < count));
      };
      var degrees = (function __do3() {
        var counts = thaw(replicate(count)(0))();
        foreach(links)(function(link2) {
          return when6(connected(link2))(function __do4() {
            modify2(link2.source)(function(v) {
              return v + 1 | 0;
            })(counts)();
            modify2(link2.target)(function(v) {
              return v + 1 | 0;
            })(counts)();
            return unit;
          });
        })();
        return unsafeFreeze(counts)();
      })();
      var degree = function(place2) {
        return toNumber(fromMaybe(0)(index(degrees)(place2)));
      };
      var joined = function(link2) {
        var v = index(nodes)(link2.target);
        var v1 = index(nodes)(link2.source);
        if (v1 instanceof Just && v instanceof Just) {
          return new Just({
            source: v1.value0,
            target: v.value0,
            sourceDegree: degree(link2.source),
            targetDegree: degree(link2.target),
            bias: degree(link2.source) / (degree(link2.source) + degree(link2.target)),
            link: link2.link
          });
        }
        ;
        return Nothing.value;
      };
      return catMaybes(map11(joined)(links));
    };
  };
  var joinById = function(dictOrd) {
    var fromFoldable11 = fromFoldable3(dictOrd)(foldableArray);
    var lookup7 = lookup2(dictOrd);
    return function(dictShow) {
      var show6 = show(dictShow);
      return function(identify) {
        return function(nodes) {
          return function(links) {
            var places = fromFoldable11(mapWithIndex2(function(place2) {
              return function(node) {
                return new Tuple(identify(node), place2);
              };
            })(nodes));
            var find3 = function(id2) {
              var v = lookup7(id2)(places);
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
            var placed = function(link2) {
              var v = find3(link2.source);
              if (v instanceof Left) {
                return new Left(v.value0);
              }
              ;
              if (v instanceof Right) {
                var v1 = find3(link2.target);
                if (v1 instanceof Left) {
                  return new Left(v1.value0);
                }
                ;
                if (v1 instanceof Right) {
                  return new Right({
                    source: v.value0,
                    target: v1.value0,
                    link: link2
                  });
                }
                ;
                throw new Error("Failed pattern match at Hylograph.Forces.Pure.Linkage (line 78, column 21 - line 80, column 53): " + [v1.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Hylograph.Forces.Pure.Linkage (line 76, column 17 - line 80, column 53): " + [v.constructor.name]);
            };
            return map14(joinPlaces(nodes))(traverse3(placed)(links));
          };
        };
      };
    };
  };

  // output/Hylograph.Forces.Pure.Directional/index.js
  var map15 = /* @__PURE__ */ map(functorArray);
  var map16 = /* @__PURE__ */ map(functorEither);
  var when7 = /* @__PURE__ */ when(applicativeEffect);
  var ordered = function(config) {
    return function(joined) {
      var usable = function(candidate) {
        return !(isNaNImpl(candidate.gap) || isNaNImpl(candidate.strength));
      };
      var link2 = function(end) {
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
        links: filter(usable)(map15(link2)(joined)),
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
              return map16(ordered(config))(joinById1(identify)(nodes)(links));
            };
          };
        };
      };
    };
  };
  var apply8 = function(v) {
    return function(alpha) {
      return forE(0)(v.iterations)(function(v1) {
        return foreachE(v.links)(function(link2) {
          var y = link2.target.y + link2.target.vy - link2.source.y - link2.source.vy;
          var x = link2.target.x + link2.target.vx - link2.source.x - link2.source.vx;
          var along = x * v.axisX + y * v.axisY;
          return when7(along < link2.gap)((function() {
            var share = 1 - link2.bias;
            var push2 = (link2.gap - along) * link2.strength * alpha;
            return function __do3() {
              writeVelocity(link2.target, link2.target.vx + v.axisX * push2 * link2.bias, link2.target.vy + v.axisY * push2 * link2.bias);
              return writeVelocity(link2.source, link2.source.vx - v.axisX * push2 * share, link2.source.vy - v.axisY * push2 * share);
            };
          })());
        });
      });
    };
  };

  // output/Hylograph.Forces.Pure.Link/index.js
  var min3 = /* @__PURE__ */ min(ordNumber);
  var map17 = /* @__PURE__ */ map(functorArray);
  var pure6 = /* @__PURE__ */ pure(applicativeEffect);
  var map18 = /* @__PURE__ */ map(functorEither);
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
          springs: map17(spring)(joined),
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
    return function(value2) {
      var $24 = value2 < 0 || value2 > 0;
      if ($24) {
        return pure6(value2);
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
                return map18(springs(config)(random))(joinById1(identify)(nodes)(links));
              };
            };
          };
        };
      };
    };
  };
  var apply9 = function(v) {
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
          return function __do3() {
            var apartX = orJiggle(v.random)(x)();
            var apartY = orJiggle(v.random)(y)();
            return pull(spring, alpha, apartX, apartY);
          };
        });
      });
    };
  };

  // output/Hylograph.Forces.Pure.ManyBody/index.js
  var $runtime_lazy7 = function(name3, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
      return val;
    };
  };
  var map19 = /* @__PURE__ */ map(functorArray);
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
    return function __do3() {
      var points = $$new(Nil.value)();
      foreachE(bodies)(function(body) {
        return function __do4() {
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
              return go(nodes);
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
              })(map19(negated)(bodies));
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
  var apply10 = function(v) {
    return function(alpha) {
      var among2 = function(bodies) {
        return function __do3() {
          var points = snapshot(bodies)();
          var chain = clusters(v.limits.theta2)(build(points));
          return foreachE(bodies)(function(body) {
            return function __do4() {
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
        return function __do3() {
          among2(v.bodies)();
          return foreachE(v.groups)(among2)();
        };
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure.ManyBody (line 146, column 32 - line 151, column 32): " + [v.scope.constructor.name]);
    };
  };

  // output/Hylograph.Forces.Pure.Position/index.js
  var map20 = /* @__PURE__ */ map(functorArray);
  var initialize6 = function(config) {
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
      return map20(pulled)(nodes);
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
  var map21 = /* @__PURE__ */ map(functorArray);
  var orTiny = function(value2) {
    var $11 = value2 < 0 || value2 > 0;
    if ($11) {
      return value2;
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
        nodes: map21(pulled)(nodes),
        x: config.x,
        y: config.y
      };
    };
  };
  var apply11 = function(v) {
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
  var $runtime_lazy8 = function(name3, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
      return val;
    };
  };
  var comparing3 = /* @__PURE__ */ comparing(ordInt);
  var when8 = /* @__PURE__ */ when(applicativeEffect);
  var pure7 = /* @__PURE__ */ pure(applicativeEffect);
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
    return sortBy2(comparing3(function(v) {
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
          var height = taller(tree.value3, fourth, taller(tree.value2, third, taller(tree.value1, second, taller(tree.value0, first, 0))));
          var width = wider(tree.value3, fourth, wider(tree.value2, third, wider(tree.value1, second, wider(tree.value0, first, 0))));
          return new Cell(width, height, x0, y0, x1, y1, first, after, Nil.value);
        }
        ;
        throw new Error("Failed pattern match at Hylograph.Forces.Pure.RectCollide (line 172, column 45 - line 194, column 54): " + [tree.constructor.name]);
      };
    });
    var thread = $lazy_thread(171);
    return thread(planted.root, planted.extent.x0, planted.extent.y0, planted.extent.x1, planted.extent.y1, Finished3.value);
  };
  var anticipate2 = function(boxes) {
    return function __do3() {
      var points = $$new(Nil.value)();
      foreachE(boxes)(function(box) {
        return function __do4() {
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
  var among = function(index3, halfWidth, halfHeight, aheadX, aheadY, occupants, start3) {
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
    return go(occupants)(start3);
  };
  var overlapping = function(index3, halfWidth, halfHeight, aheadX, aheadY, chain, start3) {
    var go = function($copy_cell) {
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
    return go(chain)(start3);
  };
  var apply12 = function(v) {
    var settle = function(strength, box, found, aheadX, aheadY, startVx, startVy) {
      var area = box.halfWidth * box.halfHeight;
      var go = function($copy_others) {
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
      return go(inIndexOrder(found))(startVx)(startVy);
    };
    return forE(0)(v.iterations)(function(v1) {
      return function __do3() {
        var points = anticipate2(v.boxes)();
        var chain = cells(build(points));
        return foreachE(v.boxes)(function(box) {
          var aheadY = box.node.y + box.node.vy;
          var aheadX = box.node.x + box.node.vx;
          return when8(isFiniteImpl(aheadX) && isFiniteImpl(aheadY))((function() {
            var v2 = overlapping(box.index, box.halfWidth, box.halfHeight, aheadX, aheadY, chain, Nil.value);
            if (v2 instanceof Nil) {
              return pure7(unit);
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
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var when9 = /* @__PURE__ */ when(applicativeEffect);
  var pin = function(nodes) {
    return foreachE(nodes)(function(node) {
      var v = toMaybe(node.fy);
      var v1 = toMaybe(node.fx);
      if (v1 instanceof Nothing && v instanceof Nothing) {
        return pure8(unit);
      }
      ;
      if (v1 instanceof Just && v instanceof Nothing) {
        return function __do3() {
          writePosition(node, v1.value0, node.y);
          return writeVelocity(node, 0, node.vy);
        };
      }
      ;
      if (v1 instanceof Nothing && v instanceof Just) {
        return function __do3() {
          writePosition(node, node.x, v.value0);
          return writeVelocity(node, node.vx, 0);
        };
      }
      ;
      if (v1 instanceof Just && v instanceof Just) {
        return function __do3() {
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
          return function __do3() {
            writePosition(node, node.x + node.vx * velocityRetention, node.y + node.vy * velocityRetention);
            return writeVelocity(node, node.vx * velocityRetention, node.vy * velocityRetention);
          };
        }
        ;
        if (v1 instanceof Just && v instanceof Nothing) {
          return function __do3() {
            writePosition(node, v1.value0, node.y + node.vy * velocityRetention);
            return writeVelocity(node, 0, node.vy * velocityRetention);
          };
        }
        ;
        if (v1 instanceof Nothing && v instanceof Just) {
          return function __do3() {
            writePosition(node, node.x + node.vx * velocityRetention, v.value0);
            return writeVelocity(node, node.vx * velocityRetention, 0);
          };
        }
        ;
        if (v1 instanceof Just && v instanceof Just) {
          return function __do3() {
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
      return function __do3() {
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
  var map23 = /* @__PURE__ */ map(functorArray);
  var initialize9 = function(config) {
    return function(nodes) {
      var pulled = function(node) {
        var target = config.target(node);
        var strength = config.strength(node);
        return {
          node,
          x: target.x,
          y: target.y,
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
      return filter(aimed)(map23(pulled)(nodes));
    };
  };
  var apply13 = function(v) {
    return function(alpha) {
      return foreachE(v)(function(pulled) {
        return function() {
          return writeVelocity(pulled.node, pulled.node.vx + (pulled.x - pulled.node.x) * pulled.strength * alpha, pulled.node.vy + (pulled.y - pulled.node.y) * pulled.strength * alpha);
        };
      });
    };
  };

  // output/Hylograph.Forces.Pure/index.js
  var map24 = /* @__PURE__ */ map(functorEither);
  var when10 = /* @__PURE__ */ when(applicativeEffect);
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
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
                return map24(LinkForce.create)(initializeById1(identify)(config)(random)(nodes)(links));
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
              return map24(DirectionalForce.create)(initializeById1(identify)(config)(nodes)(links));
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
      return when10(any2(clamps)(forces))(function __do3() {
        foreachE(forces)(function(force) {
          if (force instanceof ContainForce) {
            return constrain(force.value0);
          }
          ;
          return pure9(unit);
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
        return apply10(force.value0)(alpha);
      }
      ;
      if (force instanceof LinkForce) {
        return apply9(force.value0)(alpha);
      }
      ;
      if (force instanceof CollideForce) {
        return apply6(force.value0)(alpha);
      }
      ;
      if (force instanceof CenterForce) {
        return apply4(force.value0);
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
        return apply11(force.value0)(alpha);
      }
      ;
      if (force instanceof TowardsForce) {
        return apply13(force.value0)(alpha);
      }
      ;
      if (force instanceof RectCollideForce) {
        return apply12(force.value0);
      }
      ;
      if (force instanceof ContainForce) {
        return apply7(force.value0);
      }
      ;
      if (force instanceof CohesionForce) {
        return apply5(force.value0)(alpha);
      }
      ;
      if (force instanceof DirectionalForce) {
        return apply8(force.value0)(alpha);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.Forces.Pure (line 156, column 26 - line 168, column 58): " + [force.constructor.name]);
    };
  };

  // output/Hylograph.ForceEngine.Core/index.js
  var bind3 = /* @__PURE__ */ bind(bindEffect);
  var when11 = /* @__PURE__ */ when(applicativeEffect);
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var map25 = /* @__PURE__ */ map(functorEither);
  var map110 = /* @__PURE__ */ map(functorArray);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableArray);
  var startAnimation = function(onFrame) {
    return function __do3() {
      var cancelRef = newCancelRef();
      var loop = function(timestamp) {
        return function __do4() {
          var $$continue = onFrame(timestamp)();
          return when11($$continue)(function __do5() {
            var cancel2 = requestAnimationFrame_(loop)();
            return setCancelRef(cancelRef)(cancel2)();
          })();
        };
      };
      var cancel = requestAnimationFrame_(loop)();
      setCancelRef(cancelRef)(cancel)();
      return bind3(getCancelRef(cancelRef))(identity8);
    };
  };
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
          return map25(function(force) {
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
      return constrain2(map110(function(v) {
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
  var fromFoldable8 = /* @__PURE__ */ fromFoldable3(ordInt)(foldableArray);
  var map26 = /* @__PURE__ */ map(functorArray);
  var lookup4 = /* @__PURE__ */ lookup2(ordInt);
  var show3 = /* @__PURE__ */ show(showInt);
  var bind4 = /* @__PURE__ */ bind(bindEither);
  var pure10 = /* @__PURE__ */ pure(applicativeEither);
  var traverse4 = /* @__PURE__ */ traverse(traversableArray)(applicativeEither);
  var apply14 = /* @__PURE__ */ apply(applyMaybe);
  var map111 = /* @__PURE__ */ map(functorMaybe);
  var fromFoldableWith2 = /* @__PURE__ */ fromFoldableWith(ordInt)(foldableArray);
  var add2 = /* @__PURE__ */ add(semiringInt);
  var min4 = /* @__PURE__ */ min(ordInt);
  var linkById2 = /* @__PURE__ */ linkById(ordInt)(showInt);
  var bind1 = /* @__PURE__ */ bind(bindMaybe);
  var directionalById2 = /* @__PURE__ */ directionalById(ordInt)(showInt);
  var map27 = /* @__PURE__ */ map(functorEither);
  var fromFoldable1 = /* @__PURE__ */ fromFoldable3(ordString)(foldableArray);
  var fromFoldable22 = /* @__PURE__ */ fromFoldable4(foldableArray);
  var fromFoldable32 = /* @__PURE__ */ fromFoldable22(ordInt);
  var member3 = /* @__PURE__ */ member2(ordInt);
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordInt)(ordInt);
  var fromFoldable42 = /* @__PURE__ */ fromFoldable22(ordTuple2);
  var difference4 = /* @__PURE__ */ difference3(ordTuple2);
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
  var withRadius = function(v) {
    return function(v1) {
      if (v1 instanceof CollideForce2) {
        return new CollideForce2({
          name: v1.value0.name,
          strength: v1.value0.strength,
          iterations: v1.value0.iterations,
          filter: v1.value0.filter,
          alphaScaled: v1.value0.alphaScaled,
          radius: v
        });
      }
      ;
      if (v1 instanceof RadialForce2) {
        return new RadialForce2({
          name: v1.value0.name,
          x: v1.value0.x,
          y: v1.value0.y,
          strength: v1.value0.strength,
          filter: v1.value0.filter,
          radius: v
        });
      }
      ;
      if (v1 instanceof ContainForce2) {
        return new ContainForce2({
          name: v1.value0.name,
          regions: v1.value0.regions,
          region: v1.value0.region,
          strength: v1.value0.strength,
          hard: v1.value0.hard,
          radius: v
        });
      }
      ;
      return v1;
    };
  };
  var withEnds = function(nodes) {
    return function(links) {
      var byId = fromFoldable8(map26(function(node) {
        return new Tuple(node.id, node);
      })(nodes));
      var find3 = function(id2) {
        var v = lookup4(id2)(byId);
        if (v instanceof Just) {
          return new Right(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return new Left("node not found: " + show3(id2));
        }
        ;
        throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 1065, column 13 - line 1067, column 52): " + [v.constructor.name]);
      };
      var ended = function(v) {
        return bind4(find3(v.value1.source))(function(source) {
          return bind4(find3(v.value1.target))(function(target) {
            return pure10({
              source: v.value1.source,
              target: v.value1.target,
              ends: {
                source,
                target,
                index: v.value0
              }
            });
          });
        });
      };
      return traverse4(ended)(mapWithIndex2(Tuple.create)(links));
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
      var byId = fromFoldable8(map26(function(node) {
        return new Tuple(node.id, node);
      })(nodes));
      var endsOf = function(l) {
        return apply14(map111(function(v) {
          return function(v1) {
            return {
              source: v,
              target: v1
            };
          };
        })(lookup4(l.source)(byId)))(lookup4(l.target)(byId));
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
  var membership = function(group4) {
    return function(node) {
      var v = group4(node);
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
  var manyBody2 = function(name3) {
    return new ManyBodyForce2({
      name: name3,
      strength: new Static(-30),
      theta: 0.9,
      distanceMin: 1,
      distanceMax: 1e10,
      filter: Nothing.value,
      scope: AllPairs.value
    });
  };
  var linkValues = function(value2) {
    return function(ends) {
      if (value2 instanceof Static) {
        return map26($$const(value2.value0))(ends);
      }
      ;
      if (value2 instanceof Dynamic) {
        return map26(value2.value0)(ends);
      }
      ;
      throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup (line 733, column 25 - line 735, column 26): " + [value2.constructor.name]);
    };
  };
  var linkStrengths = function(strength) {
    return function(ends) {
      var count = fromFoldableWith2(add2)(concatMap(function(e) {
        return [new Tuple(e.source.id, 1), new Tuple(e.target.id, 1)];
      })(ends));
      var degree = function(id2) {
        return fromMaybe(1)(lookup4(id2)(count));
      };
      var byDegree = function(e) {
        return 1 / toNumber(min4(degree(e.source.id))(degree(e.target.id)));
      };
      if (strength instanceof LinkStrength) {
        return linkValues(strength.value0)(ends);
      }
      ;
      if (strength instanceof ByDegree) {
        return map26(byDegree)(ends);
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
            return bind4(withEnds(nodes)(links))(function(ended) {
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
              })(random)(nodes)(ended);
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
            var regions2 = map26(regionOf)(v.value0.regions);
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
            return bind4(withEnds(nodes)(links))(function(ended) {
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
              })(nodes)(ended);
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
      return function __do3() {
        var nodes = read(sim.nodes)();
        var links = read(sim.links)();
        var ids2 = map26(function(l) {
          return {
            source: l.source,
            target: l.target
          };
        })(links);
        var made = traverse4(function(fc) {
          return map27(Tuple.create(forceName(fc)))(initializeForce(sim.random)(nodes)(ids2)(recipe(fc)));
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
  var dynamic = /* @__PURE__ */ (function() {
    return Dynamic.create;
  })();
  var defaultParams = {
    alpha: 1,
    alphaMin: 1e-3,
    alphaDecay: 0.01,
    alphaTarget: 0,
    velocityRetention: 0.4
  };
  var setup = function(name3) {
    return function(forces) {
      return {
        name: name3,
        forces,
        params: defaultParams
      };
    };
  };
  var computeNodeGUP = function(currentNodes) {
    return function(desiredNodes) {
      var desiredIds = fromFoldable32(map26(function(v) {
        return v.id;
      })(desiredNodes));
      var exited = filter(function(n) {
        return !member3(n.id)(desiredIds);
      })(currentNodes);
      var currentById = fromFoldable8(map26(function(n) {
        return new Tuple(n.id, n);
      })(currentNodes));
      var processDesired = function(desired) {
        var v = lookup4(desired.id)(currentById);
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
      var processed = map26(processDesired)(desiredNodes);
      var entered = map26(function(v) {
        return v.node;
      })(filter(function(v) {
        return v.isEnter;
      })(processed));
      var merged = map26(function(v) {
        return v.node;
      })(processed);
      var updated = map26(function(v) {
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
      var desiredKeys = fromFoldable42(map26(linkKey)(desiredLinks));
      var currentKeys = fromFoldable42(map26(linkKey)(currentLinks));
      var enterKeys = difference4(desiredKeys)(currentKeys);
      var entered = filter(function(l) {
        return member1(linkKey(l))(enterKeys);
      })(desiredLinks);
      var exitKeys = difference4(currentKeys)(desiredKeys);
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
  var collide2 = function(name3) {
    return new CollideForce2({
      name: name3,
      radius: new Static(1),
      strength: 1,
      iterations: 1,
      filter: Nothing.value,
      alphaScaled: false
    });
  };
  var center2 = function(name3) {
    return new CenterForce2({
      name: name3,
      x: 0,
      y: 0,
      strength: 1
    });
  };
  var applySetup = function(setupConfig) {
    return function(sim) {
      return function __do3() {
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
          return function __do3() {
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

  // output/Hylograph.ForceEngine.Simulation/foreign.js
  function interpolatePositionsInPlace_(startPositions) {
    return function(targetPositions) {
      return function(progress) {
        return function(nodesRef) {
          return function() {
            const nodes = nodesRef.value;
            for (let i = 0; i < nodes.length; i++) {
              const node = nodes[i];
              const key = String(node.id);
              const start3 = startPositions[key];
              const target = targetPositions[key];
              if (start3 && target) {
                node.x = start3.x + (target.x - start3.x) * progress;
                node.y = start3.y + (target.y - start3.y) * progress;
              }
            }
          };
        };
      };
    };
  }
  function pinNodesInPlace_(nodesRef) {
    return function() {
      const nodes = nodesRef.value;
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        node.fx = node.x;
        node.fy = node.y;
      }
    };
  }
  function unpinNodesInPlace_(nodesRef) {
    return function() {
      const nodes = nodesRef.value;
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        node.fx = null;
        node.fy = null;
      }
    };
  }

  // output/Hylograph.ForceEngine.Types/index.js
  var defaultSimParams = {
    alpha: 1,
    alphaMin: 1e-3,
    alphaDecay: 0.01,
    alphaTarget: 0,
    velocityRetention: 0.4
  };

  // output/Hylograph.ForceEngine.Simulation/index.js
  var pure11 = /* @__PURE__ */ pure(applicativeEffect);
  var for_3 = /* @__PURE__ */ for_(applicativeEffect);
  var for_1 = /* @__PURE__ */ for_3(foldableArray);
  var when12 = /* @__PURE__ */ when(applicativeEffect);
  var fromFoldable9 = /* @__PURE__ */ fromFoldable(foldableList);
  var unless2 = /* @__PURE__ */ unless(applicativeEffect);
  var unpinNodesInPlace = function(sim) {
    return unpinNodesInPlace_(sim.nodes);
  };
  var pinNodesInPlace = function(sim) {
    return pinNodesInPlace_(sim.nodes);
  };
  var invokeTickCallback = function(sim) {
    if (sim.callbacks instanceof Nothing) {
      return pure11(unit);
    }
    ;
    if (sim.callbacks instanceof Just) {
      return function __do3() {
        var callback = read(sim.callbacks.value0.onTick)();
        return callback();
      };
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 492, column 26 - line 496, column 13): " + [sim.callbacks.constructor.name]);
  };
  var invokeStopCallback = function(sim) {
    if (sim.callbacks instanceof Nothing) {
      return pure11(unit);
    }
    ;
    if (sim.callbacks instanceof Just) {
      return function __do3() {
        var callback = read(sim.callbacks.value0.onStop)();
        return callback();
      };
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 482, column 26 - line 486, column 13): " + [sim.callbacks.constructor.name]);
  };
  var invokeStartCallback = function(sim) {
    if (sim.callbacks instanceof Nothing) {
      return pure11(unit);
    }
    ;
    if (sim.callbacks instanceof Just) {
      return function __do3() {
        var callback = read(sim.callbacks.value0.onStart)();
        return callback();
      };
    }
    ;
    throw new Error("Failed pattern match at Hylograph.ForceEngine.Simulation (line 472, column 27 - line 476, column 13): " + [sim.callbacks.constructor.name]);
  };
  var interpolatePositionsInPlace = function(startPos) {
    return function(targetPos) {
      return function(progress) {
        return function(sim) {
          return interpolatePositionsInPlace_(startPos)(targetPos)(progress)(sim.nodes);
        };
      };
    };
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
    return function __do3() {
      var nodesRef = $$new([])();
      var linksRef = $$new([])();
      var forcesRef = $$new(empty3)();
      var alphaRef = $$new(1)();
      var prevAlphaRef = $$new(1)();
      var runningRef = $$new(false)();
      var cancelRef = $$new(pure11(unit))();
      var tickRef = $$new(pure11(unit))();
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
          return pure11(unit);
        }
        ;
        if (sim.callbacks instanceof Just) {
          return for_1(alphaThresholds)(function(threshold) {
            return when12(crossedThreshold(prevAlpha)(newAlpha)(threshold))(function __do3() {
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
    return function __do3() {
      var nodes = read(sim.nodes)();
      var forces = read(sim.forces)();
      var alpha = read(sim.alpha)();
      var prevAlpha = read(sim.prevAlpha)();
      var forceHandles = fromFoldable9(values2(forces));
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
  var start = function(sim) {
    return function __do3() {
      var alreadyRunning = read(sim.running)();
      return unless2(alreadyRunning)(function __do4() {
        write(true)(sim.running)();
        invokeStartCallback(sim)();
        var cancel = startAnimation(function(v) {
          return function __do5() {
            var running = read(sim.running)();
            if (running) {
              var newAlpha = tick(sim)();
              var shouldContinue = newAlpha > 0;
              unless2(shouldContinue)(function __do6() {
                write(false)(sim.running)();
                return invokeStopCallback(sim)();
              })();
              return shouldContinue;
            }
            ;
            return false;
          };
        })();
        return write(cancel)(sim.cancelAnimation)();
      })();
    };
  };
  var reheat = function(sim) {
    return function __do3() {
      write(1)(sim.alpha)();
      var running = read(sim.running)();
      return unless2(running)(start(sim))();
    };
  };

  // output/Hylograph.ForceEngine.WASM/foreign.js
  var WasmSimulation = null;
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
  function reheat_(sim) {
    return function() {
      sim.reheat();
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
  function addForceX_(sim, target, strength) {
    return sim.add_force_x(target, strength);
  }
  function addForceY_(sim, target, strength) {
    return sim.add_force_y(target, strength);
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
  function enableForcesAll_(sim, manyBody3, links, center3, forceX, forceY, collide3) {
    sim.enable_forces_all(manyBody3, links, center3, forceX, forceY, collide3);
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
  var ids = (values3) => Uint32Array.from(values3, (v) => v < 0 ? 4294967295 : v);
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
  function setManyBodyScope_(sim, force, scope) {
    return sim.set_many_body_scope(force, scope);
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

  // output/Hylograph.ForceEngine.WASM/index.js
  var map28 = /* @__PURE__ */ map(functorArray);
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
      return function(scope) {
        return function() {
          return setManyBodyScope_(sim, force, (function() {
            if (scope instanceof AllPairs2) {
              return 0;
            }
            ;
            if (scope instanceof WithinGroups3) {
              return 1;
            }
            ;
            if (scope instanceof BetweenGroups3) {
              return 2;
            }
            ;
            throw new Error("Failed pattern match at Hylograph.ForceEngine.WASM (line 570, column 77 - line 573, column 21): " + [scope.constructor.name]);
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
  var reheat2 = reheat_;
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
          return setForceGroups_(sim, force, map28(orNone)(groups));
        };
      };
    };
  };
  var setForceRegions = function(sim) {
    return function(force) {
      return function(regions2) {
        return function() {
          return setForceRegions_(sim, force, map28(orNone)(regions2));
        };
      };
    };
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
  var pure12 = /* @__PURE__ */ pure(applicativeEffect);
  var fromFoldable10 = /* @__PURE__ */ fromFoldable3(ordInt)(foldableArray);
  var apply15 = /* @__PURE__ */ apply(applyMaybe);
  var map29 = /* @__PURE__ */ map(functorMaybe);
  var lookup5 = /* @__PURE__ */ lookup2(ordInt);
  var when13 = /* @__PURE__ */ when(applicativeEffect);
  var show4 = /* @__PURE__ */ show(showInt);
  var $$void5 = /* @__PURE__ */ $$void(functorEffect);
  var map112 = /* @__PURE__ */ map(functorArray);
  var fromFoldable12 = /* @__PURE__ */ fromFoldable(foldableList);
  var fromFoldable23 = /* @__PURE__ */ fromFoldable3(ordString)(foldableArray);
  var nub3 = /* @__PURE__ */ nub(ordInt);
  var bind12 = /* @__PURE__ */ bind(bindMaybe);
  var for_4 = /* @__PURE__ */ for_(applicativeEffect);
  var for_12 = /* @__PURE__ */ for_4(foldableMaybe);
  var for_22 = /* @__PURE__ */ for_4(foldableArray);
  var traverse6 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var map210 = /* @__PURE__ */ map(functorEffect);
  var tick3 = function(sim) {
    return function __do3() {
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
        var indexOfId = fromFoldable10(mapWithIndex2(function(index3) {
          return function(node) {
            return new Tuple(node.id, index3);
          };
        })(nodes));
        var endpoints = function(link2) {
          return apply15(map29(function(v) {
            return function(v1) {
              return [v, v1];
            };
          })(lookup5(link2.source)(indexOfId)))(lookup5(link2.target)(indexOfId));
        };
        var resolved = mapMaybe(endpoints)(links);
        var missing = length(links) - length(resolved) | 0;
        return function __do3() {
          when13(missing > 0)(warn("WASM Setup: " + (show4(missing) + " link(s) name a node id that is not in the simulation; left out")))();
          return setLinks(wasmSim)(concat(resolved))();
        };
      };
    };
  };
  var reheat3 = function(sim) {
    return reheat2(sim.wasmSim);
  };
  var inApplicationOrder = function(forces) {
    return fromFoldable12(values2(fromFoldable23(map112(function(fc) {
      return new Tuple(forceName(fc), fc);
    })(forces))));
  };
  var getNodes2 = function(sim) {
    return function __do3() {
      var nodes = read(sim.nodesRef)();
      return syncNodesFromWasm(sim.wasmSim)(nodes)();
    };
  };
  var flatTargets = function(target) {
    return concatMap(function(node) {
      var v = target(node);
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
      return function __do3() {
        var wasmSim = create2(length(nodes))();
        initializeNodes2(nodes)();
        syncNodesToWasm(wasmSim)(nodes)();
        setLinks2(wasmSim)(nodes)(links)();
        var nodesRef = $$new(nodes)();
        var linksRef = $$new(links)();
        var forceIds = $$new(empty3)();
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
        var perNode = function(value2) {
          return function(send) {
            if (value2 instanceof Static) {
              return pure12(unit);
            }
            ;
            if (value2 instanceof Dynamic) {
              return $$void5(send(map112(value2.value0)(nodes)));
            }
            ;
            throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 399, column 24 - line 401, column 43): " + [value2.constructor.name]);
          };
        };
        var perLink = function(value2) {
          return function(send) {
            if (value2 instanceof Static) {
              return pure12(unit);
            }
            ;
            if (value2 instanceof Dynamic) {
              return $$void5(send(linkValues(value2)(ends)));
            }
            ;
            throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 405, column 24 - line 407, column 53): " + [value2.constructor.name]);
          };
        };
        var groupsOf = function(group4) {
          var given = map112(group4)(nodes);
          var numbering = fromFoldable10(mapWithIndex2(function(index3) {
            return function(g) {
              return new Tuple(g, index3);
            };
          })(nub3(catMaybes(given))));
          return map112(function(v) {
            return bind12(v)(function(g) {
              return lookup5(g)(numbering);
            });
          })(given);
        };
        var filtered2 = function(filter3) {
          return function(force) {
            return for_12(filter3)(function(keep) {
              return $$void5(setForceMask(wasmSim)(force)(map112(keep)(nodes)));
            });
          };
        };
        return function(v) {
          if (v instanceof ManyBodyForce2) {
            return function __do3() {
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
            return function __do3() {
              var force = addCollide(wasmSim)({
                radius: valueToNumber(v.value0.radius),
                strength: v.value0.strength,
                iterations: v.value0.iterations
              })();
              perNode(v.value0.radius)(setForceRadii(wasmSim)(force))();
              for_12(v.value0.filter)(function(keep) {
                return $$void5(setCollideFilter(wasmSim)(force)(map112(keep)(nodes)));
              })();
              when13(v.value0.alphaScaled)($$void5(setCollideAlphaScaled(wasmSim)(force)(true)))();
              return force;
            };
          }
          ;
          if (v instanceof LinkForce2) {
            return function __do3() {
              var force = addLinks(wasmSim)({
                distance: valueToNumber(v.value0.distance),
                strength: 1,
                iterations: v.value0.iterations
              })();
              perLink(v.value0.distance)(setForceDistances(wasmSim)(force))();
              (function() {
                if (v.value0.strength instanceof LinkStrength && v.value0.strength.value0 instanceof Static) {
                  return $$void5(setForceStrengths(wasmSim)(force)(map112($$const(v.value0.strength.value0.value0))(ends)))();
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
            return function __do3() {
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
            return function __do3() {
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
            return function __do3() {
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
            return function __do3() {
              var force = addTowards(wasmSim)(valueToNumber(v.value0.strength))();
              $$void5(setForceTargetsXY(wasmSim)(force)(flatTargets(v.value0.target)(nodes)))();
              perNode(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              filtered2(v.value0.filter)(force)();
              return force;
            };
          }
          ;
          if (v instanceof RectCollideForce2) {
            return function __do3() {
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
            return function __do3() {
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
              $$void5(setForceRegions(wasmSim)(force)(map112(v.value0.region)(nodes)))();
              $$void5(setForceRadii(wasmSim)(force)((function() {
                if (v.value0.radius instanceof Static) {
                  return map112($$const(v.value0.radius.value0))(nodes);
                }
                ;
                if (v.value0.radius instanceof Dynamic) {
                  return map112(v.value0.radius.value0)(nodes);
                }
                ;
                throw new Error("Failed pattern match at Hylograph.ForceEngine.Setup.WASM (line 373, column 45 - line 375, column 31): " + [v.value0.radius.constructor.name]);
              })()))();
              return force;
            };
          }
          ;
          if (v instanceof CohesionForce2) {
            return function __do3() {
              var force = addCohesion(wasmSim)(valueToNumber(v.value0.strength))();
              $$void5(setForceGroups(wasmSim)(force)(groupsOf(v.value0.group)))();
              perNode(v.value0.strength)(setForceStrengths(wasmSim)(force))();
              filtered2(v.value0.filter)(force)();
              return force;
            };
          }
          ;
          if (v instanceof DirectionalForce2) {
            return function __do3() {
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
      return function __do3() {
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
        var ids2 = traverse6(function(fc) {
          return map210(Tuple.create(forceName(fc)))(addForce(sim.wasmSim)(nodes)(ends)(fc));
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
          return function __do3() {
            var currentNodes = read(sim.nodesRef)();
            var currentLinks = read(sim.linksRef)();
            var nodeResult = computeNodeGUP(currentNodes)(desiredNodes);
            write(nodeResult.merged)(sim.nodesRef)();
            var currentCount = length(currentNodes);
            var newCount = length(nodeResult.merged);
            when13(currentCount !== newCount)(setNodeCount(sim.wasmSim)(newCount))();
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

  // output/Hylograph.Simulation.Emitter/index.js
  var foldM3 = /* @__PURE__ */ foldM(monadEffect);
  var Tick = /* @__PURE__ */ (function() {
    function Tick2(value0) {
      this.value0 = value0;
    }
    ;
    Tick2.create = function(value0) {
      return new Tick2(value0);
    };
    return Tick2;
  })();
  var Started = /* @__PURE__ */ (function() {
    function Started2() {
    }
    ;
    Started2.value = new Started2();
    return Started2;
  })();
  var Stopped = /* @__PURE__ */ (function() {
    function Stopped2() {
    }
    ;
    Stopped2.value = new Stopped2();
    return Stopped2;
  })();
  var Completed = /* @__PURE__ */ (function() {
    function Completed3() {
    }
    ;
    Completed3.value = new Completed3();
    return Completed3;
  })();
  var subscribe = function(v) {
    return function(callback) {
      return function __do3() {
        var listenerId = read(v.nextId)();
        modify_(function(v1) {
          return v1 + 1 | 0;
        })(v.nextId)();
        modify_(function(ls) {
          return snoc(ls)({
            id: listenerId,
            callback
          });
        })(v.listeners)();
        return modify_(filter(function(l) {
          return l.id !== listenerId;
        }))(v.listeners);
      };
    };
  };
  var emit = function(v) {
    return function(event) {
      return function __do3() {
        var ls = read(v.emitter.listeners)();
        return foldM3(function(v1) {
          return function(listener) {
            return listener.callback(event);
          };
        })(unit)(ls)();
      };
    };
  };
  var create4 = function __do() {
    var listeners = $$new([])();
    var nextId = $$new(0)();
    var emitter = {
      listeners,
      nextId
    };
    return {
      emitter,
      handle: {
        emitter
      }
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
  var cancelAnimationFrame_ = function(id2) {
    return function() {
      window.cancelAnimationFrame(id2);
    };
  };
  var performanceNow_ = function() {
    return performance.now();
  };

  // output/Hylograph.Transition.RAF/index.js
  var requestAnimationFrame2 = requestAnimationFrame_2;
  var performanceNow = performanceNow_;
  var cancelAnimationFrame2 = cancelAnimationFrame_;

  // output/Hylograph.Transition.Coordinator/index.js
  var when14 = /* @__PURE__ */ when(applicativeEffect);
  var for_5 = /* @__PURE__ */ for_(applicativeEffect);
  var for_13 = /* @__PURE__ */ for_5(foldableMaybe);
  var append4 = /* @__PURE__ */ append(semigroupArray);
  var $$for2 = /* @__PURE__ */ $$for(applicativeEffect)(traversableArray);
  var for_23 = /* @__PURE__ */ for_5(foldableArray);
  var unless3 = /* @__PURE__ */ unless(applicativeEffect);
  var Completed2 = /* @__PURE__ */ (function() {
    function Completed3() {
    }
    ;
    Completed3.value = new Completed3();
    return Completed3;
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
  var stop = function(v) {
    return function __do3() {
      var state2 = read(v)();
      return when14(state2.running)(function __do4() {
        for_13(state2.frameId)(cancelAnimationFrame2)();
        return modify_(function(v1) {
          return {
            consumers: v1.consumers,
            lastTimestamp: v1.lastTimestamp,
            nextId: v1.nextId,
            running: false,
            frameId: Nothing.value
          };
        })(v)();
      })();
    };
  };
  var register = function(v) {
    return function(spec) {
      return function __do3() {
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
          consumers: append4(state2.consumers)([consumer]),
          nextId: state2.nextId + 1 | 0
        })(v)();
        return state2.nextId;
      };
    };
  };
  var isCompleted = function(v) {
    if (v instanceof Completed2) {
      return true;
    }
    ;
    return false;
  };
  var scheduleFrame = function(v) {
    return function __do3() {
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
      return function __do3() {
        var state2 = read(v)();
        return when14(state2.running)((function() {
          var deltaMs = timestamp - state2.lastTimestamp;
          return function __do4() {
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
              return function __do5() {
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
              return function __do5() {
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
    return function __do3() {
      var state2 = read(v)();
      return unless3(state2.running)(function __do4() {
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
  var create5 = function __do2() {
    var ref = $$new({
      consumers: [],
      running: false,
      lastTimestamp: 0,
      nextId: 0,
      frameId: Nothing.value
    })();
    return ref;
  };

  // output/Hylograph.Simulation/index.js
  var pure13 = /* @__PURE__ */ pure(applicativeEffect);
  var unless4 = /* @__PURE__ */ unless(applicativeEffect);
  var when15 = /* @__PURE__ */ when(applicativeEffect);
  var Pure = /* @__PURE__ */ (function() {
    function Pure2() {
    }
    ;
    Pure2.value = new Pure2();
    return Pure2;
  })();
  var WASM = /* @__PURE__ */ (function() {
    function WASM2() {
    }
    ;
    WASM2.value = new WASM2();
    return WASM2;
  })();
  var D3 = /* @__PURE__ */ (function() {
    function D32() {
    }
    ;
    D32.value = new D32();
    return D32;
  })();
  var wasmConsumer = function(sim) {
    return function(emitterHandle) {
      return function(alphaRef) {
        return function(alphaMin) {
          return function(_deltaMs) {
            return function __do3() {
              var alpha = tick3(sim)();
              write(alpha)(alphaRef)();
              var nodes = getNodes2(sim)();
              emit(emitterHandle)(new Tick({
                alpha,
                nodeCount: length(nodes)
              }))();
              var $13 = alpha < alphaMin;
              if ($13) {
                return Completed2.value;
              }
              ;
              return new Converged(alpha);
            };
          };
        };
      };
    };
  };
  var runWASMSimulation = function(config) {
    return function __do3() {
      var v = create4();
      var sim = create3(config.nodes)(config.links)();
      applySetupWithData2(config.setup)(config.nodes)(config.links)(sim)();
      var setupRef = $$new(config.setup)();
      var alphaRef = $$new(1)();
      var coord = create5();
      var coordRef = $$new(coord)();
      var runningRef = $$new(true)();
      emit(v.handle)(Started.value)();
      register(coord)({
        tick: wasmConsumer(sim)(v.handle)(alphaRef)(config.alphaMin),
        onComplete: function __do4() {
          write(false)(runningRef)();
          return emit(v.handle)(Completed.value)();
        }
      })();
      start2(coord)();
      return {
        events: v.emitter,
        handle: {
          updateData: function(nodes) {
            return function(links) {
              return function __do4() {
                var currentSetup = read(setupRef)();
                var result = applySetupWithData2(currentSetup)(nodes)(links)(sim)();
                reheat3(sim)();
                write(1)(alphaRef)();
                emit(v.handle)(Started.value)();
                var running = read(runningRef)();
                unless4(running)(function __do5() {
                  write(true)(runningRef)();
                  var oldCoord = read(coordRef)();
                  stop(oldCoord)();
                  var c = create5();
                  write(c)(coordRef)();
                  register(c)({
                    tick: wasmConsumer(sim)(v.handle)(alphaRef)(config.alphaMin),
                    onComplete: function __do6() {
                      write(false)(runningRef)();
                      return emit(v.handle)(Completed.value)();
                    }
                  })();
                  return start2(c)();
                })();
                return result;
              };
            };
          },
          updateSetup: function(newSetup) {
            return function __do4() {
              write(newSetup)(setupRef)();
              applySetup2(newSetup)(sim)();
              reheat3(sim)();
              write(1)(alphaRef)();
              emit(v.handle)(Started.value)();
              var running = read(runningRef)();
              return unless4(running)(function __do5() {
                write(true)(runningRef)();
                var oldCoord = read(coordRef)();
                stop(oldCoord)();
                var c = create5();
                write(c)(coordRef)();
                register(c)({
                  tick: wasmConsumer(sim)(v.handle)(alphaRef)(config.alphaMin),
                  onComplete: function __do6() {
                    write(false)(runningRef)();
                    return emit(v.handle)(Completed.value)();
                  }
                })();
                return start2(c)();
              })();
            };
          },
          getNodes: getNodes2(sim),
          getAlpha: read(alphaRef),
          stop: function __do4() {
            var running = read(runningRef)();
            return when15(running)(function __do5() {
              write(false)(runningRef)();
              return emit(v.handle)(Stopped.value)();
            })();
          },
          start: function __do4() {
            var running = read(runningRef)();
            return unless4(running)(function __do5() {
              reheat3(sim)();
              write(1)(alphaRef)();
              write(true)(runningRef)();
              emit(v.handle)(Started.value)();
              var oldCoord = read(coordRef)();
              stop(oldCoord)();
              var c = create5();
              write(c)(coordRef)();
              register(c)({
                tick: wasmConsumer(sim)(v.handle)(alphaRef)(config.alphaMin),
                onComplete: function __do6() {
                  write(false)(runningRef)();
                  return emit(v.handle)(Completed.value)();
                }
              })();
              return start2(c)();
            })();
          },
          reheat: function __do4() {
            reheat3(sim)();
            write(1)(alphaRef)();
            return emit(v.handle)(Started.value)();
          },
          interpolatePositions: function(v1) {
            return function(v2) {
              return function(v3) {
                return pure13(unit);
              };
            };
          },
          pinNodes: pure13(unit),
          unpinNodes: pure13(unit)
        }
      };
    };
  };
  var pureConsumer = function(sim) {
    return function(emitterHandle) {
      return function(alphaRef) {
        return function(alphaMin) {
          return function(_deltaMs) {
            return function __do3() {
              var alpha = tick(sim)();
              write(alpha)(alphaRef)();
              var nodes = getNodes(sim)();
              emit(emitterHandle)(new Tick({
                alpha,
                nodeCount: length(nodes)
              }))();
              var $17 = alpha < alphaMin;
              if ($17) {
                return Completed2.value;
              }
              ;
              return new Converged(alpha);
            };
          };
        };
      };
    };
  };
  var runPureSimulation = function(config) {
    return function __do3() {
      var v = create4();
      var simConfig = {
        alphaTarget: defaultConfig.alphaTarget,
        velocityRetention: defaultConfig.velocityRetention,
        alphaDecay: config.setup.params.alphaDecay,
        alphaMin: config.alphaMin
      };
      var sim = create(simConfig)();
      applySetupWithData(config.setup)(config.nodes)(config.links)(sim)();
      var setupRef = $$new(config.setup)();
      var alphaRef = $$new(1)();
      var coord = create5();
      var coordRef = $$new(coord)();
      var runningRef = $$new(true)();
      emit(v.handle)(Started.value)();
      register(coord)({
        tick: pureConsumer(sim)(v.handle)(alphaRef)(config.alphaMin),
        onComplete: function __do4() {
          write(false)(runningRef)();
          return emit(v.handle)(Completed.value)();
        }
      })();
      start2(coord)();
      return {
        events: v.emitter,
        handle: {
          updateData: function(nodes) {
            return function(links) {
              return function __do4() {
                var currentSetup = read(setupRef)();
                var result = applySetupWithData(currentSetup)(nodes)(links)(sim)();
                reheat(sim)();
                write(1)(alphaRef)();
                emit(v.handle)(Started.value)();
                var running = read(runningRef)();
                unless4(running)(function __do5() {
                  write(true)(runningRef)();
                  var oldCoord = read(coordRef)();
                  stop(oldCoord)();
                  var c = create5();
                  write(c)(coordRef)();
                  register(c)({
                    tick: pureConsumer(sim)(v.handle)(alphaRef)(config.alphaMin),
                    onComplete: function __do6() {
                      write(false)(runningRef)();
                      return emit(v.handle)(Completed.value)();
                    }
                  })();
                  return start2(c)();
                })();
                return result;
              };
            };
          },
          updateSetup: function(newSetup) {
            return function __do4() {
              write(newSetup)(setupRef)();
              applySetup(newSetup)(sim)();
              reheat(sim)();
              write(1)(alphaRef)();
              emit(v.handle)(Started.value)();
              var running = read(runningRef)();
              return unless4(running)(function __do5() {
                write(true)(runningRef)();
                var oldCoord = read(coordRef)();
                stop(oldCoord)();
                var c = create5();
                write(c)(coordRef)();
                register(c)({
                  tick: pureConsumer(sim)(v.handle)(alphaRef)(config.alphaMin),
                  onComplete: function __do6() {
                    write(false)(runningRef)();
                    return emit(v.handle)(Completed.value)();
                  }
                })();
                return start2(c)();
              })();
            };
          },
          getNodes: getNodes(sim),
          getAlpha: read(alphaRef),
          stop: function __do4() {
            var running = read(runningRef)();
            return when15(running)(function __do5() {
              write(false)(runningRef)();
              return emit(v.handle)(Stopped.value)();
            })();
          },
          start: function __do4() {
            var running = read(runningRef)();
            return unless4(running)(function __do5() {
              reheat(sim)();
              write(1)(alphaRef)();
              write(true)(runningRef)();
              emit(v.handle)(Started.value)();
              var oldCoord = read(coordRef)();
              stop(oldCoord)();
              var c = create5();
              write(c)(coordRef)();
              register(c)({
                tick: pureConsumer(sim)(v.handle)(alphaRef)(config.alphaMin),
                onComplete: function __do6() {
                  write(false)(runningRef)();
                  return emit(v.handle)(Completed.value)();
                }
              })();
              return start2(c)();
            })();
          },
          reheat: function __do4() {
            reheat(sim)();
            write(1)(alphaRef)();
            return emit(v.handle)(Started.value)();
          },
          interpolatePositions: function(startPos) {
            return function(targetPos) {
              return function(progress) {
                return interpolatePositionsInPlace(startPos)(targetPos)(progress)(sim);
              };
            };
          },
          pinNodes: pinNodesInPlace(sim),
          unpinNodes: unpinNodesInPlace(sim)
        }
      };
    };
  };
  var runSimulation = function(config) {
    if (config.engine instanceof Pure) {
      return runPureSimulation(config);
    }
    ;
    if (config.engine instanceof D3) {
      return runPureSimulation(config);
    }
    ;
    if (config.engine instanceof WASM) {
      return runWASMSimulation(config);
    }
    ;
    throw new Error("Failed pattern match at Hylograph.Simulation (line 239, column 24 - line 242, column 35): " + [config.engine.constructor.name]);
  };

  // output/Test.CirclePackForceDemo/index.js
  var pure14 = /* @__PURE__ */ pure(applicativeEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var log4 = /* @__PURE__ */ log3(monadEffectEffect);
  var show5 = /* @__PURE__ */ show(showInt);
  var alter3 = /* @__PURE__ */ alter(ordInt);
  var foldl5 = /* @__PURE__ */ foldl(foldableArray);
  var bind13 = /* @__PURE__ */ bind(bindEither);
  var getField3 = /* @__PURE__ */ getField2(decodeJsonInt);
  var getField1 = /* @__PURE__ */ getField2(decodeJsonString);
  var pure15 = /* @__PURE__ */ pure(applicativeEither);
  var getField22 = /* @__PURE__ */ getField2(/* @__PURE__ */ decodeForeignObject2(decodeJsonJson));
  var map30 = /* @__PURE__ */ map(functorArray);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var lookup6 = /* @__PURE__ */ lookup2(ordInt);
  var bind22 = /* @__PURE__ */ bind(bindAff);
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var discard22 = /* @__PURE__ */ discard2(bindAff);
  var log1 = /* @__PURE__ */ log3(monadEffectAff);
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var startSimulation = function(nodes) {
    return function __do3() {
      var v = runSimulation({
        engine: D3.value,
        setup: setup("circle-pack")([withStrength($$static(-30))(manyBody2("charge")), withRadius(dynamic(function(n) {
          return n.r + 5;
        }))(collide2("collision")), withY($$static(0))(withX($$static(0))(center2("center")))]),
        nodes,
        links: [],
        container: "#packages-group",
        alphaMin: 1e-3
      })();
      subscribe(v.events)(function(event) {
        if (event instanceof Tick) {
          return pure14(unit);
        }
        ;
        if (event instanceof Completed) {
          return function __do4() {
            log4("[CirclePackDemo] Simulation converged")();
            var finalNodes = v.handle.getNodes();
            return log4("[CirclePackDemo] Final node positions: " + (show5(length(finalNodes)) + " nodes"))();
          };
        }
        ;
        if (event instanceof Started) {
          return log4("[CirclePackDemo] Simulation started");
        }
        ;
        return pure14(unit);
      })();
      return unit;
    };
  };
  var groupModulesByPackage = function(modules) {
    var addModule = function(acc) {
      return function(v) {
        return alter3((function() {
          var $105 = cons(v);
          var $106 = fromMaybe([]);
          return function($107) {
            return Just.create($105($106($107)));
          };
        })())(v["package"].id)(acc);
      };
    };
    return foldl5(addModule)(empty3)(modules);
  };
  var decodeAPIPackage = {
    decodeJson: function(json2) {
      var note2 = function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return new Left(new TypeMismatch2(v));
          }
          ;
          if (v1 instanceof Just) {
            return new Right(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 106, column 5 - line 106, column 52): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return bind13(note2("Expected object")(toObject(json2)))(function(obj) {
        return bind13(getField3(obj)("id"))(function(id2) {
          return bind13(getField1(obj)("name"))(function(name3) {
            return bind13(getField1(obj)("source"))(function(source) {
              return bind13(getField3(obj)("moduleCount"))(function(moduleCount) {
                return pure15({
                  id: id2,
                  name: name3,
                  source,
                  moduleCount
                });
              });
            });
          });
        });
      });
    }
  };
  var getField32 = /* @__PURE__ */ getField2(/* @__PURE__ */ decodeArray2(decodeAPIPackage));
  var decodePackagesResponse = {
    decodeJson: function(json2) {
      var note2 = function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return new Left(new TypeMismatch2(v));
          }
          ;
          if (v1 instanceof Just) {
            return new Right(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 85, column 5 - line 85, column 52): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return bind13(note2("Expected object")(toObject(json2)))(function(obj) {
        return bind13(getField32(obj)("packages"))(function(packages) {
          return pure15({
            packages
          });
        });
      });
    }
  };
  var decodeJson2 = /* @__PURE__ */ decodeJson(decodePackagesResponse);
  var decodeAPIModule = {
    decodeJson: function(json2) {
      var note2 = function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return new Left(new TypeMismatch2(v));
          }
          ;
          if (v1 instanceof Just) {
            return new Right(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 119, column 5 - line 119, column 52): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return bind13(note2("Expected object")(toObject(json2)))(function(obj) {
        return bind13(getField3(obj)("id"))(function(id2) {
          return bind13(getField1(obj)("name"))(function(name3) {
            return bind13(getField22(obj)("package"))(function(pkgObj) {
              return bind13(getField3(pkgObj)("id"))(function(pkgId) {
                return bind13(getField1(pkgObj)("name"))(function(pkgName) {
                  return pure15({
                    id: id2,
                    name: name3,
                    "package": {
                      id: pkgId,
                      name: pkgName
                    }
                  });
                });
              });
            });
          });
        });
      });
    }
  };
  var getField4 = /* @__PURE__ */ getField2(/* @__PURE__ */ decodeArray2(decodeAPIModule));
  var decodeModulesResponse = {
    decodeJson: function(json2) {
      var note2 = function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return new Left(new TypeMismatch2(v));
          }
          ;
          if (v1 instanceof Just) {
            return new Right(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 94, column 5 - line 94, column 52): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return bind13(note2("Expected object")(toObject(json2)))(function(obj) {
        return bind13(getField4(obj)("modules"))(function(modules) {
          return pure15({
            modules
          });
        });
      });
    }
  };
  var decodeJson1 = /* @__PURE__ */ decodeJson(decodeModulesResponse);
  var createPackedPackage = function(idx) {
    return function(v) {
      return function(modules) {
        var toModuleCircle = function(v1) {
          return function(c) {
            return {
              id: v1.id,
              name: v1.name,
              x: c.x,
              y: c.y,
              r: c.r
            };
          };
        };
        var moduleCircles = map30(function(v1) {
          return {
            x: 0,
            y: 0,
            r: 5 + sqrt(toNumber(length4(v1.name))) * 1.5
          };
        })(modules);
        var packed = packSiblingsMap(moduleCircles);
        var packedModules = zipWith(toModuleCircle)(modules)(packed.circles);
        var r = packed.radius + 10;
        var row = div2(idx)(8);
        var y = (toNumber(row) - 3) * 120;
        var col = mod3(idx)(8);
        var x = (toNumber(col) - 4) * 120;
        return {
          id: v.id,
          x,
          y,
          vx: 0,
          vy: 0,
          fx: nullImpl,
          fy: nullImpl,
          name: v.name,
          source: v.source,
          moduleCount: v.moduleCount,
          r,
          modules: packedModules
        };
      };
    };
  };
  var preparePackages = function(packages) {
    return function(modules) {
      var packagesWithModules = take(40)(sortBy(function(v) {
        return function(v1) {
          return compare2(v1.moduleCount)(v.moduleCount);
        };
      })(filter(function(v) {
        return v.moduleCount > 0;
      })(packages)));
      var modulesByPkg = groupModulesByPackage(modules);
      return mapWithIndex2(function(idx) {
        return function(v) {
          var pkgModules = fromMaybe([])(lookup6(v.id)(modulesByPkg));
          return createPackedPackage(idx)(v)(pkgModules);
        };
      })(packagesWithModules);
    };
  };
  var apiBaseUrl = "http://localhost:3000";
  var fetchModules = /* @__PURE__ */ (function() {
    return bind22(get2(json)(apiBaseUrl + "/api/v2/modules"))(function(result) {
      return pure22((function() {
        if (result instanceof Left) {
          return new Left(printError(result.value0));
        }
        ;
        if (result instanceof Right) {
          var v = decodeJson1(result.value0.body);
          if (v instanceof Left) {
            return new Left(printJsonDecodeError(v.value0));
          }
          ;
          if (v instanceof Right) {
            return new Right(v.value0.modules);
          }
          ;
          throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 166, column 23 - line 168, column 59): " + [v.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 164, column 10 - line 168, column 59): " + [result.constructor.name]);
      })());
    });
  })();
  var fetchPackages = /* @__PURE__ */ (function() {
    return bind22(get2(json)(apiBaseUrl + "/api/v2/packages"))(function(result) {
      return pure22((function() {
        if (result instanceof Left) {
          return new Left(printError(result.value0));
        }
        ;
        if (result instanceof Right) {
          var v = decodeJson2(result.value0.body);
          if (v instanceof Left) {
            return new Left(printJsonDecodeError(v.value0));
          }
          ;
          if (v instanceof Right) {
            return new Right(v.value0.packages);
          }
          ;
          throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 157, column 23 - line 159, column 62): " + [v.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 155, column 10 - line 159, column 62): " + [result.constructor.name]);
      })());
    });
  })();
  var main = /* @__PURE__ */ launchAff_(/* @__PURE__ */ discard22(/* @__PURE__ */ log1("[CirclePackDemo] Starting..."))(function() {
    return discard22(log1("[CirclePackDemo] Fetching packages..."))(function() {
      return bind22(fetchPackages)(function(packagesResult) {
        return discard22(log1("[CirclePackDemo] Fetching modules..."))(function() {
          return bind22(fetchModules)(function(modulesResult) {
            if (packagesResult instanceof Right && modulesResult instanceof Right) {
              return discard22(log1("[CirclePackDemo] Loaded " + (show5(length(packagesResult.value0)) + (" packages, " + (show5(length(modulesResult.value0)) + " modules")))))(function() {
                var nodes = preparePackages(packagesResult.value0)(modulesResult.value0);
                return discard22(log1("[CirclePackDemo] Prepared " + (show5(length(nodes)) + " package nodes")))(function() {
                  return liftEffect2(startSimulation(nodes));
                });
              });
            }
            ;
            if (packagesResult instanceof Left) {
              return log1("[CirclePackDemo] Failed to fetch packages: " + packagesResult.value0);
            }
            ;
            if (modulesResult instanceof Left) {
              return log1("[CirclePackDemo] Failed to fetch modules: " + modulesResult.value0);
            }
            ;
            throw new Error("Failed pattern match at Test.CirclePackForceDemo (line 297, column 3 - line 310, column 77): " + [packagesResult.constructor.name, modulesResult.constructor.name]);
          });
        });
      });
    });
  }));

  // <stdin>
  main();
})();

module.exports = __ob_eval__ = function (path, eoe, outputPath) {
  result = eval(require('fs').readFileSync(path, { encoding: 'utf8' }));
  stringify = function (thingToStringify, isFunctionFull, maxLengthForStrings) {
    class Serialize {
      constructor({ thing, verboseFunctions, maxStringLength }) {
        this.seen = [];
        this.thing = thing;
        this.verboseFunctions = verboseFunctions;
        this.maxStringLength = maxStringLength;
        this.circular = JSON.stringify('#<Circular>');

        this.isFunction = this.isFunction.bind(this);
        this.isString = this.isString.bind(this);
        this.isArray = this.isArray.bind(this);
        this.isBoolean = this.isBoolean.bind(this);
        this.isNumber = this.isNumber.bind(this);
        this.isWindow = this.isWindow.bind(this);
        this.isWindowNode = this.isWindowNode.bind(this);
        this.isDate = this.isDate.bind(this);
        this.tryStringify = this.tryStringify.bind(this);
        this.annotateFunction = this.annotateFunction.bind(this);
        this.stringify = this.stringify.bind(this);
        this.serialize = this.serialize.bind(this);
        this.tryStorage = this.tryStorage.bind(this);
        this.annotateFn2 = this.annotateFn2.bind(this);
      }

      static makeSerialize(params) {
        return new Serialize(params);
      }

      seen;
      thing;
      maxStringLength;
      verboseFunctions;
      circular = JSON.stringify('#<Circular>');

      isFunction(v) {
        return v instanceof Function || typeof v === 'function';
      }

      removeComments(str) {
        return str.replace(/\/\*[\s\S]*?\*\/|\/\/.*/g, '').trim();
      }

      isString(v) {
        return typeof v === 'string';
      }

      isArray(v) {
        return v instanceof Array || Array.isArray(v);
      }

      isNumber(v) {
        return typeof v === 'number';
      }

      isBoolean(v) {
        return typeof v === 'boolean';
      }

      isWindow(it) {
        return globalThis.window && globalThis.window === it;
      }

      isWindowNode(v) {
        return (
          globalThis.window &&
          globalThis.window.Node != null &&
          v instanceof globalThis.window.Node
        );
      }

      isDate(v) {
        return Object.prototype.toString.call(v) === '[object Date]';
      }
      annotateFunction(str) {
        let parts = this.removeComments(str.toString()).split('').reverse();
        let processed = [];
        let curr;
        let bracketsOpen = 0;
        let bracketsClosed = 0;
        let openCount = 0;
        let closedCount = 0;
        let result;

        while ((curr = !result && parts.pop())) {
          if (curr === '(') {
            openCount += 1;
          } else if (curr === ')') {
            closedCount += 1;
          }
          if (openCount > 0) {
            processed.push(curr);
            if (curr === '{') {
              bracketsOpen += 1;
            } else if (curr === '}') {
              bracketsClosed += 1;
            }
          }
          result =
            result ||
            (bracketsOpen === bracketsClosed &&
              openCount === closedCount &&
              openCount > 0)
              ? processed.join('')
              : undefined;
        }

        return result
          ? 'function'.concat(result).concat('{}')
          : this.annotateFn2(str);
      }

      annotateFn2(fn) {
        const name = fn.name || fn.toString() || '';
        const len = fn.length || 0;
        let idx = 0;
        let list;
        list = new Array(len);
        while (idx < len) {
          list[idx] = `arg${idx}`;
          idx += 1;
        }
        return 'function ' + name + '(' + list.join(', ') + ') {}';
      }

      stringify(obj) {
        if (this.isBoolean(obj)) {
          return obj.toString();
        } else if (obj === undefined) {
          return 'undefined';
        } else if (obj === null) {
          return 'null';
        } else if (this.isNumber(obj)) {
          return obj.toString();
        } else if (this.isString(obj)) {
          return this.maxStringLength && obj.length > this.maxStringLength
            ? this.tryStringify(obj.substring(0, 100).concat('...'))
            : JSON.stringify(obj);
        } else if (this.isWindowNode(obj)) {
          return this.tryStringify(obj);
        } else if (this.isFunction(obj)) {
          return this.verboseFunctions
            ? obj.toString().replace(/{\s\[native code\]\s}/g, '{}')
            : this.annotateFunction(obj);
        } else if (this.isDate(obj)) {
          return this.tryStringify(obj);
        } else if (this.isArray(obj)) {
          if (this.seen.indexOf(obj) >= 0) {
            return this.circular;
          } else {
            this.seen.push(obj);
            return `[${obj.map(this.serialize).join(', ')}]`;
          }
        } else {
          if (this.seen.indexOf(obj) >= 0) {
            return this.circular;
          } else {
            this.seen.push(obj);

            const pairs = [];

            for (let key in obj) {
              if (
                obj &&
                obj.hasOwnProperty &&
                this.isFunction(obj.hasOwnProperty) &&
                obj.hasOwnProperty(key)
              ) {
                let pair = this.stringify(key) + ': ';
                pair += this.serialize(obj[key]);
                pairs.push(pair);
              }
            }
            return `{ ${pairs.join(', ')} }`;
          }
        }
      }

      tryStorage(storage, storageType) {
        return this.stringify({});
      }

      serialize(...args) {
        const it = args.length > 0 ? args[0] : this.thing;
        if (!this.isWindow(it)) {
          return this.stringify(it);
        }
        if (this.seen.indexOf(it) >= 0) {
          return this.circular;
        } else {
          this.seen.push(it);
        }

        const storageMock = {
          sessionStorage: this.tryStorage,
          localStorage: this.tryStorage,
        };

        const res = Object.keys(it).reduce((pairs, key) => {
          const value = storageMock[key]
            ? storageMock[key]()
            : this.isWindow(it[key])
            ? this.circular
            : this.stringify(it[key]);

          pairs.push(`${this.tryStringify(key)}: ${value}`);

          return pairs;
        }, []);
        return `{ ${res.join(', ')} }`;
      }

      tryStringify(it) {
        let result;
        try {
          result = JSON.stringify(it);
        } catch (error) {
          result = JSON.stringify(error.message || error);
        }
        return result;
      }
    }

    return Serialize.makeSerialize({
      thing: thingToStringify,
      verboseFunctions: isFunctionFull,
      maxStringLength: maxLengthForStrings,
    }).serialize();
  };
  write = function (obj) {
    if (outputPath) {
      if (obj instanceof Buffer) {
        require('fs').writeFileSync(outputPath, obj);
      } else if (obj && 'function' === typeof obj.pipe) {
        obj.pipe(require('fs').createWriteStream(outputPath));
      }
    } else {
      console.log(stringify(obj));
    }
    process.stdout.write(eoe);
  };
  if (result && 'function' === typeof result.then) {
    result.then(write, write);
  } else {
    write(result);
  }
};

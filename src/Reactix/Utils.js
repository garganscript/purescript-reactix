'use strict';
exports._tuple = function tuple(ctor, v) { return ctor(v[0])(v[1]); };


const zlib = require('zlib');

exports.createGzip = () => zlib.createGzip();

exports.createGunzip = () => zlib.createGunzip();

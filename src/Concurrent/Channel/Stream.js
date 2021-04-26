exports.isWritable = (s) => () => s.writable;

exports.isReadable = (s) => () => s.readable;

exports.readableLength = (s) => () => s.readableLength;

exports.onceDrain = (s) => (f) => () => s.once("drain", f);

exports.onceReadable = (s) => (f) => () => s.once("readable", f);

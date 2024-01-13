const std = @import("std");
const fs = std.fs;

const termios = @import("term/termios.zig");
const console = @import("term/console.zig");

const builtin = @import("builtin");

pub const Screen = struct { row: usize, col: usize };

pub const Term = switch (builtin.os.tag) {
    .linux => termios.Termios,
    .windows => console.Console,
    inline else => |tag| @compileError(@tagName(tag) ++ " port not supported yet."),
};

pub fn term(stdin: *fs.File, stdout: *fs.File, writer: anytype, reader: std.io.AnyReader) !Term(@TypeOf(writer)) {
    return try Term(fs.File.Writer).init(stdin, stdout, reader, writer);
}

test {
    switch (builtin.os.tag) {
        .linux => _ = @import("term/termios.zig"),
        .windows => _ = @import("term/console.zig"),
        else => {},
    }
}

const std = @import("std");
const os = std.os;
const fs = std.fs;

const KeyResult = enum {
    CtrlQPressed,
    Default,
};

const Screen = struct { row: u16, col: u16 };

pub fn Termios(comptime WriterType: anytype) type {
    return struct {
        stdin: *fs.File,
        stdout: *fs.File,
        writer: WriterType,
        reader: std.io.AnyReader,
        termios: os.termios,

        pub fn init(stdin: *fs.File, stdout: *fs.File, reader: std.io.AnyReader, writer: WriterType) !@This() {
            return .{
                .stdin = stdin,
                .stdout = stdout,
                .termios = try os.tcgetattr(stdin.handle),
                .reader = reader,
                .writer = writer,
            };
        }

        pub fn enableRawMode(self: *@This()) !void {
            var raw = self.termios;

            raw.iflag &= ~@as(os.system.tcflag_t, os.system.BRKINT | os.system.ICRNL | os.system.INPCK | os.system.ISTRIP | os.system.IXON);
            raw.oflag &= ~@as(os.system.tcflag_t, os.system.OPOST);
            raw.cflag |= @as(os.system.tcflag_t, os.system.CS8);
            raw.lflag &= ~@as(os.system.tcflag_t, os.system.ECHO | os.system.ICANON | os.system.IEXTEN | os.system.ISIG);
            raw.cc[os.system.V.MIN] = 0;
            raw.cc[os.system.V.TIME] = 1;

            try os.tcsetattr(self.stdin.handle, .FLUSH, raw);
        }

        pub fn disableRawMode(self: *@This()) !void {
            try os.tcsetattr(self.stdin.handle, .FLUSH, self.termios);
        }

        ///
        /// yoinked from https://github.com/ziglang/zig/blob/4129996211edd30b25c23454520fd78b2a70394b/lib/std/os.zig#L3289
        ///
        pub fn getWindowSize(self: *@This()) !Screen {
            var wsz: os.linux.winsize = undefined;

            const rc = os.linux.syscall3(
                .ioctl,
                @as(usize, @bitCast(@as(isize, self.stdout.handle))),
                os.linux.T.IOCGWINSZ,
                @intFromPtr(&wsz),
            );

            return if (os.linux.getErrno(rc) != .SUCCESS or wsz.ws_col == 0) blk: {
                break :blk if (try self.writer.write("\x1b[999C\x1b[999B") == 12) try self.getCursorPosition() else error.WinSizeSyscallError;
            } else .{ .row = wsz.ws_row, .col = wsz.ws_col };
        }

        fn getCursorPosition(self: *@This()) !Screen {
            if (try self.writer.write("\x1b[6n") != 4) return error.getCursorError;

            var buf: [32]u8 = undefined;
            var buffered_stream = std.io.fixedBufferStream(&buf);
            try self.reader.skipBytes(2, .{});
            try self.reader.streamUntilDelimiter(buffered_stream.writer(), 'R', null);

            // if (buf[0] != '\x1b' or buf[1] != '[') return error.UnexpectedScreenDimFormat;

            var parts = std.mem.splitScalar(u8, buf[0..try buffered_stream.getPos()], ';');

            return .{
                .row = try std.fmt.parseInt(u16, parts.next().?, 10),
                .col = try std.fmt.parseInt(u16, parts.next().?, 10),
            };
        }
    };
}

pub fn termios(stdin: *fs.File, stdout: *fs.File, writer: anytype, reader: std.io.AnyReader) !Termios(@TypeOf(writer)) {
    return try Termios(fs.File.Writer).init(stdin, stdout, reader, writer);
}

pub fn kilo(
    writer: anytype,
    reader: std.io.AnyReader,
    screen_size: Screen,
) Editor(@TypeOf(writer)) {
    return .{ .writer = writer, .reader = reader, .screen_size = screen_size };
}

pub fn Editor(comptime WriterType: anytype) type {
    return struct {
        writer: WriterType,
        reader: std.io.AnyReader,
        screen_size: Screen,
        buffer: [1]u8 = .{0},

        pub fn readKey(self: *@This()) !u8 {
            self.buffer[0] = 0;
            _ = try self.reader.read(self.buffer[0..1]);
            const c = self.buffer[0];

            if (c != 0) {
                if (std.ascii.isControl(c)) {
                    try self.writer.print("{d}\r\n", .{c});
                } else {
                    try self.writer.print("{d} ('{c}')\r\n", .{ c, c });
                }
            }

            return c;
        }

        pub fn processKeyPress(self: *@This()) !KeyResult {
            switch (try self.readKey()) {
                std.ascii.control_code.xon => return .CtrlQPressed,
                else => return .Default,
            }
        }

        pub fn refreshScreen(self: *@This()) !void {
            _ = try self.writer.write("\x1b[2J");
            _ = try self.writer.write("\x1b[H");

            try self.drawRows();

            _ = try self.writer.write("\x1b[H");
        }

        pub fn drawRows(self: *@This()) !void {
            for (0..self.screen_size.row) |y| {
                _ = try self.writer.write("~");

                if (y < self.screen_size.row - 1) {
                    _ = try self.writer.write("\r\n");
                }
            }
        }
    };
}

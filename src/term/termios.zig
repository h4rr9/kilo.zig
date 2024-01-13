const std = @import("std");
const fs = std.fs;
const os = std.os;
const term = @import("../term.zig");
const Screen = term.Screen;

/// Some code taken from - https://github.com/microsoft/terminal/issues/8820
pub fn Termios(comptime WriterType: anytype) type {
    return struct {
        stdin: *fs.File,
        stdout: *fs.File,
        writer: WriterType,
        reader: std.io.AnyReader,
        termios: os.linux.termios,

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

            raw.iflag &= ~@as(os.linux.tcflag_t, os.linux.BRKINT | os.linux.ICRNL | os.linux.INPCK | os.linux.ISTRIP | os.linux.IXON);
            raw.oflag &= ~@as(os.linux.tcflag_t, os.linux.OPOST);
            raw.cflag |= @as(os.linux.tcflag_t, os.linux.CS8);
            raw.lflag &= ~@as(os.linux.tcflag_t, os.linux.ECHO | os.linux.ICANON | os.linux.IEXTEN | os.linux.ISIG);
            raw.cc[os.linux.V.MIN] = 0;
            raw.cc[os.linux.V.TIME] = 1;

            try os.tcsetattr(self.stdin.handle, .FLUSH, raw);
        }

        pub fn disableRawMode(self: *@This()) !void {
            _ = try self.writer.write("\x1b[0m");
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
            // try self.reader.skipBytes(2, .{});
            try self.reader.streamUntilDelimiter(buffered_stream.writer(), 'R', null);

            if (buf[0] != '\x1b' or buf[1] != '[') return error.UnexpectedScreenDimFormat;

            var parts = std.mem.splitScalar(u8, buf[0..try buffered_stream.getPos()], ';');

            return .{
                .row = try std.fmt.parseInt(u16, parts.next().?, 10),
                .col = try std.fmt.parseInt(u16, parts.next().?, 10),
            };
        }
    };
}

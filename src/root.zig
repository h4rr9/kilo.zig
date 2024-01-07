const std = @import("std");
const os = std.os;
const fs = std.fs;

const KILO_VERSION = "0.0.1";
const KILO_TAB_STOP = 8;
const KILO_STATUS_MESSAGE_DURATION = 5;

const KeyResult = enum {
    CtrlQPressed,
    Default,
};

const EditorKey = enum {
    ARROW_LEFT,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    HOME_KEY,
    END_KEY,
    DEL_KEY,
    PAGE_UP,
    PAGE_DOWN,
};

const Key = union(enum) {
    key: u8,
    editor_key: EditorKey,
    exit,
};

const Screen = struct { row: usize, col: usize };
const ERow = struct {
    chars: []u8,
    render: []u8,

    fn renderCol(row: *ERow, cx: usize) usize {
        var rx: usize = 0;
        return for (row.chars, 0..) |c, j| {
            if (j >= cx) break rx;
            if (c == '\t') rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
            rx += 1;
        } else rx;
    }
};

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

            raw.iflag &= ~@as(os.linux.tcflag_t, os.linux.BRKINT | os.linux.ICRNL | os.linux.INPCK | os.linux.ISTRIP | os.linux.IXON);
            raw.oflag &= ~@as(os.linux.tcflag_t, os.linux.OPOST);
            raw.cflag |= @as(os.linux.tcflag_t, os.linux.CS8);
            raw.lflag &= ~@as(os.linux.tcflag_t, os.linux.ECHO | os.linux.ICANON | os.linux.IEXTEN | os.linux.ISIG);
            raw.cc[os.linux.V.MIN] = 0;
            raw.cc[os.linux.V.TIME] = 1;

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

pub fn termios(stdin: *fs.File, stdout: *fs.File, writer: anytype, reader: std.io.AnyReader) !Termios(@TypeOf(writer)) {
    return try Termios(fs.File.Writer).init(stdin, stdout, reader, writer);
}

pub fn kilo(
    writer: anytype,
    reader: std.io.AnyReader,
    screen: Screen,
    alloc: std.mem.Allocator,
) Editor(@TypeOf(writer)) {
    return std.mem.zeroInit(Editor(@TypeOf(writer)), .{
        .writer = writer,
        .reader = reader,
        .screen = .{
            .row = screen.row -| 2,
            .col = screen.col,
        }, // status bar and message
        .alloc = alloc,
        .rows = std.ArrayList(ERow).init(alloc),
    });
}

pub fn Editor(comptime WriterType: anytype) type {
    return struct {
        writer: WriterType,
        reader: std.io.AnyReader,
        alloc: std.mem.Allocator,

        file_name: ?[]u8,
        status_msg: [80]u8,
        status: []u8,
        status_msg_time: i64,

        screen: Screen,
        rows: std.ArrayList(ERow),
        cursor: Screen,
        row_offset: usize,
        col_offset: usize,
        render_col: usize,

        pub fn open(self: *@This(), file_name: []const u8) !void {
            self.file_name = try self.alloc.dupe(u8, file_name);
            errdefer self.alloc.free(self.file_name.?);

            const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
            const file_reader = file.reader().any();

            var line = std.ArrayList(u8).init(self.alloc);
            errdefer line.deinit();

            const writer = line.writer();

            while (file_reader.streamUntilDelimiter(writer, '\n', null)) {
                try self.appendRow(try line.toOwnedSlice());
            } else |err| switch (err) {
                error.EndOfStream => {},
                else => |e| return e,
            }
        }

        pub fn close(self: *@This()) void {
            for (self.rows.items) |*item| {
                self.alloc.free(item.chars);
                self.alloc.free(item.render);
            }
            self.rows.deinit();
            if (self.file_name) |f| self.alloc.free(f);
        }

        pub fn processKeyPress(self: *@This()) !KeyResult {
            switch (try self.readKey()) {
                .exit => {
                    _ = try self.writer.write("\x1b[2J");
                    _ = try self.writer.write("\x1b[H");

                    return .CtrlQPressed;
                },
                .editor_key => |c| switch (c) {
                    .ARROW_UP, .ARROW_DOWN, .ARROW_LEFT, .ARROW_RIGHT => |a_key| self.moveCursor(a_key),
                    .PAGE_UP, .PAGE_DOWN => |p_key| {
                        self.cursor.row = switch (p_key) {
                            .PAGE_UP => self.row_offset,
                            .PAGE_DOWN => @min(self.row_offset + self.screen.row - 1, self.rows.items.len),
                            else => unreachable,
                        };

                        var times: u16 = 0;
                        const key: EditorKey = if (p_key == .PAGE_UP) .ARROW_UP else .ARROW_DOWN;
                        while (times < self.screen.row) : (times += 1)
                            self.moveCursor(key);
                    },
                    .HOME_KEY => self.cursor.col = 0,
                    .END_KEY => if (self.cursor.row < self.rows.items.len) {
                        self.cursor.col = self.rows.items[self.cursor.row].render.len;
                    },
                    else => {},
                },
                else => {},
            }

            return .Default;
        }

        pub fn refreshScreen(self: *@This()) !void {
            self.scroll();

            var abuf: Abuf = .{ .b = try self.alloc.alloc(u8, 0) };
            defer self.alloc.free(abuf.b);

            // hide cursor
            // and
            // move cursor to top left
            try abuf.append("\x1b[?25l", self.alloc);
            try abuf.append("\x1b[H", self.alloc);

            try self.drawRows(&abuf);
            try self.drawStatusBar(&abuf);
            try self.drawStatusMessageBar(&abuf);

            // put cursor at position
            const slice = try std.fmt.allocPrint(
                self.alloc,
                "\x1b[{d};{d}H",
                .{
                    self.cursor.row - self.row_offset + 1,
                    self.render_col - self.col_offset + 1,
                },
            );
            defer self.alloc.free(slice);
            try abuf.append(slice, self.alloc);

            // show cursor
            try abuf.append("\x1b[?25h", self.alloc);

            _ = try self.writer.write(abuf.b);
        }

        fn drawStatusMessageBar(self: *@This(), abuf: *Abuf) !void {
            try abuf.append("\x1b[K", self.alloc);

            const msglen = @min(self.status.len, self.screen.col);

            if (msglen > 0 and std.time.timestamp() - self.status_msg_time < KILO_STATUS_MESSAGE_DURATION)
                try abuf.append(self.status[0..msglen], self.alloc);
        }

        fn drawStatusBar(self: *@This(), abuf: *Abuf) !void {
            try abuf.append("\x1b[7m", self.alloc);

            const status = try std.fmt.allocPrint(self.alloc, "{s} - {d} lines", .{
                self.file_name orelse "[No Name]",
                self.rows.items.len,
            });
            defer self.alloc.free(status);
            const rstatus = try std.fmt.allocPrint(self.alloc, "{d}/{d}", .{
                self.cursor.row + 1,
                self.rows.items.len,
            });
            defer self.alloc.free(rstatus);

            const len = @min(self.screen.col, status.len);
            try abuf.append(status[0..len], self.alloc);
            try abuf.appendN(" ", self.screen.col -| (len + rstatus.len + 1), self.alloc);
            try abuf.append(rstatus, self.alloc);

            try abuf.append("\x1b[m\r\n", self.alloc);
        }

        pub fn setStatusMessage(self: *@This(), status: []const u8) void {
            const len = @min(self.status_msg.len, status.len);
            std.mem.copyForwards(u8, &self.status_msg, status[0..len]);
            self.status = &self.status_msg;
            self.status.len = len;
            self.status_msg_time = std.time.timestamp();
        }

        fn appendRow(self: *@This(), s: []u8) !void {
            const count_tabs = std.mem.count(u8, s, "\t");

            const render = try self.alloc.alloc(u8, s.len + (KILO_TAB_STOP - 1) * count_tabs);
            errdefer self.alloc.free(render);

            var idx: usize = 0;
            for (s) |c|
                if (c == '\t') {
                    render[idx] = ' ';
                    idx += 1;
                    while (idx % KILO_TAB_STOP != 0) : (idx += 1) render[idx] = ' ';
                } else {
                    render[idx] = c;
                    idx += 1;
                };

            // REVIEW append better ?
            const row = try self.rows.addOne();
            row.* = .{ .chars = s, .render = render };
        }

        fn readKey(self: *@This()) !Key {
            var buffer: [1]u8 = undefined;
            var seq: [3]u8 = undefined;
            while (try self.reader.read(&buffer) != 1) std.atomic.spinLoopHint();
            const c: u8 = buffer[0];

            if (c == std.ascii.control_code.xon) return .exit;

            if (c == '\x1b') {
                _ = self.reader.read(seq[0..1]) catch return .{ .key = c };
                _ = self.reader.read(seq[1..2]) catch return .{ .key = c };

                if (seq[0] == '[')
                    if (seq[1] >= '0' and seq[1] <= '9') {
                        _ = self.reader.read(seq[2..3]) catch return .{ .key = c };
                        if (seq[2] == '~')
                            return switch (seq[1]) {
                                '1', '7' => .{ .editor_key = .HOME_KEY },
                                '4', '8' => .{ .editor_key = .END_KEY },
                                '3' => .{ .editor_key = .DEL_KEY },
                                '5' => .{ .editor_key = .PAGE_UP },
                                '6' => .{ .editor_key = .PAGE_DOWN },
                                else => .{ .key = c },
                            };
                    } else return switch (seq[1]) {
                        'A' => .{ .editor_key = .ARROW_UP },
                        'B' => .{ .editor_key = .ARROW_DOWN },
                        'C' => .{ .editor_key = .ARROW_RIGHT },
                        'D' => .{ .editor_key = .ARROW_LEFT },
                        'H' => .{ .editor_key = .HOME_KEY },
                        'F' => .{ .editor_key = .END_KEY },
                        else => .{ .key = c },
                    }
                else if (seq[0] == 'O')
                    return switch (seq[1]) {
                        'H' => .{ .editor_key = .HOME_KEY },
                        'F' => .{ .editor_key = .END_KEY },
                        else => .{ .key = c },
                    };
            }
            return .{ .key = c };
        }

        fn drawRows(self: *@This(), abuf: *Abuf) !void {
            const banner = try std.fmt.allocPrint(self.alloc, "Kilo editor -- version {s}", .{KILO_VERSION});
            defer self.alloc.free(banner);

            for (0..self.screen.row) |y| {
                const file_row: usize = y + self.row_offset;
                if (file_row >= self.rows.items.len) {
                    if (self.rows.items.len == 0 and y == self.screen.row / 3) {
                        const banner_len = @min(banner.len, self.screen.col);

                        const padding = (self.screen.col - banner_len) / 2;

                        var i: usize = 0;
                        while (i < padding) : (i += 1) {
                            try if (i == 0)
                                abuf.append("~", self.alloc)
                            else
                                abuf.append(" ", self.alloc);
                        }

                        try abuf.append(banner[0..banner_len], self.alloc);
                    } else try abuf.append("~", self.alloc);
                } else {
                    const row = &self.rows.items[file_row];
                    if (row.render.len > self.col_offset) {
                        const len = @min(row.render.len - self.col_offset, self.screen.col);
                        try abuf.append(row.render[self.col_offset .. self.col_offset + len], self.alloc);
                    }
                }

                // erase part of current line
                try abuf.append("\x1b[K", self.alloc);

                try abuf.append("\r\n", self.alloc);
            }
        }

        fn scroll(self: *@This()) void {
            self.render_col = if (self.cursor.row < self.rows.items.len) blk: {
                const row = &self.rows.items[self.cursor.row];
                break :blk row.renderCol(self.cursor.col);
            } else 0;

            if (self.cursor.row < self.row_offset) {
                self.row_offset = self.cursor.row;
            } else if (self.cursor.row >= self.row_offset + self.screen.row) {
                self.row_offset = self.cursor.row - self.screen.row + 1;
            }
            if (self.render_col < self.col_offset) {
                self.col_offset = self.render_col;
            } else if (self.render_col >= self.col_offset + self.screen.col) {
                self.col_offset = self.render_col - self.screen.col + 1;
            }
        }

        fn moveCursor(self: *@This(), key: EditorKey) void {
            var row: ?*ERow = if (self.cursor.row >= self.rows.items.len) null else &self.rows.items[self.cursor.row];

            switch (key) {
                .ARROW_LEFT => if (self.cursor.col != 0) {
                    self.cursor.col -|= 1;
                } else if (self.cursor.row != 0) {
                    self.cursor.row -|= 1;
                    self.cursor.col = self.rows.items[self.cursor.row].chars.len;
                },
                .ARROW_RIGHT => if (row) |r| {
                    if (self.cursor.col < r.chars.len) {
                        self.cursor.col += 1;
                    } else if (self.cursor.col == r.chars.len) {
                        self.cursor.row += 1;
                        self.cursor.col = 0;
                    }
                },
                .ARROW_UP => self.cursor.row -|= 1,
                .ARROW_DOWN => if (self.cursor.row < self.rows.items.len) {
                    self.cursor.row += 1;
                },
                else => unreachable,
            }

            // row could have changed
            row = if (self.cursor.row >= self.rows.items.len) null else &self.rows.items[self.cursor.row];

            const row_len = if (row) |r| r.chars.len else 0;
            if (self.cursor.col > row_len) {
                self.cursor.col = row_len;
            }
        }
    };
}

const Abuf = struct {
    b: []u8,

    fn appendN(ab: *Abuf, s: []const u8, n: usize, alloc: std.mem.Allocator) !void {
        const new_len = s.len * n;
        const s_ntimes = try alloc.alloc(u8, new_len);
        defer alloc.free(s_ntimes);

        var offset: usize = 0;
        while (offset < s.len * n) : (offset += s.len)
            @memcpy(s_ntimes[offset .. offset + s.len], s);

        try ab.append(s_ntimes, alloc);
    }

    fn append(ab: *Abuf, s: []const u8, alloc: std.mem.Allocator) !void {

        // resize before realloc
        if (alloc.resize(ab.b, ab.b.len + s.len)) {
            ab.b.len += s.len;
            @memcpy(ab.b[ab.b.len - s.len ..], s);
            return;
        }

        var new = try alloc.realloc(ab.b, ab.b.len + s.len);
        @memcpy(new[ab.b.len..], s);
        ab.b = new;
    }
};

const std = @import("std");
const os = std.os;
const fs = std.fs;
const builtin = @import("builtin");
const term = @import("term.zig");
const Screen = term.Screen;
// from kilo
const KILO_VERSION = "0.0.1";
const KILO_TAB_STOP = 8;
const KILO_STATUS_MESSAGE_DURATION = 5;
const KILO_QUIT_TIMES = 3;

const KeyResult = enum {
    CtrlQPressed,
    Default,
};

const Highlight = enum {
    number,
    normal,
    match,

    fn toColor(hl: *const Highlight) usize {
        return switch (hl.*) {
            .number => 31,
            .match => 34,
            else => 37,
        };
    }
};

const EditorKey = enum {
    arrow_left,
    arrow_right,
    arrow_up,
    arrow_down,
    home,
    end,
    delete,
    page_up,
    page_down,
    backspace,
    enter,
    escape,
    save,
    find,
    exit,
};

const Key = union(enum) {
    key: u8,
    editor_key: EditorKey,
};

const ERow = struct {
    chars: []u8,
    size: usize,
    render: []u8,
    hl: []Highlight,

    fn deleteChar(row: *ERow, at: usize, alloc: std.mem.Allocator) !void {
        if (at < 0 or at > row.size) return;
        std.mem.copyForwards(u8, row.chars[at .. row.size - 1], row.chars[at + 1 .. row.size]);
        row.size -|= 1;

        alloc.free(row.render);
        alloc.free(row.hl);
        row.render = try renderSlice(row.chars[0..row.size], alloc);
        row.hl = try updateSyntax(row.render, alloc);
    }

    fn insertChar(row: *ERow, c: u8, at: usize, alloc: std.mem.Allocator) !void {
        const idx = if (at < 0 or at > row.size) row.size else at;

        row.chars = try alloc.realloc(row.chars, row.size + 1);
        std.mem.copyBackwards(u8, row.chars[idx + 1 .. row.size + 1], row.chars[idx..row.size]);
        row.chars[idx] = c;
        row.size +|= 1;

        alloc.free(row.render);
        alloc.free(row.hl);
        row.render = try renderSlice(row.chars[0..row.size], alloc);
        row.hl = try updateSyntax(row.render, alloc);
    }

    ///
    /// returned slice needs to be freed
    ///
    fn renderSlice(chars: []const u8, alloc: std.mem.Allocator) ![]u8 {
        const count_tabs = std.mem.count(u8, chars, "\t");
        const render = try alloc.alloc(u8, chars.len + (KILO_TAB_STOP - 1) * count_tabs);
        errdefer alloc.free(render);

        var idx: usize = 0;
        for (chars) |c|
            if (c == '\t') {
                render[idx] = ' ';
                idx += 1;
                while (idx % KILO_TAB_STOP != 0) : (idx += 1) render[idx] = ' ';
            } else {
                render[idx] = c;
                idx += 1;
            };

        return render;
    }

    ///
    /// returned slice needs to be freed
    ///
    fn updateSyntax(chars: []const u8, alloc: std.mem.Allocator) ![]Highlight {
        const hl = try alloc.alloc(Highlight, chars.len);
        @memset(hl, .normal);

        for (chars, hl) |c, *i| {
            if (std.ascii.isDigit(c))
                i.* = .number;
        }

        return hl;
    }

    fn appendString(row: *ERow, s: []u8, alloc: std.mem.Allocator) !void {
        row.chars = try alloc.realloc(row.chars, row.size + s.len);
        @memcpy(row.chars[row.size..], s);
        row.size +|= s.len;

        row.render = try ERow.renderSlice(row.chars[0..row.size], alloc);
    }

    fn deinit(row: *const ERow, alloc: std.mem.Allocator) void {
        alloc.free(row.chars);
        alloc.free(row.render);
        alloc.free(row.hl);
    }

    fn cxToRx(row: *const ERow, cx: usize) usize {
        var rx: usize = 0;
        return for (row.chars[0..row.size], 0..) |c, j| {
            if (j >= cx) break rx;
            if (c == '\t') rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
            rx += 1;
        } else rx;
    }

    fn rxToCx(row: *const ERow, rx: usize) usize {
        var c_rx: usize = 0;
        return for (row.chars[0..row.size], 0..) |c, j| {
            if (c == '\t') c_rx += (KILO_TAB_STOP - 1) - (c_rx % KILO_TAB_STOP);
            c_rx += 1;
            if (c_rx > rx) return j;
        } else row.size;
    }
};

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

        dirty: bool,

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
                try self.insertRow(try line.toOwnedSlice(), self.rows.items.len);
            } else |err| switch (err) {
                error.EndOfStream => {},
                else => |e| return e,
            }

            self.dirty = false;
        }

        pub fn close(self: *@This()) void {
            for (self.rows.items) |*row| row.deinit(self.alloc);
            if (self.file_name) |f| self.alloc.free(f);
            self.rows.deinit();
        }

        var quit_times: u2 = KILO_QUIT_TIMES;
        pub fn processKeyPress(self: *@This()) !KeyResult {
            switch (try self.readKey()) {
                .editor_key => |c| switch (c) {
                    .exit => {
                        if (self.dirty and quit_times > 0) {
                            const status = try std.fmt.allocPrint(
                                self.alloc,
                                "WARNING!!! File has unsaved changes. Press Ctrl-Q {d} more times to quit.",
                                .{quit_times},
                            );
                            defer self.alloc.free(status);
                            self.setStatusMessage(status);
                            quit_times -= 1;
                            return .Default;
                        }

                        _ = try self.writer.write("\x1b[2J");
                        _ = try self.writer.write("\x1b[H");

                        return .CtrlQPressed;
                    },
                    .arrow_up, .arrow_down, .arrow_left, .arrow_right => |a_key| self.moveCursor(a_key),
                    .page_up, .page_down => |p_key| {
                        self.cursor.row = if (p_key == .page_up)
                            self.row_offset
                        else
                            @min(self.row_offset + self.screen.row - 1, self.rows.items.len);
                        const key: EditorKey = if (p_key == .page_up) .arrow_up else .arrow_down;

                        var times: u16 = 0;
                        while (times < self.screen.row) : (times += 1)
                            self.moveCursor(key);
                    },
                    .home => self.cursor.col = 0,
                    .end => if (self.cursor.row < self.rows.items.len) {
                        self.cursor.col = self.rows.items[self.cursor.row].render.len;
                    },
                    .enter => try self.insertNewLine(),
                    .escape => {},
                    .delete, .backspace => |k| {
                        if (k == .delete)
                            self.moveCursor(.arrow_right);

                        try self.deleteChar();
                    },
                    .save => try self.save(),
                    .find => try self.find(),
                },
                .key => |k| try self.insertChar(k),
            }

            quit_times = KILO_QUIT_TIMES;

            return .Default;
        }

        pub fn refreshScreen(self: *@This()) !void {
            self.scroll();

            var abuf: Abuf = try Abuf.init(self.alloc);
            defer abuf.deinit();

            // hide cursor
            // and
            // move cursor to top left
            try abuf.append("\x1b[?25l");
            try abuf.append("\x1b[H");

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
            try abuf.append(slice);

            // show cursor
            try abuf.append("\x1b[?25h");

            _ = try self.writer.write(abuf.b);
        }

        pub fn setStatusMessage(self: *@This(), status: []const u8) void {
            const len = @min(self.status_msg.len, status.len);
            std.mem.copyForwards(u8, &self.status_msg, status[0..len]);
            self.status = &self.status_msg;
            self.status.len = len;
            self.status_msg_time = std.time.timestamp();
        }

        const CallBack = ?*const fn (ctx: *@This(), s: []u8, c: Key) error{OutOfMemory}!void;

        ///
        /// The returned slice needs to be freed
        ///
        fn statusPrompt(self: *@This(), comptime prompt: []const u8, callback: CallBack) !?[]u8 {
            var buf = try std.ArrayList(u8).initCapacity(self.alloc, 128);
            errdefer buf.deinit();

            const writer = buf.writer();

            return while (true) {
                const status = try std.fmt.allocPrint(self.alloc, prompt, .{buf.items});
                defer self.alloc.free(status);

                self.setStatusMessage(status);
                try self.refreshScreen();

                switch (try self.readKey()) {
                    .editor_key => |ek| switch (ek) {
                        .enter => {
                            self.setStatusMessage("");
                            const buf_slice = try buf.toOwnedSlice();
                            if (callback) |cb| try cb(self, buf_slice, .{ .editor_key = .enter });
                            break buf_slice;
                        },
                        .escape => {
                            self.setStatusMessage("");
                            defer buf.deinit();

                            if (callback) |cb| try cb(self, buf.items, .{ .editor_key = .escape });
                            break null;
                        },
                        .delete, .backspace => _ = buf.popOrNull(),
                        else => if (callback) |cb| try cb(self, buf.items, .{ .editor_key = ek }),
                    },
                    .key => |k| if (!std.ascii.isControl(k) and k < 128) {
                        try writer.writeByte(k);
                        if (callback) |cb| try cb(self, buf.items, .{ .key = k });
                    },
                }
            };
        }

        ///
        /// The returned slice needs to be freed
        ///
        fn rowsToString(self: *@This()) ![]u8 {
            var total_len: usize = 0;
            for (self.rows.items) |*row|
                total_len += row.size + 1; // 1 for newline.

            const buf = try self.alloc.alloc(u8, total_len);
            errdefer self.alloc.free(buf);

            var len: usize = 0;
            for (self.rows.items) |*row| {
                @memcpy(buf[len..][0..row.size], row.chars[0..row.size]);
                len += row.size;
                buf[len] = '\n';
                len += 1;
            }

            return buf;
        }

        fn find(self: *@This()) !void {
            const saved_cursor_row = self.cursor.row;
            const saved_cursor_col = self.cursor.col;
            const saved_col_offset = self.col_offset;
            const saved_row_offset = self.row_offset;

            const query = try self.statusPrompt("Search {s} (Use ESC/Arrows/Enter)", @This().findCallBack);
            if (query) |q| self.alloc.free(q) else {
                self.cursor.row = saved_cursor_row;
                self.cursor.col = saved_cursor_col;
                self.row_offset = saved_row_offset;
                self.col_offset = saved_col_offset;
            }
        }

        var last_match: ?usize = null;
        var find_forward: bool = true;
        var saved_hl: []Highlight = undefined;
        var saved_hl_line: ?usize = null;
        fn findCallBack(self: *@This(), query: []u8, key: Key) error{OutOfMemory}!void {
            if (saved_hl_line) |saved_line| {
                @memcpy(self.rows.items[saved_line].hl, saved_hl);
                self.alloc.free(saved_hl);
                saved_hl_line = null;
            }

            switch (key) {
                .editor_key => |ek| switch (ek) {
                    .enter, .escape => {
                        last_match = null;
                        find_forward = true;
                        return;
                    },
                    .arrow_right, .arrow_down => find_forward = true,
                    .arrow_left, .arrow_up => find_forward = false,
                    else => {
                        last_match = null;
                        find_forward = true;
                    },
                },
                else => {
                    last_match = null;
                    find_forward = true;
                },
            }

            if (last_match == null) find_forward = true;
            var current = last_match;

            var i: usize = 0;
            while (i < self.rows.items.len) : (i += 1) {
                if (current) |*c| {
                    if (find_forward)
                        c.* = if (c.* == self.rows.items.len - 1) 0 else c.* + 1
                    else
                        c.* = if (c.* == 0) self.rows.items.len - 1 else c.* - 1;
                } else current = if (find_forward)
                    0
                else
                    self.rows.items.len - 1;
                const r = &self.rows.items[current.?];

                if (std.mem.indexOf(u8, r.render, query)) |pos| {
                    last_match = current;
                    self.cursor.row = current.?;
                    self.cursor.col = r.rxToCx(pos);
                    self.row_offset = self.rows.items.len;

                    saved_hl_line = current.?;
                    saved_hl = try self.alloc.dupe(Highlight, r.hl);

                    @memset(r.hl[pos..][0..query.len], .match);
                    break;
                }
            }
        }

        fn save(self: *@This()) !void {
            const file_name = self.file_name orelse f: {
                if (try self.statusPrompt("Save as: {s}", null)) |file_name|
                    break :f file_name;

                self.setStatusMessage("Save aborted");
                return;
            };
            defer if (self.file_name == null) self.alloc.free(file_name);

            const buf = try self.rowsToString();
            defer self.alloc.free(buf);

            const file = try std.fs.cwd().createFile(file_name, .{});
            defer file.close();
            const result: anyerror!void = blk: {
                file.setEndPos(buf.len) catch |e| break :blk e;
                file.writeAll(buf) catch |e| break :blk e;
            };

            const status = try if (result) |_|
                std.fmt.allocPrint(self.alloc, "{d} bytes written to dist", .{buf.len})
            else |e|
                std.fmt.allocPrint(self.alloc, "Can't save! I/O error: {!}", .{e});
            defer self.alloc.free(status);
            self.setStatusMessage(status);

            self.dirty = false;
        }

        fn delRow(self: *@This(), at: usize) !void {
            if (at < 0 or at >= self.rows.items.len) return;

            const row = self.rows.orderedRemove(at);
            row.deinit(self.alloc);

            self.dirty = true;
        }

        fn deleteChar(self: *@This()) !void {
            if (self.cursor.row == self.rows.items.len) return;
            if (self.cursor.col == 0 and self.cursor.row == 0) return;

            const row = &self.rows.items[self.cursor.row];
            if (self.cursor.col > 0) {
                try row.deleteChar(self.cursor.col - 1, self.alloc);
                self.cursor.col -|= 1;
                self.dirty = true;
            } else {
                const prev_row = &self.rows.items[self.cursor.row - 1];

                self.cursor.col = prev_row.size;
                try prev_row.appendString(row.chars[0..row.size], self.alloc);
                try self.delRow(self.cursor.row);
                self.cursor.row -= 1;
            }
        }

        fn insertChar(self: *@This(), c: u8) !void {
            if (self.cursor.row == self.rows.items.len) {
                try self.insertRow("", self.rows.items.len);
            }
            const row = &self.rows.items[self.cursor.row];
            try row.insertChar(c, self.cursor.col, self.alloc);
            self.cursor.col +|= 1;
            self.dirty = true;
        }

        fn insertNewLine(self: *@This()) !void {
            if (self.cursor.col == 0) {
                try self.insertRow("", self.cursor.row);
            } else {
                var row = &self.rows.items[self.cursor.row];
                const duped_chars = try self.alloc.dupe(u8, row.chars[self.cursor.col..row.size]);
                errdefer self.alloc.free(duped_chars);
                try self.insertRow(duped_chars, self.cursor.row + 1);
                row = &self.rows.items[self.cursor.row];
                row.size = self.cursor.col;

                self.alloc.free(row.render);
                row.render = try ERow.renderSlice(row.chars[0..row.size], self.alloc);
            }

            self.cursor.row +|= 1;
            self.cursor.col = 0;
        }

        fn insertRow(self: *@This(), s: []u8, at: usize) !void {
            if (at < 0 or at > self.rows.items.len) return;

            const render = try ERow.renderSlice(s, self.alloc);
            const hl = try ERow.updateSyntax(render, self.alloc);

            try self.rows.insert(at, .{
                .chars = s,
                .size = s.len,
                .render = render,
                .hl = hl,
            });

            self.dirty = true;
        }

        fn drawStatusMessageBar(self: *@This(), abuf: *Abuf) !void {
            try abuf.append("\x1b[K");

            const msglen = @min(self.status.len, self.screen.col);

            if (msglen > 0 and std.time.timestamp() - self.status_msg_time < KILO_STATUS_MESSAGE_DURATION)
                try abuf.append(self.status[0..msglen]);
        }

        fn drawStatusBar(self: *@This(), abuf: *Abuf) !void {
            try abuf.append("\x1b[7m");

            const status = try std.fmt.allocPrint(self.alloc, "{s} - {d} lines {s}", .{
                self.file_name orelse "[No Name]",
                self.rows.items.len,
                if (self.dirty) "(modified)" else "",
            });
            defer self.alloc.free(status);
            const rstatus = try std.fmt.allocPrint(self.alloc, "{d}/{d}", .{
                self.cursor.row + 1,
                self.rows.items.len,
            });
            defer self.alloc.free(rstatus);

            const len = @min(self.screen.col, status.len);
            try abuf.append(status[0..len]);
            try abuf.appendN(" ", self.screen.col -| (len + rstatus.len + 1));
            try abuf.append(rstatus);

            try abuf.append("\x1b[m\r\n");
        }

        fn readKey(self: *@This()) !Key {
            var buffer: [1]u8 = undefined;
            var seq: [3]u8 = undefined;
            while (try self.reader.read(&buffer) != 1) std.atomic.spinLoopHint();
            const c: u8 = buffer[0];

            if (c == std.ascii.control_code.esc) {
                _ = self.reader.read(seq[0..1]) catch return .{ .key = c };
                _ = self.reader.read(seq[1..2]) catch return .{ .key = c };

                if (seq[0] == '[')
                    if (seq[1] >= '0' and seq[1] <= '9') {
                        _ = self.reader.read(seq[2..3]) catch return .{ .key = c };
                        if (seq[2] == '~')
                            return switch (seq[1]) {
                                '1', '7' => .{ .editor_key = .home },
                                '4', '8' => .{ .editor_key = .end },
                                '3' => .{ .editor_key = .delete },
                                '5' => .{ .editor_key = .page_up },
                                '6' => .{ .editor_key = .page_down },
                                else => .{ .key = c },
                            };
                    } else return switch (seq[1]) {
                        'A' => .{ .editor_key = .arrow_up },
                        'B' => .{ .editor_key = .arrow_down },
                        'C' => .{ .editor_key = .arrow_right },
                        'D' => .{ .editor_key = .arrow_left },
                        'H' => .{ .editor_key = .home },
                        'F' => .{ .editor_key = .end },
                        else => .{ .key = c },
                    }
                else if (seq[0] == 'O')
                    return switch (seq[1]) {
                        'H' => .{ .editor_key = .home },
                        'F' => .{ .editor_key = .end },
                        else => .{ .key = c },
                    };
            }

            return switch (c) {
                std.ascii.control_code.xon => .{ .editor_key = .exit },
                // NOTE: ascii del is backspace, <esc>[3~ is del
                std.ascii.control_code.bs, std.ascii.control_code.del => .{ .editor_key = .backspace },
                std.ascii.control_code.ff, std.ascii.control_code.esc => .{ .editor_key = .escape },
                std.ascii.control_code.cr => .{ .editor_key = .enter },
                std.ascii.control_code.dc3 => .{ .editor_key = .save },
                std.ascii.control_code.ack => .{ .editor_key = .find },
                else => |x| .{ .key = x },
            };
        }

        fn drawRows(self: *@This(), abuf: *Abuf) !void {
            var arena = std.heap.ArenaAllocator.init(self.alloc);
            const arena_alloc = arena.allocator();

            for (0..self.screen.row) |y| {
                const file_row: usize = y + self.row_offset;
                if (file_row >= self.rows.items.len) {
                    if (self.rows.items.len == 0 and y == self.screen.row / 3) {
                        const banner = try std.fmt.allocPrint(arena_alloc, "Kilo editor -- version {s}", .{KILO_VERSION});
                        const banner_len = @min(banner.len, self.screen.col);

                        const padding = (self.screen.col - banner_len) / 2;

                        if (padding > 0) {
                            try abuf.append("~");
                            if (padding > 2) {
                                try abuf.appendN(" ", padding - 1);
                            }
                        }

                        try abuf.append(banner[0..banner_len]);
                    } else try abuf.append("~");
                } else {
                    const row = &self.rows.items[file_row];
                    if (row.render.len > self.col_offset) {
                        const len = @min(row.render.len - self.col_offset, self.screen.col);

                        const render = row.render[self.col_offset..][0..len];
                        const hls = row.hl[self.col_offset..][0..len];
                        var current_color: ?usize = null;

                        for (render, hls) |c, hl| {
                            switch (hl) {
                                .normal => {
                                    if (current_color != null) {
                                        try abuf.append("\x1b[39m");
                                        current_color = null;
                                    }
                                    try abuf.append(&[1]u8{c});
                                },
                                else => |h| {
                                    const color = h.toColor();
                                    var color_changed: bool = false;
                                    if (current_color) |*cur_color| {
                                        if (cur_color.* != color) {
                                            cur_color.* = color;
                                            color_changed = true;
                                        }
                                    } else {
                                        current_color = color;
                                        color_changed = true;
                                    }
                                    if (color_changed) {
                                        const buf = try std.fmt.allocPrint(arena_alloc, "\x1b[{d}m", .{color});
                                        try abuf.append(buf);
                                    }

                                    try abuf.append(&[1]u8{c});
                                },
                            }
                        }
                        try abuf.append("\x1b[39m");
                    }
                }

                // erase part of current line
                try abuf.append("\x1b[K\r\n");
            }

            arena.deinit();
        }

        fn scroll(self: *@This()) void {
            self.render_col = if (self.cursor.row < self.rows.items.len) blk: {
                const row = &self.rows.items[self.cursor.row];
                break :blk row.cxToRx(self.cursor.col);
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
                .arrow_left => if (self.cursor.col != 0) {
                    self.cursor.col -|= 1;
                } else if (self.cursor.row != 0) {
                    self.cursor.row -|= 1;
                    self.cursor.col = self.rows.items[self.cursor.row].size;
                },
                .arrow_right => if (row) |r| {
                    if (self.cursor.col < r.size) {
                        self.cursor.col += 1;
                    } else if (self.cursor.col == r.size) {
                        self.cursor.row += 1;
                        self.cursor.col = 0;
                    }
                },
                .arrow_up => self.cursor.row -|= 1,
                .arrow_down => if (self.cursor.row < self.rows.items.len) {
                    self.cursor.row += 1;
                },
                else => unreachable,
            }

            // row could have changed
            row = if (self.cursor.row >= self.rows.items.len) null else &self.rows.items[self.cursor.row];

            const row_len = if (row) |r| r.size else 0;
            if (self.cursor.col > row_len) {
                self.cursor.col = row_len;
            }
        }
    };
}

const Abuf = struct {
    b: []u8,
    alloc: std.mem.Allocator,

    fn init(alloc: std.mem.Allocator) !Abuf {
        return .{
            .alloc = alloc,
            .b = try alloc.alloc(u8, 0),
        };
    }

    fn deinit(ab: *Abuf) void {
        ab.alloc.free(ab.b);
    }

    fn appendN(ab: *Abuf, s: []const u8, n: usize) !void {
        const new_len = s.len * n;
        const s_ntimes = try ab.alloc.alloc(u8, new_len);
        defer ab.alloc.free(s_ntimes);

        var offset: usize = 0;
        while (offset < s.len * n) : (offset += s.len)
            @memcpy(s_ntimes[offset..][0..s.len], s);

        try ab.append(s_ntimes);
    }

    fn append(ab: *Abuf, s: []const u8) !void {
        var new = try ab.alloc.realloc(ab.b, ab.b.len + s.len);
        errdefer ab.alloc.free(new);
        @memcpy(new[ab.b.len..], s);
        ab.b = new;
    }
};

test {
    _ = @import("term.zig");
}

test "hl" {
    const hl = try ERow.updateSyntax("a11b22c33", std.testing.allocator);
    defer std.testing.allocator.free(hl);

    try std.testing.expectEqualSlices(Highlight, &[9]Highlight{
        .normal, // a
        .number, // 1
        .number, // 1
        .normal, // b
        .number, // 2
        .number, // 2
        .normal, // c
        .number, // 3
        .number, // 3
    }, hl);
}

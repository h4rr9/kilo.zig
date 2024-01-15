const std = @import("std");
const os = std.os;
const fs = std.fs;
const root = @import("root.zig");
const term = @import("term.zig");
const builtin = @import("builtin");

pub fn main() !void {
    const alloc = if (builtin.mode == .Debug) blk: {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        break :blk gpa.allocator();
    } else std.heap.c_allocator;

    var stdout = std.io.getStdOut();
    var stdin = std.io.getStdIn();

    defer stdin.close();
    defer stdout.close();

    const stdin_reader = stdin.reader().any();
    const stdout_writer = stdout.writer();

    var terminal = try term.term(&stdin, &stdout, stdout_writer, stdin_reader);
    // enable raw mode
    try terminal.enableRawMode();
    // disable raw mode on exit
    defer terminal.disableRawMode() catch @panic("Unable to reset termios.");
    // init screen
    const screen = try terminal.getWindowSize();

    var editor = root.kilo(stdout_writer, stdin_reader, screen, alloc);

    var arg_iter = try std.process.argsWithAllocator(alloc);
    defer arg_iter.deinit();

    _ = arg_iter.next().?;
    if (arg_iter.next()) |file|
        editor.open(file) catch |e| switch (e) {
            else => {
                try stdout_writer.print("Error opening file {s}", .{file});
                return;
            },
        };

    defer editor.close();

    editor.setStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (true) {
        try editor.refreshScreen();
        switch (try editor.processKeyPress()) {
            .CtrlQPressed => {
                break;
            },
            else => std.atomic.spinLoopHint(),
        }
    }
}

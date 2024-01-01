const std = @import("std");
const os = std.os;
const fs = std.fs;
const root = @import("root.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var stdout = std.io.getStdOut();
    var stdin = std.io.getStdIn();

    defer stdin.close();
    defer stdout.close();

    const stdin_reader = stdin.reader().any();
    const stdout_writer = stdout.writer();

    var termios = try root.termios(&stdin, &stdout, stdout_writer, stdin_reader);
    // enable raw mode
    try termios.enableRawMode();
    // disable raw mode on exit
    defer termios.disableRawMode() catch @panic("Unable to reset termios.");
    // init screen
    const screen = try termios.getWindowSize();

    var editor = root.kilo(stdout_writer, stdin_reader, screen, gpa.allocator());

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

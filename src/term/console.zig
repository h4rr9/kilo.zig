const std = @import("std");
const fs = std.fs;
const os = std.os;
const term = @import("../term.zig");
const Screen = term.Screen;
const windows = std.os.windows;

const DWOutputMode = struct {
    const ENABLE_PROCESSED_OUTPUT: u32 = 0x0001;
    const ENABLE_WRAP_AT_EOL_OUTPUT: u32 = 0x0002;
    const ENABLE_VIRTUAL_TERMINAL_PROCESSING: u32 = 0x0004;
};

const DWInputMode = struct {
    const ENABLE_ECHO_INPUT: u32 = 0x0004;
    const ENABLE_LINE_INPUT: u32 = 0x0002;
    const ENABLE_PROCESSED_INPUT: u32 = 0x0001;
    const ENABLE_VIRTUAL_TERMINAL_INPUT: u32 = 0x0200;
};

pub extern "kernel32" fn SetConsoleMode(in_hConsoleHandle: windows.HANDLE, out_lpMode: windows.DWORD) callconv(windows.WINAPI) windows.BOOL;

inline fn Win32Error() anyerror!void {
    switch (windows.kernel32.GetLastError()) {
        else => |err| return windows.unexpectedError(err),
    }
}

fn getConsoleMode(in_hConsoleHandle: windows.HANDLE) !windows.DWORD {
    var console_mode: windows.DWORD = 0;

    return switch (windows.kernel32.GetConsoleMode(in_hConsoleHandle, &console_mode)) {
        0 => try Win32Error(),
        else => console_mode,
    };
}

fn setConsoleMode(in_hConsoleHandle: windows.HANDLE, out_lpMode: windows.DWORD) !void {
    return switch (SetConsoleMode(in_hConsoleHandle, out_lpMode)) {
        0 => try Win32Error(),
        else => {},
    };
}

/// Some code taken from - https://github.com/microsoft/terminal/issues/8820
pub fn Console(comptime WriterType: anytype) type {
    return struct {
        stdin: *fs.File,
        stdout: *fs.File,
        writer: WriterType,
        reader: std.io.AnyReader,
        saved_input_mode: windows.DWORD,
        saved_output_mode: windows.DWORD,

        pub fn init(stdin: *fs.File, stdout: *fs.File, reader: std.io.AnyReader, writer: WriterType) !@This() {
            return .{
                .stdin = stdin,
                .stdout = stdout,
                .reader = reader,
                .writer = writer,
                .saved_input_mode = try getConsoleMode(stdin.handle),
                .saved_output_mode = try getConsoleMode(stdout.handle),
            };
        }

        pub fn enableRawMode(self: *@This()) !void {
            var output_mode = self.saved_output_mode;
            output_mode |= @as(windows.DWORD, DWOutputMode.ENABLE_PROCESSED_OUTPUT);
            output_mode &= ~@as(windows.DWORD, DWOutputMode.ENABLE_WRAP_AT_EOL_OUTPUT);
            output_mode |= @as(windows.DWORD, DWOutputMode.ENABLE_VIRTUAL_TERMINAL_PROCESSING);

            try setConsoleMode(self.stdout.handle, output_mode);

            var input_mode = self.saved_input_mode;
            input_mode &= ~@as(windows.DWORD, DWInputMode.ENABLE_ECHO_INPUT);
            input_mode &= ~@as(windows.DWORD, DWInputMode.ENABLE_LINE_INPUT);
            input_mode &= ~@as(windows.DWORD, DWInputMode.ENABLE_PROCESSED_INPUT);
            input_mode |= @as(windows.DWORD, DWInputMode.ENABLE_VIRTUAL_TERMINAL_INPUT);

            try setConsoleMode(self.stdin.handle, input_mode);
        }

        pub fn disableRawMode(self: *@This()) !void {
            _ = try self.writer.write("\x1b[0m");
            try setConsoleMode(self.stdin.handle, self.saved_input_mode);
            try setConsoleMode(self.stdout.handle, self.saved_output_mode);
        }

        ///
        /// yoinked from https://github.com/ziglang/zig/blob/4129996211edd30b25c23454520fd78b2a70394b/lib/std/os.zig#L3289
        ///
        pub fn getWindowSize(self: *@This()) !Screen {
            var csbi: windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;

            switch (windows.kernel32.GetConsoleScreenBufferInfo(self.stdout.handle, &csbi)) {
                0 => try Win32Error(),
                else => {},
            }

            return .{
                .col = @intCast(csbi.srWindow.Right - csbi.srWindow.Left + 1),
                .row = @intCast(csbi.srWindow.Bottom - csbi.srWindow.Top + 1),
            };
        }
    };
}

test "input-mode" {
    const stdin = std.io.getStdIn();
    const mode = try getConsoleMode(stdin.handle);

    try std.testing.expect(mode > 0);
}

test "output-mode" {
    const stdout = std.io.getStdOut();
    const mode = try getConsoleMode(stdout.handle);

    try std.testing.expect(mode > 0);
}

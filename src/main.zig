const std = @import("std");
const os = std.os;
const fs = std.fs;

const Termios = struct {
    tty: *fs.File,
    termios: os.termios,

    pub fn init(tty: *fs.File) !Termios {
        return .{ .tty = tty, .termios = try os.tcgetattr(tty.handle) };
    }

    fn enableRawMode(self: *Termios) !void {
        var raw = self.termios;

        raw.iflag &= ~@as(os.system.tcflag_t, os.system.BRKINT | os.system.ICRNL | os.system.INPCK | os.system.ISTRIP | os.system.IXON);
        raw.oflag &= ~@as(os.system.tcflag_t, os.system.OPOST);
        raw.cflag &= ~@as(os.system.tcflag_t, os.system.CS8);
        raw.lflag &= ~@as(os.system.tcflag_t, os.system.ECHO | os.system.ICANON | os.system.IEXTEN | os.system.ISIG);
        raw.cc[os.system.V.MIN] = 0;
        raw.cc[os.system.V.TIME] = 1;

        try os.tcsetattr(self.tty.handle, .FLUSH, raw);
    }

    fn disableRawMode(self: *Termios) !void {
        try os.tcsetattr(self.tty.handle, .FLUSH, self.termios);
    }
};

pub fn main() !void {
    var tty = try fs.openFileAbsolute("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    var termios = try Termios.init(&tty);

    // disable raw mode on exit
    defer termios.disableRawMode() catch @panic("Unable to reset termios.");

    // enable raw mode
    try termios.enableRawMode();

    var stdin = std.io.getStdIn();
    var stdin_reader = stdin.reader().any();
    var stdin_writer = stdin.writer();

    var buffer: [1]u8 = .{0};

    while (true) {
        buffer[0] = 0;
        _ = try stdin_reader.read(&buffer);
        const c = buffer[0];

        if (std.ascii.isControl(c)) {
            try stdin_writer.print("{d}\r\n", .{c});
        } else {
            try stdin_writer.print("{d} ('{c}')\r\n", .{ c, c });
        }

        if (c == 'q') break;
    }
}

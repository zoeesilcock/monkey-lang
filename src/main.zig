const std = @import("std");
const repl = @import("repl.zig");

pub fn main(init: std.process.Init) !void {
    std.debug.print("Hello! This is the Monkey programming language!\n", .{});
    std.debug.print("Feel free to type in commands.\n", .{});

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(init.io, &stdout_buffer);
    const stdout: *std.Io.Writer = &stdout_writer.interface;

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().reader(init.io, &stdin_buffer);
    const stdin: *std.Io.Reader = &stdin_reader.interface;

    try repl.start(stdout, stdin);
}

test {
    std.testing.refAllDecls(@This());
}

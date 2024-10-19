const std = @import("std");
const repl = @import("repl.zig");

pub fn main() !void {
    std.debug.print("Hello! This is the Monkey programming language!\n", .{});
    std.debug.print("Feel free to type in commands.\n", .{});

    const out = std.io.getStdOut();
    const in = std.io.getStdIn();

    try repl.start(out, in);
}

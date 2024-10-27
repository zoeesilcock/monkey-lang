const std = @import("std");

const OUTPUT_TRACING = false;
const TRACE_INDENT_PLACEHOLDER = "\t";
var trace_level: u32 = 0;

fn tracePrint(prefix: []const u8, message: []const u8) void {
    if (OUTPUT_TRACING) {
        for (0..trace_level) |_| {
            std.debug.print("{s}", .{ TRACE_INDENT_PLACEHOLDER });
        }
        std.debug.print("{s}{s}\n", .{ prefix, message });
    }
}

pub fn trace(message: []const u8) void {
    trace_level += 1;
    tracePrint("BEGIN ", message);
}

pub fn untrace(message: []const u8) void {
    tracePrint("END ", message);
    trace_level -= 1;
}

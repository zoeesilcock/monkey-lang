const std = @import("std");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;

const PROMPT = ">> ";

pub fn start(out: std.fs.File, in: std.fs.File) !void {
    const stdout = out.writer();
    const stdin = in.reader();
    var input_buffer: [1024]u8 = undefined;

    while (true) {
        _ = try stdout.write(PROMPT);

        const input = try stdin.readUntilDelimiter(&input_buffer, '\n');

        var l = Lexer.new(input);
        defer l.deinit();

        var tok = try l.nextToken();
        while (!std.mem.eql(u8, tok.token_type, token.EOF)) : (tok = try l.nextToken()) {
            try stdout.print("{?}\n", .{ tok });
        }
    }
}

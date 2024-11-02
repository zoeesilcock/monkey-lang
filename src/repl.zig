const std = @import("std");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

const PROMPT = ">> ";
const MONKEY_FACE =
\\   .--.  .-"     "-.  .--.
\\  / .. \/  .-. .-.  \/ .. \
\\ | |  '|  /   Y   \  |'  | |
\\ | \   \  \ 0 | 0 /  /   / |
\\  \ '- ,\.-"""""""-./, -' /
\\   ''-' /_   ^ ^   _\ '-''
\\       |  \._   _./  |
\\       \   \ '~' /   /
\\        '._ '-=-' _.'
\\           '-----'
;

pub fn start(out: std.fs.File, in: std.fs.File) !void {
    const stdout = out.writer();
    const stdin = in.reader();
    var input_buffer: [1024]u8 = undefined;

    while (true) {
        _ = try stdout.write(PROMPT);

        const input = try stdin.readUntilDelimiter(&input_buffer, '\n');

        var l = Lexer.new(input);
        defer l.deinit();
        var p = try Parser.new(&l);

        const program = try p.parseProgram();

        if (p.errors.len > 0) {
            try printParserErrors(out, p.errors);
            continue;
        }

        try stdout.print("{s}\n", .{ program.string(p.arena.allocator()) });
    }
}

fn printParserErrors(out: std.fs.File, errors: []const []const u8) !void {
    const stdout = out.writer();

    try stdout.print("{s}\n", .{ MONKEY_FACE });
    try stdout.print("Woops! We ran into some monkey business here!\n", .{ });
    try stdout.print(" parser errors:\n", .{ });

    for (errors) |message| {
        try stdout.print("\t{s}\n", .{ message });
    }
}

const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");
const object = @import("object.zig");

const PROMPT = ">> ";
const MONKEY_FACE =
\\            __,__
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

    var permanent_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var env = try object.Environment.init(permanent_arena.allocator());
    defer env.deinit();

    while (true) {
        _ = try stdout.write(PROMPT);

        const input = try stdin.readUntilDelimiter(&input_buffer, '\n');

        var temporary_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer temporary_arena.deinit();

        var l = lexer.Lexer.init(input, temporary_arena.allocator());
        defer l.deinit();

        var p = try parser.Parser.new(&l, permanent_arena.allocator(), temporary_arena.allocator());
        var program = try p.parseProgram();

        if (p.errors.len > 0) {
            try printParserErrors(out, p.errors);
            continue;
        }

        if (try evaluator.eval(ast.Node.init(&program), env, permanent_arena.allocator())) |evaluated| {
            try stdout.print("{s}\n", .{ evaluated.inspect(temporary_arena.allocator()) });
        }
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

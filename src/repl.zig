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

pub fn start(stdout: *std.Io.Writer, stdin: *std.Io.Reader) !void {
    var permanent_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var env = try object.Environment.init(permanent_arena.allocator());
    defer env.deinit();

    // try processInput(stdout, @embedFile("std/array.mnk"), env, &permanent_arena);

    while (true) {
        _ = try stdout.write(PROMPT);
        try stdout.flush();

        const input = try stdin.takeDelimiterInclusive('\n');
        processInput(stdout, input, env, &permanent_arena) catch continue;
    }
}

fn processInput(
    stdout: *std.Io.Writer,
    input: []const u8,
    env: *object.Environment,
    permanent_arena: *std.heap.ArenaAllocator,
) !void {
    var temporary_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer temporary_arena.deinit();

    var l = lexer.Lexer.init(input, temporary_arena.allocator());
    defer l.deinit();

    var p = try parser.Parser.new(&l, permanent_arena.allocator(), temporary_arena.allocator());
    var program = try p.parseProgram();

    if (p.errors.len > 0) {
        try printParserErrors(stdout, p.errors);
    }

    if (try evaluator.eval(ast.Node.init(&program), env, permanent_arena.allocator())) |evaluated| {
        try stdout.print("{s}\n", .{evaluated.inspect(temporary_arena.allocator())});
        try stdout.flush();
    }
}

fn printParserErrors(stdout: *std.Io.Writer, errors: []const []const u8) !void {
    try stdout.print("{s}\n", .{MONKEY_FACE});
    try stdout.print("Woops! We ran into some monkey business here!\n", .{});
    try stdout.print(" parser errors:\n", .{});

    for (errors) |message| {
        try stdout.print("\t{s}\n", .{message});
    }

    try stdout.flush();
}

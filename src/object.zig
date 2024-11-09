const std = @import("std");
const ast = @import("ast.zig");

pub const ObjectType: type = []const u8;

pub const INTEGER_OBJ: ObjectType = "INTEGER";
pub const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
pub const NULL_OBJ: ObjectType = "NULL";
pub const RETURN_VALUE_OBJ = "RETURN_VALUE";
pub const ERROR_OBJ = "ERROR";
pub const FUNCTION_OBJ = "FUNCTION";

pub const ObjectInnerType = enum {
    Integer,
    Boolean,
    Null,
    ReturnValue,
    Error,
    Function,
};

pub const Object = struct {
    ptr: *anyopaque,
    inner_type: ObjectInnerType,
    vtab: *const VTab,

    const VTab = struct {
        objectType: *const fn(ptr: *anyopaque) ObjectType,
        inspect: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) []const u8,
    };

    pub fn objectType(self: Object) ObjectType {
        return self.vtab.objectType(self.ptr);
    }

    pub fn inspect(self: Object, allocator: std.mem.Allocator) []const u8 {
        return self.vtab.inspect(self.ptr, allocator);
    }

    pub fn unwrap(self: Object, comptime T: type) *T {
        return @ptrCast(@alignCast(self.ptr));
    }

    pub fn init(obj: anytype) Object {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);

        std.debug.assert(PtrInfo == .Pointer);
        std.debug.assert(PtrInfo.Pointer.size == .One);
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct);

        const inner_type: ObjectInnerType = switch(Ptr) {
            *Integer => ObjectInnerType.Integer,
            *Boolean => ObjectInnerType.Boolean,
            *Null => ObjectInnerType.Null,
            *ReturnValue => ObjectInnerType.ReturnValue,
            *Error => ObjectInnerType.Error,
            *Function => ObjectInnerType.Function,
            else => {
                std.debug.print("Unsupported Object type: {?}\n", .{ Ptr });
                unreachable;
            },
        };

        const impl = struct {
            fn objectType(ptr: *anyopaque) ObjectType {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.objectType();
            }
            fn inspect(ptr: *anyopaque, allocator: std.mem.Allocator) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.inspect(allocator);
            }
        };

        return .{
            .ptr = obj,
            .inner_type = inner_type,
            .vtab = &.{
                .objectType = impl.objectType,
                .inspect = impl.inspect,
            },
        };
    }
};

pub const Integer = struct {
    value: i64,

    pub fn objectType(self: Integer) ObjectType {
        _ = self;
        return INTEGER_OBJ;
    }

    pub fn inspect(self: Integer, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{d}", .{ self.value }) catch "";
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn objectType(self: Boolean) ObjectType {
        _ = self;
        return BOOLEAN_OBJ;
    }

    pub fn inspect(self: Boolean, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{ if (self.value) "true" else "false" }) catch "";
    }
};

pub const Null = struct {
    pub fn objectType(self: Null) ObjectType {
        _ = self;
        return NULL_OBJ;
    }

    pub fn inspect(self: Null, allocator: std.mem.Allocator) []const u8 {
        _ = self;
        _ = allocator;
        return "null";
    }
};

pub const ReturnValue = struct {
    value: Object,

    pub fn objectType(self: ReturnValue) ObjectType {
        _ = self;
        return RETURN_VALUE_OBJ;
    }

    pub fn inspect(self: ReturnValue, allocator: std.mem.Allocator) []const u8 {
        return self.value.inspect(allocator);
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn objectType(self: Error) ObjectType {
        _ = self;
        return ERROR_OBJ;
    }

    pub fn inspect(self: Error, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "ERROR: {s}", .{ self.message }) catch "";
    }
};

pub const Function = struct {
    parameters: []*ast.Identifier,
    body: ?*ast.BlockStatement,
    env: *Environment,

    pub fn objectType(self: Function) ObjectType {
        _ = self;
        return FUNCTION_OBJ;
    }

    pub fn inspect(self: Function, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "fn",
            "(",
        }) catch "";


        for (self.parameters, 0..) |param, i| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                param.string(allocator),
                if (i < self.parameters.len - 1) ", " else "",
            }) catch "";
        }

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            ") {\n",
            if (self.body) |body| body.string(allocator) else "",
            "\n}",
        }) catch "";

        return out;
    }
};

pub const Environment = struct {
    store: std.StringHashMap(Object),
    outer: ?*Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*Environment {
        var env = try allocator.create(Environment);
        env.store = std.StringHashMap(Object).init(allocator);
        env.allocator = allocator;
        env.outer = null;
        return env;
    }

    pub fn newEnclosed(outer: *Environment, allocator: std.mem.Allocator) !*Environment {
        var env = try Environment.init(allocator);
        env.outer = outer;
        return env;
    }

    pub fn deinit(self: *Environment) void {
        var iterator = self.store.iterator();
        while (iterator.next()) | entry | {
            self.allocator.free(entry.key_ptr.*);
        }
        self.store.deinit();
    }

    pub fn get(self: *Environment, name: []const u8) ?Object {
        var result = self.store.get(name);

        if (result == null) {
            if (self.outer) |outer| {
                result = outer.get(name);
            }
        }

        return result;
    }

    pub fn set(self: *Environment, name: []const u8, value: Object) !Object {
        try self.store.put(try self.allocator.dupe(u8, name), value);
        return value;
    }
};

test "environment" {
    var env = try Environment.init(std.testing.allocator);
    defer std.testing.allocator.destroy(env);
    defer env.deinit();

    const expected_value: i64 = 5;
    var test_object = Integer{ .value = expected_value };
    _ = try env.set("a", Object.init(&test_object));

    const opt_retreived = env.get("a");
    try std.testing.expectEqual(false, opt_retreived == null);

    if (opt_retreived) |retreived| {
        const integer: *Integer = retreived.unwrap(Integer);
        try std.testing.expectEqual(expected_value, integer.value);
    }
}

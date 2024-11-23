const std = @import("std");
const ast = @import("ast.zig");

pub const ObjectType: type = []const u8;
pub const BuiltinFunction: type = *const fn (args: []Object, allocator: std.mem.Allocator) std.mem.Allocator.Error!?Object;

pub const INTEGER_OBJ: ObjectType = "INTEGER";
pub const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
pub const STRING_OBJ: ObjectType = "STRING";
pub const NULL_OBJ: ObjectType = "NULL";
pub const RETURN_VALUE_OBJ = "RETURN_VALUE";
pub const ERROR_OBJ = "ERROR";
pub const FUNCTION_OBJ = "FUNCTION";
pub const BUILTIN_OBJ = "BUILTIN";
pub const ARRAY_OBJ = "ARRAY";
pub const HASH_OBJ = "HASH";

pub const HashableInnerKey = enum {
    Integer,
    Boolean,
    String,
};

pub const HashKey = struct {
    object_type: ObjectType,
    value: u32,
};

pub const Hashable = struct {
    ptr: *anyopaque,
    inner_type: HashableInnerKey,
    vtab: *const VTab,

    const VTab = struct {
        hashKey: *const fn(ptr: *anyopaque) HashKey,
    };

    pub fn hashKey(self: Hashable) HashKey {
        return self.vtab.hashKey(self.ptr);
    }

    pub fn init(obj: anytype) Hashable {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);

        std.debug.assert(PtrInfo == .Pointer);
        std.debug.assert(PtrInfo.Pointer.size == .One);
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct);

        const inner_type: HashableInnerKey = switch(Ptr) {
            *Integer => HashableInnerKey.Integer,
            *Boolean => HashableInnerKey.Boolean,
            *String => HashableInnerKey.String,
            else => {
                std.debug.print("Unsupported Hashable type: {?}\n", .{ Ptr });
                unreachable;
            },
        };

        const impl = struct {
            fn hashKey(ptr: *anyopaque) HashKey {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.hashKey();
            }
        };

        return .{
            .ptr = obj,
            .inner_type = inner_type,
            .vtab = &.{
                .hashKey = impl.hashKey,
            },
        };
    }
};

pub const ObjectInnerType = enum {
    Integer,
    Boolean,
    String,
    Array,
    Hash,
    Null,
    ReturnValue,
    Error,
    Function,
    Builtin,
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
            *String => ObjectInnerType.String,
            *Array => ObjectInnerType.Array,
            *Hash => ObjectInnerType.Hash,
            *Null => ObjectInnerType.Null,
            *ReturnValue => ObjectInnerType.ReturnValue,
            *Error => ObjectInnerType.Error,
            *Function => ObjectInnerType.Function,
            *Builtin => ObjectInnerType.Builtin,
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

    pub fn hashKey(self: Integer) HashKey {
        return HashKey{ .object_type = self.objectType(), .value = @intCast(self.value) };
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

    pub fn hashKey(self: Boolean) HashKey {
        return HashKey{ .object_type = self.objectType(), .value = if (self.value) 1 else 0 };
    }
};

pub const String = struct {
    value: []const u8,

    pub fn objectType(self: String) ObjectType {
        _ = self;
        return STRING_OBJ;
    }

    pub fn inspect(self: String, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{ self.value }) catch "";
    }

    pub fn hashKey(self: String) HashKey {
        var h = std.hash.Fnv1a_32.init();
        h.update(self.value);
        return HashKey{ .object_type = self.objectType(), .value = h.final() };
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

pub const Builtin = struct {
    function: BuiltinFunction,

    pub fn objectType(self: Builtin) ObjectType {
        _ = self;
        return BUILTIN_OBJ;
    }

    pub fn inspect(self: Builtin, allocator: std.mem.Allocator) []const u8 {
        _ = self;
        _ = allocator;
        return "builtin function";
    }
};

pub const Array = struct {
    elements: []Object,

    pub fn objectType(self: Array) ObjectType {
        _ = self;
        return ARRAY_OBJ;
    }

    pub fn inspect(self: Array, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "[",
        }) catch "";

        for (self.elements, 0..) |element, i| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                element.inspect(allocator),
                if (i < self.elements.len - 1) ", " else "",
            }) catch "";
        }

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "]",
        }) catch "";

        return out;
    }
};

pub const HashPair = struct {
    key: Object,
    value: Object,
};

pub const HashContext = struct {
    pub fn hash(_: HashContext, key: HashKey) u32 {
        return key.value + key.object_type[0];
    }

    pub fn eql(_: HashContext, a: HashKey, b: HashKey, _: usize) bool {
        return a.value == b.value and std.mem.eql(u8, a.object_type, b.object_type);
    }
};

pub const Hash = struct {
    pairs: std.ArrayHashMap(HashKey, HashPair, HashContext, true),

    pub fn objectType(self: Hash) ObjectType {
        _ = self;
        return HASH_OBJ;
    }

    pub fn inspect(self: Hash, allocator: std.mem.Allocator) []const u8 {
        var out: []u8 = "";

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "{",
        }) catch "";

        var i: u32 = 0;
        var iterator = self.pairs.iterator();
        while (iterator.next()) |pair| {
            out = std.mem.concat(allocator, u8, &.{ 
                out,
                pair.value_ptr.key.inspect(allocator),
                ":",
                pair.value_ptr.value.inspect(allocator),
                if (i < self.pairs.count() - 1) ", " else "",
            }) catch "";
            i += 1;
        }

        out = std.mem.concat(allocator, u8, &.{ 
            out,
            "}",
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

test "string hash key" {
    const hello1 = &String{ .value = "Hello World" };
    const hello2 = &String{ .value = "Hello World" };
    const diff1 = &String{ .value = "My name is johnny" };
    const diff2 = &String{ .value = "My name is johnny" };

    try std.testing.expectEqual(hello1.hashKey(), hello2.hashKey());
    try std.testing.expectEqual(diff1.hashKey(), diff2.hashKey());
}

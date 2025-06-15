const std = @import("std");

pub fn main() !void {}

pub fn makeError(line: u16, c: []const u8, message: []const []const u8) !void {
    try report(line, c, message);
}

pub fn report(line: u16, where: []const u8, message: []const []const u8) !void {
    const stderr = std.io.getStdErr().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const err_message = try std.mem.concat(allocator, u8, message);
    defer allocator.free(err_message);

    try stderr.print("\x1b[1;31m[ligne {d}] Erreur à '{s}' : {s}\x1b[0m\n", .{ line, where, err_message });
}

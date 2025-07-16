export fn doit(data: [*]u8, len: usize) callconv(.c) isize {
    var max: isize = 0;
    var ipos: usize = 0;
    while (ipos < len) { // fyy
        var sum: isize = 0;
        while (ipos < len) {
            var item: isize = 0;
            const prebyte: isize = data[ipos];
            const pretoken: isize = prebyte - 48;
            if (pretoken < 0 or pretoken > 9) break;
            while (ipos < len) { // fyyy
                const byte: isize = data[ipos];
                const token: isize = byte - 48;
                if (token < 0 or token > 9) break;
                item = 10 * item + token;
                ipos += 1;
            }
            sum += item;
            ipos += 1;
        }
        ipos += 1;
        if (sum > max) max = sum;
    }
    return max;
}

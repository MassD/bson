### encoding / decoding in little endian

[BSON](http://bsonspec.org/) is using little endian order to encode / decode `int32`, `int64` and `double`.

Let's take a look at the `ocaml` code for encoding an *int32* to a *bson object*.

	let encode_int32 buf v = 
		for i = 0 to 3 do
			let b = Int32.logand 255l (Int32.shift_right v (i*8)) in
			Buffer.add_char buf (Char.chr (Int32.to_int b))
		done;;

What basically the code does is that 

1. iterate four times `as we have 4 bytes`
2. everytime, we shift v right for `i*8` bits
3. then we `logic and` `255` with the shifted v
4. then we convert the result into char (taking the least significant one byte only)
5. We have to get the least significant byte first as we are using little endian

For example, encode *0100 0000  1110 0001  0000 1000  0000 0010*

Initially i = 0, so we don't shift.

then we do *0000 0000  0000 0000  0000 0000  1111 1111* (255) `and` *0100 0000  1110 0001  0000 1000  0000 0010*, so we get *0000 0000  0000 0000  0000 0000  0000 0010*.

then we convert it to char by taking the least significant byte which in this case is *0000 0010*.

Next, i = 1, so we shift right for 8 bits and get *0000 0000  0100 0000  1110 0001  0000 1000*

We do *255* `and` with it again, and get *0000 0000  0000 0000  0000 0000  0000 1000*.

then we `char` it and get *0000 1000*.

So on so forth.

### Why little endian

From [Can I get more explanations for BSON?](http://stackoverflow.com/questions/16169879/can-i-get-more-explanations-for-bson):

>Question 2
>
>And it is little endian format. But why? Why not take big endian?
>
>Answer:
>
>Choice of endianness is largely a matter of preference. One advantage of little endian is that commonly used platforms are little endian, and thus don't need to reverse the bytes.

***

From  [What is the advantage of little endian format?](http://programmers.stackexchange.com/questions/95556/what-is-the-advantage-of-little-endian-format)

The good point from the above link is 

>There are arguments either way, but one point is that in a little-endian system, the address of a given value in memory, taken as a 32, 16, or 8 bit width, is the same.
>
>In other words, if you have in memory a two byte value:
>
>0x00f0   16
>0x00f1    0
>taking that '16' as a 16-bit value (c 'short' on most 32-bit systems) or as an 8-bit value (generally c 'char') changes only the fetch instruction you use — not the address you fetch from.
>
>On a big-endian system, with the above layed out as:
>
>0x00f0    0
>0x00f1   16
>you would need to increment the pointer and then perform the narrower fetch operation on the new value.
>
>So, in short, ‘on little endian systems, casts are a no-op.’

Let's say we have `hl`. `h` is the most significant byte and `l` is the least one. So in *big endian*, it is stored as `h l`. When doing casting, i.e., get the least byte, we have to pass reading `h` first, then get `l`. But in *little endian*, we just read `l` as in *little endian*, it is stored as `l h`.

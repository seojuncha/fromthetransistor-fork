# How to use C Examples

## only-main.c

```shell
$ arm-none-eabi-gcc -c only-main.c

```
```shell
$ arm-none-eabi-objdump -D only-main.o
```

```shell
00000000 <main>:
   0:   e52db004        push    {fp}            ; (str fp, [sp, #-4]!)
   4:   e28db000        add     fp, sp, #0
   8:   e3a03027        mov     r3, #39 ; 0x27
   c:   e1a00003        mov     r0, r3
  10:   e28bd000        add     sp, fp, #0
  14:   e49db004        pop     {fp}            ; (ldr fp, [sp], #4)
  18:   e12fff1e        bx      lr
```

```shell
$ arm-none-eabi-gcc -S -o only-main.s only-main.c
$ ls
only-main.c only-main.s
```
```armasm
@ ...
main:
  @ Function supports interworking.
  @ args = 0, pretend = 0, frame = 0
  @ frame_needed = 1, uses_anonymous_args = 0
  @ link register save eliminated.
  str fp, [sp, #-4]!
  add fp, sp, #0
  mov r3, #39
  mov r0, r3
  add sp, fp, #0
  @ sp needed
  ldr fp, [sp], #4
  bx  lr
@ ...
```

module first;
import std.stdio;

alias Byte     = byte;
alias Word     = ushort;
alias DWord    = uint;
alias QWord    = ulong;
alias Register = uint;

enum Instruction {
  INVALID,
  MOV,
  PUSH,
  POP,
  ADD,
  SUB,
  RET,
  MAX,
};
alias I = Instruction;

/**
 * A REX prefix must be encoded when:
 * - using 64-bit operand size and the instruction does not default to
 *   64-bit operand size
 * - case using one of the extended registers (R8 to R15, XMM8 to XMM15:
 *   YMM8 to YMM15, CR8 to CR15 and DR8 to DR15)
 * - using one of the uniform byte registers SPL, BPL, SIL or DIL
 *
 * A REX prefix must not be encoded when using one of the high byte
 * registers AH, CH, BH or DH
 *
 * In all other cases, the REX prefix is ignored. The use of multiple
 * REX prefixes is undefined, although processors seem to use only the
 * last REX prefix.
 *
 * Instructions that default to 64-bit operand size in long mode are:
 * - CALL (near)
 * - ENTER
 * - Jcc
 * - JrCXZ
 * - JMP (near)
 * - LEAVE
 * - LGDT
 * - LIDT
 * - LLDT
 * - LOOP
 * - LOOPcc
 * - LTR
 * - MOV CR(n)
 * - MOV DR(n)
 * - POP reg/mem
 * - POP reg
 * - POP FS
 * - POP GS
 * - POPFQ
 * - PUSH imm8
 * - PUSH imm32
 * - PUSH reg/mem
 * - PUSH reg
 * - PUSH FS
 * - PUSH GS
 * - PUSHFQ
 * - RET (near)
 */
void rex(File f, bool write, bool reg, bool index, bool base) {
  Byte[1] b;
  b[0] |= 0b01000000;
  b[0] |= write << 3;
  b[0] |= reg   << 2;
  b[0] |= index << 1;
  b[0] |= base;
  f.rawWrite(b);
}

void rexw(File f) {
  rex(f, true, false, false, false);
}

void emit(File f, Instruction i) {
  static assert(I.MAX == 7);
  switch (i) {

  case I.RET:
    static Byte[1] b = [cast(Byte)0xc3];
    f.rawWrite(b);
    break;

  case I.INVALID:
  // Instructions which take operands must not be handled by this function.
  case I.MOV:
  case I.PUSH:
  case I.POP:
  case I.ADD:
  case I.SUB:
  default: assert(0);
  }
}

void emit(File f, Instruction i, QWord op) {
  static assert(I.MAX == 7);
  switch (i) {
  case MOV:
  case PUSH:
  case POP:
  case ADD:
  case SUB:
  case RET:
  default: assert(0);
  }
}

void emit(File f, Instruction i, DWord op) {
  static assert(I.MAX == 7);
  switch (i) {

  case I.PUSH:
    static QWord[1] q = [op];
    f.rawWrite(q);
    break;

  case MOV:
  case POP:
  case ADD:
  case SUB:
  case RET:
  default: assert(0);
  }
}

void emit(File f, Instruction i, Word op) {
  static assert(I.MAX == 7);
  switch (i) {
  case MOV:
  case PUSH:
  case POP:
  case ADD:
  case SUB:
  case RET:
  default: assert(0);
  }
}

void emit(File f, Instruction i, Byte op) {
  static assert(I.MAX == 7);
  switch (i) {
  case MOV:
  case PUSH:
  case POP:
  case ADD:
  case SUB:
  case RET:
  default: assert(0);
  }
}

void main() {
  auto machine_code = File("out.bin", "w");
  emit(machine_code, I.RET);
}

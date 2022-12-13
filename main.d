module first;
import std.stdio;
import std.format;

alias Byte     = byte;
alias Word     = ushort;
alias DWord    = uint;
alias QWord    = ulong;

enum Size {
  BYTE,
  WORD,
  DWORD,
  QWORD,
}

enum Register {
  RAX,
  EAX,
  AX,
  AH,
  AL,

  RBX,
  EBX,
  BX,
  BH,
  BL,

  RCX,
  ECX,
  CX,
  CH,
  CL,

  RDX,
  EDX,
  DX,
  DH,
  DL,

  RSI,
  ESI,
  SI,
  SIL,

  RDI,
  EDI,
  DI,
  DIL,

  RSP,
  ESP,
  SP,
  SPL,

  RBP,
  EBP,
  BP,
  BPL,

  RIP,
  EIP,
  IP,
  IPL,

  R8,
  R8D,
  R8W,
  R8B,

  R9,
  R9D,
  R9W,
  R9B,

  R10,
  R10D,
  R10W,
  R10B,

  R11,
  R11D,
  R11W,
  R11B,

  R12,
  R12D,
  R12W,
  R12B,

  R13,
  R13D,
  R13W,
  R13B,

  R14,
  R14D,
  R14W,
  R14B,

  R15,
  R15D,
  R15W,
  R15B,

  // TODO: XMM...

};
alias R = Register;

static Size regsize(Register r) {
  switch (r) {
  case R.RAX:
  case R.RCX:
  case R.RDX:
  case R.RBX:
  case R.RSP:
  case R.RBP:
  case R.RSI:
  case R.RDI:
  case R.R8:
  case R.R9:
  case R.R10:
  case R.R11:
  case R.R12:
  case R.R13:
  case R.R14:
  case R.R15:
  case R.RIP:
    return Size.QWORD;
    break;

  case R.EAX:
  case R.EBX:
  case R.ECX:
  case R.EDX:
  case R.ESI:
  case R.EDI:
  case R.ESP:
  case R.EBP:
  case R.EIP:
  case R.R8D:
  case R.R9D:
  case R.R10D:
  case R.R11D:
  case R.R12D:
  case R.R13D:
  case R.R14D:
  case R.R15D:
    return Size.DWORD;
    break;

  case R.AX:
  case R.BX:
  case R.CX:
  case R.DX:
  case R.SI:
  case R.DI:
  case R.SP:
  case R.BP:
  case R.IP:
  case R.R8W:
  case R.R9W:
  case R.R10W:
  case R.R11W:
  case R.R12W:
  case R.R13W:
  case R.R14W:
  case R.R15W:
    return Size.WORD;
    break;

  case R.AH:
  case R.BH:
  case R.CH:
  case R.DH:
    // TODO: Should we have separate size for high byte?
    return Size.BYTE;
    break;

  case R.AL:
  case R.BL:
  case R.CL:
  case R.DL:
  case R.SIL:
  case R.DIL:
  case R.SPL:
  case R.BPL:
  case R.IPL:
  case R.R8B:
  case R.R9B:
  case R.R10B:
  case R.R11B:
  case R.R12B:
  case R.R13B:
  case R.R14B:
  case R.R15B:
    return Size.BYTE;
    break;
  default: assert(0);
  }
}

static Byte regbits(Register r) {
  switch (r) {
  case R.RAX: .. case R.AL:
    return 0b0000;
    break;
  case R.RCX: .. case R.CL:
    return 0b0001;
    break;
  case R.RDX: .. case R.DL:
    return 0b0010;
    break;
  case R.RBX: .. case R.BL:
    return 0b0011;
    break;
  case R.RSP: .. case R.SPL:
    return 0b0100;
    break;
  case R.RBP: .. case R.BPL:
    return 0b0101;
    break;
  case R.RSI: .. case R.SIL:
    return 0b0110;
    break;
  case R.RDI: .. case R.DIL:
    return 0b0111;
    break;
  case R.R8: .. case R.R8B:
    return 0b1000;
    break;
  case R.R9: .. case R.R9B:
    return 0b1001;
    break;
  case R.R10: .. case R.R10B:
    return 0b1010;
    break;
  case R.R11: .. case R.R11B:
    return 0b1011;
    break;
  case R.R12: .. case R.R12B:
    return 0b1100;
    break;
  case R.R13: .. case R.R13B:
    return 0b1101;
    break;
  case R.R14: .. case R.R14B:
    return 0b1110;
    break;
  case R.R15: .. case R.R15B:
    return 0b1111;
    break;
  default: assert(0);
  }
}

static Byte regtop(Register r) {
  return regbits(r) & 0b1000;
}

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
static void rex(File f, bool write, bool reg, bool index, bool base) {
  Byte[1] b;
  b[0] |= 0b01000000;
  b[0] |= write << 3;
  b[0] |= reg   << 2;
  b[0] |= index << 1;
  b[0] |= base;
  f.rawWrite(b);
}

static void rexw(File f) {
  rex(f, true, false, false, false);
}

/**
 * MOD bits:
 * - 0b00: Indirect register addressing mode
 *         SIB with no displacement when R/M == 0b100
 *         Displacement only when R/M == 0b101
 * - 0b01: One-byte signed displacement
 * - 0b10: Four-byte signed displacement
 * - 0b11: Direct register addressing mode
 *
 * REG bits: refers to a register
 *
 * RM bits: refers to either register or memory
 */
static void modrm(File f, Byte mod, Byte reg, Byte rm) {
  Byte[1] b;
  b[0] = 0;
  b[0] |= (mod << 6) & 0b11000000;
  b[0] |= (reg << 3) & 0b00111000;
  b[0] |= rm         & 0b00000111;
  f.rawWrite(b);
}


static void sib(File f, Byte scale, Byte index, Byte base) {
  Byte[1] b;
  b[0] |= (scale << 6) & 0b11000000;
  b[0] |= (index << 3) & 0b00111000;
  b[0] |= base         & 0b00000111;
  f.rawWrite(b);
}

/** Order of opcode:
 * [1. Legacy Prefix(es)]
 * 2. Opcode with Prefix(es)
 * [3. ModR/M]
 * [4. SIB]
 * [5. Displacement]
 * [6. Immediate]
 */

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

void emit(File f, Instruction i, Register r) {
  static assert(I.MAX == 7);
  switch (i) {

  case I.PUSH:
    Size s = regsize(r);
    switch (s) {
    case Size.WORD:
      static Byte[1] b = [0x66];
      f.rawWrite(b);
      goto case;
    case Size.QWORD:
      if (regtop(r)) {
        rex(f, false, false, false, true);
      }
      Byte[1] b = [0x50];
      b[0] |= regbits(r) & 0b111;
      f.rawWrite(b);
      break;
    default:
      assert(0, format("Invalid register (size) passed with instruction %s: %s (%s)", i, r, s));
    }
    break;

  case I.MOV:
  case I.POP:
  case I.ADD:
  case I.SUB:
  case I.RET:
  default: assert(0);
  }
}

void emit(File f, Instruction i, QWord op) {
  static assert(I.MAX == 7);
  switch (i) {
  case I.MOV:
  case I.PUSH:
  case I.POP:
  case I.ADD:
  case I.SUB:
  case I.RET:
  default: assert(0);
  }
}

void emit(File f, Instruction i, DWord op) {
  static assert(I.MAX == 7);
  switch (i) {

  case I.PUSH:
    QWord[1] q = [op];
    f.rawWrite(q);
    break;

  case I.MOV:
  case I.POP:
  case I.ADD:
  case I.SUB:
  case I.RET:
  default: assert(0);
  }
}

void emit(File f, Instruction i, Word op) {
  static assert(I.MAX == 7);
  switch (i) {
  case I.MOV:
  case I.PUSH:
  case I.POP:
  case I.ADD:
  case I.SUB:
  case I.RET:
  default: assert(0);
  }
}

void emit(File f, Instruction i, Byte op) {
  static assert(I.MAX == 7);
  switch (i) {
  case I.MOV:
  case I.PUSH:
  case I.POP:
  case I.ADD:
  case I.SUB:
  case I.RET:
  default: assert(0);
  }
}

void main() {
  auto machine_code = File("out.bin", "w");
  emit(machine_code, I.RET);
}

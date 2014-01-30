// 8086tiny: a tiny, highly functional, highly portable PC emulator/VM
// Copyright 2013, Adrian Cable (adrian.cable@gmail.com) - http://www.megalith.co.uk/8086tiny
//
// Revision 1.01
//
// This work is licensed under the MIT License. See included LICENSE.TXT.

#include <time.h>

#ifndef _WIN32
#include <unistd.h>
#endif

#ifndef NO_GRAPHICS
#include "SDL.h"
#endif

// Emulator system constants

#define GRAPHICS_X 720
#define GRAPHICS_Y 348
#define IO_PORT_COUNT 0x10000
#define RAM_SIZE 0x10FFF0
#define REGS_BASE 0xF0000

// 16-bit register decodes

#define REG_AX 0
#define REG_CX 1
#define REG_DX 2
#define REG_BX 3
#define REG_SP 4
#define REG_BP 5
#define REG_SI 6
#define REG_DI 7

#define REG_ES 8
#define REG_CS 9
#define REG_SS 10
#define REG_DS 11

#define REG_ZERO 12
#define REG_SCRATCH 13

// 8-bit register decodes

#define REG_AL 0
#define REG_AH 1
#define REG_CL 2
#define REG_CH 3
#define REG_BL 4
#define REG_BH 5
#define REG_DL 6
#define REG_DH 7

// FLAGS register decodes

#define FLAG_CF 40
#define FLAG_PF 41
#define FLAG_AF 42
#define FLAG_ZF 43
#define FLAG_SF 44
#define FLAG_TF 45
#define FLAG_IF 46
#define FLAG_DF 47
#define FLAG_OF 48

// Lookup tables in the BIOS binary

#define TABLE_XLAT_OPCODE 8
#define TABLE_XLAT_SUBFUNCTION 14
#define TABLE_STD_FLAGS_SZP 15
#define TABLE_STD_FLAGS_ARITH 16
#define TABLE_STD_FLAGS_LOGIC 17
#define TABLE_BASE_INST_SIZE 18
#define TABLE_I_W_SIZE 19
#define TABLE_I_MOD_SIZE 20
#define TABLE_COND_JUMP_DECODE_A 21
#define TABLE_COND_JUMP_DECODE_B 22
#define TABLE_COND_JUMP_DECODE_C 23
#define TABLE_COND_JUMP_DECODE_D 24
#define TABLE_FLAGS_BITFIELDS 25
#define TABLE_PARITY_FLAG 50
#define TABLE_OPCODE_LOOKUP 51

// Helper macros

// Increment IP by size of instruction
#define IP_RM_SIZE reg_ip += (i_mod % 3 + 2*(!i_mod * i_rm == 6))

// Decode mod, r_m and reg fields in instruction
#define DECODE_RM_REG scratch2_uint = 4 * !i_mod, \
					  scratch_int = i_rm, \
					  op_to_addr = rm_addr = i_mod < 3 ? SEGREG(seg_override_en ? seg_override : bios_table_lookup(scratch2_uint + 3), bios_table_lookup(scratch2_uint), regs16[bios_table_lookup(scratch2_uint + 1)] + bios_table_lookup(scratch2_uint + 2) * i_data1+) : get_reg_addr(i_rm), \
					  op_from_addr = scratch_uint = get_reg_addr(i_reg), \
					  i_d ? op_from_addr = rm_addr, op_to_addr = scratch_uint : scratch_uint

// Returns number of top bit in operand (i.e. 8 for 8-bit operands, 16-bit operands)
#define TOP_BIT 8*(i_w+1)

// Returns next opcode byte from instruction stream
#define GET_NEXT_OPCODE CAST(short)opcode_stream[++scratch_int]

// Opcode execution unit helpers
#define NEXT_OPCODE_SUBFUNCTION ),i_reg--||(
#define NEXT_OPCODE ),xlat_opcode_id--||(

// MUL/IMUL/DIV/IDIV/ADC/SBB helpers
#define MUL_MACRO(op_data_type,out_regs) (raw_opcode_id = 19, \
										  out_regs[i_w + 1] = (op_result = CAST(op_data_type)mem[rm_addr] * (op_data_type)*out_regs) >> 16, \
										  regs16[REG_AX] = op_result, \
										  set_OF(set_CF(op_result - (op_data_type)op_result)))
#define DIV_MACRO(out_data_type,in_data_type,out_regs) (scratch_int = CAST(out_data_type)mem[rm_addr]) && !(scratch2_uint = (in_data_type)(scratch_uint = (out_regs[i_w+1] << 16) + regs16[REG_AX]) / scratch_int, scratch2_uint - (out_data_type)scratch2_uint) ? out_regs[i_w+1] = scratch_uint - scratch_int * (*out_regs = scratch2_uint) : pc_interrupt(0)
#define ADC_SBB_MACRO(a) OP(a##= regs8[FLAG_CF] +), \
						 set_AF_OF_arith(set_CF(regs8[FLAG_CF] & op_result == op_dest | a op_result < a(int)op_dest))

// Execute arithmetic/logic operations in emulator memory/registers
#define R_M_OP(dest,op,src) (op_dest = i_w ? CAST(unsigned short)dest : dest, \
						     op_result = i_w ? CAST(unsigned short)dest op (op_source = CAST(unsigned short)src) : (dest op (op_source = CAST(unsigned char)src)))
#define MEM_OP(dest,op,src) R_M_OP(mem[dest],op,mem[src])
#define OP(op) MEM_OP(op_to_addr,op,op_from_addr)

// Helpers for stack operations
#define R_M_PUSH(a) (i_w = 1, R_M_OP(mem[SEGREG(REG_SS, REG_SP, --)], =, a))
#define R_M_POP(a) (i_w = 1, regs16[REG_SP] += 2, R_M_OP(a, =, mem[SEGREG(REG_SS, REG_SP, -2+)]))

// Convert segment:offset to linear address in emulator memory space
#define SEGREG(reg_seg,reg_ofs,op) 16 * regs16[reg_seg] + (unsigned short)(op regs16[reg_ofs])

// Returns sign bit of an 8-bit or 16-bit operand
#define SIGN_OF(a) (1 & (i_w ? CAST(short)a : a) >> TOP_BIT - 1)

// Reinterpretation cast
#define CAST(a) *(a*)&

// Keyboard and timer driver. This may need changing for UNIX/non-UNIX platforms
#ifdef _WIN32
#define KEYBOARD_TIMER_DRIVER (pc_interrupt(8), int8_asap = 0, kbhit()) && (mem[0x4A6] = getch(), pc_interrupt(7));
#else
#define KEYBOARD_TIMER_DRIVER (pc_interrupt(8), int8_asap = 0, read(0, mem + 0x4A6, 1)) && pc_interrupt(7)
#endif

// Global variable definitions

unsigned char mem[RAM_SIZE], io_ports[IO_PORT_COUNT], *opcode_stream, *regs8, i_rm, i_w, i_reg, i_mod, i_d, i_reg4bit, raw_opcode_id, xlat_opcode_id, extra, rep_mode, seg_override_en, rep_override_en, trap_flag;
unsigned short *regs16, reg_ip, seg_override, inst_counter, file_index;
unsigned int op_source, op_dest, rm_addr, op_to_addr, op_from_addr, i_data0, i_data1, i_data2, int8_asap, scratch_uint, scratch2_uint;
int i_data1r, op_result, disk[5], scratch_int;

#ifndef NO_GRAPHICS
SDL_Surface *sdl_screen;
#endif

// Helper functions

// Set carry flag
int set_CF(int new_CF)
{
	return regs8[FLAG_CF] = !!new_CF;
}

// Set auxiliary flag
int set_AF(int new_AF)
{
	return regs8[FLAG_AF] = !!new_AF;
}

// Set overflow flag
int set_OF(int new_OF)
{
	return regs8[FLAG_OF] = !!new_OF;
}

// Lookup a value in BIOS helper table #table_num, index #scratch_int
int bios_table_lookup(int table_num)
{
	return mem[CAST(unsigned)regs8[0x103 + 4 * table_num] + scratch_int];
}

// Increment or decrement a register #reg_id (usually SI or DI), depending on direction flag and operand size (given by i_w)
int index_inc(int reg_id)
{
	return regs16[reg_id] -= (2 * regs8[FLAG_DF] - 1)*(i_w + 1);
}

// Set auxiliary and overflow flag after arithmetic operations
int set_AF_OF_arith()
{
	set_AF((op_source ^= op_dest ^ op_result) & 0x10);
	if (op_result == op_dest)
		return set_OF(0);
	else
		return set_OF(1 & (regs8[FLAG_CF] ^ op_source >> TOP_BIT - 1));
}

// Assemble and return emulated CPU FLAGS register in scratch_uint
void make_flags()
{
	scratch_uint = 0xF002; // 8086 has reserved and unused flags set to 1
	for (scratch_int = 9; scratch_int--;)
		scratch_uint += regs8[FLAG_CF + scratch_int] << bios_table_lookup(TABLE_FLAGS_BITFIELDS);
}

// Set emulated CPU FLAGS register from regs8[FLAG_xx] values
int set_flags(int new_flags)
{
	for (scratch_int = 9; scratch_int--;)
		regs8[FLAG_CF + scratch_int] = !!(1 << bios_table_lookup(TABLE_FLAGS_BITFIELDS) & new_flags);

	return 0;
}

// Refresh SDL display from emulated Hercules graphics card video memory
#ifndef NO_GRAPHICS
void video_mem_update()
{	
	for (scratch_int = GRAPHICS_X * GRAPHICS_Y; scratch_int--;)
		((unsigned*)sdl_screen->pixels)[scratch_int] = -!!(1 << 7 - scratch_int % 8 & mem[scratch_int / 2880 * 90 + scratch_int % 720 / 8 + (88 + io_ports[0x3B8] / 128 * 4 + scratch_int / 720 % 4 << 13)]);
	
	SDL_Flip(sdl_screen);
}
#endif

// Execute INT #interrupt_num on the emulated machine
int pc_interrupt(int interrupt_num)
{
	raw_opcode_id = 76;

	make_flags();
	R_M_PUSH(scratch_uint);
	R_M_PUSH(regs16[REG_CS]);
	R_M_PUSH(reg_ip);
	MEM_OP(REGS_BASE + 2 * REG_CS, =, 4 * interrupt_num + 2);
	R_M_OP(reg_ip, =, mem[4 * interrupt_num]);

	return regs8[FLAG_TF] = regs8[FLAG_IF] = 0;
}

// AAA and AAS instructions - which_operation is +1 for AAA, and -1 for AAS
int AAA_AAS(int which_operation)
{
	return (regs16[REG_AX] += 262 * which_operation*set_AF(set_CF((regs8[REG_AL] & 0x0F) > 9 | regs8[FLAG_AF])), regs8[REG_AL] &= 0x0F);
}

// Helper function for string operations (MOVS etc.)
int string_op_support(int stop_on_CX_zero)
{
	index_inc(REG_DI);

	if (rep_override_en)
	{
		if (--regs16[REG_CX] && stop_on_CX_zero)
		{
			rep_override_en++;
			if (seg_override_en)
				seg_override_en++;
			reg_ip--;
		}
	}

	return 0;
}

// Return memory-mapped register location (offset into mem array) for register #reg_id
int get_reg_addr(int reg_id)
{
	return REGS_BASE + (i_w ? 2 * reg_id : 2 * reg_id + reg_id / 4 & 7);
}

// Emulator entry point

int main(int argc, char **argv)
{
	// regs16 and reg8 point to F000:0, the start of memory-mapped registers
	regs16 = regs8 = mem + REGS_BASE;
	// CS is initialised to F000
	regs16[REG_CS] = REGS_BASE >> 4;

	// Open BIOS (file id disk[2]), floppy disk image (disk[1]), and hard disk image (disk[0]) if specified
	for (file_index = 3; file_index;)
		disk[--file_index] = *++argv ? open(*argv, 32898) : 0;

	// Set CX:AX equal to the hard disk image size, if present
	CAST(unsigned)regs16[REG_AX] = *disk ? lseek(*disk, 0, 2) >> 9 : 0;
	// Load BIOS image into F000:0100, and set IP to 0100
	read(disk[2], regs8 + (reg_ip = 0x100), 0xFF00);

	// Instruction execution loop. Terminates if CS:IP = 0:0
	for (; opcode_stream = mem + 16 * regs16[REG_CS] + reg_ip, opcode_stream != mem;)
	{
		// Extract i_w, i_d, i_rm, i_reg, i_mod and i_data_xxx fields from instruction
		i_w = (i_reg4bit = *opcode_stream & 7) & 1;
		i_d = i_reg4bit / 2 & 1;
		scratch_int = io_ports[0x20] = 0;
		i_rm = (i_data0 = GET_NEXT_OPCODE) & 7;
		i_reg = i_data0 / 8 & 7;
		i_mod = opcode_stream[1] >> 6;

		i_data1 = i_mod == 1 ? (char)GET_NEXT_OPCODE : GET_NEXT_OPCODE;
		i_data2 = i_data1r = GET_NEXT_OPCODE;

		// 8086 instruction format is rather irregular!
		if ((!i_mod*i_rm != 6) && (i_mod != 2))
		{
			if (i_mod != 1)
				i_data2 = i_data1;
		}
		else
			i_data2 = GET_NEXT_OPCODE;

		// seg_override_en and rep_override_en hold segment override and REP prefix respectively if non-zero
		if (seg_override_en)
			seg_override_en--;
		if (rep_override_en)
			rep_override_en--;

		// PIT channel 0 data port - decrement timer
		--io_ports[0x40];
		DECODE_RM_REG;

		(
			// Convert raw opcode to translated opcode index. This condenses a large number of different encodings of similar
			// instructions into a much smaller number of distinct functions, which we then execute
			scratch_int = *opcode_stream,
			scratch_int = raw_opcode_id = bios_table_lookup(TABLE_OPCODE_LOOKUP),
			xlat_opcode_id = bios_table_lookup(TABLE_XLAT_OPCODE),
			extra = bios_table_lookup(TABLE_XLAT_SUBFUNCTION)

			// printf("raw_opcode_id = %d, xlat_opcode_id = %d\n", raw_opcode_id, xlat_opcode_id)

			// Instruction execution unit
			NEXT_OPCODE // Conditional jump (JAE, JNAE, etc.)
				scratch_int = *opcode_stream / 2 & 7,
				reg_ip += (char)i_data0 * (i_w ^ (regs8[bios_table_lookup(TABLE_COND_JUMP_DECODE_A)] | regs8[bios_table_lookup(TABLE_COND_JUMP_DECODE_B)] | regs8[bios_table_lookup(TABLE_COND_JUMP_DECODE_C)] ^ regs8[bios_table_lookup(TABLE_COND_JUMP_DECODE_D)]))
			NEXT_OPCODE // MOV reg, imm
				i_w = !!(*opcode_stream & 8),
				R_M_OP(mem[get_reg_addr(i_reg4bit)], =, i_data0)
			NEXT_OPCODE // INC/DEC reg16
				i_w = 1,
				i_d = 0,
				xlat_opcode_id += 3,
				i_reg = i_reg4bit,
				DECODE_RM_REG,
				i_reg = extra
			NEXT_OPCODE // PUSH reg16
				R_M_PUSH(i_reg4bit[regs16])
			NEXT_OPCODE // POP reg16
				R_M_POP(i_reg4bit[regs16])
			NEXT_OPCODE // INC/DEC/JMP/CALL/PUSH
				i_reg < 2 ? // INC/DEC
					MEM_OP(op_from_addr, += 1 - 2 * i_reg +, REGS_BASE + 24),
					op_source = 1,
					set_AF_OF_arith(),
					set_OF(op_dest + 1 - i_reg == 1 << TOP_BIT - 1),
					raw_opcode_id = raw_opcode_id & 4 ? 19 : 57
				:
					i_reg != 6 ? // JMP/CALL
						IP_RM_SIZE + 2, // JMP or CALL
						i_reg - 3 || R_M_PUSH(regs16[REG_CS]), // CALL (far)
						i_reg & 2 && R_M_PUSH(reg_ip), // CALL (near or far)
						i_reg & 1 && MEM_OP(REGS_BASE + 2 * REG_CS, =, op_from_addr + 2), // JMP or CALL (far)
						R_M_OP(reg_ip, =, op_from_addr[mem]), // JMP or CALL
						raw_opcode_id = 67 // Funge so we don't subsequently adjust IP by the instruction length
					:
						R_M_PUSH(mem[rm_addr]) // PUSH
			NEXT_OPCODE // TEST r/m, imm16 / NOT/NEG/MUL/IMUL/DIV/IDIV reg
			(
				op_to_addr = op_from_addr

				NEXT_OPCODE_SUBFUNCTION // TEST
					raw_opcode_id = extra,
					reg_ip += i_w + 1,
					R_M_OP(op_to_addr[mem], &, i_data2)
				NEXT_OPCODE_SUBFUNCTION 0 // Unused on 8086
				NEXT_OPCODE_SUBFUNCTION // NOT
					OP(=~)
				NEXT_OPCODE_SUBFUNCTION // NEG
					OP(=-),
					op_dest = 0,
					raw_opcode_id = 22, // Funge to set flags like SUB
					set_CF(op_result > op_dest)
				NEXT_OPCODE_SUBFUNCTION // MUL
				    i_w ? MUL_MACRO(unsigned short, regs16) : MUL_MACRO(unsigned char, regs8)
				NEXT_OPCODE_SUBFUNCTION // IMUL
				    i_w ? MUL_MACRO(short, regs16) : MUL_MACRO(char, regs8)
				NEXT_OPCODE_SUBFUNCTION // DIV
					i_w ? DIV_MACRO(unsigned short, unsigned, regs16) : DIV_MACRO(unsigned char, unsigned short, regs8)
				NEXT_OPCODE_SUBFUNCTION // IDIV
					i_w ? DIV_MACRO(short, int, regs16) : DIV_MACRO(char, short, regs8)
			)
			NEXT_OPCODE // ADD/OR/ADC/SBB/AND/SUB/XOR/CMP AL/AX, immed
				++xlat_opcode_id,
				rm_addr = REGS_BASE,
				i_data2 = i_data0,
				i_mod = 3,
				i_reg = extra,
				reg_ip--
			NEXT_OPCODE // ADD/OR/ADC/SBB/AND/SUB/XOR/CMP reg, immed
				++xlat_opcode_id, // The next NEXT_OPCODE block will do most of the work here
				op_to_addr = rm_addr,
				regs16[REG_SCRATCH] = (i_d |= !i_w) ? (char)i_data2 : i_data2,
				op_from_addr = REGS_BASE + 2 * REG_SCRATCH,
				reg_ip += !i_d + 1,
				raw_opcode_id = 17 + (extra = i_reg)
			NEXT_OPCODE // ADD/OR/ADC/SBB/AND/SUB/XOR/CMP/MOV reg, r/m
			(
				i_reg = extra

				NEXT_OPCODE_SUBFUNCTION // ADD
					OP(+=),
					set_CF(op_result < op_dest)
				NEXT_OPCODE_SUBFUNCTION // OR
					OP(|=)
				NEXT_OPCODE_SUBFUNCTION // ADC
					ADC_SBB_MACRO(+)
				NEXT_OPCODE_SUBFUNCTION // SBB
					ADC_SBB_MACRO(-)
				NEXT_OPCODE_SUBFUNCTION // AND
					OP(&=)
				NEXT_OPCODE_SUBFUNCTION // SUB
					OP(-=),
					set_CF(op_result > op_dest)
				NEXT_OPCODE_SUBFUNCTION // XOR
					OP(^=)
				NEXT_OPCODE_SUBFUNCTION // CMP
					OP(-),
					set_CF(op_result > op_dest)
				NEXT_OPCODE_SUBFUNCTION // MOV
					OP(=)
			)
			NEXT_OPCODE // MOV sreg, r/m | POP r/m | LEA reg, r/m
				!i_w ? // MOV
					i_w = 1,
					i_reg += 8,
					DECODE_RM_REG,
					OP(=)
				:
					!i_d ? // LEA
						seg_override_en = 1,
						seg_override = REG_ZERO,
						DECODE_RM_REG,
						R_M_OP(mem[scratch_uint], =, rm_addr)
					: // POP
						R_M_POP(mem[rm_addr])
			NEXT_OPCODE // MOV AL/AX, [loc]
				i_mod = i_reg = 0,
				i_rm = 6,
				i_data1 = i_data0,
				DECODE_RM_REG,
				MEM_OP(op_from_addr, =, op_to_addr)
			NEXT_OPCODE // ROL | ROR | RCL | RCR | SHL | SHR | ??? | SAR reg/mem, 1/CL/imm (80186)
			(
				scratch2_uint = SIGN_OF(mem[rm_addr]),
				scratch_uint = extra ? // xxx reg/mem, imm
					++reg_ip,
					(char)i_data1
				: // xxx reg/mem, CL
					i_d
						? 31 & regs8[REG_CL]
				: // xxx reg/mem, 1
					1
			) &&
			(
				i_reg < 4 ? // Rotate operations
						scratch_uint %= i_reg / 2 + TOP_BIT,
						R_M_OP(scratch2_uint, =, mem[rm_addr])
					: 0,
				i_reg & 1 ? // Rotate/shift right operations
					R_M_OP(mem[rm_addr], >>=, scratch_uint)
				: // Rotate/shift left operations
					R_M_OP(mem[rm_addr], <<=, scratch_uint),
				i_reg > 3 ? // Funge opcode ID to set SZP flags like an arithmetic operation
					raw_opcode_id = 19
				: 0,
				i_reg < 5 ? // Rotate operations or SHL
					0
				: // SHR and SAR
					set_CF(op_dest >> scratch_uint - 1 & 1)

				NEXT_OPCODE_SUBFUNCTION // ROL
					R_M_OP(mem[rm_addr], += , scratch2_uint >> TOP_BIT - scratch_uint),
					set_OF(SIGN_OF(op_result) ^ set_CF(op_result & 1))
				NEXT_OPCODE_SUBFUNCTION // ROR
					scratch2_uint &= (1 << scratch_uint) - 1,
					R_M_OP(mem[rm_addr], += , scratch2_uint << TOP_BIT - scratch_uint),
					set_OF(SIGN_OF(op_result * 2) ^ set_CF(SIGN_OF(op_result)))
				NEXT_OPCODE_SUBFUNCTION // RCL
					R_M_OP(mem[rm_addr], += (regs8[FLAG_CF] << scratch_uint - 1) + , scratch2_uint >> 1 + TOP_BIT - scratch_uint),
					set_OF(SIGN_OF(op_result) ^ set_CF(scratch2_uint & 1 << TOP_BIT - scratch_uint))
				NEXT_OPCODE_SUBFUNCTION // RCR
					R_M_OP(mem[rm_addr], += (regs8[FLAG_CF] << TOP_BIT - scratch_uint) + , scratch2_uint << 1 + TOP_BIT - scratch_uint),
					set_CF(scratch2_uint & 1 << scratch_uint - 1),
					set_OF(SIGN_OF(op_result) ^ SIGN_OF(op_result * 2))
				NEXT_OPCODE_SUBFUNCTION // SHL
					set_OF(SIGN_OF(op_result) ^ set_CF(SIGN_OF(op_dest << scratch_uint - 1)))
				NEXT_OPCODE_SUBFUNCTION // SHR
					set_OF(SIGN_OF(op_dest))
				NEXT_OPCODE_SUBFUNCTION 0 // Unused on 8086
				NEXT_OPCODE_SUBFUNCTION // SAR
					scratch_uint < TOP_BIT || set_CF(scratch2_uint),
					set_OF(0),
					R_M_OP(mem[rm_addr], +=, scratch2_uint *= ~((1 << TOP_BIT) - 1 >> scratch_uint))
			)
			NEXT_OPCODE // LOOPxx | JCZX
			(
				i_reg = i_reg4bit,
				scratch_uint = !!--regs16[REG_CX]

				NEXT_OPCODE_SUBFUNCTION  // LOOPNZ
					scratch_uint &= !regs8[FLAG_ZF]
				NEXT_OPCODE_SUBFUNCTION // LOOPZ
					scratch_uint &= regs8[FLAG_ZF]
				NEXT_OPCODE_SUBFUNCTION // LOOP
					0
				NEXT_OPCODE_SUBFUNCTION // JCXXZ
					scratch_uint = !++regs16[REG_CX]
			),
			reg_ip += scratch_uint*(char)i_data0
			NEXT_OPCODE // JMP | CALL short/near
				reg_ip += 3 - i_d,
				i_w ?
					0
				:
					i_d ? // JMP far
						reg_ip = 0,
						regs16[REG_CS] = i_data1r
					: R_M_PUSH(reg_ip), // CALL
				reg_ip += i_d*i_w ? (char)i_data0 : i_data0
			NEXT_OPCODE // TEST reg, r/m
				MEM_OP(op_from_addr, &, op_to_addr)
			NEXT_OPCODE // XCHG AX, reg16
				i_w = 1,
				xlat_opcode_id += 8,
				op_to_addr = REGS_BASE,
				op_from_addr = get_reg_addr(i_reg4bit)
			NEXT_OPCODE // MOVSx | STOSx | LODSx
				!rep_override_en || regs16[REG_CX] ? MEM_OP(extra<2 ? SEGREG(REG_ES, REG_DI, ) : REGS_BASE, = , extra & 1 ? REGS_BASE : SEGREG(seg_override_en ? seg_override : REG_DS, REG_SI, )), extra & 1 || index_inc(REG_SI), extra & 2 || string_op_support(1) : 0
			NEXT_OPCODE // CMPSx | SCASx
				!rep_override_en || regs16[REG_CX] ? MEM_OP(extra ? REGS_BASE : SEGREG(seg_override_en ? seg_override : REG_DS, REG_SI, ), -, SEGREG(REG_ES, REG_DI, )), raw_opcode_id = 92, regs8[FLAG_ZF] = !op_result, set_CF(op_result>op_dest), extra || index_inc(REG_SI), string_op_support(!op_result == rep_mode) : 0
			NEXT_OPCODE // RET | RETF | IRET
				i_d = i_w,
				R_M_POP(reg_ip),
				extra && // RETF | RETF imm16
					R_M_POP(regs16[REG_CS]),
				extra & 2 ? // IRET
					set_flags(R_M_POP(scratch_uint))
				: // RET | RETF imm16
					i_d || (regs16[REG_SP] += i_data0)
			NEXT_OPCODE // MOV r/m, immed
				R_M_OP(op_from_addr[mem], =, i_data2)
			NEXT_OPCODE // IN AL/AX, DX/imm8
				io_ports[0x3DA] ^= 9,
				R_M_OP(regs8[REG_AL], =, io_ports[extra ? regs16[REG_DX] : (char)i_data0])
			NEXT_OPCODE // OUT DX/imm8, AL/AX
				R_M_OP(io_ports[extra ? regs16[REG_DX] : (char)i_data0], =, regs8[REG_AL])
			NEXT_OPCODE // REPxx
				rep_override_en = 2,
				rep_mode = i_w,
				seg_override_en && seg_override_en++
			NEXT_OPCODE // XCHG reg, r/m | NOP
				op_to_addr != op_from_addr ?
					OP(^= ),
					MEM_OP(op_from_addr, ^= , op_to_addr),
					OP(^= )
				: 0
			NEXT_OPCODE // PUSH reg
				R_M_PUSH(regs16[extra])
			NEXT_OPCODE // POP reg
				R_M_POP(regs16[extra])
			NEXT_OPCODE // xS: segment overrides
				seg_override_en = 2,
				seg_override = extra,
				rep_override_en && rep_override_en++
			NEXT_OPCODE // DAA/DAS - these are implemented via look-up tables in the BIOS binary for speed/size reasons
				i_w = 0,
				scratch_int = regs8[REG_AL],
				set_CF(bios_table_lookup(extra += 3 * regs8[FLAG_AF] + 6 * regs8[FLAG_CF])), // extra = 27 for DAA, 39 for DAS
				set_AF(bios_table_lookup(1 + extra)),
				op_result = regs8[REG_AL] = bios_table_lookup(extra - 1)
			NEXT_OPCODE // AAA/AAS
				op_result = AAA_AAS(extra - 1)
			NEXT_OPCODE // CBW
				regs8[REG_AH] = -SIGN_OF(regs8[REG_AL])
			NEXT_OPCODE // CWD
				regs16[REG_DX] = -SIGN_OF(regs16[REG_AX])
			NEXT_OPCODE // CALL FAR imm16:imm16
				R_M_PUSH(regs16[REG_CS]),
				R_M_PUSH(reg_ip + 5),
				regs16[REG_CS] = i_data1r,
				reg_ip = i_data0
			NEXT_OPCODE // PUSHF
				make_flags(),
				R_M_PUSH(scratch_uint)
			NEXT_OPCODE // POPF
				set_flags(R_M_POP(scratch_uint))
			NEXT_OPCODE // SAHF
				make_flags(),
				set_flags((scratch_uint & 0xFF00) + regs8[REG_AH])
			NEXT_OPCODE // LAHF
				make_flags(),
				regs8[REG_AH] = scratch_uint
			NEXT_OPCODE // LES | LDS reg, r/m
				i_w = i_d = 1,
				DECODE_RM_REG,
				OP(=),
				MEM_OP(REGS_BASE + extra, =, rm_addr + 2)
			NEXT_OPCODE // INT 3
				++reg_ip,
				pc_interrupt(3)
			NEXT_OPCODE // INT imm8
				reg_ip += 2,
				pc_interrupt(i_data0 & 0xFF)
			NEXT_OPCODE // INTO
				++reg_ip,
				regs8[FLAG_OF] &&
					pc_interrupt(4)
			NEXT_OPCODE // AAM
				(i_data0 &= 0xFF) ?
					regs8[REG_AH] = regs8[REG_AL] / i_data0,
					op_result = regs8[REG_AL] %= i_data0
				: // Divide by zero
					pc_interrupt(0)
			NEXT_OPCODE // AAD
				i_w = 0,
				regs16[REG_AX] = op_result = 0xFF & regs8[REG_AL] + i_data0 * regs8[REG_AH]
			NEXT_OPCODE // SALC
				regs8[REG_AL] = -regs8[FLAG_CF]
			NEXT_OPCODE // XLAT
				regs8[REG_AL] = mem[SEGREG(seg_override_en ? seg_override : REG_DS, REG_BX, regs8[REG_AL] +)]
			NEXT_OPCODE // CMC
				regs8[FLAG_CF] ^= 1
			NEXT_OPCODE // CLC | STC | CLI | STI | CLD | STD
				regs8[extra / 2] = extra & 1
			NEXT_OPCODE // TEST AL/AX, immed
				R_M_OP(regs8[REG_AL], &, i_data0)
			NEXT_OPCODE // Emulator-specific 0F xx opcodes
			(
				i_reg = i_data0

				NEXT_OPCODE_SUBFUNCTION // PUTCHAR_AL
					write(1, regs8, 1)
				NEXT_OPCODE_SUBFUNCTION // GET_RTC
					time(disk + 3),
					memcpy(mem + SEGREG(REG_ES, REG_BX, ), localtime(disk + 3), sizeof(struct tm))
			),
			i_reg < 2 ? // READ_DISK | WRITE_DISK
				regs8[REG_AL] = ~lseek(scratch_int = disk[regs8[REG_BL]], CAST(unsigned)regs16[REG_BP] << 9, 0) ?
					((unsigned(*)())(i_reg ? write : read))(scratch_int, mem + SEGREG(REG_ES, REG_BX, ), regs16[REG_AX])
				: 0
			: 0
		);

		scratch_int = raw_opcode_id;

		// Increment instruction pointer by computed instruction length. Tables in the BIOS binary
		// help us here.
		IP_RM_SIZE*bios_table_lookup(TABLE_I_MOD_SIZE) + bios_table_lookup(TABLE_BASE_INST_SIZE) + bios_table_lookup(TABLE_I_W_SIZE)*(i_w + 1);

		// If instruction is an arithmetic or logic operation, set AF/OF/CF as appropriate.
		if (bios_table_lookup(TABLE_STD_FLAGS_ARITH))
			set_AF_OF_arith();
		else if (bios_table_lookup(TABLE_STD_FLAGS_LOGIC))
		{
			set_CF(0);
			set_OF(0);
		}
		// If instruction needs to update SF, ZF and PF, set them as appropriate
		if (bios_table_lookup(TABLE_STD_FLAGS_SZP))
		{
			scratch_int = extra = op_result;
			regs8[FLAG_SF] = SIGN_OF(op_result);
			regs8[FLAG_ZF] = !op_result;
			regs8[FLAG_PF] = bios_table_lookup(TABLE_PARITY_FLAG);
		}

		// Update the video graphics display when inst_counter wraps around, i.e. every 64K instructions
		if (!++inst_counter)
		{
			// Set flag to execute an INT 8 as soon as appropriate (see below)
			int8_asap = 1;
#ifndef NO_GRAPHICS
			if (io_ports[0x3B8] & 2)
			{
				// Hercules card is in graphics mode. If we don't already have an SDL window open, set it up
				SDL_PumpEvents();
				if (!sdl_screen)
					sdl_screen = SDL_SetVideoMode(GRAPHICS_X, GRAPHICS_Y, 32, 0);

				// Update the display from video RAM
				video_mem_update();
			}
			else if (sdl_screen) // Application has gone back to text mode, so close the SDL window
			{
				SDL_Quit();
				sdl_screen = 0;
			}
#endif
		}

		// Application has set trap flag, so fire INT 1
		if (trap_flag)
			pc_interrupt(1);

		trap_flag = regs8[FLAG_TF];

		// If an INT 8 is pending, and no segment overrides or REP prefixes are active, and IF is enabled, and TF is disabled,
		// then process the INT 8 and check for new keystrokes from the terminal
		if (!seg_override_en && !rep_override_en && int8_asap && regs8[FLAG_IF] && !regs8[FLAG_TF])
			KEYBOARD_TIMER_DRIVER;
	}

	return 0;
}
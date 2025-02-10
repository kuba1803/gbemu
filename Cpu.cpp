//
// Created by Jakub on 16.01.2025.
//

#include "Cpu.h"


CPU::Cpu::Cpu(BUS::Bus &bus): registers({0, 0, 0, 0, 0, 0, 0, 0}) {
    this->bus = &bus;
    cycleNumber = 0;
    cycleToExecute = 0;
}


template<CPU::OPERATION_TYPE type>
    requires (type == CPU::OPERATION_TYPE::ADD ||
              type == CPU::OPERATION_TYPE::ADC ||
              type == CPU::OPERATION_TYPE::SUB ||
              type == CPU::OPERATION_TYPE::SBC ||
              type == CPU::OPERATION_TYPE::AND ||
              type == CPU::OPERATION_TYPE::OR ||
              type == CPU::OPERATION_TYPE::XOR ||
              type == CPU::OPERATION_TYPE::CP ||
              type == CPU::OPERATION_TYPE::INC ||
              type == CPU::OPERATION_TYPE::DEC)
constexpr uint8_t arithmetic_8(const uint8_t x, const uint8_t y, CPU::Registers &registers) {
    uint16_t result = 0;
    if constexpr (type == CPU::OPERATION_TYPE::ADD) {
        result = x + y;
        registers.h = (x & 0x0F) + (y & 0x0F) > 0x0F;
        registers.n = false;
        registers.cy = result > 0xFF;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::ADC) {
        result = x + y + registers.cy;
        registers.h = (x & 0x0F) + (y & 0x0F) + registers.cy > 0x0F;
        registers.n = false;
        registers.cy = result > 0xFF;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SUB) {
        result = x - y;
        registers.h = (x & 0x0F) < (y & 0x0F);
        registers.n = true;
        registers.cy = x < y;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SBC) {
        result = x - y - registers.cy;
        registers.h = (x & 0x0F) < (y & 0x0F) + registers.cy;
        registers.n = true;
        registers.cy = x < y + registers.cy;
        registers.z = result == 0;
    }
    if constexpr (type == CPU::OPERATION_TYPE::AND) {
        result = x & y;
        registers.h = true;
        registers.n = false;
        registers.cy = false;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::OR) {
        result = x | y;
        registers.h = false;
        registers.n = false;
        registers.cy = false;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::XOR) {
        result = x ^ y;
        registers.h = false;
        registers.n = false;
        registers.cy = false;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::CP) {
        result = x;
        registers.h = (x & 0x0F) < (y & 0x0F);
        registers.n = true;
        registers.cy = x < y;
        registers.z = x - y == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::INC) {
        result = x + 1;
        registers.h = (x & 0x0F) + 1 > 0x0F;
        registers.n = false;
        registers.cy = result > 0xFF;
        registers.z = x - y == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::DEC) {
        result = x - 1;
        registers.h = (x & 0x0F) < 1;
        registers.n = true;
        registers.cy = x < 1;
        registers.z = x - y == 0;
    }
    return static_cast<uint8_t>(result);
}

template<CPU::OPERATION_TYPE type>
    requires (type == CPU::OPERATION_TYPE::ADD ||
              type == CPU::OPERATION_TYPE::INC ||
              type == CPU::OPERATION_TYPE::DEC)
constexpr uint16_t arithmetic_16(const uint16_t x, const uint16_t y, CPU::Registers &registers) {
    uint32_t result = 0;

    if constexpr (type == CPU::OPERATION_TYPE::ADD) {
        result = x + y;
        registers.h = (x & 0x0FFF) + (y & 0x0FFF) > 0x0FFF;
        registers.n = false;
        registers.cy = result > 0xFFFF;
    }

    if constexpr (type == CPU::OPERATION_TYPE::INC) {
        result = x + 1;
    }

    if constexpr (type == CPU::OPERATION_TYPE::DEC) {
        result = x - 1;
    }

    return static_cast<uint16_t>(result);
}

template<CPU::OPERATION_TYPE type>
    requires (type == CPU::OPERATION_TYPE::RLC ||
              type == CPU::OPERATION_TYPE::RL ||
              type == CPU::OPERATION_TYPE::RRC ||
              type == CPU::OPERATION_TYPE::RR ||
              type == CPU::OPERATION_TYPE::SLA ||
              type == CPU::OPERATION_TYPE::SRA ||
              type == CPU::OPERATION_TYPE::SRL ||
              type == CPU::OPERATION_TYPE::SWAP)
constexpr uint8_t shift_rotation(const uint8_t x, CPU::Registers &registers) {
    uint16_t result = 0;

    if constexpr (type == CPU::OPERATION_TYPE::RLC) {
        result = x << 1;
        registers.cy = result > 0xFF;
        result = result + registers.cy;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RL) {
        result = x << 1;
        result = result + registers.cy;
        registers.cy = result > 0xFF;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RRC) {
        registers.cy = x & 1;
        result = x >> 1;
        result = result + registers.cy << 7;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RR) {
        result = x + registers.cy << 8;
        registers.cy = x & 1;
        result = result >> 1;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SLA) {
        result = x << 1;
        registers.cy = result > 0xFF;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SRA) {
        registers.cy = x & 1;
        result = x >> 1 + x & 80;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SRL) {
        registers.cy = x & 1;
        result = x >> 1;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SWAP) {
        result = x >> 4 + x << 4;
        registers.z = result && 0xFF == 0;
    }

    return static_cast<uint8_t>(result);
}

template<CPU::OPERATION_TYPE type>
    requires (type == CPU::OPERATION_TYPE::BIT ||
              type == CPU::OPERATION_TYPE::SET ||
              type == CPU::OPERATION_TYPE::RES)
constexpr uint8_t bit_operation(const uint8_t n, const uint8_t &x, CPU::Registers &registers) {
    uint8_t value = x;
    if constexpr (type == CPU::OPERATION_TYPE::BIT) {
        registers.n = false;
        registers.h = false;
        registers.z = (x & (1 << (n - 1))) == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SET) {
        value = x | (1 << (n - 1));
    }

    if constexpr (type == CPU::OPERATION_TYPE::RES) {
        value = x & ~(1 << (n - 1));
    }

    return value;
}

void CPU::Cpu::executeCycle() {
    std::cout << "Execute Cycle:" << cycleNumber << "\n";
    uint16_t helpVariable16;
    uint8_t helpVariable8;
    uint8_t instruction;
    if (!cycleToExecute) {
        instruction = bus->read(PC);
        switch (instruction) {
#pragma region 8_BIT_TRANSFER
            //LD A A
            case 0b01111111:
                ++PC;
                break;
            //LD A B
            case 0b01111000:
                registers.reg_8[A] = registers.reg_8[B];
                break;
            //LD A C
            case 0b01111001:
                registers.reg_8[A] = registers.reg_8[C];
                ++PC;
                break;
            //LD A D
            case 0b01111010:
                registers.reg_8[A] = registers.reg_8[D];
                ++PC;
                break;
            //LD A E
            case 0b01111011:
                registers.reg_8[A] = registers.reg_8[E];
                ++PC;
                break;
            //LD A H
            case 0b01111100:
                registers.reg_8[A] = registers.reg_8[H];
                ++PC;
                break;
            //LD A L
            case 0b01111101:
                registers.reg_8[A] = registers.reg_8[L];
                ++PC;
                break;
            //LD B A
            case 0b01000111:
                registers.reg_8[B] = registers.reg_8[A];
                ++PC;
                break;
            //LD B B
            case 0b01000000:
                registers.reg_8[B] = registers.reg_8[B];
                ++PC;
                break;
            //LD B C
            case 0b01000001:
                registers.reg_8[B] = registers.reg_8[C];
                ++PC;
                break;
            //LD B D
            case 0b01000010:
                registers.reg_8[B] = registers.reg_8[D];
                ++PC;
                break;
            //LD B E
            case 0b01000011:
                registers.reg_8[B] = registers.reg_8[E];
                ++PC;
                break;
            //LD B H
            case 0b01000100:
                registers.reg_8[B] = registers.reg_8[H];
                ++PC;
                break;
            //LD B L
            case 0b01000101:
                registers.reg_8[B] = registers.reg_8[L];
                ++PC;
                break;
            //LD C A
            case 0b01001111:
                registers.reg_8[C] = registers.reg_8[A];
                ++PC;
                break;
            //LD C B
            case 0b01001000:
                registers.reg_8[C] = registers.reg_8[B];
                ++PC;
                break;
            //LD C C
            case 0b01001001:
                registers.reg_8[C] = registers.reg_8[C];
                ++PC;
                break;
            //LD C D
            case 0b01001010:
                registers.reg_8[C] = registers.reg_8[D];
                ++PC;
                break;
            //LD C E
            case 0b01001011:
                registers.reg_8[C] = registers.reg_8[E];
                ++PC;
                break;
            //LD C H
            case 0b01001100:
                registers.reg_8[C] = registers.reg_8[H];
                ++PC;
                break;
            //LD C L
            case 0b01001101:
                registers.reg_8[C] = registers.reg_8[L];
                ++PC;
                break;
            //LD D A
            case 0b01010111:
                registers.reg_8[D] = registers.reg_8[A];
                ++PC;
                break;
            //LD D B
            case 0b01010000:
                registers.reg_8[D] = registers.reg_8[B];
                ++PC;
                break;
            //LD D C
            case 0b01010001:
                registers.reg_8[D] = registers.reg_8[C];
                ++PC;
                break;
            //LD D D
            case 0b01010010:
                registers.reg_8[D] = registers.reg_8[D];
                ++PC;
                break;
            //LD D E
            case 0b01010011:
                registers.reg_8[D] = registers.reg_8[E];
                ++PC;
                break;
            //LD D H
            case 0b01010100:
                registers.reg_8[D] = registers.reg_8[H];
                ++PC;
                break;
            //LD D L
            case 0b01010101:
                registers.reg_8[D] = registers.reg_8[L];
                ++PC;
                break;
            //LD E A
            case 0b01011111:
                registers.reg_8[E] = registers.reg_8[A];
                ++PC;
                break;
            //LD E B
            case 0b01011000:
                registers.reg_8[E] = registers.reg_8[B];
                ++PC;
                break;
            //LD E C
            case 0b01011001:
                registers.reg_8[E] = registers.reg_8[C];
                ++PC;
                break;
            //LD E D
            case 0b01011010:
                registers.reg_8[E] = registers.reg_8[D];
                ++PC;
                break;
            //LD E E
            case 0b01011011:
                registers.reg_8[E] = registers.reg_8[E];
                ++PC;
                break;
            //LD E H
            case 0b01011100:
                registers.reg_8[E] = registers.reg_8[H];
                ++PC;
                break;
            //LD E L
            case 0b01011101:
                registers.reg_8[E] = registers.reg_8[L];
                ++PC;
                break;
            //LD H A
            case 0b01100111:
                registers.reg_8[H] = registers.reg_8[A];
                ++PC;
                break;
            //LD H B
            case 0b01100000:
                registers.reg_8[H] = registers.reg_8[B];
                ++PC;
                break;
            //LD H C
            case 0b01100001:
                registers.reg_8[H] = registers.reg_8[C];
                ++PC;
                break;
            //LD H D
            case 0b01100010:
                registers.reg_8[H] = registers.reg_8[D];
                ++PC;
                break;
            //LD H E
            case 0b01100011:
                registers.reg_8[H] = registers.reg_8[E];
                ++PC;
                break;
            //LD H H
            case 0b01100100:
                registers.reg_8[H] = registers.reg_8[H];
                ++PC;
                break;
            //LD H L
            case 0b01100101:
                registers.reg_8[H] = registers.reg_8[L];
                ++PC;
                break;
            //LD L A
            case 0b01101111:
                registers.reg_8[L] = registers.reg_8[A];
                ++PC;
                break;
            //LD L B
            case 0b01101000:
                registers.reg_8[L] = registers.reg_8[B];
                ++PC;
                break;
            //LD L C
            case 0b01101001:
                registers.reg_8[L] = registers.reg_8[C];
                ++PC;
                break;
            //LD L D
            case 0b01101010:
                registers.reg_8[L] = registers.reg_8[D];
                ++PC;
                break;
            //LD L E
            case 0b01101011:
                registers.reg_8[L] = registers.reg_8[E];
                ++PC;
                break;
            //LD L H
            case 0b01101100:
                registers.reg_8[L] = registers.reg_8[H];
                ++PC;
                break;
            //LD L L
            case 0b01101101:
                registers.reg_8[L] = registers.reg_8[L];
                ++PC;
                break;
            //LD A n
            case 0b00111110:
                ++PC;
                registers.reg_8[A] = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD B n
            case 0b00000110:
                ++PC;
                registers.reg_8[B] = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD C n
            case 0b00001110:
                ++PC;
                registers.reg_8[C] = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD D n
            case 0b00010110:
                ++PC;
                registers.reg_8[D] = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD E n
            case 0b00011110:
                ++PC;
                registers.reg_8[E] = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD H n
            case 0b00100110:
                ++PC;
                registers.reg_8[H] = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD L n
            case 0b00101110:
                ++PC;
                registers.reg_8[L] = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD A (HL)
            case 0b01111110:
                registers.reg_8[A] = bus->read(registers.HL);
                ++cycleToExecute;
                ++PC;
                break;
            //LD B (HL)
            case 0b01000110:
                registers.reg_8[B] = bus->read(registers.HL);
                ++cycleToExecute;
                ++PC;
                break;
            //LD C (HL)
            case 0b01001110:
                registers.reg_8[C] = bus->read(registers.HL);
                ++cycleToExecute;
                ++PC;
                break;
            //LD D (HL)
            case 0b01010110:
                registers.reg_8[D] = bus->read(registers.HL);
                ++cycleToExecute;
                ++PC;
                break;
            //LD E (HL)
            case 0b01011110:
                registers.reg_8[E] = bus->read(registers.HL);
                ++cycleToExecute;
                ++PC;
                break;
            //LD H (HL)
            case 0b01100110:
                registers.reg_8[H] = bus->read(registers.HL);
                ++cycleToExecute;
                ++PC;
                break;
            //LD L (HL)
            case 0b01101110:
                registers.reg_8[L] = bus->read(registers.HL);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) A
            case 0b01110111:
                bus->write(registers.HL, A);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) B
            case 0b01110000:
                bus->write(registers.HL, B);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) C
            case 0b01110001:
                bus->write(registers.HL, C);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) D
            case 0b01110010:
                bus->write(registers.HL, D);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) E
            case 0b01110011:
                bus->write(registers.HL, E);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) H
            case 0b01110100:
                bus->write(registers.HL, H);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) L
            case 0b01110101:
                bus->write(registers.HL, L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) n
            case 0b00110110:
                ++PC;
                bus->write(registers.HL, bus->read(PC));
                cycleToExecute += 2;
                ++PC;
            //LD A (BC)
            case 0b00001010:
                registers.reg_8[A] = bus->read(registers.BC);
                ++cycleToExecute;
                ++PC;
            //LD A (DE)
            case 0b00011010:
                registers.reg_8[A] = bus->read(registers.DE);
                ++cycleToExecute;
                ++PC;
            //LD A (C)
            case 0b11110010:
                registers.reg_8[A] = bus->read(0xFF00 | C);
                ++cycleToExecute;
                ++PC;
            //LD (C) A
            case 0b11100010:
                bus->write(0xFF00 | C, A);
                ++cycleToExecute;
                ++PC;
            //LD A (n)
            case 0b11110000:
                ++PC;
                registers.reg_8[A] = bus->read(0xFF00 | bus->read(PC));
                cycleToExecute += 2;
                ++PC;
            //LD (n) A
            case 0b11100000:
                ++PC;
                bus->write(0xFF00 | bus->read(PC), A);
                cycleToExecute += 2;
                ++PC;
            //LD A (nn)
            case 0b11111010:
                ++PC;
                registers.reg_8[A] = bus->read(bus->read(PC << 8 | bus->read(PC + 1)));
                PC += 2;
                cycleToExecute += 4;
            //LD (nn) A
            case 0b11101010:
                ++PC;
                bus->write(bus->read(PC << 8 | bus->read(PC + 1)), A);
                PC += 2;
                cycleToExecute += 4;
            //LD A (HLI)
            case 0b00101010:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = bus->read(helpVariable16);
                ++helpVariable16;
                registers.HL = helpVariable16;
                ++PC;
                ++cycleToExecute;
            //LD A (HLD)
            case 0b00111010:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = bus->read(helpVariable16);
                --helpVariable16;
                registers.HL = helpVariable16;
                ++PC;
                ++cycleToExecute;
            //LD (BC) A
            case 0b00000010:
                bus->write(registers.BC, A);
                ++cycleToExecute;
                ++PC;
            //LD (DE) A
            case 0b00010010:
                bus->write(registers.DE, A);
                ++cycleToExecute;
                ++PC;
            //LD (HLI) A
            case 0b00100010:
                helpVariable16 = registers.HL;
                bus->write(helpVariable16, A);
                ++helpVariable16;
                registers.HL = helpVariable16;
                ++PC;
                ++cycleToExecute;
            case 0b00110010:
                //LD (HLD) A
                helpVariable16 = registers.HL;
                bus->write(helpVariable16, A);
                --helpVariable16;
                registers.HL = helpVariable16;
                ++PC;
                ++cycleToExecute;
#pragma endregion 8_BIT_TRANSFER
#pragma region 16_BIT_TRANSFER
            // LM BC nn
            case 0b00000001:
                ++PC;
                registers.reg_8[B] = bus->read(bus->read(PC));
                ++PC;
                registers.reg_8[C] = bus->read(bus->read(PC));
                ++PC;
                cycleNumber += 2;
                break;
            // LM DE nn
            case 0b00010001:
                ++PC;
                registers.reg_8[E] = bus->read(bus->read(PC));
                ++PC;
                registers.reg_8[D] = bus->read(bus->read(PC));
                ++PC;
                cycleNumber += 2;
                break;
            // LM HL nn
            case 0b00100001:
                ++PC;
                registers.reg_8[H] = bus->read(bus->read(PC));
                ++PC;
                registers.reg_8[L] = bus->read(bus->read(PC));
                ++PC;
                cycleNumber += 2;
                break;
            // LM SP nn
            case 0b00110001:
                ++PC;
                SP = bus->read(bus->read(PC)) | bus->read(bus->read(PC + 1) << 8);
                PC += 2;
                cycleNumber += 2;
                break;
            //LD SP HL
            case 0b011111001:
                SP = registers.HL;
                PC += 2;
                ++cycleNumber;
                break;
            //PUSH BC
            case 0b11000101:
                --SP;
                bus->write(SP, B);
                --SP;
                bus->write(SP, C);
                ++PC;
                cycleToExecute += 3;
                break;
            //PUSH DE
            case 0b11010101:
                --SP;
                bus->write(SP, D);
                --SP;
                bus->write(SP, E);
                ++PC;
                cycleToExecute += 3;
                break;
            //PUSH HL
            case 0b11100101:
                --SP;
                bus->write(SP, H);
                --SP;
                bus->write(SP, L);
                ++PC;
                cycleToExecute += 3;
                break;
            //PUSH AF
            case 0b11110101:
                --SP;
                bus->write(SP, A);
                --SP;
                bus->write(SP, registers.reg_8[F]);
                ++PC;
                cycleToExecute += 3;
                break;
            //POP BC
            case 0b11000001:
                registers.reg_8[C] = bus->read(bus->read(SP));
                ++SP;
                registers.reg_8[B] = bus->read(bus->read(SP));
                ++SP;
                ++PC;
                cycleToExecute += 2;
                break;
            //POP DE
            case 0b11010001:
                registers.reg_8[E] = bus->read(bus->read(SP));
                ++SP;
                registers.reg_8[D] = bus->read(bus->read(SP));
                ++SP;
                ++PC;
                cycleToExecute += 2;
                break;
            //POP HL
            case 0b11100001:
                registers.reg_8[L] = bus->read(bus->read(SP));
                ++SP;
                registers.reg_8[H] = bus->read(bus->read(SP));
                ++SP;
                ++PC;
                cycleToExecute += 2;
                break;
            //POP AF
            case 0b11110001:
                registers.reg_8[F] = bus->read(bus->read(SP));
                ++SP;
                registers.reg_8[A] = bus->read(bus->read(SP));
                ++SP;
                ++PC;
                cycleToExecute += 2;
                break;
            //LDHL SP e
            case 0b11111000:
                helpVariable8 = bus->read(bus->read(PC));
                helpVariable16 = SP + static_cast<int8_t>(helpVariable8);
                registers.reg_8[F] = (SP > helpVariable16) << 4 | ((SP & 0x0F) + (helpVariable8 & 0x0F) > 0x0F) << 5;
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                cycleToExecute += 2;
            //LD (nn) SP
            case 0b00001000:
                ++PC;
                bus->write(bus->read(PC), SP & 0xFF);
                ++PC;
                bus->write(bus->read(PC), SP >> 8);
                ++PC;
                cycleToExecute += 4;
#pragma endregion 16_BIT_TRANSFER
#pragma region 8_BIT_ARITHMETIC
            //ADD A A
            case 0b10000111:
                registers.reg_8[A] = arithmetic_8<ADD>(A, A, registers);
                ++PC;
                break;
            //ADD A B
            case 0b10000000:
                registers.reg_8[A] = arithmetic_8<ADD>(A, B, registers);
                ++PC;
                break;
            //ADD A C
            case 0b10000001:
                registers.reg_8[A] = arithmetic_8<ADD>(A, C, registers);
                ++PC;
                break;
            //ADD A D
            case 0b10000010:
                registers.reg_8[A] = arithmetic_8<ADD>(A, D, registers);
                ++PC;
                break;
            //ADD A E
            case 0b10000011:
                registers.reg_8[A] = arithmetic_8<ADD>(A, E, registers);
                ++PC;
                break;
            //ADD A H
            case 0b10000100:
                registers.reg_8[A] = arithmetic_8<ADD>(A, H, registers);
                ++PC;
                break;
            //ADD A L
            case 0b10000101:
                registers.reg_8[A] = arithmetic_8<ADD>(A, L, registers);
                ++PC;
                break;
            //ADD A n
            case 0b11000110:
                ++PC;
                registers.reg_8[A] = arithmetic_8<ADD>(A, bus->read(PC), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //ADD A (HL)
            case 0b10000110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<ADD>(A, bus->read(helpVariable16), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //ADC A A
            case 0b10001111:
                registers.reg_8[A] = arithmetic_8<ADC>(A, A, registers);
                ++PC;
                break;
            //ADC A B
            case 0b10001000:
                registers.reg_8[A] = arithmetic_8<ADC>(A, B, registers);
                ++PC;
                break;
            //ADC A C
            case 0b10001001:
                registers.reg_8[A] = arithmetic_8<ADC>(A, C, registers);
                ++PC;
                break;
            //ADC A D
            case 0b10001010:
                registers.reg_8[A] = arithmetic_8<ADC>(A, D, registers);
                ++PC;
                break;
            //ADC A E
            case 0b10001011:
                registers.reg_8[A] = arithmetic_8<ADC>(A, E, registers);
                ++PC;
                break;
            //ADC A H
            case 0b10001100:
                registers.reg_8[A] = arithmetic_8<ADC>(A, H, registers);
                ++PC;
                break;
            //ADC A L
            case 0b10001101:
                registers.reg_8[A] = arithmetic_8<ADC>(A, L, registers);
                ++PC;
                break;
            //ADC A n
            case 0b11001110:
                ++PC;
                registers.reg_8[A] = arithmetic_8<ADC>(A, bus->read(PC), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //ADC A (HL)
            case 0b10001110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<ADC>(A, bus->read(helpVariable16), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //SUB A A
            case 0b10010111:
                registers.reg_8[A] = arithmetic_8<SUB>(A, A, registers);
                ++PC;
                break;
            //SUB A B
            case 0b10010000:
                registers.reg_8[A] = arithmetic_8<SUB>(A, B, registers);
                ++PC;
                break;
            //SUB A C
            case 0b10010001:
                registers.reg_8[A] = arithmetic_8<SUB>(A, C, registers);
                ++PC;
                break;
            //SUB A D
            case 0b10010010:
                registers.reg_8[A] = arithmetic_8<SUB>(A, D, registers);
                ++PC;
                break;
            //SUB A E
            case 0b10010011:
                registers.reg_8[A] = arithmetic_8<SUB>(A, E, registers);
                ++PC;
                break;
            //SUB A H
            case 0b10010100:
                registers.reg_8[A] = arithmetic_8<SUB>(A, H, registers);
                ++PC;
                break;
            //SUB A L
            case 0b10010101:
                registers.reg_8[A] = arithmetic_8<SUB>(A, L, registers);
                ++PC;
                break;
            //SUB A n
            case 0b11010110:
                ++PC;
                registers.reg_8[A] = arithmetic_8<SUB>(A, bus->read(PC), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //SUB A (HL)
            case 0b10010110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<SUB>(A, bus->read(helpVariable16), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //SBC A A
            case 0b10011111:
                registers.reg_8[A] = arithmetic_8<SBC>(A, A, registers);
                ++PC;
                break;
            //SBC A B
            case 0b10011000:
                registers.reg_8[A] = arithmetic_8<SBC>(A, B, registers);
                ++PC;
                break;
            //SBC A C
            case 0b10011001:
                registers.reg_8[A] = arithmetic_8<SBC>(A, C, registers);
                ++PC;
                break;
            //SBC A D
            case 0b10011010:
                registers.reg_8[A] = arithmetic_8<SBC>(A, D, registers);
                ++PC;
                break;
            //SBC A E
            case 0b10011011:
                registers.reg_8[A] = arithmetic_8<SBC>(A, E, registers);
                ++PC;
                break;
            //SBC A H
            case 0b10011100:
                registers.reg_8[A] = arithmetic_8<SBC>(A, H, registers);
                ++PC;
                break;
            //SBC A L
            case 0b10011101:
                registers.reg_8[A] = arithmetic_8<SBC>(A, L, registers);
                ++PC;
                break;
            //SBC A n
            case 0b11011110:
                ++PC;
                registers.reg_8[A] = arithmetic_8<SBC>(A, bus->read(PC), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //SBC A (HL)
            case 0b10011110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<SUB>(A, bus->read(helpVariable16), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //AND A A
            case 0b10100111:
                registers.reg_8[A] = arithmetic_8<AND>(A, A, registers);
                ++PC;
                break;
            //AND A B
            case 0b10100000:
                registers.reg_8[A] = arithmetic_8<AND>(A, B, registers);
                ++PC;
                break;
            //AND A C
            case 0b10100001:
                registers.reg_8[A] = arithmetic_8<AND>(A, C, registers);
                ++PC;
                break;
            //AND A D
            case 0b10100010:
                registers.reg_8[A] = arithmetic_8<AND>(A, D, registers);
                ++PC;
                break;
            //AND A E
            case 0b10100011:
                registers.reg_8[A] = arithmetic_8<AND>(A, E, registers);
                ++PC;
                break;
            //AND A H
            case 0b10100100:
                registers.reg_8[A] = arithmetic_8<AND>(A, H, registers);
                ++PC;
                break;
            //AND A L
            case 0b10100101:
                registers.reg_8[A] = arithmetic_8<AND>(A, L, registers);
                ++PC;
                break;
            //AND A n
            case 0b11100110:
                ++PC;
                registers.reg_8[A] = arithmetic_8<AND>(A, bus->read(PC), registers);
                registers.reg_8[A] &= bus->read(PC);
                ++PC;
                ++cycleToExecute;
                break;
            //AND A (HL)
            case 0b10100110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<AND>(A, bus->read(helpVariable16), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //OR A A
            case 0b10110111:
                registers.reg_8[A] = arithmetic_8<OR>(A, A, registers);
                ++PC;
                break;
            //OR A B
            case 0b10110000:
                registers.reg_8[A] = arithmetic_8<OR>(A, B, registers);
                ++PC;
                break;
            //OR A C
            case 0b10110001:
                registers.reg_8[A] = arithmetic_8<OR>(A, C, registers);
                ++PC;
                break;
            //OR A D
            case 0b10110010:
                registers.reg_8[A] = arithmetic_8<OR>(A, D, registers);
                ++PC;
                break;
            //OR A E
            case 0b10110011:
                registers.reg_8[A] = arithmetic_8<OR>(A, E, registers);
                ++PC;
                break;
            //OR A H
            case 0b10110100:
                registers.reg_8[A] = arithmetic_8<OR>(A, H, registers);
                ++PC;
                break;
            //OR A L
            case 0b10110101:
                registers.reg_8[A] = arithmetic_8<OR>(A, L, registers);
                ++PC;
                break;
            //OR A n
            case 0b11110110:
                ++PC;
                registers.reg_8[A] = arithmetic_8<OR>(A, bus->read(PC), registers);
                registers.reg_8[A] &= bus->read(PC);
                ++PC;
                ++cycleToExecute;
                break;
            //OR A (HL)
            case 0b10110110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<OR>(A, bus->read(helpVariable16), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //XOR A A
            case 0b10101111:
                registers.reg_8[A] = arithmetic_8<XOR>(A, A, registers);
                ++PC;
                break;
            //XOR A B
            case 0b10101000:
                registers.reg_8[A] = arithmetic_8<XOR>(A, B, registers);
                ++PC;
                break;
            //XOR A C
            case 0b10101001:
                registers.reg_8[A] = arithmetic_8<XOR>(A, C, registers);
                ++PC;
                break;
            //XOR A D
            case 0b10101010:
                registers.reg_8[A] = arithmetic_8<XOR>(A, D, registers);
                ++PC;
                break;
            //XOR A E
            case 0b10101011:
                registers.reg_8[A] = arithmetic_8<XOR>(A, E, registers);
                ++PC;
                break;
            //XOR A H
            case 0b10101100:
                registers.reg_8[A] = arithmetic_8<XOR>(A, H, registers);
                ++PC;
                break;
            //XOR A L
            case 0b10101101:
                registers.reg_8[A] = arithmetic_8<XOR>(A, L, registers);
                ++PC;
                break;
            //XOR A n
            case 0b11101110:
                ++PC;
                registers.reg_8[A] = arithmetic_8<XOR>(A, bus->read(PC), registers);
                registers.reg_8[A] &= bus->read(PC);
                ++PC;
                ++cycleToExecute;
                break;
            //XOR A (HL)
            case 0b10101110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<XOR>(A, bus->read(helpVariable16), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //CP A A
            case 0b10111111:
                registers.reg_8[A] = arithmetic_8<CP>(A, A, registers);
                ++PC;
                break;
            //CP A B
            case 0b10111000:
                registers.reg_8[A] = arithmetic_8<CP>(A, B, registers);
                ++PC;
                break;
            //CP A C
            case 0b10111001:
                registers.reg_8[A] = arithmetic_8<CP>(A, C, registers);
                ++PC;
                break;
            //CP A D
            case 0b10111010:
                registers.reg_8[A] = arithmetic_8<CP>(A, D, registers);
                ++PC;
                break;
            //CP A E
            case 0b10111011:
                registers.reg_8[A] = arithmetic_8<CP>(A, E, registers);
                ++PC;
                break;
            //CP A H
            case 0b10111100:
                registers.reg_8[A] = arithmetic_8<CP>(A, H, registers);
                ++PC;
                break;
            //CP A L
            case 0b10111101:
                registers.reg_8[A] = arithmetic_8<CP>(A, L, registers);
                ++PC;
                break;
            //CP A n
            case 0b11111110:
                ++PC;
                registers.reg_8[A] = arithmetic_8<CP>(A, bus->read(PC), registers);
                registers.reg_8[A] &= bus->read(PC);
                ++PC;
                ++cycleToExecute;
                break;
            //CP A (HL)
            case 0b10111110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<CP>(A, bus->read(helpVariable16), registers);
                ++PC;
                ++cycleToExecute;
                break;
            //INC A
            case 0b00111100:
                registers.reg_8[A] = arithmetic_8<INC>(A, 1, registers);
                ++PC;
                break;
            //INC B
            case 0b00000100:
                registers.reg_8[B] = arithmetic_8<INC>(B, 1, registers);
                ++PC;
                break;
            //INC C
            case 0b00001100:
                registers.reg_8[C] = arithmetic_8<INC>(C, 1, registers);
                ++PC;
                break;
            //INC D
            case 0b00010100:
                registers.reg_8[D] = arithmetic_8<INC>(D, 1, registers);
                ++PC;
                break;
            //INC E
            case 0b00011100:
                registers.reg_8[E] = arithmetic_8<INC>(E, 1, registers);
                ++PC;
                break;
            //INC H
            case 0b00100100:
                registers.reg_8[H] = arithmetic_8<INC>(H, 1, registers);
                ++PC;
                break;
            //INC L
            case 0b00101100:
                registers.reg_8[L] = arithmetic_8<INC>(L, 1, registers);
                ++PC;
                break;
            //INC (HL)
            case 0b00110100:
                helpVariable16 = registers.HL;
                bus->write(helpVariable16, arithmetic_8<INC>(bus->read(helpVariable16), 1, registers));
                cycleToExecute += 2;
                ++PC;
                break;
            //DEC A
            case 0b00111101:
                registers.reg_8[A] = arithmetic_8<DEC>(A, 1, registers);
                ++PC;
                break;
            //DEC B
            case 0b00000101:
                registers.reg_8[B] = arithmetic_8<DEC>(B, 1, registers);
                ++PC;
                break;
            //DEC C
            case 0b00001101:
                registers.reg_8[C] = arithmetic_8<DEC>(C, 1, registers);
                ++PC;
                break;
            //DEC D
            case 0b00010101:
                registers.reg_8[D] = arithmetic_8<DEC>(D, 1, registers);
                ++PC;
                break;
            //DEC E
            case 0b00011101:
                registers.reg_8[E] = arithmetic_8<DEC>(E, 1, registers);
                ++PC;
                break;
            //DEC H
            case 0b00100101:
                registers.reg_8[H] = arithmetic_8<DEC>(H, 1, registers);
                ++PC;
                break;
            //DEC L
            case 0b00101101:
                registers.reg_8[L] = arithmetic_8<DEC>(L, 1, registers);
                ++PC;
                break;
            //DEC (HL)
            case 0b00110101:
                helpVariable16 = registers.HL;
                bus->write(helpVariable16, arithmetic_8<DEC>(bus->read(helpVariable16), 1, registers));
                cycleToExecute += 2;
                ++PC;
                break;


#pragma endregion 8_BIT_ARITHMETIC
#pragma region 16_BIT_ARITHMETIC
            // ADD HL BC
            case 0b00001001:
                helpVariable16 = arithmetic_16<ADD>(registers.HL, registers.BC, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            // ADD HL DE
            case 0b00011001:
                helpVariable16 = arithmetic_16<ADD>(registers.HL, registers.DE, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            // ADD HL HL
            case 0b00101001:
                helpVariable16 = registers.HL;
                helpVariable16 = arithmetic_16<ADD>(helpVariable16, helpVariable16, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            // ADD HL SP
            case 0b00111001:
                helpVariable16 = arithmetic_16<ADD>(registers.HL, SP, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //ADD SP e
            case 0b11101000:
                ++PC;
                registers.reg_8[F] &= 0b00111111;
                SP = arithmetic_16<ADD>(SP, bus->read(PC), registers);
                ++PC;
                cycleToExecute += 3;
                break;
            //INC BC
            case 0b00000011:
                helpVariable16 = arithmetic_16<INC>(registers.BC, 1, registers);
                registers.reg_8[B] = helpVariable16 >> 8;
                registers.reg_8[C] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //INC DE
            case 0b00010011:
                helpVariable16 = arithmetic_16<INC>(registers.DE, 1, registers);
                registers.reg_8[D] = helpVariable16 >> 8;
                registers.reg_8[E] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //INC HL
            case 0b00100011:
                helpVariable16 = arithmetic_16<INC>(registers.HL, 1, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //INC SP
            case 0b00110011:
                SP = arithmetic_16<INC>(SP, 1, registers);
                ++PC;
                ++cycleToExecute;
                break;
            //DEC BC
            case 0b00001011:
                helpVariable16 = arithmetic_16<DEC>(registers.BC, 1, registers);
                registers.reg_8[B] = helpVariable16 >> 8;
                registers.reg_8[C] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //DEC DE
            case 0b00011011:
                helpVariable16 = arithmetic_16<DEC>(registers.DE, 1, registers);
                registers.reg_8[D] = helpVariable16 >> 8;
                registers.reg_8[E] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //DEC HL
            case 0b00101011:
                helpVariable16 = arithmetic_16<DEC>(registers.HL, 1, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //DEC SP
            case 0b00111011:
                SP = arithmetic_16<DEC>(SP, 1, registers);
                ++PC;
                ++cycleToExecute;
                break;


#pragma endregion 16_BIT_ARITHMETIC
#pragma region ROTATION_SHIFT
            //RLCA
            case 0b00000111:
                registers.reg_8[A] = shift_rotation<RLC>(A, registers);
                ++PC;
                break;
            //RLA
            case 0b00010111:
                registers.reg_8[A] = shift_rotation<RL>(A, registers);
                ++PC;
                break;
            //RRCA
            case 0b00001111:
                registers.reg_8[A] = shift_rotation<RRC>(A, registers);
                ++PC;
                break;
            //RRA
            case 0b00011111:
                registers.reg_8[A] = shift_rotation<RR>(A, registers);
                ++PC;
                break;
            // double line instruction
            case 0b11001011:
                ++PC;
                switch (bus->read(PC)) {
                    //RLC A
                    case 0b00000111:
                        registers.reg_8[A] = shift_rotation<RLC>(A, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC B
                    case 0b00000000:
                        registers.reg_8[B] = shift_rotation<RLC>(B, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC C
                    case 0b00000001:
                        registers.reg_8[C] = shift_rotation<RLC>(C, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC D
                    case 0b00000010:
                        registers.reg_8[D] = shift_rotation<RLC>(D, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC E
                    case 0b00000011:
                        registers.reg_8[E] = shift_rotation<RLC>(E, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC H
                    case 0b00000100:
                        registers.reg_8[H] = shift_rotation<RLC>(H, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC L
                    case 0b00000101:
                        registers.reg_8[L] = shift_rotation<RLC>(L, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC (HL)
                    case 0b00000110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<RLC>(bus->read(helpVariable16), registers), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //RL A
                    case 0b00010111:
                        registers.reg_8[A] = shift_rotation<RL>(A, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL B
                    case 0b00010000:
                        registers.reg_8[B] = shift_rotation<RL>(B, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL C
                    case 0b00010001:
                        registers.reg_8[C] = shift_rotation<RL>(C, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL D
                    case 0b00010010:
                        registers.reg_8[D] = shift_rotation<RL>(D, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL E
                    case 0b00010011:
                        registers.reg_8[E] = shift_rotation<RL>(E, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL H
                    case 0b00010100:
                        registers.reg_8[H] = shift_rotation<RL>(H, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL L
                    case 0b00010101:
                        registers.reg_8[L] = shift_rotation<RL>(L, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL (HL)
                    case 0b00010110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<RL>(bus->read(helpVariable16), registers), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //RRC A
                    case 0b00001111:
                        registers.reg_8[A] = shift_rotation<RRC>(A, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC B
                    case 0b00001000:
                        registers.reg_8[B] = shift_rotation<RRC>(B, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC C
                    case 0b00001001:
                        registers.reg_8[C] = shift_rotation<RRC>(C, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC D
                    case 0b00001010:
                        registers.reg_8[D] = shift_rotation<RRC>(D, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC E
                    case 0b00001011:
                        registers.reg_8[E] = shift_rotation<RRC>(E, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC H
                    case 0b00001100:
                        registers.reg_8[H] = shift_rotation<RRC>(H, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC L
                    case 0b00001101:
                        registers.reg_8[L] = shift_rotation<RRC>(L, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC (HL)
                    case 0b00001110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<RRC>(bus->read(helpVariable16), registers), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //RR A
                    case 0b00011111:
                        registers.reg_8[A] = shift_rotation<RR>(A, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR B
                    case 0b00011000:
                        registers.reg_8[B] = shift_rotation<RR>(B, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR C
                    case 0b00011001:
                        registers.reg_8[C] = shift_rotation<RR>(C, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR D
                    case 0b00011010:
                        registers.reg_8[D] = shift_rotation<RR>(D, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR E
                    case 0b00011011:
                        registers.reg_8[E] = shift_rotation<RR>(E, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR H
                    case 0b00011100:
                        registers.reg_8[H] = shift_rotation<RR>(H, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR L
                    case 0b00011101:
                        registers.reg_8[L] = shift_rotation<RR>(L, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR (HL)
                    case 0b00011110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<RR>(bus->read(helpVariable16), registers), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //SLA A
                    case 0b00100111:
                        registers.reg_8[A] = shift_rotation<SLA>(A, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA B
                    case 0b00100000:
                        registers.reg_8[B] = shift_rotation<SLA>(B, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA C
                    case 0b00100001:
                        registers.reg_8[C] = shift_rotation<SLA>(C, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA D
                    case 0b00100010:
                        registers.reg_8[D] = shift_rotation<SLA>(D, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA E
                    case 0b00100011:
                        registers.reg_8[E] = shift_rotation<SLA>(E, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA H
                    case 0b00100100:
                        registers.reg_8[H] = shift_rotation<SLA>(H, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA L
                    case 0b00100101:
                        registers.reg_8[L] = shift_rotation<SLA>(L, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA (HL)
                    case 0b00100110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<SLA>(bus->read(helpVariable16), registers), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //SRA A
                    case 0b00101111:
                        registers.reg_8[A] = shift_rotation<SRA>(A, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA B
                    case 0b00101000:
                        registers.reg_8[B] = shift_rotation<SRA>(B, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA C
                    case 0b00101001:
                        registers.reg_8[C] = shift_rotation<SRA>(C, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA D
                    case 0b00101010:
                        registers.reg_8[D] = shift_rotation<SRA>(D, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA E
                    case 0b00101011:
                        registers.reg_8[E] = shift_rotation<SRA>(E, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA H
                    case 0b00101100:
                        registers.reg_8[H] = shift_rotation<SRA>(H, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA L
                    case 0b00101101:
                        registers.reg_8[L] = shift_rotation<SRA>(L, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA (HL)
                    case 0b00101110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<SRA>(bus->read(helpVariable16), registers), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //SRL A
                    case 0b00111111:
                        registers.reg_8[A] = shift_rotation<SRL>(A, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRL B
                    case 0b00111000:
                        registers.reg_8[B] = shift_rotation<SRL>(B, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRL C
                    case 0b00111001:
                        registers.reg_8[C] = shift_rotation<SRL>(C, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRL D
                    case 0b00111010:
                        registers.reg_8[D] = shift_rotation<SRL>(D, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRL E
                    case 0b00111011:
                        registers.reg_8[E] = shift_rotation<SRL>(E, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRL H
                    case 0b00111100:
                        registers.reg_8[H] = shift_rotation<SRL>(H, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRL L
                    case 0b00111101:
                        registers.reg_8[L] = shift_rotation<SRL>(L, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRL (HL)
                    case 0b00111110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<SRL>(bus->read(helpVariable16), registers), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //SWAP A
                    case 0b00110111:
                        registers.reg_8[A] = shift_rotation<SRL>(A, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SWAP B
                    case 0b00110000:
                        registers.reg_8[B] = shift_rotation<SRL>(B, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SWAP C
                    case 0b00110001:
                        registers.reg_8[C] = shift_rotation<SRL>(C, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SWAP D
                    case 0b00110010:
                        registers.reg_8[D] = shift_rotation<SRL>(D, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SWAP E
                    case 0b00110011:
                        registers.reg_8[E] = shift_rotation<SRL>(E, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SWAP H
                    case 0b00110100:
                        registers.reg_8[H] = shift_rotation<SRL>(H, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SWAP L
                    case 0b00110101:
                        registers.reg_8[L] = shift_rotation<SRL>(L, registers);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SWAP (HL)
                    case 0b00110110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<SRL>(bus->read(helpVariable16), registers), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
#pragma endregion ROTATION_SHIFT
#pragma region BIT_OPERATORS
                    default:
                        switch (instruction >> 6) {
                            //BIT
                            case 0b01:
                                if (instruction & 0b111 == 0b110) {
                                    bit_operation<BIT>((instruction & 0b111000) >> 3, bus->read(registers.HL),
                                                       registers);
                                    cycleToExecute += 2;
                                } else {
                                    bit_operation<BIT>((instruction & 0b111000) >> 3,
                                                       registers.reg_8[instruction & 0b111], registers);
                                    ++cycleToExecute;
                                }
                                ++PC;
                                break;
                            //SET
                            case 0b11:
                                if (instruction & 0b111 == 0b110) {
                                    bus->write(registers.HL,
                                               bit_operation<SET>((instruction & 0b111000) >> 3,
                                                                  bus->read(registers.HL), registers));
                                    cycleToExecute += 3;
                                } else {
                                    registers.reg_8[instruction & 0b111] = bit_operation<SET>(
                                        (instruction & 0b111000) >> 3, registers.reg_8[instruction & 0b111], registers);
                                    ++cycleToExecute;
                                }
                                ++PC;
                                break;
                            //RES
                            case 0b10:
                                if (instruction & 0b111 == 0b110) {
                                    bus->write(registers.HL,
                                               bit_operation<RES>((instruction & 0b111000) >> 3,
                                                                  bus->read(registers.HL), registers));
                                    cycleToExecute += 3;
                                } else {
                                    registers.reg_8[instruction & 0b111] = bit_operation<RES>(
                                        (instruction & 0b111000) >> 3, registers.reg_8[instruction & 0b111], registers);
                                    ++cycleToExecute;
                                }
                                ++PC;
                                break;
                        }

                    // BIT b A
                }
#pragma endregion BIT_OPERATORS
#pragma region JUMP
            // JP nn
            case 0b11000011:
                PC = bus->read(++PC) << 8 | bus->read(++PC);
                cycleNumber += 3;
                break;
            // JP NZ nn
            case 0b11000010:
                helpVariable16 = bus->read(++PC) << 8 | bus->read(++PC);
                if (!registers.z) {
                    PC = helpVariable16;
                    cycleToExecute += 4;
                } else {
                    ++PC;
                    cycleToExecute += 2;
                }
                break;
            // JP Z nn
            case 0b11001010:
                helpVariable16 = bus->read(++PC) << 8 | bus->read(++PC);
                if (registers.z) {
                    PC = helpVariable16;
                    cycleToExecute += 4;
                } else {
                    ++PC;
                    cycleToExecute += 2;
                }
                break;
            // JP NC nn
            case 0b11010010:
                helpVariable16 = bus->read(++PC) << 8 | bus->read(++PC);
                if (!registers.cy) {
                    PC = helpVariable16;
                    cycleToExecute += 4;
                } else {
                    ++PC;
                    cycleToExecute += 2;
                }
                break;
            // JP C nn
            case 0b11011010:
                helpVariable16 = bus->read(++PC) << 8 | bus->read(++PC);
                if (registers.cy) {
                    PC = helpVariable16;
                    cycleToExecute += 4;
                } else {
                    ++PC;
                    cycleToExecute += 2;
                }
                break;
            // JR e
            case 0b00011000:
                PC += static_cast<int8_t>(bus->read(++PC));
                cycleNumber += 2;
                break;
            // JR NZ e
            case 0b00100000:
                helpVariable16 = PC + static_cast<int8_t>(bus->read(++PC));
                if (!registers.z) {
                    PC = helpVariable16;
                    cycleToExecute += 2;
                } else {
                    ++PC;
                    cycleToExecute += 1;
                }
                break;
            // JR Z 3
            case 0b00101000:
                helpVariable16 = PC + static_cast<int8_t>(bus->read(++PC));
                if (registers.z) {
                    PC = helpVariable16;
                    cycleToExecute += 2;
                } else {
                    ++PC;
                    cycleToExecute += 1;
                }
                break;
            // JR NC e
            case 0b00110000:
                helpVariable16 = PC + static_cast<int8_t>(bus->read(++PC));
                if (!registers.cy) {
                    PC = helpVariable16;
                    cycleToExecute += 2;
                } else {
                    ++PC;
                    cycleToExecute += 1;
                }
                break;
            // JR C 3
            case 0b00111000:
                helpVariable16 = PC + static_cast<int8_t>(bus->read(++PC));
                if (registers.cy) {
                    PC = helpVariable16;
                    cycleToExecute += 2;
                } else {
                    ++PC;
                    cycleToExecute += 1;
                }
                break;
            // JP (HL)
            case 0b11101001:
                PC = registers.HL;
                break;
#pragma endregion JUMP


            default:
                std::cout << "Invalid instruction :" << bus->read(PC) << "\n";;
        }
    } else {
        --cycleToExecute;
    }
    cycleNumber++;
}

void CPU::Cpu::executeNCycles(const uint32_t n) {
    for (uint32_t i = 0; i < n; ++i) {
        executeCycle();
    }
}

void CPU::Cpu::run() {
    while (true) {
        executeCycle();
    }
}
